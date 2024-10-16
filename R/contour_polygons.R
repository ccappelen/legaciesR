#' @title Create Contour Polygons
#' @description This function creates contour polygons from multiple overlapping polygons.

#' @param shp An sf dataframe to be aggregated. Must consist of more than one feature.
#' @param id_vars Group identifiers.
#' @param cuts Integer of length 1 specifying the number of (equally spaced) contour polygons to be returned.
#'   For example, if `cuts = 10` the function will return 10 polygons representing the 10 deciles (0-0.1, 0.1-0.2, ...).
# OLD PARAM smooth_method Character scalar specifying the smoothing method used to convert raster to polygon.
#  The options are \code{"ksmooth"} (default), \code{"chaikin"}, and \code{"spline"}.
# OLD PARAM smoothness_factor Numerical scalar indicating the amount of smoothing the \code{\link[smoothr]{smooth}} function should do.
#  Default is 2.
#' @param smoothing Logical, whether to apply smoothing after polygonizing raster. Default is `TRUE.`
#' @param res Resolution of the raster (in degrees) used to calculate polygon density. Higher resolution (i.e., lower numbers)
#'   creates smoother borders but also increases processing time. Default is 1/30 degrees.
#' @param nmap_threshold Integer indicating the number of geometries required. Default is 2. If threshold is 1, a single geometry will
#'   be returned unmodified.
#' @param invalid_geom Character scalar, how invalid geometries are handled.
#'   Can be either "none" (default), "exclude", "fix_exclude", or "fix_all". See *Invalid geometries* for details.
#' @param include_higher logical, whether the contour polygons should include percentiles above current interval.
#'  If TRUE (default), the 50 % polygon will include all areas covered by at least 50 % of the shapes
#'  (and not just within the specified interval, e.g., 0.75-1, 0.50-1, 0.25-1...). If FALSE, the polygon will include only areas
#'  within the specified interval (e.g., 0.75-1, 0.50-0.75, 0.25-0.50...).

#' @return Returns an sf dataframe with the same number of features as specified in `cuts.` The density,
#'  or percentile, is stored in the columns `prob` and `label.`

#' @section Invalid geometries:
#' The function attempts to rebuild invalid geometries using [sf::st_make_valid()]. However, some geometries may be invalid even after attempting
#' to rebuild. If "none" the function will return an error in case of invalid geometries (after attempting to rebuild). If "exclude", any invalid
#' geometries will be removed. If "fix_exclude", invalid geometries will be rebuilt using default arguments and any remaining invalid
#' geometries will be removed. If "fix_all", the function will attempt to rebuild invalid geometries by iteratively lowering the
#' snapping precision (in `s2::s2_snap_precision()`). It will return an error if it fails to rebuild valid geometries.
#' The use of "fix_all" is experimental and the result should be inspected manually.

#' @importFrom smoothr smooth
#' @import sf
#' @importFrom terra rast
#' @importFrom terra ifel
#' @importFrom terra rasterize
#' @importFrom terra as.polygons
#' @import rlang
#' @import cli
#' @import dplyr
#' @import s2

#' @export

contour_polygons <- function(shp,
                             cuts = 4,
                             id_vars,
                             # smooth_method = c("ksmooth", "chaikin", "spline"), smoothness_factor = 2,
                             res = 1/30,
                             nmap_threshold = 2,
                             smoothing = TRUE,
                             invalid_geom = c("none", "exclude", "fix_exclude", "fix_all"),
                             include_higher = TRUE) {

  # Check arguments
  if (missing(id_vars)) {
    cli::cli_abort(c(
      "Missing identifiers",
      "i" = "Must specify at least one column in {.arg id_vars}."
    ))
  }

  invalid_geom <- match.arg(invalid_geom)

  if (nrow(shp) < nmap_threshold) {
    cli::cli_abort(c(
      "Not enough features",
      "i" = "{.var shp} must contain at least as many features as specified in {.arg nmap_threshold}.
      Consider lowering the threshold."
    ))
  }

  if (sf::st_crs(shp) != sf::st_crs("EPSG:4326")) {
    shp <- shp %>%
      sf::st_transform(crs = 4326)
  }


  # Fix invalid geometries (depending on argument invalid_geom)
  if (invalid_geom == "exclude") {
    invalid <- !all(sf::st_is_valid(shp))

    if (invalid) {
      shp <- shp[sf::st_is_valid(shp), ]
      if(nrow(shp) <= nmap_threshold) {
        cli::cli_abort(c(
          "Number of valid features is less than {.arg nmap_threshold}.",
          "i" = "Consider lowering the threshold or setting {.arg invalid_geom} to 'fix_exclude' or 'fix_all'."
        ))
        }

      if (!all(sf::st_is_valid(shp))) {
        cli::cli_abort(c(
          "Unable to remove invalid features."
        ))
      }

      cli::cli_inform("Invalid geometries have been removed from the dataset.")

    } else {

      shp <- shp[sf::st_is_valid(shp), ]

    }

  }

  if (invalid_geom == "none") {
    invalid <- !all(sf::st_is_valid(shp))

    if (invalid) {
      shp <- sf::st_make_valid(shp)
      if (!all(sf::st_is_valid(shp))) {
        cli::cli_abort(c(
          "Invalid geometries",
          "i" = "{.code sf::st_make_valid()} was unable to rebuild valid geometries.
          Consider setting {.arg invalid_geom} to 'exlude, 'fix_exclude' or 'fix_all'."
        ))
      }

      cli::cli_inform(
        "Some geometries were invalid. They have been fixed using {.code sf::st_make_valid()}."
        )

    } else {

      shp <- shp

    }
  }

  if (invalid_geom == "fix_exclude") {

    invalid <- !all(sf::st_is_valid(shp))

    if (invalid) {
      shp <- sf::st_make_valid(shp)
      shp <- shp[st_is_valid(shp), ]

      if (!all(sf::st_is_valid(shp))) {
        cli::cli_abort(c(
          "Unable to remove invalid geometries",
          "i" = "Consider setting {.arg invalid_geom} to 'fix_all' instead."
        ))
      }

      cli::cli_inform(
        "Invalid geometries have been rebuilt. Geometries that are invalid after rebuilding have been removed."
        )

    } else {

      shp <- shp

    }
  }


  if (invalid_geom == "fix_all") {

    invalid <- !all(sf::st_is_valid(shp))

    if (invalid) {

      shp <- fix_invalid(shp = shp,
                         max_precision = 10^7,
                         min_precision = 10,
                         stop_if_invalid = F,
                         progress = F,
                         parallel = F,
                         report = F,
                         reportColumns = F)

      shp_invalid <- !all(sf::st_is_valid(shp))

      if (shp_invalid == TRUE) {
        cli::cli_abort(c(
          "Invalid geometries",
          "i" = "{.code sf::st_make_valid()} failed to rebuild valid geometries within acceptable snapping precision.",
          "i" = "Likely caused by severe edge errors in geometry that should be identified and corrected manually."
        ))
      }

      cli::cli_inform(
        "Some geometries are invalid. They have been fixed using sf::st_make_valid(). Consider checking spatial features before continuing."
        )

    } else {

      shp <- shp

    }

  }


  # Make contours
  cuts <- cuts
  n_maps <- nrow(shp)

  ids <- shp[1,] |>
    sf::st_drop_geometry() |>
    select({{ id_vars }})

  if (n_maps == 1) {

    shp_smoothed <- shp |>
      dplyr::mutate(prob = 1) |>
      dplyr::mutate(label = "0-1") |>
      cbind(ids) |>
      dplyr::mutate(nmaps = n_maps) |>
      dplyr::select({{ id_vars }}, "prob", "label", "nmaps")

  } else {

    if (cuts > n_maps - 1) {
      cuts <- n_maps - 1
    }

    shp_union <- sf::st_union(shp) |>
      sf::st_as_sf() |>
      suppressMessages()

    r <- terra::rast(shp_union, resolution = res)
    r_count <- terra::rasterize(shp, r, fun = "count")
    r_prob <- r_count / n_maps

    if (missing(cuts)) {
      cli::cli_abort(c(
        "Please specify the number of cuts. For example, if the polygons should be split into deciles, the number of cuts should be 10 (for quartiles it should be 4)."
      ))
    }

    cut_seq <- seq(from = 0, to = 1, length.out = cuts+1)[-1]
    cut_interval <- 1/cuts
    max_cut <- max(cut_seq)

    ## If there are no grid cells with probability in a certain cut range (most likely the uppermost range), remove this from cut_seq
    cut_seq <- cut_seq[!cut_seq - cut_interval >= terra::values(r_prob) %>% max(na.rm = T)]

    if (include_higher) {

      shp_smoothed <- lapply(
        cut_seq,
        FUN = function(x){
          terra::ifel(r_prob > max_cut - x & r_prob <= max_cut, 1, NA) |>
            terra::as.polygons() |>
            sf::st_as_sf() |>
            {\(.) if (smoothing) smoothr::smooth(.) else . }() |>
            # {if(smoothing) smoothr::smooth(_) else _ } |>  # method = smooth_method, smoothness = smoothness_factor
            dplyr::mutate(prob = round(max_cut - x, 2)) |>
            dplyr::mutate(label = paste0(round(max_cut - x, 2), "-", max_cut)) |>
            cbind(ids) |>
            dplyr::mutate(nmaps = n_maps) |>
            dplyr::select({{ id_vars }}, "prob", "label", "nmaps")
        }) |>
        dplyr::bind_rows() |>
        dplyr::arrange(prob)

    } else {

      shp_smoothed <- lapply(
        cut_seq,
        FUN = function(x){
          terra::ifel(r_prob > x - cut_interval & r_prob <= x, 1, NA) |>
            terra::as.polygons() |>
            sf::st_as_sf() |>
            {\(.) if (smoothing) smoothr::smooth(.) else . }() |>
            # {if(smoothing) smoothr::smooth(_) else _ } |>  # method = smooth_method, smoothness = smoothness_factor
            dplyr::mutate(prob = round(x, 2)) |>
            dplyr::mutate(label = paste0(round(x-cut_interval, 2), "-", x)) |>
            cbind(ids) |>
            dplyr::mutate(nmaps = n_maps) |>
            dplyr::select({{ id_vars }}, "prob", "label", "nmaps")
        }) |>
        dplyr::bind_rows()

    }

  }

  if (!all(sf::st_is_valid(shp_smoothed))) {
    cli::cli_warn(c(
      "Invalid geometries",
      "i" = "Some contour polygons may not be valid geometries. If you encounter invalid geometries,
      consider lowering resolution or use fewer cuts."
    ))
  }

  return(shp_smoothed)

}
