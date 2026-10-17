#' @title Create Contour Polygons
#' @description This function creates contour polygons from multiple overlapping polygons.

#' @param shp An sf dataframe to be aggregated. Must consist of more than one feature.
#' @param id_vars Group identifiers.
#' @param n.breaks Integer of length 1 specifying the number of intervals. If `breaks` is not specified, the intervals
#' will be evenly spaced. For example, if `n.breaks` is 4, the intervals will be 0-0.25, 0.25-0.5, 0.5-0.75, 0.75-1.
#' @param breaks Numeric vector specifying the interval breaks. Include only the inner break values,
#' not the outer values of 0 and 1. For instance, if the desired intervals are 0-0.15, 0.15-0.75, and 0.75-1,
#' `breaks` should be `c(0.15, 0.75)`. `breaks` supersedes `n.breaks`.
#' @param label.breaks Character vector with optional labels for each interval. Has to be same length as `n.breaks`
#' (or one more than the length of `breaks`).
# @param cuts Integer of length 1 specifying the number of (equally spaced) contour polygons to be returned.
#   For example, if `cuts = 10` the function will return 10 polygons representing the 10 deciles (0-0.1, 0.1-0.2, ...).
# OLD PARAM smooth_method Character scalar specifying the smoothing method used to convert raster to polygon.
#  The options are \code{"ksmooth"} (default), \code{"chaikin"}, and \code{"spline"}.
# OLD PARAM smoothness_factor Numerical scalar indicating the amount of smoothing the \code{\link[smoothr]{smooth}} function should do.
#  Default is 2.
#' @param smoothing Logical, whether to apply smoothing after polygonizing raster. Default is `TRUE.`
#' @param res Resolution of the raster (in degrees) used to calculate polygon density. Higher resolution (i.e., lower numbers)
#'   creates smoother borders but also increases processing time. Default is 1/30 degrees.
#' @param nmap_threshold Integer indicating the number of geometries required. Default is 2. If threshold is 1, a single geometry will
#'   be returned unmodified.
# #' @param invalid_geom Character scalar, how invalid geometries are handled.
# #'   Can be either "none" (default), "exclude", "fix_exclude", or "fix_all". See *Invalid geometries* for details.
#' @param invalid_geom Character, how to handle invalid geometries. If `"stop"`, returns an error
#'   if there are invalid geometries; if `"exclude"`, invalid geometries will be removed before
#'   calculating contours; if `"fix"`, invalid geometries will be rebuilt using [fix_invalid].
#'   It is recommended to check for invalid geometries and run [fix_invalid] separately.
#' @param include_higher Logical, whether the contour polygons should include percentiles above current interval.
#'   If TRUE (default), the 50 % polygon will include all areas covered by at least 50 % of the shapes
#'   (and not just within the specified interval, e.g., 0.75-1, 0.50-1, 0.25-1...). If FALSE, the polygon will include only areas
#'   within the specified interval (e.g., 0.75-1, 0.50-0.75, 0.25-0.50...).
#' @param exclude_lower_bound Numeric, if specified, the areas of least coverage will be excluded before generating the
#'   contour polygons. Must be a number between 0 and 1. For example, 0.05 means that areas with less than 5 percent coverage
#'   will be excluded from the contour polygons. Default is NA, meaning all areas are included.
# @param cut3 Numeric vector of length 2. Instead of equal range cuts specified with `cuts`, this will result in 3 contour
#   polygons corresponding to periphery, intermediate, and core separated by the cut values. This will overwrite the `cuts``` option.
#   Default is NA, which results in the equal range cuts being used.
#'

#' @return Returns an sf dataframe with the same number of features as specified in `cuts.` The density,
#'  or percentile, is stored in the columns `prob` and `label.`

# #' @section Invalid geometries:
# #' The function attempts to rebuild invalid geometries using [sf::st_make_valid()]. However, some geometries may be invalid even after attempting
# #' to rebuild. If "none" the function will return an error in case of invalid geometries (after attempting to rebuild). If "exclude", any invalid
# #' geometries will be removed. If "fix_exclude", invalid geometries will be rebuilt using default arguments and any remaining invalid
# #' geometries will be removed. If "fix_all", the function will attempt to rebuild invalid geometries by iteratively lowering the
# #' snapping precision (in `s2::s2_snap_precision()`). It will return an error if it fails to rebuild valid geometries.
# #' The use of "fix_all" is experimental and the result should be inspected manually.

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
                             n.breaks = 4,
                             breaks = NULL,
                             label.breaks = NULL,
                             id_vars,
                             # smooth_method = c("ksmooth", "chaikin", "spline"), smoothness_factor = 2,
                             res = 1/30,
                             nmap_threshold = 2,
                             smoothing = TRUE,
                             invalid_geom = c("stop", "fix", "exclude"),
                             include_higher = TRUE,
                             exclude_lower_bound = NA) {

  prob <- NULL

  # Check arguments
  if (missing(id_vars)) {
    cli::cli_abort(c(
      "Missing identifiers",
      "i" = "Must specify at least one column in {.arg id_vars}."
    ))
  }

  invalid_geom <- match.arg(invalid_geom)

  if (sf::st_crs(shp) != sf::st_crs("EPSG:4326")) {
    shp <- shp %>%
      sf::st_transform(crs = 4326)
  }

  # Check for invalid geometries
  invalid <- any(!st_is_valid(shp))

  if (invalid_geom == "stop" && invalid) {
    cli::cli_abort(c(
      "Invalid geometries",
      "x" = "{.arg shp} contains invalid geometries.",
      "i" = "Consider setting {.arg invalid_geom} to 'fix' or 'exclude'."
    ))
  }

  if (invalid_geom == "exclude") {
    shp <- shp[sf::st_is_valid(shp),]
  }

  if (invalid_geom == "fix") {
    shp <- fix_invalid(shp, progress = FALSE, parallel = FALSE,
                       report = FALSE, reportColumns = FALSE)
  }



  # Check number of maps
  if (nrow(shp) < nmap_threshold) {
    cli::cli_abort(c(
      "Not enough features",
      "i" = "{.var shp} must contain at least as many features as specified in {.arg nmap_threshold}.
      Consider lowering the threshold."
    ))
  }

  #==========================
  # MAKE CONTOURS
  #==========================

  ## Count number of geometries
  n_maps <- nrow(shp)

  ## Extract geometry columns
  ids <- shp[1,] |>
    sf::st_drop_geometry() |>
    dplyr::select({{ id_vars }})



  ## Specify cuts
  if (!is.null(breaks)) {
    if (!is.numeric(breaks) | length(breaks) < 2) {
      cli::cli_abort("{.arg breaks} must be a numeric vector of length >= 2")
    }

    if (any(breaks %in% c(0,1))) {
      cli::cli_abort("Only include the inner break point in {.arg breaks}, i.e. do not include 0 and 1 as the endpoints.")
    }

    n.breaks <- length(breaks) + 1
    breaks <- c(0, breaks, 1)

  } else {

    if (is.null(n.breaks)) {
      cli::cli_abort("Either {.arg n.breaks} or {.arg breaks} must be specified.")
    }

    n.breaks <- n.breaks
    # Create equal interval breaks based on n.breaks
    breaks <- seq(0, 1, 1/n.breaks)

  }

  ## Exclude areas with probability less than specified value
  if (!is.na(exclude_lower_bound)) {
    if (exclude_lower_bound <= 0 || exclude_lower_bound > 1) {
      cli::cli_abort("{.arg exclude_lower_bound} must be a value between 0 and 1.")
    }

    if (exclude_lower_bound >= breaks[2]) {
      cli::cli_abort("{.arg exclude_lower_bound} must be less than the first interval break. If you specified {.arg n.breaks},
                     the lower bound must be less than 1/{.arg n.breaks}.")
    }

    # r_prob <- terra::ifel(r_prob < exclude_lower_bound, 0, r_prob)
    breaks[1] <- exclude_lower_bound
  }

  breaks.cat <- seq_len(n.breaks)
  breaks.mat <- cbind(from = breaks[-length(breaks)],
                      to = breaks[-1],
                      becomes = breaks.cat)

  # Create labels if none specified
  if (!is.null(label.breaks)) {
    if (length(label.breaks) != n.breaks || !is.character(label.breaks)) {
      cli::cli_abort("{.arg label.breaks} must be a character vector of same length as {.arg n.breaks}")
    }
  } else {
    label.breaks <- if(include_higher) paste0(round(breaks[-length(breaks)], 2), "-", 1) else paste0(round(breaks[-length(breaks)], 2), "-", round(breaks[-1], 2))
  }



  ## Create raster
  shp_union <- sf::st_union(shp) |>
    sf::st_as_sf() |>
    suppressMessages()
  r <- terra::rast(shp_union, resolution = res)
  r_count <- terra::rasterize(shp, r, fun = "count")
  r_prob <- r_count / n_maps




  ## Reclassify raster probabilities
  r_cat <- terra::classify(r_prob, breaks.mat, include.lowest = FALSE, right = TRUE, others = NA)



  ## Create contour polygons
  if (n_maps > 1) {

    if (include_higher) {

      shp_smoothed <- lapply(
        breaks.cat,
        FUN = function(x){
          if (!any(terra::values(r_cat) >= x, na.rm = TRUE)) return(NULL)
          terra::ifel(r_cat >= x, 1, NA) |>
            terra::as.polygons() |>
            sf::st_as_sf() |>
            {\(.) if (smoothing) smoothr::smooth(.) else . }() |>
            # {if(smoothing) smoothr::smooth(_) else _ } |>  # method = smooth_method, smoothness = smoothness_factor
            dplyr::mutate(prob = round(breaks[x], 2)) |>
            dplyr::mutate(label = label.breaks[x]) |>
            cbind(ids) |>
            dplyr::mutate(nmaps = n_maps) |>
            dplyr::select({{ id_vars }}, "prob", "label", "nmaps") |>
            sf::st_cast("MULTIPOLYGON")
        }) |>
        dplyr::bind_rows()

    } else {

      shp_smoothed <- lapply(
        breaks.cat,
        FUN = function(x){
          if (!any(terra::values(r_cat) == x, na.rm = TRUE)) return(NULL)
          terra::ifel(r_cat == x, 1, NA) |>
            terra::as.polygons() |>
            sf::st_as_sf() |>
            {\(.) if (smoothing) smoothr::smooth(.) else . }() |>
            # {if(smoothing) smoothr::smooth(_) else _ } |>  # method = smooth_method, smoothness = smoothness_factor
            dplyr::mutate(prob = round(breaks[x], 2)) |>
            dplyr::mutate(label = label.breaks[x]) |>
            cbind(ids) |>
            dplyr::mutate(nmaps = n_maps) |>
            dplyr::select({{ id_vars }}, "prob", "label", "nmaps") |>
            sf::st_cast("MULTIPOLYGON")
        }) |>
        dplyr::bind_rows()

    }

  } else {

    shp_smoothed <- shp |>
      sf::st_union() |>
      sf::st_collection_extract("POLYGON") |>
      sf::st_as_sf() |>
      dplyr::rename(geometry = x) |>
      {\(.) if (smoothing) smoothr::smooth(.) else . }() |>
      dplyr::mutate(prob = 0) |>
      dplyr::mutate(label = "0-1") |>
      cbind(ids) |>
      dplyr::mutate(nmaps = n_maps) |>
      dplyr::select({{ id_vars }}, "prob", "label", "nmaps")

  }






  #
  #
  # if (n_maps == 1) {
  #
  #   shp_smoothed <- shp |>
  #     sf::st_union() |>
  #     sf::st_collection_extract("POLYGON") |>
  #     sf::st_as_sf() |>
  #     dplyr::rename(geometry = x) |>
  #     {\(.) if (smoothing) smoothr::smooth(.) else . }() |>
  #     dplyr::mutate(prob = 1) |>
  #     dplyr::mutate(label = "0-1") |>
  #     cbind(ids) |>
  #     dplyr::mutate(nmaps = n_maps) |>
  #     dplyr::select({{ id_vars }}, "prob", "label", "nmaps")
  #
  # } else {
  #
  #   if (!missing(cuts)) {
  #     if (!is.numeric(cuts) | length(cuts) < 2) {
  #       cli::cli_stop("{.arg cuts} must be a numeric vector of length >= 2")
  #     }
  #
  #     ncuts <- length(cuts) + 1
  #
  #   } else {
  #     ncuts <- ncuts
  #     cut_seq <- seq(0, 1, 1/ncuts)[-c(1, ncuts + 1)]
  #
  #     if (ncuts > n_maps - 1) {
  #       cli::cli_alert_warning("Fewer maps than {.arg ncuts} specified for id_var = {ids}. This will result in fewer contour polygons than the specified number (cuts).
  #                       If you prefer to exclude those cases, you can set {.arg threshold_exclude} = TRUE.", wrap = TRUE)
  #       ncuts <- n_maps - 1
  #     }
  #   }
  #
  #   shp_union <- sf::st_union(shp) |>
  #     sf::st_as_sf() |>
  #     suppressMessages()
  #
  #   r <- terra::rast(shp_union, resolution = res)
  #   r_count <- terra::rasterize(shp, r, fun = "count")
  #   r_prob <- r_count / n_maps
  #
  #   if (!is.na(exclude_lower_bound)) {
  #     if (exclude_lower_bound <= 0 || exclude_lower_bound > 1) {
  #       cli::cli_abort("{.arg exclude_lower_bound} must be a value between 0 and 1.")
  #     }
  #
  #     r_prob <- terra::ifel(r_prob < exclude_lower_bound, 0, r_prob)
  #   }
  #
  #   # if (any(!is.na(cut3))) {
  #   #   if (!is.numeric(cut3)) {
  #   #     cli::cli_abort("{.arg cut3} must be a numeric vector of length two with values between 0 and 1
  #   #                    specifying the two cuts separating periphery, intermediate, and core areas.")
  #   #   }
  #   #
  #   #   if (length(cut3) != 2) {
  #   #     cli::cli_abort("{.arg cut3} must be a numeric vector of length two with values between 0 and 1
  #   #                    specifying the two cuts separating periphery, intermediate, and core areas.")
  #   #   }
  #
  #     cuts <- 3
  #     # r_prob <- terra::ifel(r_prob < cut3[1] & r_prob > 0, 1/3 - 0.01, r_prob)
  #     # r_prob <- terra::ifel(r_prob >= cut3[1] & r_prob < cut3[2], 2/3 - 0.01, r_prob)
  #     # r_prob <- terra::ifel(r_prob >= cut3[2], 1 - 0.01, r_prob)
  #
  #     m <- matrix(c(0,cut3[1], 1/3-0.01,
  #                   cut3[1], cut3[2], 2/3-0.01,
  #                   cut3[2], 1, 1 - 0.01),
  #                 ncol = 3, byrow = TRUE)
  #     r_prob <- terra::classify(r_prob, m, include.lowest = FALSE, right = TRUE)
  #
  #   }
  #
  #
  #   if (missing(cuts)) {
  #     cli::cli_abort(c(
  #       "Please specify the number of cuts. For example, if the polygons should be split into deciles, the number of cuts should be 10 (for quartiles it should be 4)."
  #     ))
  #   }
  #
  #   cut_seq <- seq(from = 0, to = 1, length.out = cuts+1)[-1]
  #   cut_interval <- 1/cuts
  #
  #   ## If there are no grid cells with probability in a certain cut range (most likely the uppermost range), remove this from cut_seq
  #   # cut_seq <- cut_seq[!cut_seq - cut_interval >= terra::values(r_prob) %>% max(na.rm = T)]
  #   r_vals <- terra::values(r_prob) |> as.numeric()
  #   r_vals <- r_vals[!is.na(r_vals)]
  #   cut_seq_remove <- sapply(seq_len(length(cut_seq)), FUN = function(x) {
  #     if (x == 1) {
  #       res <- any(r_vals > 0 & r_vals <= cut_seq[x])
  #       if (!res) return(x)
  #     } else {
  #       res <- any(r_vals > cut_seq[x-1] & r_vals <= cut_seq[x])
  #       if (!res) return(x)
  #     }
  #   }) |> unlist()
  #
  #   if (!include_higher && any(cut_seq_remove)) {
  #     cut_seq_orig <- cut_seq
  #     cut_seq <- cut_seq[-cut_seq_remove]
  #   }
  #
  #   if (include_higher && max(cut_seq_remove) == length(cut_seq)) {
  #     cut_seq <- cut_seq[!cut_seq - cut_interval >= terra::values(r_prob) %>% max(na.rm = T)]
  #   }
  #
  #   max_cut <- max(cut_seq)
  #
  #   if (include_higher) {
  #
  #     shp_smoothed <- lapply(
  #       cut_seq,
  #       FUN = function(x){
  #         terra::ifel(r_prob > max_cut - x & r_prob <= max_cut, 1, NA) |>
  #           terra::as.polygons() |>
  #           sf::st_as_sf() |>
  #           {\(.) if (smoothing) smoothr::smooth(.) else . }() |>
  #           # {if(smoothing) smoothr::smooth(_) else _ } |>  # method = smooth_method, smoothness = smoothness_factor
  #           dplyr::mutate(prob = round(max_cut - x, 2)) |>
  #           dplyr::mutate(label = paste0(round(max_cut - x, 2), "-", round(max_cut, 2))) |>
  #           cbind(ids) |>
  #           dplyr::mutate(nmaps = n_maps) |>
  #           dplyr::select({{ id_vars }}, "prob", "label", "nmaps")
  #       }) |>
  #       dplyr::bind_rows() |>
  #       dplyr::arrange(prob)
  #
  #     if (any(!is.na(cut3))) {
  #       shp_smoothed <- shp_smoothed |>
  #         dplyr::mutate(prob = dplyr::case_when(
  #           prob == round(0,2) ~ cut3[1],
  #           prob == round(cut_seq[1],2) ~ cut3[2],
  #           prob == round(cut_seq[2],2) ~ 1.00
  #         )) |>
  #         dplyr::mutate(label = dplyr::case_when(
  #           prob == cut3[1] ~ "Periphery",
  #           prob == cut3[2] ~ "Intermediate",
  #           prob == 1.00 ~ "Core"
  #         ))
  #     }
  #
  #   } else {
  #
  #     shp_smoothed <- lapply(
  #       cut_seq,
  #       FUN = function(x){
  #         terra::ifel(r_prob > x - cut_interval & r_prob <= x, 1, NA) |>
  #           terra::as.polygons() |>
  #           sf::st_as_sf() |>
  #           {\(.) if (smoothing) smoothr::smooth(.) else . }() |>
  #           # {if(smoothing) smoothr::smooth(_) else _ } |>  # method = smooth_method, smoothness = smoothness_factor
  #           dplyr::mutate(prob = round(x, 2)) |>
  #           dplyr::mutate(label = paste0(round(x-cut_interval, 2), "-", round(x, 2))) |>
  #           cbind(ids) |>
  #           dplyr::mutate(nmaps = n_maps) |>
  #           dplyr::select({{ id_vars }}, "prob", "label", "nmaps")
  #       }) |>
  #       dplyr::bind_rows()
  #
  #     if (any(!is.na(cut3))) {
  #       shp_smoothed <- shp_smoothed |>
  #         dplyr::mutate(prob = dplyr::case_when(
  #           prob == round(cut_seq[1],2) ~ cut3[1],
  #           prob == round(cut_seq[2],2) ~ cut3[2],
  #           prob == round(cut_seq[3],2) ~ 1.00
  #         )) |>
  #         dplyr::mutate(label = dplyr::case_when(
  #           prob == cut3[1] ~ "Periphery",
  #           prob == cut3[2] ~ "Intermediate",
  #           prob == 1.00 ~ "Core"
  #         ))
  #     }
  #
  #   }
  #
  # }


  if (!all(sf::st_is_valid(shp_smoothed))) {
    cli::cli_warn(c(
      "Invalid geometries",
      "i" = "Some contour polygons may not be valid geometries. If you encounter invalid geometries,
      consider lowering resolution or use fewer cuts."
    ))
  }

  return(shp_smoothed)

}
