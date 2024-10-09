#' @title Create Contour Polygons
#' @description This function creates contour polygons from multiple overlapping polygons.
#' @param shp The SF PolygonsDataFrame to be aggregated. Must consist of more than one feature.
#' @param id_vars Group identifiers
#' @param cuts An integer of length 1 specifying the number of (equally spaced) contour polygons to be returned.
#'  For example, if \code{cuts = 10} the function will return 10 polygons representing the 10 deciles (0-0.1,0.1-0.2,...).
# OLD PARAM smooth_method Character scalar specifying the smoothing method used to convert raster to polygon.
#  The options are \code{"ksmooth"} (default), \code{"chaikin"}, and \code{"spline"}.
# OLD PARAM smoothness_factor Numerical scalar indicating the amount of smoothing the \code{\link[smoothr]{smooth}} function should do.
#  Default is 2.
#' @param smoothing logical, whether to apply smoothing after polygonizing raster. Default is TRUE.
#' @param res Resolution of the raster used to calculate polygon density. Higher resolution (i.e., lower numbers)
#'  creates smoother borders but also increases processing time. Default is 1/30 degrees.
#' @param nmap_threshold scalar, indicating the number of geometries required. Default is 2. If threshold is 1, a single geometry will
#' be returned unmodified.
#' @param invalid_geom One of "none" (default), "exclude", "fix_exclude", or "fix_all". The function attempts to rebuild invalid geometries
#'  using sf::st_make_valid(shp). However, some geometries may be invalid even after attempting to rebuild. If "none" the function will
#'  return an error in case of invalid geometries (after attempting to rebuild). If "exclude", any invalid geometries will be removed.
#'  If "fix_exclude", invalid geometries will be rebuilt using default arguments and any remaining invalid
#'  geometries will be removed. If "fix_all", the function will attempt to rebuild invalid geometries by
#'  iteratively lowering the snappingnprecision (in \code{s2::s2_snap_precision()}). It will return an error
#'  if it fails to rebuild valid geometries. The use of "fix_all" is experimental and the result should be
#'  inspected manually.
#' @param include_higher logical, whether the contour polygons should include percentiles above current interval.
#'  If TRUE (default), the 50 % polygon will include all areas covered by at least 50 % of the shapes
#'  (and not just within the specified interval, e.g., 0.75-1, 0.50-1, 0.25-1...). If FALSE, the polygon will include only areas
#'  within the specified interval (e.g., 0.75-1, 0.50-0.75, 0.25-0.50...).
#' @return Returns an SF PolygonsDataFrame with the same number of features as specified in *cuts*. The density,
#'  or percentile, is stored in the attributes \code{"prob"} and \code{"label"}.
#' @importFrom smoothr smooth
#' @import sf
#' @importFrom terra rast
#' @importFrom terra ifel
#' @importFrom terra rasterize
#' @importFrom terra as.polygons
#' @importFrom rlang .data
#' @import dplyr
#' @import s2
#' @export

## RETURN ORIGINAL DATAFRAME (INCL. YEAR IF SEVERAL PERIODS, PERHAPS IN OTHER FUNCTION)
## RETURN DATA FRAME (CONDITION ON KEEP_VARS)

contour_polygons <- function(shp, cuts = 4,
                             id_vars,
                             # smooth_method = c("ksmooth", "chaikin", "spline"), smoothness_factor = 2,
                             res = 1/30, nmap_threshold = 2,
                             smoothing = TRUE,
                             invalid_geom = c("none", "exclude", "fix_exclude", "fix_all"),
                             include_higher = TRUE){

  if(missing(id_vars)){
    stop("Please specify at least one column in id_vars.")
  }

  # smooth_method <- match.arg(smooth_method)
  invalid_geom <- match.arg(invalid_geom)

  if(nrow(shp) < nmap_threshold) stop("Not enough geometries. Consider using lower threshold.")

  if(sf::st_crs(shp) != sf::st_crs("EPSG:4326")){
    shp <- shp %>%
      sf::st_transform(crs = 4326)
  }


  if(invalid_geom == "exclude"){

    invalid <- !all(sf::st_is_valid(shp))

    if(invalid){
      shp <- shp[sf::st_is_valid(shp), ]
      if(nrow(shp) <= 1) stop("Less than two valid geometries.")
      if(!all(sf::st_is_valid(shp))) stop("Invalid features, unable to remove")
      warning("Invalid geometries have been removed from the dataset.")
    }else{
      shp <- shp[sf::st_is_valid(shp), ]
    }

  }

  if(invalid_geom == "none"){

    invalid <- !all(sf::st_is_valid(shp))

    if(invalid){
      shp <- sf::st_make_valid(shp)
      if(!all(sf::st_is_valid(shp))){
        stop("Invalid geometries: sf::st_make_valid() was unable to rebuild valid geometries.")
      }
      warning("Some geometries are invalid. They have been fixed using sf::st_make_valid(). Consider checking spatial features before continuing.")
    }else{
      shp <- shp
    }
  }

  if(invalid_geom == "fix_exclude"){

    invalid <- !all(sf::st_is_valid(shp))

    if(invalid){
      shp <- sf::st_make_valid(shp)
      shp <- shp[st_is_valid(shp), ]
      if(!all(sf::st_is_valid(shp))) stop("Invalid features, unable to remove")
      warning("Invalid geometries have been rebuilt. Geometries unable to rebuild have been removed.")
    }else{
      shp <- shp
    }
  }

  if(invalid_geom == "fix_all"){

    invalid <- !all(sf::st_is_valid(shp))

    if(invalid){

      shp <- fix_invalid(shp, max_precision = 10^7, min_precision = 10,
                         stop_if_invalid = F, progress = F, parallel = F, report = F, reportColumns = F)
      shp_invalid <- !all(sf::st_is_valid(shp))
      if(shp_invalid == TRUE){
        stop("Invalid geometries: sf::st_make_valid() failed to rebuild valid geometries withint acceptable snapping precision.")
      }

      # shp <- sf::st_make_valid(shp)
      # shp_invalid <- !all(sf::st_is_valid(shp))
      # snap_prec <- 10^7
      #
      # while(shp_invalid == TRUE & snap_prec >= 10){
      #
      #   shp <- st_make_valid(shp, s2_options = s2::s2_options(snap = s2::s2_snap_precision(snap_prec)))
      #   shp_invalid <- !all(st_is_valid(shp))
      #   snap_prec <- ifelse(snap_prec > 100, snap_prec / 10, snap_prec - 10)
      # }
      #
      # if(shp_invalid == TRUE) stop("Invalid geometries: sf::st_make_valid() failed to rebuild
      #                                valid geometries withint acceptable snapping precision.")

      warning("Some geometries are invalid. They have been fixed using sf::st_make_valid(). Consider checking spatial features before continuing.")
    }else{
      shp <- shp
    }

  }



  #
  # if(!all(sf::st_is_valid(shp))){
  #
  #   ## Option invalid_geom == "exclude"
  #   if(invalid_geom == "exclude"){
  #
  #     shp <- shp[sf::st_is_valid(shp), ]
  #     if(nrow(shp) <= 1) stop("Less than two valid geometries.")
  #     if(!all(sf::st_is_valid(shp))) stop("Invalid features, unable to remove")
  #
  #     warning("Invalid geometries have been removed from the dataset.")
  #
  #     }else{
  #
  #     shp <- sf::st_make_valid(shp)
  #     warning("Some geometries are invalid. They have been fixed using sf::st_make_valid().
  #           Consider checking spatial features before continuing.")
  #
  #   }
  #
  #
  #   if(!all(sf::st_is_valid(shp))){
  #
  #     ## Option invalid_geom == "none"
  #     if(invalid_geom == "none"){
  #
  #       stop("Invalid geometries: sf::st_make_valid() was unable to rebuild valid geometries.")
  #
  #     }
  #
  #     ## Option invalid_geom == "fix_exclude"
  #     if(invalid_geom == "fix_exclude"){
  #
  #       shp <- shp[st_is_valid(shp), ]
  #       if(!all(sf::st_is_valid(shp))) stop("Invalid features, unable to remove")
  #
  #       warning("Invalid geometries have been removed from the dataset.")
  #     }
  #
  #     ## Option invalid_geom == "fix_all"
  #     if(invalid_geom == "fix_all"){
  #
  #       shp_invalid <- TRUE
  #       snap_prec <- 10000000
  #
  #       while(shp_invalid == TRUE & snap_prec >= 10){
  #
  #         shp <- st_make_valid(shp, s2_options = s2::s2_options(snap = s2::s2_snap_precision(snap_prec)))
  #         shp_invalid <- !all(st_is_valid(shp))
  #         if(snap_prec < 100){
  #           snap_prec <- snap_prec * 0.9
  #         }else{
  #           snap_prec <- snap_prec * 0.5
  #         }
  #
  #       }
  #
  #       if(shp_invalid == TRUE) stop("Invalid geometries: sf::st_make_valid() failed to rebuild
  #                                    valid geometries withint acceptable snapping precision.")
  #
  #       warning("Some geometries are invalid. They have been fixed using sf::st_make_valid().
  #           Consider checking spatial features before continuing.")
  #
  #     }
  #
  #   }
  #
  # }


  cuts <- cuts
  n_maps <- nrow(shp)

  ids <- shp[1,] %>%
    sf::st_drop_geometry() %>%
    select({{ id_vars }})


  if(n_maps == 1){

    shp_smoothed <- shp %>%
      dplyr::mutate(cut = 1) %>%
      dplyr::mutate(prob_interval = "0-1") %>%
      cbind(ids) %>%
      dplyr::mutate(nmaps = n_maps) %>%
      dplyr::select({{ id_vars }}, "cut", "prob_interval", "nmaps")

  }else{

    if(cuts > n_maps - 1){
      cuts <- n_maps - 1
    }

    shp_union <- sf::st_union(shp) %>%
      sf::st_as_sf() %>%
      suppressMessages()

    r <- terra::rast(shp_union, resolution = res)
    r_count <- terra::rasterize(shp, r, fun = "count")
    r_prob <- r_count / n_maps

    if(missing(cuts)){
      stop("Error: Please specify the number of cuts. For example, if the polygons should be split into deciles, the number of cuts should be 10 (for quartiles it should be 4).")
    }

    # keep_vars <- shp[1,] %>%
    #   sf::st_drop_geometry() %>%
    #   dplyr::select({{ id_vars }})

    cut_seq <- seq(from = 0, to = 1, length.out = cuts+1)[-1]
    cut_interval <- 1/cuts

    cut_seq <- cut_seq[!cut_seq - cut_interval >= terra::values(r_prob) %>% max(na.rm = T)]

    max_cut <- max(cut_seq)

    if(include_higher){

      shp_smoothed <- lapply(
        cut_seq,
        FUN = function(x){
          terra::ifel(r_prob > max_cut - x & r_prob <= max_cut, 1, NA) %>%
            terra::as.polygons() %>%
            sf::st_as_sf() %>%
            {if(smoothing) smoothr::smooth(.) else . } %>% # method = smooth_method, smoothness = smoothness_factor
            dplyr::mutate(cut = round(max_cut - x, 2)) %>%
            dplyr::mutate(prob_interval = paste0(round(max_cut - x, 2), "-", max_cut)) %>%
            cbind(ids) %>%
            dplyr::mutate(nmaps = n_maps) %>%
            dplyr::select({{ id_vars }}, "cut", "prob_interval", "nmaps")
        }) %>%
        dplyr::bind_rows()

    }else{

      shp_smoothed <- lapply(
        cut_seq,
        FUN = function(x){
          terra::ifel(r_prob > x - cut_interval & r_prob <= x, 1, NA) %>%
            terra::as.polygons() %>%
            sf::st_as_sf() %>%
            {if(smoothing) smoothr::smooth(.) else . } %>% # method = smooth_method, smoothness = smoothness_factor
            dplyr::mutate(cut = round(x, 2)) %>%
            dplyr::mutate(prob_interval = paste0(round(x-cut_interval, 2), "-", x)) %>%
            cbind(ids) %>%
            dplyr::mutate(nmaps = n_maps) %>%
            dplyr::select({{ id_vars }}, "cut", "prob_interval", "nmaps")
        }) %>%
        dplyr::bind_rows()

    }

  }


  # if(!all(sf::st_is_valid(shp_smoothed))){
  #   shp_smoothed <- sf::st_make_valid(shp_smoothed)
  # }

  if(!all(sf::st_is_valid(shp_smoothed))) warning("Some contour polygons may not be valid geometries. Consider lower resolution or fewer cuts.")

  return(shp_smoothed)
}
