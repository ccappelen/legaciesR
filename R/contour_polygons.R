#' @title Create Contour Polygons
#' @description This function creates contour polygons from multiple overlapping polygons.
#' @param shp The SF PolygonsDataFrame to be aggregated. Must consist of more than one feature.
#' @param cuts An integer of length 1 specifying the number of (equally spaced) contour polygons to be returned.
#'  For example, if \code{cuts = 10} the function will return 10 polygons representing the 10 deciles (0-0.1,0.1-0.2,...).
#' @param smooth_method Character scalar specifying the smoothing method used to convert raster to polygon.
#'  The options are \code{"ksmooth"} (default), \code{"chaikin"}, and \code{"spline"}.
#' @param smoothness_factor Numerical scalar indicating the amount of smoothing the \code{\link[smoothr]{smooth}} function should do.
#'  Default is 2.
#' @param res Resolution of the raster used to calculate polygon density. Higher resolution (i.e., lower numbers)
#'  creates smoother borders but also increases processing time. Default is 1/60 degrees.
#' @param invalid_geom One of "none" (default), "exclude", or "fix". The function attempts to rebuild invalid geometries
#'  using sf::st_make_valid(shp). However, some geometries may be invalid even after attempting to rebuild. If "none" the function will
#'  return an error in case of invalid geometries (after attempting to rebuild). If "exlude", any invalid geometries will be removed.
#'  If "fix", the function will attempt to rebuild invalid geometries by iteratively lowering the snapping
#'  precision (in \code{s2::s2_snap_precision()}). It will return an error if it fails to rebuild valid geometries.
#'  The use of "include" is experimental and the result should be inspected manually.
#' @return Returns an SF PolygonsDataFrame with the same number of features as specified in *cuts*. The density,
#'  or percentile, is stored in the attributes \code{"prob"} and \code{"label"}.
#' @importFrom smoothr smooth
#' @import sf
#' @importFrom terra rast
#' @importFrom terra ifel
#' @importFrom terra rasterize
#' @importFrom terra as.polygons
#' @import dplyr

#' @export

## RETURN ORIGINAL DATAFRAME (INCL. YEAR IF SEVERAL PERIODS, PERHAPS IN OTHER FUNCTION)
## RETURN DATA FRAME (CONDITION ON KEEP_VARS)

contour_polygons <- function(shp, cuts = 10,
                             smooth_method = c("ksmooth", "chaikin", "spline"), smoothness_factor = 2,
                             res = 1/60,
                             invalid_geom = c("none", "exclude", "fix"),
                             keep_vars){

  smooth_method <- match.arg(smooth_method)
  invalid_geom <- match.arg(invalid_geom)

  if(sf::st_crs(shp) != sf::st_crs("EPSG:4326")){
    shp <- shp %>%
      sf::st_transform(crs = 4326)
  }

  if(!all(sf::st_is_valid(shp))){

    shp <- sf::st_make_valid(shp)

    if(all(sf::st_is_valid(shp))){
      warning("Some geometries are invalid. They have been fixed using sf::st_make_valid().
            Consider checking spatial features before continuing.")
    }

    if(!all(sf::st_is_valid(shp))){

      ## Option exclude_invalid == "none"
      if(invalid_geom == "none"){

        stop("Invalid geometries: sf::st_make_valid() was unable to rebuild valid geometries.")

      }

      ## Option exclude_invalid == "exclude"
      if(invalid_geom == "exclude"){

        shp <- shp[st_is_valid(shp), ]
        if(!all(sf::st_is_valid(shp))) stop("Invalid features, unable to remove")

        warning("Invalid geometries have been removed from the dataset.")
      }

      ## Option exclude_invalid == "fix"
      if(invalid_geom == "fix"){

        shp_invalid <- TRUE
        snap_prec <- 10000000

        while(shp_invalid == TRUE & snap_prec >= 10){

          shp <- st_make_valid(shp, s2_options = s2::s2_options(snap = s2::s2_snap_precision(snap_prec)))
          shp_invalid <- !all(st_is_valid(shp))
          if(snap_prec < 100){
            snap_prec <- snap_prec * 0.9
          }else{
            snap_prec <- snap_prec * 0.5
          }

        }

        if(shp_invalid == TRUE) stop("Invalid geometries: sf::st_make_valid() failed to rebuild
                                     valid geometries withint acceptable snapping precision.")

        warning("Some geometries are invalid. They have been fixed using sf::st_make_valid().
            Consider checking spatial features before continuing.")

      }

    }

  }


  cuts <- cuts
  n_maps <- nrow(shp)

  cow_id <- shp$COWID[1]
  cow_num <- shp$COWNUM[1]

  if(cuts > n_maps){
    cuts <- n_maps
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

  cut_seq <- seq(from = 0, to = 1, length.out = cuts+1)[-1]
  cut_interval <- 1/cuts
  shp_smoothed <- lapply(
    cut_seq,
    FUN = function(x){
      terra::ifel(r_prob > x - cut_interval & r_prob <= x, 1, NA) %>%
        terra::as.polygons() %>%
        sf::st_as_sf() %>%
        smoothr::smooth(method = smooth_method, smoothness = smoothness_factor) %>%
        dplyr::mutate(cut = x) %>%
        dplyr::mutate(prob_interval = paste0((x-cut_interval), "-", x)) %>%
        dplyr::mutate(COWID = cow_id,
                      COWNUM = cow_num) %>%
        dplyr::select(COWID, COWNUM, cut, prob_interval)
    }) %>%
    dplyr::bind_rows()

  return(shp_smoothed)
}
