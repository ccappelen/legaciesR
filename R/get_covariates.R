#' Merge LEGACIES data with covariates
#'
#' @param x List with named elements `r`, object of class `SpatRaster`, and `data`,
#'  object of class `data.frame`, OR a named object of class `data.frame`. The list object
#'  is returned by the `get_grid` function if `return = "list"`.
#' @param grid_id Name of column identifying the grid cell. Default is `gid` which follows convention
#' in `get_grid`.
#' @param vars Covariate data to merge. Default includes all variables. See details below.
#' @param path Character string specifying the path name for the LEGACIES team folder. Depends
#'  on the user's computer, but generally should be along the lines of
#'  `"~/user/OneDrive-NTNU/legacies-project"`.
#' @param progress Logical, whether progress updates should be displayed.
#' @param parallel Logical, whether to parallelize jobs (CURRENTLY NOT USED)
#' @param coords Character vector of length two specifying the names of the coordinate
#'  (longitude/latitude) variables. Default is `c("lon", "lat")` which follows the
#'  convention in `get_grid`.
#'
#' @import dplyr
#' @import sf
#' @importFrom terra rast
#' @importFrom terra terrain
#' @importFrom terra values
#' @importFrom terra ifel
#' @importFrom terra resample
#' @importFrom terra rasterize
#' @import cli
#' @import rnaturalearthdata
#' @import stringr
#' @import nngeo
#'
#' @return
#' Data frame or list of data frame and SpatRaster. By default, all covariates are included,
#' but it is also possible to specify which covariates to include with the argument `vars`,
#' which takes a character vector as input. The following data are available:
#'  \item{`terrain`}{.}
#'  \item{`climate`}{.}
#'  \item{`crops`}{.}
#'  \item{`rivers`}{.}
#'  \item{`coast`}{}
#'
#' @export

get_covariates <- function(x,
                           vars,
                           grid_id = gid,
                           path,
                           progress = TRUE,
                           parallel = FALSE,
                           coords){



  ## Define grid id var
  grid_id_str <- deparse(substitute(grid_id))


  ## Set internal variables (to pass R CMD check)
  featurecla <- NULL

  output_args <- c("terrain", "climate", "crops", "rivers", "coast")
  if (missing(vars)) {
    vars <- output_args
  } else {
    if (any(!vars %in% output_args)) {
      cli::cli_alert("{.arg output} contains elements that do not match any summary measure.")
    }
    vars <- match.arg(vars, choices = output_args, several.ok = TRUE)
  }

  if ("coast" %in% vars) {
    rlang::check_installed(
      "rnaturalearthdata",
      reason = "'rnaturalearthdata' must be installed")
  }

  if (!stringr::str_sub(path, start = -1) == "/") {
    path <- stringr::str_c(path, "/")
  }
  path <- stringr::str_c(path, "covariates/")

  if (missing(coords)) {
    coords <- c("lon", "lat")
  }

  if (!(is.list(x) || inherits(x$data, "data.frame"))) {
    cli::cli_abort("{.arg x} must be either (1) an object of class {.code data.frame} or a
                     list containing an object named {.code r} of class {.code SpatRaster} AND
                     a {.code data.frame} named {.code data}.")
  }

  if (is.list(x)) {
    if ("r" %in% names(x) && inherits(x$r, "SpatRaster")) {
      r <- x$r
    } else {
      cli::cli_abort("{.arg x} must be either (1) an object of class {.code data.frame} or a
                     list containing an object named {.code r} of class {.code SpatRaster} AND
                     a {.code data.frame} named {.code data}.")
    }
    if ("data" %in% names(x) && inherits(x$data, "data.frame")) {
      df <- x$data |>
        dplyr::distinct({{ grid_id }}, .keep_all = TRUE) |>
        dplyr::select({{ grid_id }}, coords)
    } else {
      cli::cli_abort("{.arg x} must be either (1) an object of class {.code data.frame} or a
                     list containing an object named {.code r} of class {.code SpatRaster} AND
                     a {.code data.frame} named {.code data}.")
    }
  }

  if ("data.frame" %in% class(x)) {

    if (!coords %in% names(x$data)) {
      cli::cli_abort("{.arg x} must contain longitude and latitude. Variable names can be specified
                     in {.arg coords}")
    }

    r <- terra::rast(x$data[,coords], crs = "epsg:4326", type = "xyz")
    df <- x |>
      dplyr::distinct(coords, .keep_all = TRUE) |>
      dplyr::select(coords)

  }





  steps <- length(vars)
  step <- 0

  ## ELEVATION AND TERRAIN RUGGEDNESS
  if ("terrain" %in% vars) {

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Extracing covariates - Terrain")

    elev <- terra::rast(paste0(path, "elevation_GMTED2010/elevation_GMTED2010.tif"))
    tri <- terra::terrain(elev, v = "TRI", progress = 0)

    elev <- terra::resample(elev, r, method = "average")
    tri <- terra::resample(tri, r, method = "average")

    df$elevation <- terra::values(elev) |> as.vector()
    df$ruggedness <- terra::values(tri) |> as.vector()

    rm(elev, tri)
    cli::cli_progress_done()
  }



  ## CLIMATE
  if ("climate" %in% vars) {

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Extracing covariates - Climate")

    climate <- terra::rast(paste0(path, "climate/koppen_geiger_0p1.tif"))
    tropical <- terra::ifel(climate <= 3, 1, 0)
    tropical <- terra::resample(tropical, r, method = "average")

    df$tropical <- terra::values(tropical) |> as.vector()

    rm(climate, tropical)
    cli::cli_progress_done()
  }



  ## CROPS
  if("crops" %in% vars) {

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Extracing covariates - Crops")

    cli::cli_progress_done()
  }



  ## RIVERS
  if ("rivers" %in% vars) {

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Extracing covariates - Rivers")

    rivers <- sf::read_sf(paste0(path, "rivers"), "naturalearth_rivers")
    rivers <- rivers |>
      dplyr::filter(featurecla == "River")
    # rivers <- rivers |>
    #   dplyr::filter(scalerank >= 6)
    df$river <- terra::rasterize(rivers, r, background = 0) |> terra::values()

    # riverdist <- nngeo::st_nn(
    #   df |> sf::st_as_sf(coords = coords, crs = 4326),
    #   rivers,
    #   k = nrow(rivers),
    #   returnDist = TRUE,
    #   parallel = 8)
    # riverdist <- terra::distance(r, rivers, progress = 0)
    # riverdist <- riverdist / 1000
    # df$dist2river <- terra::values(riverdist)
    riverdist <- nngeo::st_nn(x$data |> sf::st_as_sf(coords = coords, crs = 4326), rivers,
                       k = 1, returnDist = TRUE, parallel = future::availableCores()) |>
      suppressMessages()
    riverdist <- riverdist$dist |>
      unlist()
    df$dist2river <- riverdist / 1000

    riverflow <- terra::rast(paste0(path, "HydroSHEDS/hyd_glo_acc_15s.tif"))
    riverflow <- terra::resample(riverflow, r, method = "average")
    df$riverflow_ln <- log(terra::values(riverflow))

    # rm(riverflow, rivers, riverdist)
    cli::cli_progress_done()
  }



  ## COAST
  if ("coast" %in% vars) {

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Extracing covariates - Coast")

    coast <- rnaturalearthdata::coastline50
    # coastdist <- terra::distance(r, rivers, progress = 0)
    coastdist <- nngeo::st_nn(x$data |> sf::st_as_sf(coords = coords, crs = 4326), coast,
                              k = 1, returnDist = TRUE, parallel = future::availableCores()) |>
      suppressMessages()
    coastdist <- coastdist$dist |>
      unlist()
    df$dist2coast <- coastdist / 1000
    # coastdist <- coastdist / 1000
    # df$dist2coast <- terra::values(coastdist)

    rm(coast, coastdist)
    cli::cli_progress_done()
  }


  ## MERGE
  if (is.list(x)) {
    x$data <- x$data |>
      dplyr::left_join(df |> dplyr::select(-c(coords)), by = grid_id_str)
  }

  if ("data.frame" %in% x) {
    x <- x |>
      dplyr::left_join(df |> dplyr::select(-c(coords)), by = grid_id_str)
  }

  return(x)

}


