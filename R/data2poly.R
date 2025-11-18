#' Match covariates to polygon data.
#'
#' Adds a specified list of common covariates to an `sf` polygons data.frame. The function
#' takes an `sf` dataframe (typically the output from [get_contours]) and returns the same
#' data frame with the specified selection of covariates.
#'
#' @param shp `sf` polygons data.frame, typically the output from [get_contours].
#' @param covars Character vector specifying the covariates to add. Default is the full list,
#' i.e. `covars = c("area", "terrain", "malaria", "coast")` (see details).
#'
#' @import dplyr
#' @import terra
#' @import sf
#' @import tibble
#' @import data.table
#' @import cli
#' @import raster
#'
#' @returns
#' Returns a dataframe similar to `shp` with columsn added for covariates.
#' The following covariates are matched by default:
#' \item{`area`}{Area of the polygon in km2}
#' \item{`terrain`}{Average elevation in meters and average terrain ruggedness within each polygon.
#' Requires installation of the package `elevatr`. Source: }
#' \item{`malaria`}{Mean temperature suitability index for malaria within each polygon.
#' Requires installation of the package `malariaAtlas`. Source: }
#' \item{`coast`}{Distance to nearest coast from the border of polygons (in km).
#' Requires installation of the package `rnaturalearthdata`. Source: }
#'
#' @export
#'


data2poly <- function(shp, covars){

  ## Check if 'shp' is an sf data.frame
  if (!all(c("sf", "data.frame") %in% class(shp))) {
    cli::cli_abort("{.arg shp} must be an SF data.frame")
  }

  ## CHECK 'COVARS' OPTIONS
  covar_args <- c("area", "terrain", "malaria", "coast")
  if (missing(covars)) {
    covars <- covar_args
  } else {
    if (any(!covars %in% covar_args)) {
      cli::cli_alert("{.arg covars} contains elements that do not match any covariate.")
    }
    covars <- match.arg(covars, choices = covar_args, several.ok = TRUE)
  }

  ## CHECK DEPENDENCIES
  if ("coast" %in% covars) {
    rlang::check_installed(
      "rnaturalearthdata",
      reason = "'rnaturalearthdata' must be installed")
  }

  if ("elevatr" %in% covars) {
    rlang::check_installed(
      "elevatr",
      reason = "'elevatr' must be installed")
  }

  if ("malaria" %in% covars) {
    rlang::check_installed(
      "malariaAtlas",
      reason = "'malariaAtlas' must be installed")
  }


  ## DEFINE NUMBER OF STEPS
  steps <- length(covars)
  step <- 0


  ## Area
  if ("area" %in% covars) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Area")

    shp$area <- sf::st_area(shp) |>
      units::set_units("km2")

    cli::cli_progress_done()
  }


  ### TERRAIN
  if ("terrain" %in% covars) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Terrain")

    r <- terra::rast(terra::vect(shp))

    r_elev <- elevatr::get_elev_raster(r, z = 3, verbose = FALSE)
    shp$elevation <- exactextractr::exact_extract(r_elev, shp, fun = "mean", progress = FALSE)

    r_tri <- terra::terrain(r_elev, v = "TRI")
    shp$ruggedness <- exactextractr::exact_extract(r_tri, shp, fun = "mean", progress = FALSE)

    rm(r_elev, r_tri, r)

    cli::cli_progress_done()
  }


  ##### MALARIA ######
  if ("malaria" %in% covars) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Malaria")

    r_mal <- malariaAtlas::getRaster(dataset_id = "Explorer__2010_TempSuitability.Pv.Index.1k.global_Decompressed",
                                     extent = raster::extent(shp) |> as.matrix()) |>
      suppressMessages() |>
      capture.output() |>
      invisible()

    shp$malaria <- exactextractr::exact_extract(r_mal, shp, fun = "mean", progress = FALSE)

    rm(r_mal)

    cli::cli_progress_done()

  }


  ### Coast
  if ("coast" %in% covars) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Coast")

    coast <- rnaturalearthdata::coastline110 |>
      sf::st_union() |>
      sf::st_simplify(dTolerance = .1)
    world <- rnaturalearthdata::countries50

    df_temp <- shp |>
      sf::st_boundary() |>
      sf::st_simplify(dTolerance = .1)

    shp$dist2coast <- sf::st_distance(df_temp, coast) |>
      units::set_units("km") |>
      _[,1]

    cli::cli_progress_done()
  }



  ## FINALIZING
  return(shp)


}
