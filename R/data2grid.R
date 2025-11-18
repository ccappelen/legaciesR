#' Match covariates to raster data.
#'
#' Adds a specified list of common covariates to the gridded data frame. The function
#' takes either a SpatRaster object, in which case it returns a data frame with the
#' specified covariates, or a list object returned from [get_grid], in which case it
#' returns the same list object with covariates matched to the data frame.
#'
#' @param x Either an object of class `SpatRaster` or a list object returned from [get_grid]
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
#'
#' @returns
#' Returns either a dataframe with rows corresponding to grid cells (if `x` is a `SpatRaster` object) or
#' a the list object with the covariates matched to the data item (if `x` is the list object returned from
#' [get_grid]). The following covariates are matched by default:
#' \item{`area`}{Area of the grid cells in km2}
#' \item{`terrain`}{Average elevation in meters and average terrain ruggedness within each grid cell.
#' Requires installation of the package `elevatr`. Source: }
#' \item{`malaria`}{Mean temperature suitability index for malaria within each grid cell.
#' Requires installation of the package `malariaAtlas`. Source: }
#' \item{`coast`}{Distance to nearest coast in meters. Requires installation of the package
#' `rnaturalearthdata`. Source: }
#'
#' @export

## TODO
# MASK VALUES

data2grid <- function(x, covars){


  ## CHECK 'X': If it is a raster or output from 'get_grid'
  ## If raster: Create new data frame
  ## If 'get_grid': Use existing data frame
  if (is.list(x) && all(c("r", "info", "data") %in% names(x))) {

    ras <- x$r

    dat <- x$data |>
      dplyr::distinct(gid) |>
      as.data.table()

  } else if (inherits(x, "SpatRaster")) {

    ras <- x

    dat <- terra::as.data.frame(x, xy = T, na.rm = F) |>
      dplyr::rename(lon = x, lat = y) |>
      tibble::rownames_to_column(var = "gid")  |>
      dplyr::mutate(gid = as.numeric(gid)) |>
      data.table::as.data.table()  |>
      dplyr::select(gid, lon, lat) |>
      suppressWarnings()

  } else {
    cli::cli_abort("{.arg x} is not a valid object. It should be either be a SpatRaster object or the
                   list output from {.fun get_grid}")
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
  steps <- length(covars) + 1
  step <- 0




  ### AREA
  if ("area" %in% covars) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Area")

    r_area <- terra::cellSize(ras, unit = "km")
    terra::set.names(r_area, "area")
    df_area <- terra::as.data.frame(r_area) |>
      tibble::rownames_to_column(var = "gid") |>
      dplyr::mutate(gid = as.numeric(gid)) |>
      data.table::as.data.table()
    dat <- dat |>
      dplyr::left_join(df_area, by = "gid")
    rm(df_area, r_area)

    cli::cli_progress_done()
  }



  ### TERRAIN
  if ("terrain" %in% covars) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Terrain")

    r_elev <- elevatr::get_elev_raster(ras, z = 3, verbose = FALSE)

    r_elev <- terra::rast(r_elev)
    r_elev <- terra::resample(r_elev, ras, method = "average")
    terra::set.names(r_elev, "elevation")
    df_elev <- terra::as.data.frame(r_elev) |>
      tibble::rownames_to_column(var = "gid") |>
      dplyr::mutate(gid = as.numeric(gid)) |>
      data.table::as.data.table()

    r_tri <- terra::terrain(r_elev, v = "TRI")
    r_tri <- terra::resample(r_elev, ras, method = "average")
    terra::set.names(r_tri, "tri")
    df_tri <- terra::as.data.frame(r_tri) |>
      tibble::rownames_to_column(var = "gid") |>
      dplyr::mutate(gid = as.numeric(gid)) |>
      data.table::as.data.table()

    dat <- dat |>
      dplyr::left_join(df_elev, by = "gid") |>
      dplyr::left_join(df_tri, by = "gid")
    rm(r_elev, r_tri, df_elev, df_tri)

    cli::cli_progress_done()
  }


  ### Malaria
  if ("malaria" %in% covars) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Malaria")

    r_mal <- malariaAtlas::getRaster(
      dataset_id = "Explorer__2010_TempSuitability.Pv.Index.1k.global_Decompressed",
      extent = raster::extent(raster::raster(ras)) |> as.matrix()) |>
      suppressMessages() |>
      capture.output() |>
      invisible()
    r_mal <- terra::resample(r_mal, ras, method = "average")
    terra::set.names(r_mal, "malaria")

    df_mal <- terra::as.data.frame(r_mal) |>
      tibble::rownames_to_column(var = "gid") |>
      dplyr::mutate(gid = as.numeric(gid)) |>
      data.table::as.data.table()
    dat <- dat |>
      dplyr::left_join(df_mal, by = "gid")
    rm(df_mal, r_mal)

    cli::cli_progress_done()
  }


  ### Coast
  if ("coast" %in% covars) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Coast")

    coast <- rnaturalearthdata::coastline110
    world <- rnaturalearthdata::countries50

    r_dist2coast <- terra::distance(ras, sf::st_union(coast) |> sf::st_as_sf())
    r_dist2coast <- terra::mask(r_dist2coast, sf::st_union(world) |> sf::st_as_sf())
    terra::set.names(r_dist2coast, "dist2coast")
    df_coast <- terra::as.data.frame(r_dist2coast) |>
      tibble::rownames_to_column(var = "gid") |>
      dplyr::mutate(gid = as.numeric(gid)) |>
      dplyr::mutate(dist2coast = dist2coast*10^-3) |>
      data.table::as.data.table()
    dat <- dat |>
      dplyr::left_join(df_coast, by = "gid")
    rm(df_coast, r_dist2coast, coast)

    cli::cli_progress_done()
  }


  ### Finalizing
  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Finalizing")

  if (is.list(x) && all(c("r", "info", "data") %in% names(x))) {

    x$data <- x$data |>
      dplyr::left_join(dat, by = "gid")

    return(x)

  } else if (inherits(x, "SpatRaster")) {

    return(dat)

  } else {
    cli::cli_abort("{.arg x} is not a valid object. It should be either be a SpatRaster object or the
                   list output from {.fun get_grid}")
  }

}
