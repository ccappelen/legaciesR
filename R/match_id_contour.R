#' Match contour polygons to ID data
#'
#' The function attaches contour polygon geometries to the ID data as well as data on land area and population.
#'
#' @param id_data A dataframe with ID data (after being processed with the `parepare_id` function.)
#' @param contour_data An sf dataframe with contour polygons (from `get_contours`).
#' @param contour_level The probability level to use if matching to a single polygon per polity-year. Default is `0.25`.
#' @param multiple_levels Whether to include all contour levels, in which case there will be multiple rows per polity-year.
#' @param covar_path File path to folder containing HYDE population data. Typically the path will be something like `"~/user/OneDrive-NTNU/legacies-project"`.
#'
#' @returns A dataframe with polity-year observations and columns for land area (based on contour polygons) and decade population estimates
#' based on HYDE population data (aggregated to contour polygons).
#'
#'
#' @export
#'
#'

## TOO MANY POLITIES WITH MISSING CONTOUR POLYGONS!!!

match_id_contour <- function(id_data,
                             contour_data,
                             contour_level = 0.25,
                             multiple_levels = FALSE,
                             covar_path) {

  if (missing(id_data)) {
    cli::cli_abort("{.arg id_data} is missing.")
  }

  if (missing(contour_data)) {
    cli::cli_abort("{.arg contour_data} is missing.")
  }

  if (missing(covar_path)) {
    cli::cli_abort("{.arg covar_path} is missing. Must be a string identifying the file path.")
  }

  if (!("prob" %in% names(contour_data))) {
    cli::cli_abort("{.arg contour_data} should be an sf dataframe containing contour polygons created from the {.fn get_contours} function.")
  }


  steps <- 5
  step <- 0


  contour_shp <- contour_data

  df <- id_data

  if (multiple_levels == FALSE) {
    contour_shp <- contour_shp |>
      dplyr::filter(prob == 0.25)
  }


  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Preparing contour polygons")

  contour_shp <- contour_shp |>
    tidyr::separate(period, c("start", "end"), sep = "-") |>
    dplyr::mutate(across(c(start, end),
                         ~ as.numeric(.x))) |>
    dplyr::filter(start >= 1750 & end <= 1920) |>
    dplyr::mutate(end = ifelse(end < 1920, end - 1, end))


  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Calculating land area")

  contour_shp$area <- sf::st_area(contour_shp) |>
    units::set_units("km2") |>
    units::drop_units()

  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Expanding to polity-year")

  contour_shp <- contour_shp |>
    dplyr::mutate(year = purrr::map2(start, end, seq)) |>
    tidyr::unnest(year)

  contour_shp <- contour_shp |>
    dplyr::select(COWID, year, prob, area, geometry)

  contour_shp <- contour_shp |>
    dplyr::arrange(COWID, year, prob)


  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Aggregating population data")

  contour_decade <- contour_shp |>
    dplyr::filter(year %% 10 == 0)

  contour_list <- split(contour_decade, contour_decade$year)

  if (multiple_levels == TRUE) {

    df_pop <- lapply(contour_list,
                     FUN = function(x) {
                       yr <- unique(x$year)
                       r <- terra::rast(paste0(covar_path, "/covariates/HYDE34/", yr, "AD_pop/popc_", yr, "AD.asc"))
                       x$pop <- exactextractr::exact_extract(r, x, fun = "sum",
                                                             progress = FALSE)
                       return(x)
                     }) |>
      dplyr::bind_rows(.id = "decade") |>
      sf::st_drop_geometry() |>
      dplyr::select(COWID, decade, prob, pop) |>
      dplyr::mutate(decade = as.numeric(decade)) |>
      dplyr::arrange(COWID, decade, prob)

    contour_shp <- contour_shp |>
      dplyr::mutate(decade = plyr::round_any(year, 10, f = floor)) |>
      dplyr::left_join(df_pop, by = c("COWID", "decade", "prob")) |>
      dplyr::select(-decade)

  } else {

    df_pop <- lapply(contour_list,
                     FUN = function(x) {
                       yr <- unique(x$year)
                       r <- terra::rast(paste0(covar_path, "/covariates/HYDE34/", yr, "AD_pop/popc_", yr, "AD.asc"))
                       x$pop <- exactextractr::exact_extract(r, x, fun = "sum",
                                                             progress = FALSE)
                       return(x)
                     }) |>
      dplyr::bind_rows(.id = "decade") |>
      sf::st_drop_geometry() |>
      dplyr::select(COWID, decade, pop) |>
      dplyr::mutate(decade = as.numeric(decade)) |>
      dplyr::arrange(COWID, decade)

    contour_shp <- contour_shp |>
      dplyr::mutate(decade = plyr::round_any(year, 10, f = floor)) |>
      dplyr::left_join(df_pop, by = c("COWID", "decade")) |>
      dplyr::select(-decade)

  }


  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Finishing")

  if (multiple_levels == TRUE) {
    id_data_contour <- id_data |>
      left_join(contour_shp, by = c("COWID", "year"), relationship = "many-to-many")
  } else {
    id_data_contour <- id_data |>
      left_join(contour_shp, by = c("COWID", "year"))
  }


  return(id_data_contour)

}
