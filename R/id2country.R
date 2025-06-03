#' Aggregate ID data to modern country
#'
#' The function aggregates the ID state data (number of historical states) to modern day country borders.
#' The aggregation method is based on either destination states from source coding, the location of historical
#' capital cities, or contour polygons.
#'
#' @param id_data ID data to be aggregated. If `method` is `"polygon"`, the ID data must include contour
#' polygons (using the `match_id_contour` function).
#' @param method Whether to aggregate based on source coding (`"source"`), capital city locations (`"capital"`),
#' or contour polygons (`"polygon"`). If `method = "polygon"`, polities will only be included in a given country if
#' (i) the historical territory falls entirely within the modern country or (ii) the historical polity covers at least
#' 5 % of the modern country's territory.
#' @param cshape_year Character, the year of modern country borders (`cshapes`). Default is `"2019"`, which is currently the
#' latest year available.
#' @param multiple If `method == "source"`, whether to allow polities to be counted across multiple countries
#' if the source coding suggests multiple destination states. If `FALSE`, the most important destination state is used.
#' @param exclude_hierarchy Whether to exclude certain hierarchy types. May have already been excluded in earlier functions
#' (e.g., `prepare_id` and `prepare_shapes`).
#'
#' @import cshapes
#' @import dplyr
#' @import sf
#' @import cli
#' @import stringr
#' @import units
#'
#' @returns A dataframe of modern countries with a column for the number of unique historical states.
#' @export
#'


id2country <- function(id_data,
                       method = c("source", "capital", "polygon"),
                       cshape_year = "2019",
                       multiple = FALSE,
                       exclude_hierarchy = c("none", "tributary", "dependency", "all")){

  if (stringr::str_length(cshape_year) != 4 && as.numeric(cshape_year) <= 2019) {
    cli::cli_abort("{.arg cshape_year} must be a four digit character string. Maximum year is 2019.")
  } else {
    date <- as.Date(paste0(cshape_year,"-12-31"))
  }

  method = match.arg(method)

  exclude_hierarchy = match.arg(exclude_hierarchy)


  if (method == "source") {
    steps <- 2
  } else if (method == "capital") {
    steps <- 3
  } else {
    steps <- 4
  }

  step <- 0


  ## Load cshapes shapefile
  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Load cshapes data")

  cntry <- cshapes::cshp(useGW = FALSE, dependencies = FALSE, date = date) |>
    sf::st_make_valid() |>
    sf::st_wrap_dateline()

  cntry <- cntry |>
    dplyr::select(cowcode, country_name, status, owner, capital = capname,
                  capital_lon = caplong, capital_lat = caplat)

  if (exclude_hierarchy == "tributary") {
    id_data <- id_data |>
      dplyr::filter(independent_tributary == 1)
  } else if (exclude_hierarchy == "dependency") {
    id_data <- id_data |>
      dplyr::filter(independent_dependency == 1)
  } else if (exclude_hierarchy == "all") {
    id_data <- id_data |>
      dplyr::filter(independent == 1)
  } else {
    id_data <- id_data
  }



  ### AGGREGATE BY DESTINATION STATE
  if (method == "source") {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Aggregate to destination states")

    id_cntry <- id_data |>
      dplyr::select(COWNUM, year, destination_states_COWNUM) |>
      tidyr::separate(destination_states_COWNUM, sprintf("dest_COWNUM_[%d]", seq(1:5)),
                      sep = ";", extra = "drop", fill = "right") |>
      dplyr::mutate(across(.col = starts_with("dest_COWNUM"),
                           .fns = ~ as.numeric(stringr::str_trim(.x)))) |>
      tidyr::pivot_longer(cols = starts_with("dest_COWNUM"), names_to = "dest_rank", values_to = "dest_COWNUM") |>
      dplyr::filter(!is.na(dest_COWNUM)) |>
      dplyr::mutate(dest_COWNUM = ifelse(dest_COWNUM == 678, 679, dest_COWNUM),
                    dest_COWNUM = ifelse(dest_COWNUM == 817, 816, dest_COWNUM)) |>
      dplyr::mutate(dest_rank = as.numeric(stringr::str_extract(dest_rank, "(?<=\\[)\\d+(?=\\])")))

    if (multiple == FALSE) {
      id_cntry <- id_cntry |>
        dplyr::filter(dest_rank == 1)
    }

    id_cntry <- id_cntry |>
      dplyr::group_by(dest_COWNUM) |>
      dplyr::summarise(n_states = n_distinct(COWNUM)) |>
      dplyr::arrange(desc(n_states))

    cntry <- cntry |>
      dplyr::left_join(id_cntry, by = c("cowcode" = "dest_COWNUM"))
  }




  ### AGGREGATE BY CAPITAL LOCATION

  if (method == "capital") {

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Intersecting capitals and country borders")

    id_cap <- id_data |>
      dplyr::filter(!is.na(capital_lon)) |>
      sf::st_as_sf(coords = c("capital_lon", "capital_lat"), crs = 4326) |>
      dplyr::select(polity_name, COWNUM, year)

    id_cap <- id_cap |>
      sf::st_intersection(cntry) |>
      suppressWarnings()

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Aggregate capitals to modern states")

    id_cap <- id_cap |>
      sf::st_drop_geometry() |>
      dplyr::group_by(cowcode) |>
      dplyr::summarise(n_states = n_distinct(COWNUM))

    cntry <- cntry |>
      dplyr::left_join(id_cap, by = "cowcode")

  }



  ### AGGREGATE BY CONTOUR POLYGON

  if (method == "polygon") {

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Prepare contour polygons")

    id_cont <- id_data |>
      sf::st_as_sf(sf_column_name = "geometry") |>
      dplyr::filter(!sf::st_is_empty(geometry)) |>
      dplyr::select(polity_name, COWNUM, year)

    id_cont <- id_cont |>
      dplyr::group_by(polity_name) |>
      summarise(geometry = sf::st_union(geometry))

    id_cont$polity_area <- id_cont |>
      sf::st_area() |>
      units::set_units("km2") |>
      units::drop_units()

    cntry$cntry_area <- cntry |>
      sf::st_area() |>
      units::set_units("km2") |>
      units::drop_units()

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Intersecting contour polygons")

    id_cont <- id_cont |>
      sf::st_intersection(cntry |> dplyr::select(cowcode, cntry_area))

    id_cont$area <- id_cont |>
      sf::st_area() |>
      units::set_units("km2") |>
      units::drop_units()

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Aggregate contour polygons to modern states")

    id_poly <- id_cont |>
      dplyr::mutate(share_polity_area = area/polity_area,
                    share_cntry_area = area / cntry_area) |>
      sf::st_drop_geometry() |>
      dplyr::mutate(polity_complete_within = share_polity_area > 0.999) |>
      dplyr::filter(polity_complete_within == TRUE | share_cntry_area > 0.05) |>
      dplyr::group_by(cowcode) |>
      dplyr::summarise(n_states = n_distinct(polity_name)) |>
      dplyr::arrange(desc(n_states))

    cntry <- cntry |>
      dplyr::left_join(id_poly, by = "cowcode")
  }

}













