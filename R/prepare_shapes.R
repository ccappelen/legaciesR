#' Prepare shapefiles
#'
#' The function prepares data for further processing with other functions in the `legaciesr' package,
#' e.g., expands rows for maps coded with a range (rather than specific year), excludes certain maps, etc.
#'
#' @param shp sf dataframe
#' @param state_data dataframe with state ID data
#' @param id_var ID variable, must be the same in both `shp` and `state_data`
#' @param period_var Year variable, must be the same in both `shp` and `state_data`
#' @param range_min Name of variable for lower year interval
#' @param range_max Name of variable for upper year interval
#' @param fix_year Logical, whether to fix three-digit years. Default is `TRUE`. See details.
#' @param year_na Logical, whether to exclude maps with no year indication (exact or range)
#' @param expand_range Integer, indicating for maps with year interval (rather than specific year) the number of
#'  years between repeating maps. If `0`, only the start and end date will be included. Otherwise, the specified
#'  number indicates the how often maps should be repeated. Default is `20` years.
#' @param match_capitals Logical, whether to add information on capital cities to the `shp` data. Default is `TRUE`. See details.
# #' @param exclude_hierarchy Character vector, whether to exclude maps for states that are subordinated (tributary or dependency). `none` (default)
# #' will keep all maps regardless of hierarchy status, `tributary` will exclude tributary states (`hierarchy == 1`), `dependency` will exclude
# #' dependencies (`hierarchy == 2`), and `all` will exclude both tributaries and dependencies. See also `combine_hierarchy`.
# # @param combine_hierarchy Logical, whether to combine maps for states that are part of the same hierarchy,
# # i.e., maps with hierarchy status 1 or 2 will be subsumed in the state of which they are subordinated.
#' @param exclude_source_year Integer, indicating if maps from sources before a given year should be dropped. Default is
#' `NA`, which means all maps, regardless of source year, are included.
#' @param exclude_core Exclude maps coded as core (i.e., include only the complete shape of both core and periphery).
#' Default is `TRUE`.
#' @param exclude_incomplete Exclude maps with incomplete borders. Default is `TRUE`.
#' @param exclude_sovereign Exclude maps for states that are not sovereign at the time of the map. If `exclude_sovereign` is not `none`, then sovereign spells are defined by the same hierarchy rule.
#' Otherwise, all polity-years in the ID data will be included as sovereign spells. See details.
#' @param margin_sovereign Number of years before or after a state is sovereign that a map is still included (if `exclude_sovereign` is `TRUE`). Default is 5 years.
#' @param crop_to_land Logical, whether to crop geometries to land (default is `TRUE`)
#'
#' @import dplyr
#' @import sf
#' @import cli
#' @importFrom tidyr expand
#' @import stringr
#'
#'
#' @returns
#' An sf dataframe.
#' Further details:
#' \item{`expand_range`}{Expands rows for maps with interval range rather than specific year. The lower and
#' upper limits are always retained. In between the limits, rows are only in the specific interval. For example
#' if `expand_range = 20`, years divisible by 20 will be included (in addition to the lower and upper limits).}
#' \item{`match_capitals`}{Capital coordinates are stored as separate geometry. If there are multiple capitals
#' in the same year, all of them withh be included. The capital names will then be separated by semicolon.}
#' \item{`exclude_sovereign`}{Excludes maps for states that are not sovereign. If the year of the map is within
#' 5 years of a state being sovereign, the map is still included.}
#'
#' @export


prepare_shapes <- function(shp,
                           state_data,
                           id_var,
                           period_var,
                           range_min,
                           range_max,
                           fix_year = TRUE,
                           year_na = TRUE,
                           expand_range = 20L,
                           match_capitals = TRUE,
                           # exclude_hierarchy = c("none", "tributary", "dependency", "all"),
                           exclude_source_year = NA,
                           exclude_core = TRUE,
                           exclude_incomplete = TRUE,
                           exclude_sovereign = TRUE,
                           margin_sovereign = 5,
                           crop_to_land = TRUE){


  # Set internals
  unique_id <- inrange <- Capital <- Cap_Lon <- Cap_Lat <- capital_lon <- capital_lat <- NULL
  geometry <- capital <- NULL
  Hie_1 <- hie1 <- hie1_lag <- hie1_lead <- NULL
  Hie_2 <- hie2 <- hie2_lag <- hie2_lead <- NULL
  hie3 <- hie3_lag <- hie3_lead <- NULL
  tributary <- dependency <- tributary_dependency <- NULL
  core <- core_periphery <- note <- NULL
  incomplete <- year_diff <- new_spell <- spell <- year <- NULL
  polity_name <- COWID <- COWNUM <- name <- capital_name <- hie_tributary <- hie_dependency <- state_type <- NULL
  in_spell <- NULL
  source_year <- hyear <- lyear <- from <- to <- NULL



  # Check input
  if (missing(id_var)) {
    cli::cli_abort("{.arg id_var} is missing.")
  }

  if (is.character(substitute(id_var))) {
    cli::cli_abort("{.arg id_var} must be a variable, not a character string.")
  }

  id_var_str <- deparse(substitute(id_var))

  if (!(id_var_str %in% names(shp) && id_var_str %in% names(state_data))) {
    cli::cli_abort("{.arg id_var} must be present in both {.arg shp} AND {.arg state_data}.")
  }


  if (missing(period_var)) {
    cli::cli_abort("{.arg period_var} is missing.")
  }

  if (is.character(substitute(period_var))) {
    cli::cli_abort("{.arg period_var} must be a variable, not a character string.")
  }

  period_var_str <- deparse(substitute(period_var))

  if (!(period_var_str %in% names(shp) && period_var_str %in% names(state_data))) {
    cli::cli_abort("{.arg period_var_str} must be present in both {.arg shp} AND {.arg state_data}.")
  }

  expand_range <- as.integer(expand_range)
  if (!is.integer(expand_range)) {
    cli::cli_abort("{.arg expand_range} must be an integer.")
  }

  if (missing(range_min)) {
    cli::cli_abort("{.arg range_min} is missing.")
  }

  if (is.character(substitute(range_min))) {
    cli::cli_abort("{.arg range_min} must be a variable, not a character string.")
  }

  range_min_str <- deparse(substitute(range_min))

  if (!(range_min_str %in% names(shp))) {
    cli::cli_abort("{.arg range_min_str} must be present in {.arg shp}.")
  }


  if (missing(range_max)) {
    cli::cli_abort("{.arg range_max} is missing.")
  }

  if (is.character(substitute(range_max))) {
    cli::cli_abort("{.arg range_max} must be a variable, not a character string.")
  }

  range_max_str <- deparse(substitute(range_max))

  if (!(range_max_str %in% names(shp))) {
    cli::cli_abort("{.arg range_max_str} must be present in {.arg shp}.")
  }

  # exclude_hierarchy = match.arg(exclude_hierarchy)




  ## Remove empty geometries
  shp <- shp |>
    filter(!if_all(c(everything(),-geometry), is.na) & !sf::st_is_empty(geometry))


  ## CHECK IF THERE ARE GEOMETRIES WITH MISSING ID (WILL PRODUCE AN ERROR)
  if (any(is.na(shp[[id_var_str]]))) {
    cli::cli_alert_warning("Geometries with missing {.arg id_var}: Geometries with missing {.arg id_var} are assigned to the value '99999'.")

    shp <- shp |>
      mutate({{ id_var }} := ifelse(is.na({{ id_var }}), 99999, {{ id_var }}))
  }




  ## Prepare progress updates
  steps <- fix_year +
    year_na +
    (expand_range > 0) +
    match_capitals +
    # (exclude_hierarchy != "none") +
    exclude_core +
    exclude_incomplete +
    exclude_sovereign +
    crop_to_land
  step <- 0



  ## Fix three-digit years
  if (fix_year) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Fix three-digit years")

    shp <- shp |>
      dplyr::mutate({{ period_var }} := ifelse(stringr::str_length({{ period_var }}) == 3,
                                               stringr::str_c({{ period_var }}, 0),
                                               {{ period_var }}),
                    {{ range_min }} := ifelse(stringr::str_length({{ range_min }}) == 3,
                                               stringr::str_c({{ range_min }}, 0),
                                               {{ range_min }}),
                    {{ range_max }} := ifelse(stringr::str_length({{ range_max }}) == 3,
                                               stringr::str_c({{ range_max }}, 0),
                                               {{ range_max }}),
                    ) |>
      dplyr::mutate({{ period_var }} := as.numeric({{ period_var }}))

    cli::cli_progress_done()
  }



  ## Remove maps without year indication
  if (year_na) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Exclude maps with no date")

    shp <- shp |>
      filter(!is.na({{ period_var }}) | !is.na({{ range_min }}) | !is.na({{ range_max }}))

    cli::cli_progress_done()
  }

  ## Extract source year
  shp <- shp |>
    dplyr::mutate(source_year = stringr::str_extract(source, "\\d+")) |>
    dplyr::mutate(source_year = ifelse(stringr::str_length(source_year) == 2, paste0(source_year, "50"), source_year)) |>
    dplyr::mutate(source_year = as.numeric(source_year))

  ## Exclude source year
  if (!is.na(exclude_source_year)) {
    shp <- shp |>
      dplyr::filter(source_year >= exclude_source_year)
  }


  ## Expand range
  if (expand_range > 0) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Expand range")

    # If no range, add year in both range_min and range_max (ensures they only get only row after expanding)
    shp <- shp |>
      dplyr::mutate({{ range_min }} := ifelse(is.na({{ range_min }}) & is.na({{ range_max }}),
                                             {{ period_var }},
                                             {{ range_min }}),
                    {{ range_max }} := ifelse(is.na({{ range_min }}) & is.na({{ range_max }}),
                                             {{ period_var }},
                                             {{ range_max }}))

    # Replace range values with year if one (not both) is missing
    shp <- shp |>
      dplyr::mutate({{ range_min }} := ifelse(is.na({{ range_min }}) & !is.na({{ range_max }}),
                                              {{ range_max }},
                                              {{ range_min }}),
                    {{ range_max }} := ifelse(is.na({{ range_max }}) & !is.na({{ range_min }}),
                                              {{ range_min }},
                                              {{ range_max }}))

    # Create temporary shp with unique ID per row
    shp_tmp <- shp |>
      dplyr::mutate(unique_id = row_number())

    # Expand shp
    shp <- shp_tmp |>
      dplyr::group_by(unique_id) |>
      tidyr::expand({{ period_var }} := seq({{ range_min }}, {{ range_max }})) |>
      ungroup()

    # Merge with shp_tmp and drop in-between years not divisible by 10
    shp <- shp |>
      dplyr::left_join(shp_tmp |> select(-{{ period_var }}), by = "unique_id") |>
      dplyr::mutate(inrange = ifelse({{ range_min }} != {{ range_max }}, 1, 0)) |>
      dplyr::mutate(drop = ifelse(inrange &
                                    {{ period_var }} != {{ range_min }} &
                                    {{ period_var }} != {{ range_max }} &
                                    {{ period_var }} %% expand_range != 0, 1, 0)) |>
      dplyr::filter(drop == 0) |>
      dplyr::select(-c(unique_id, inrange, drop))

    shp <- shp |>
      sf::st_as_sf(sf_column_name = "geometry")

    cli::cli_progress_done()
  }


  ## Add unique polity names from ID data
  id_name <- state_data |>
    dplyr::distinct({{ id_var }}, polity_name)
  shp <- shp |>
    left_join(id_name, by = id_var_str)
  shp <- shp |>
    dplyr::select(COWID, COWNUM, polity_name, map_name = name, everything())




  ## Exclude core
  if (exclude_core) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Exclude core")

    if (any(shp$core > 1, na.rm = TRUE)) {
      cli::cli_abort("CODING ERROR: Core/periphery coding should only take on values `0` or `1`.
      Check both column `Core.Great` and column `core` for errors.")
    }

    shp <- shp |>
      dplyr::rowwise() |>
      dplyr::mutate(core_periphery = core) |>
      dplyr::ungroup() |>
      dplyr::mutate(core_periphery = ifelse(is.na(core), 0, core_periphery)) |>
      dplyr::mutate(core_periphery = dplyr::case_when(
        core_periphery == 1 ~ "Core",
        core_periphery == 0 ~ "Full",
        is.na(core_periphery) ~ "Full"
      ))

    shp <- shp |>
      dplyr::filter(core_periphery == "Full")

    cli::cli_progress_done()
  }


  ## Exclude incomplete
  if (exclude_incomplete) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Exclude incomplete")

    shp <- shp |>
      dplyr::mutate(incomplete = stringr::str_detect(note, "incomplete|Incomplete")) |>
      dplyr::mutate(incomplete = ifelse(is.na(incomplete), FALSE, incomplete)) |>
      dplyr::filter(incomplete == FALSE)

    cli::cli_progress_done()
  }



  ## Explicitly exclude maps with year before 1750 AND source_year before 1750
  shp <- shp |>
    dplyr::filter(!(year < 1750 & source_year < 1750))


  ## Fix years outside range but within 10 year margin
  shp <- shp |>
    mutate(year = ifelse(year < 1750 & year >= 1740 & hyear < 1750, 1750, year),
           year = ifelse(year > 1920 & year <= 1930 & lyear > 1920, 1920, year))

  ## Exclude sovereign

  # Create variable saving original map years
  shp <- shp |>
    dplyr::mutate(map_year = year)

  if (exclude_sovereign) {
    step <- step + 1
    e <- environment()
    msg <- ""
    cli::cli_progress_step("{step}/{steps}: Exclude non-sovereign maps{msg}", spinner = TRUE)

    # if (exclude_hierarchy != "none") {
    #   sovereign_spell <- df_hierarchy
    # } else {
    #   sovereign_spell <- state_data
    # }

    # if (exclude_hierarchy == "none") {
    #   sovereign_spell <- state_data
    # } else if (exclude_hierarchy == "tributary") {
    #   sovereign_spell <- state_data |>
    #     dplyr::filter(independent_tributary == 1)
    # } else if (exclude_hierarchy == "dependency") {
    #   sovereign_spell <- state_data |>
    #     dplyr::filter(independent_dependency == 1)
    # } else if (exclude_hierarchy == "all") {
    #   sovereign_spell <- state_data |>
    #     dplyr::filter(independent == 1)
    # }


    sovereign_spell <- state_data

    sovereign_spell <- sovereign_spell |>
      dplyr::select({{ id_var }}, {{ period_var }}) |>
      dplyr::group_by({{ id_var }}) |>
      dplyr::mutate(year_diff = {{ period_var }} - lag({{ period_var }})) |>
      dplyr::mutate(new_spell = ifelse(is.na(year_diff) | year_diff > 1, 1, 0)) |>
      dplyr::mutate(spell = cumsum(new_spell)) |>
      dplyr::ungroup() |>
      dplyr::group_by({{ id_var }}, spell) |>
      dplyr::summarise(from = min(year),
                       to = max(year)) |>
      dplyr::ungroup() |>
      dplyr::mutate(unique_id = row_number())

    sovereign_exp <- sovereign_spell |>
      dplyr::group_by(unique_id) |>
      tidyr::expand({{ period_var }} := seq(from, to)) |>
      dplyr::ungroup()

    sovereign_exp <- sovereign_exp |>
      dplyr::left_join(sovereign_spell, by = "unique_id")

    #
    #
    # shp_list <- split(shp, f = shp[[id_var_str]])
    # shp <- lapply(shp_list,
    #                 FUN = function(x) {
    #
    #                   tmp_length <- nrow(x)
    #                   tmp_id <- x[[id_var_str]][1]
    #                   tmp_year <- x[[period_var_str]]
    #
    #                   in_spell <- tmp_year %in% (state_data[state_data[[id_var_str]] == tmp_id,][[period_var_str]]) |
    #                     tmp_year %in% (state_data[state_data[[id_var_str]] == tmp_id,][[period_var_str]] - margin_sovereign) |
    #                     tmp_year %in% (state_data[state_data[[id_var_str]] == tmp_id,][[period_var_str]] + margin_sovereign)
    #
    #                   if (nrow(x) != length(in_spell)) stop(paste0("ERROR: ID ", tmp_id))
    #
    #                   x$in_spell <- in_spell
    #
    #                   .step_id <- x[[id_var_str]][1]
    #                   e$msg <- glue::glue(": {.step_id}")
    #                   cli::cli_progress_update(.envir = e)
    #
    #                   if (nrow(x) != tmp_length) stop(paste0("ERROR: ID ", tmp_id))
    #                   return(x)
    #
    #                 }) |>
    #   bind_rows()



    shp_list <- split(shp, f = shp[[id_var_str]])

    shp <- lapply(shp_list,
           FUN = function(x) {
             tmp_length <- nrow(x)
             tmp_id <- x[[id_var_str]][1]
             tmp_years <- x[[period_var_str]]
             sov_years <- sovereign_exp |> filter(COWID == tmp_id) |> pull(year)

             if (length(sov_years) == 0) {
               in_spell <- rep(FALSE, tmp_length)
               in_window <- rep(FALSE, tmp_length)
               in_window_year <- rep(NA, tmp_length)
               # window_diff <- rep(NA, tmp_length)
             } else {
               in_spell <- tmp_years %in% sov_years
               # window_diff <- sapply(tmp_years, FUN = function(x) min(abs(x - sov_years)))
               in_window <- sapply(tmp_years, FUN = function(x) min(abs(x - sov_years)) %in% 1:margin_sovereign)
               in_window_year <- sapply(tmp_years, FUN = function(x) sov_years[which.min(abs(x - sov_years))])
               in_window_year[!in_window] <- NA
               in_window_year <- in_window_year |> unlist()
             }

             if (nrow(x) != length(in_spell)) stop(paste0("ERROR: ID ", tmp_id))

             x$in_spell <- in_spell
             x$in_window <- in_window
             x$in_window_year <- in_window_year
             # x$window_diff <- window_diff

             x <- x |>
               dplyr::mutate(year = ifelse(in_window == TRUE, in_window_year, year),
                             in_spell = ifelse(in_window == TRUE, TRUE, in_spell))

             x <- x |>
               dplyr::select(-c(in_window, in_window_year))

             .step_id <- x[[id_var_str]][1]
             e$msg <- glue::glue(": {.step_id}")
             cli::cli_progress_update(.envir = e)

             if (nrow(x) != tmp_length) stop(paste0("ERROR: ID ", tmp_id))
             return(x)
           }) |>
      bind_rows()


    shp <- shp |>
      dplyr::filter(in_spell == TRUE)

    msg <- ""
    cli::cli_progress_update()

    cli::cli_progress_done()
  }



  ## Match capitals
  if (match_capitals) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Matching capitals")

    cap <- state_data |>
      dplyr::select({{ id_var }}, {{ period_var }}, capital_name, capital_lon, capital_lat) |>
      dplyr::filter(!is.na(capital_lon) & !is.na(capital_lat)) |>
      sf::st_as_sf(coords = c("capital_lon", "capital_lat"), crs = 4326) |>
      dplyr::group_by({{ id_var }}, {{ period_var }}) |>
      dplyr::summarise(
        geometry = st_combine(geometry),
        capital_name = paste(capital_name, collapse = ";")
      ) |>
      dplyr::mutate(capital_coords = geometry) |>
      sf::st_drop_geometry()

    # cap <- state_data |>
    #   dplyr::select({{ id_var }}, {{ period_var }}, capital = Capital, capital_lon = Cap_Lon, capital_lat = Cap_Lat) |>
    #   dplyr::filter(!is.na(capital_lon) & !is.na(capital_lat)) |>
    #   sf::st_as_sf(coords = c("capital_lon", "capital_lat"), crs = 4326) |>
    #   dplyr::group_by({{ id_var }}, {{ period_var }}) |>
    #   dplyr::summarise(
    #     geometry = st_combine(geometry),
    #     capital_names = paste(capital, collapse = ";")
    #   ) |>
    #   dplyr::mutate(capital_coords = geometry) |>
    #   sf::st_drop_geometry()

    shp <- shp |>
      dplyr::left_join(cap, by = c(id_var_str, period_var_str))

    cli::cli_progress_done()
  }


  ## Match state type
  df_hierarchy <- state_data |>
    dplyr::select({{ id_var }}, {{ period_var }}, hie_tributary, hie_dependency, state_type)

  shp <- shp |>
    dplyr::left_join(df_hierarchy, by = c(id_var_str, period_var_str))


  # ## Exclude hierarchy
  # df_hierarchy <- state_data |>
  #   dplyr::select({{ id_var }}, {{ period_var }}, hie_tributary, hie_dependency, state_type)
  #
  # if (exclude_hierarchy != "none") {
  #   step <- step + 1
  #   cli::cli_progress_step("{step}/{steps}: Exclude hierarchy")
  #
  #   shp <- shp |>
  #     dplyr::left_join(df_hierarchy, by = c(id_var_str, period_var_str))
  #
  #   if (exclude_hierarchy == "tributary") {
  #     shp <- shp |>
  #       dplyr::filter(independent_tributary == 1)
  #   }
  #
  #   if (exclude_hierarchy == "dependency") {
  #     shp <- shp |>
  #       dplyr::filter(independent_dependency == 1)
  #   }
  #
  #   if (exclude_hierarchy == "all") {
  #     shp <- shp |>
  #       dplyr::filter(independent == 1)
  #   }
  #
  #   cli::cli_progress_done()
  # }



  ## Crop to land
  if (crop_to_land) {

    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Crop geometries to land")

    rlang::check_installed(
      "rnaturalearthdata",
      reason = "'rnaturalearthdata' must be installed")
    world <- rnaturalearthdata::countries50
    world_land <- world |>
      sf::st_wrap_dateline() |>
      sf::st_combine() |>
      sf::st_as_sf() |>
      suppressWarnings()

    world_land_fixed <- legaciesr::fix_invalid(world_land,
                                               report = FALSE,
                                               reportColumns = FALSE,
                                               parallel = FALSE) |>
      suppressMessages()

    shp <- sf::st_intersection(shp, world_land_fixed) |>
      suppressWarnings()

    cli::cli_progress_done()
  }




  return(shp)


}




