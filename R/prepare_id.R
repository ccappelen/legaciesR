#' Prepare ID data
#'
#' The function prepares the raw ID data for further use by, e.g., expanding rows into
#' polity-year observations and extracting information on capitals and hierarchies.
#'
#' @param data Raw ID data to be cleaned and expanded. Typically called `legacies_id_coding.xlsx`.
#' @param multiple_capital Logical, whether to keep all capitals if there are multiple capitals in
#' the same polity-year. In that case, a polity-year will be expanded to one row per capital.
#' Default is `FALSE`, in which case only the capital with the longest spell will be included.
#' If two or more capitals have the same spell, one capital will be selected at random.
#'
#' @import tidyr
#' @import dplyr
#' @import cli
#' @import measurements
#' @importFrom stats na.omit
#'
#
# ## Aggregating:
# destination_state_x (options for destination_state_1 only and all destination_states)
# Capital (option for either multiple or the oldest capital/continuous capital)
# Polygons: Contour polygons. Option for single (most overlap) or multiple
#     If multiple: If polity complately contained within, include
#                  If overlapping with multiple, threshold of 5 %
#
#'
#' @returns
#' A data frame with polity-year observations and the variables decribed below:
#' \item{`polity_name`}{Name of polity}
#' \item{`COWID`}{COW code for the polity}
#' \item{`COWNUM`}{Numeric COW code for the polity}
#' \item{`year`}{Year of observation}
#' \item{`other_names`}{Alternative polity names}
#' \item{`start_year`}{Start year of a given polity spell}
#' \item{`end_year`}{End year of a given polity spell}
#' \item{`population_10k`}{Whether the polity-year lives up to the population criterium}
#' \item{`autonomy`}{Whether the polity-year is regarded as autonomous}
#' \item{`capital_name`}{Name of capital of the polity in a given year. If there are multiple capitals,
#' the most prominent capital is recorded here.}
#' \item{`capital_lon`}{Longitude of capital city}
#' \item{`capital_lat`}{Latitude of capital city}
#' \item{`capital_raw`}{Raw capital city information. Can contain information for multiple capital cities.
#' The capital variables above contain information on only the most prominent capital.}
#' \item{`hie_COWID`}{COWID for which the given polity is in a hierarchical relationship.
#' Can contain multiple COWIDs if the polity is in multiple hierarchical relationships.}
#' \item{`hie_type`}{Type of hierarchy (`1 = tributary`, `2 = dependency`). Can contain multiple.}
#' \item{`hie_tributary`}{Dummy equal to `1` if the polity is in at least one tributary relationship.}
#' \item{`hie_dependency`}{Dummy equal to `1` if the polity is in at least one dependency relationship.}
#' \item{`independent_dependency`}{Dummy equal to `1` if the polity is independent, i.e. it is not a dependency.}
#' \item{`independent_tributary`}{Dummy equal to `1` if the polity is independent, i.e. it is not a tributary.}
#' \item{`independent`}{Dummy equal to `1` if the polity is independent, i.e. neither dependency nor tributary.}
#' \item{`hie_raw`}{Raw hierarchy coding.}
#' \item{`ethnonational_group`}{}
#' \item{`EPR_link`}{}
#' \item{`ACD_actor_link`}{}
#' \item{`end_agent`}{}
#' \item{`destination_states`}{}
#' \item{`destination_state_1-5`}{}
#'
#' @export

## Multiple capitals: Prior year continuity - check how many cases (if few, go through)
##

prepare_id <- function(data,
                       multiple_capital = FALSE) {

  Polity <- Polity_Start <- Polity_End <- start_year <- end_year <- year <- NULL
  Entity_Name <- COWNum <- Capital <- Capital_Info <- Capital_Name <- Capital_Years <- NULL
  Capital_Lat <- Capital_Lon <- Capital_Start <- Capital_End <- Capital_Lat_dec <- Capital_Lon_dec <- NULL
  Capital_Spell <- spell_max <- spell_max_sum <- cap_keep <- Hie <- Hie_Info <- Hie_Year <- NULL
  Hie_Start <- Hie_End <- Hie_Type <- Hie_COWID <- Hie_Tributary <- Hie_Dependency <- NULL
  Hie_Dependency_lag <- Hie_Dependency_lead <- Hie_Tributary_lag <- Hie_Tributary_lead <- NULL
  independent_tributary <- independent_dependency <- independent <- NULL
  COWID <- Other_Names <- Population_10K <- Autonomy <- EPR_Link <- NULL
  Destination_States <- Destination_State_1 <- Destination_State_2 <- Destination_State_3 <- Destination_State_4 <- Destination_State_5 <- NULL
  Capital_Coord <- NULL
  state_type <- Hie_Tributary_inclusive <- Hie_Dependency_inclusive <- NULL

  # id_path <- "data_private/legacies_id_coding.xlsx"
  # data <- readxl::read_excel(id_path, col_types = "text")

  cli::cli_progress_cleanup()
  step <- 0
  steps <- 4

  ########## PREPROCESSING ###########

  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Cleaning ID data")

  # Remove everything in square brackets
  data <- data |>
    dplyr::mutate(across(.cols = everything(),
                         .fns = ~ stringr::str_remove_all(.x, "\\[.*?\\]")))

  # Keep only polities
  data <- data |>
    dplyr::filter(Polity == 1)

  # Create start and end year
  data <- data |>
    dplyr::mutate(start_year = sub('.*(\\d{4}).*', '\\1', Polity_Start),
                  end_year = sub('.*(\\d{4}).*', '\\1', Polity_End))


  # DEBUGGING
  if (dplyr::filter(data, start_year < 1750 | end_year > 1920) |> nrow() > 0) {
    cli::cli_abort("Data contains start or end years outside the interval 1750-1920.")
  }
  if (dplyr::filter(data, is.na(start_year) | is.na(end_year)) |> nrow() > 0) {
    cli::cli_abort("Data contains missing start and/or end years.")
  }

  # Keep this version
  data_raw <- data

  # Expand df to polity-year observations
  data <- data |>
    dplyr::mutate(year = purrr::map2(start_year, end_year, seq)) |>
    tidyr::unnest(year)

  cli::cli_progress_done()

  ########## CAPITALS ###########

  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Extracting capitals")

  capitals <- data_raw |>

    # Select necessary columns
    dplyr::select(Entity_Name, COWNum, Capital) |>

    # Multiple capitals in separate columns
    tidyr::separate(Capital, sprintf("Cap_[%d]", seq(1:10)),
                    sep = ";", extra = "drop", fill = "right") |>

    # Pivot longer: One row per capital column
    tidyr::pivot_longer(cols = starts_with("Cap_"),
                        names_to = "Capital_Num", values_to = "Capital_Info") |>

    # Remove rows with no capital
    dplyr::filter(!is.na(Capital_Info)) |>

    # Split capital information into separate columns
    tidyr::separate("Capital_Info", into = c("Capital_Coord", "Capital_Name", "Capital_Years"),
                    sep = "_", extra = "drop", fill = "right") |>

    # dplyr::filter(stringr::str_detect(Capital_Years, "-8")) |>
    # Replace -8 with NA
    dplyr::mutate(across(.cols = c(Capital_Coord, Capital_Name, Capital_Years),
                         .fns = ~ stringr::str_replace_all(.x, "-8", "NA"))) |>

    # Remove brackets from columns
    dplyr::mutate(Capital_Coord = gsub("[()]", "", Capital_Coord),
                  Capital_Years = gsub("[()]", "", Capital_Years)) |>

    # Convert to lower case
    dplyr::mutate(across(.cols = Capital_Coord:Capital_Years,
                         .fns = tolower)) |>

    # Extract latitude and longitude from coord
    tidyr::separate("Capital_Coord", c("Capital_Lat", "Capital_Lon"),
                    sep = ",", extra = "drop", fill = "right") |>

    # Extract start and end years
    tidyr::separate("Capital_Years", c("Capital_Start", "Capital_End"),
                    sep = "-", extra = "drop", fill = "right") |>

    # Convert to NA
    dplyr::mutate(across(.cols = c(Capital_Lat, Capital_Lon, Capital_Start, Capital_End),
                         .fns = ~ stringr::str_replace_all(.x, "NA|na", NA_character_)),
                  Capital_Lat = ifelse(Capital_Lat == "", NA_character_, Capital_Lat),
                  Capital_Lon = ifelse(Capital_Lon == "", NA_character_, Capital_Lon)) |>

    # Prepare coordinates (trim and NA)
    dplyr::mutate(Capital_Lat_dec = stringr::str_trim(stringr::str_remove_all(Capital_Lat, "[nsewa]")),
                  Capital_Lon_dec = stringr::str_trim(stringr::str_remove_all(Capital_Lon, "[nsewa]")))


  # Debugging capitals

  if (dplyr::filter(capitals, is.na(Capital_Lat_dec) & !is.na(Capital_Lon_dec)) |> nrow() > 0) {
    cli::cli_abort("Capitals with missing latitude but valid longitude")
  }

  if (dplyr::filter(capitals, !is.na(Capital_Lat_dec) & is.na(Capital_Lon_dec)) |> nrow() > 0) {
    cli::cli_abort("Capitals with missing longitude but valid latitude")
  }

  if (dplyr::filter(capitals,
                    stringr::str_count(Capital_Lat_dec, "[:space:]") != 2 |
                    stringr::str_count(Capital_Lon_dec, "[:space:]") != 2 |
                    stringr::str_count(Capital_Lat_dec, "![:digit:]") != 0 |
                    stringr::str_count(Capital_Lon_dec, "![:digit:]") != 0) |>
      nrow() > 0) {
    cli::cli_abort("Incorrectly formatted coordinates or non-numeric characters")
  }

  if (dplyr::filter(capitals, is.na(Capital_Lat_dec) | is.na(Capital_Lon_dec)) |> nrow() > 0) {
    capitals <- capitals |>
      dplyr::filter(!is.na(Capital_Lat_dec))
    cli::cli_warn("Capitals with missing latitude and/or longitude have been removed.")
  }

  if (dplyr::filter(capitals, (!is.na(Capital_Lat) & !is.na(Capital_Lon)) &
                    (is.na(Capital_Start) | is.na(Capital_End))) |> nrow() > 0) {
    capitals <- capitals |>
      dplyr::filter(!is.na(Capital_Start) & !is.na(Capital_End))
    cli::cli_warn("Rows with missing information on start and end dates of capitals (with valid coordinates) have been removed.")
  }



  # Convert coordinates into decimal format
  capitals <- capitals |>
    dplyr::mutate(Capital_Lat_dec = as.numeric(measurements::conv_unit(Capital_Lat_dec, from = 'deg_min_sec', to = 'dec_deg')),
                  Capital_Lon_dec = as.numeric(measurements::conv_unit(Capital_Lon_dec, from = 'deg_min_sec', to = 'dec_deg'))) |>
    dplyr::mutate(Capital_Lat_dec = ifelse(stringr::str_detect(Capital_Lat, "s"), Capital_Lat_dec*-1, Capital_Lat_dec),
                  Capital_Lon_dec = ifelse(stringr::str_detect(Capital_Lon, "w"), Capital_Lon_dec*-1, Capital_Lon_dec))

  # Replace three-digit years with 0
  capitals <- capitals |>
    dplyr::mutate(across(.cols = c(Capital_Start, Capital_End),
                         .fns = ~ stringr::str_replace_all(.x, "x", "0")),
                  Capital_Spell = as.numeric(Capital_End)-as.numeric(Capital_Start) + 1) |>
    dplyr::arrange(COWNum)



  # Error check
  if (dplyr::filter(capitals, stringr::str_count(trimws(Capital_Start)) !=4 | stringr::str_count(trimws(Capital_End)) !=4) |> nrow() > 0) {
    cli::cli_abort("Capitals with start or end date that are more or less than 4 digits.")
  }


  # Expand to capital-year observations
  capitals <- capitals |>
    dplyr::filter(!(is.na(Capital_Start) | is.na(Capital_End))) |>
    dplyr::mutate(year = purrr::map2(as.numeric(Capital_Start), as.numeric(Capital_End), seq)) |>
    tidyr::unnest(year)

  # Remove observations with start or end outside range
  capitals <- capitals |>
    dplyr::filter(!is.na(Capital_Start) & !is.na(Capital_End))


  # Reduce to one capital per polity-year (or keep multiple)
  if (multiple_capital == FALSE) {

    capitals <- capitals |>
      dplyr::group_by(Entity_Name, year) |>
      dplyr::arrange(desc(Capital_Start)) |>
      dplyr::mutate(spell_max = ifelse(Capital_Spell == max(Capital_Spell), 1, 0)) |>
      dplyr::mutate(spell_max_sum = sum(spell_max)) |>
      dplyr::mutate(cap_keep = ifelse(spell_max_sum == 1 & Capital_Spell == max(Capital_Spell), 1, 0),
                    cap_keep = ifelse(spell_max_sum > 1 & row_number() == 1, 1, cap_keep)) |>
      dplyr::ungroup() |>
      dplyr::arrange(Entity_Name, year) |>
      dplyr::filter(cap_keep == 1) |>
      dplyr::select(Entity_Name, year, Capital_Lat_dec, Capital_Lon_dec, Capital_Name)

  } else {

    capitals <- capitals |>
      dplyr::group_by(Entity_Name, year) |>
      dplyr::arrange(desc(Capital_Start)) |>
      dplyr::ungroup() |>
      dplyr::arrange(Entity_Name, year) |>
      dplyr::select(Entity_Name, year, Capital_Lat_dec, Capital_Lon_dec, Capital_Name)

  }

  # capitals <- capitals |>
  #   dplyr::group_by(Entity_Name, year) |>
  #   dplyr::arrange(desc(Capital_Start)) |>
  #   dplyr::summarise(
  #     Capital_Lat_dec = dplyr::first(Capital_Lat_dec),
  #     Capital_Lon_dec = dplyr::first(Capital_Lon_dec),
  #     Capital_Name = dplyr::first(Capital_Name)
  #   )


  # Merge polity-year obs with capital-year obs
  data <- data |>
    dplyr::left_join(capitals, by = c("Entity_Name", "year"))

  cli::cli_progress_done()

  ########## HIERARCHIES ###########

  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Extract hierarchy information")

  # For each polity, separate hierarchies by ";"
  hierarchy <- data_raw |>
    dplyr::select(Entity_Name, Hie) |>
    tidyr::separate("Hie", sprintf("Hie_[%d]", seq(1:10)),
                    sep = ";", extra = "drop", fill = "right")

  # Gather the data into long format, retaining all other information but hierarchies
  hierarchy <- hierarchy |>

    # Pivot longer
    tidyr::pivot_longer(cols = starts_with("Hie_["),
                        names_to = "Hie_Num",
                        values_to = "Hie_Info") |>
    dplyr::mutate(Hie_Info = stringr::str_trim(Hie_Info)) |>

    # Separate hierarchy information
    tidyr::separate("Hie_Info", c("Hie_COWID", "Hie_Year", "Hie_Type"),
                    sep = "_", extra = "drop", fill = "right") |>

    # Remove brackets
    dplyr::mutate(Hie_Year = gsub("[()]", "", Hie_Year)) |>

    # Separate year information
    tidyr::separate("Hie_Year", c("Hie_Start", "Hie_End"),
                    sep = "-", extra = "drop", fill = "right") |>

    # Sort and drop rows with missing year
    dplyr::arrange(Entity_Name) |>
    dplyr::filter(!is.na(Hie_Start)) |>
    dplyr::mutate(Hie_Start = stringr::str_trim(Hie_Start),
                  Hie_End = stringr::str_trim(Hie_End))


  ## Check for years with more or less than 4 digits (or missing)
  if (dplyr::filter(hierarchy, stringr::str_count(trimws(Hie_Start)) != 4 |
                    stringr::str_count(trimws(Hie_End)) != 4 |
                    is.na(Hie_Start) |
                    is.na(Hie_End)) |>
      nrow() > 0) {
    cli::cli_abort("Hierarchy start or end dates that are more or less than 4 digits or missing")
  }

  # Expand to hierarchy-year data set
  hierarchy <- hierarchy |>
    dplyr::mutate(year = purrr::map2(Hie_Start, Hie_End, seq)) |>
    tidyr::unnest(year)

  # Create polity-year data set with summary of hierarchy information over time
  hierarchy <- hierarchy |>
    dplyr::group_by(Entity_Name, year) |>
    dplyr::summarise(
      Hie_Type = as.character(paste0(stats::na.omit(Hie_Type), collapse = ";")),
      Hie_COWID = as.character(paste0(Hie_COWID, collapse = ";"))) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(Hie_Tributary = ifelse(grepl("1", Hie_Type) == TRUE, 1, 0),
                  Hie_Dependency = ifelse(grepl("2", Hie_Type) == TRUE, 1, 0))


  # Match to df
  data <- data |>
    dplyr::left_join(hierarchy, by = c("Entity_Name", "year"))

  # Recode missing hierarchy to 0
  data <- data |>
    dplyr::mutate(across(.cols = c(Hie_Tributary, Hie_Dependency),
                         .fns = ~ ifelse(is.na(.x), 0, .x)))

  # Create new hierarchy dummies based on lag and lead values (e.g., if lag is zero, hierarchy in t is 0)
  data <- data |>
    dplyr::arrange(COWNum, year) |>
    dplyr::group_by(COWNum) |>
    dplyr::mutate(Hie_Tributary_lag = dplyr::lag(Hie_Tributary, 1),
                  Hie_Dependency_lag = dplyr::lag(Hie_Dependency, 1),
                  Hie_Tributary_lead = dplyr::lead(Hie_Tributary, 1),
                  Hie_Dependency_lead = dplyr::lead(Hie_Dependency, 1)) |>
    dplyr::ungroup()

  data <- data |>
    dplyr::mutate(
      Hie_Tributary_inclusive = dplyr::case_when(
        Hie_Tributary == 1 & (Hie_Tributary_lag == 0 | Hie_Tributary_lead == 0) ~ 0,
        .default = Hie_Tributary),
      Hie_Dependency_inclusive = dplyr::case_when(
        Hie_Dependency == 1 & (Hie_Dependency_lag == 0 | Hie_Dependency_lead == 0) ~ 0,
        .default = Hie_Dependency)
    )

  # Create independence codings, i.e. state types (state, quasi-state, polity)
  data <- data |>
    dplyr::mutate(
      state_type = dplyr::case_when(
        Hie_Tributary_inclusive == 0 & Hie_Dependency_inclusive == 0 ~ "State",
        Hie_Tributary_inclusive == 1 & Hie_Dependency_inclusive == 0 ~ "Quasi-State",
        Hie_Tributary_inclusive == 0 & Hie_Dependency_inclusive == 1 ~ "Polity",
        Hie_Tributary_inclusive == 1 & Hie_Dependency_inclusive == 1 ~ "Polity"
      )
    )

  # Manually recode Australia and New Zealand in 1920 (because they gain independence in January 1920)
  data <- data |>
    dplyr::mutate(state_type = ifelse(COWNum == "900" & year == 1920, "State", state_type), # Australia
                  state_type = ifelse(COWNum == "920" & year == 1920, "State", state_type) # New Zeland
                  )

  # Remove temporary columns
  data <- data |>
    dplyr::select(-c(Hie_Tributary_lag, Hie_Tributary_lead, Hie_Dependency_lag, Hie_Dependency_lead,
                     Hie_Tributary_inclusive, Hie_Dependency_inclusive))

  # # For each polity, separate hierarchies by ";"
  # hierarchy <- data_raw |>
  #   dplyr::select(Entity_Name, Hie) |>
  #   tidyr::separate("Hie", sprintf("Hie_[%d]", seq(1:10)),
  #                   sep = ";", extra = "drop", fill = "right")
  #
  # # Gather the data into long format, retaining all other information but hierarchies
  # hierarchy <- hierarchy |>
  #
  #   # Pivot longer
  #   tidyr::pivot_longer(cols = starts_with("Hie_["),
  #                       names_to = "Hie_Num",
  #                       values_to = "Hie_Info") |>
  #   dplyr::mutate(Hie_Info = stringr::str_trim(Hie_Info)) |>
  #
  #   # Separate hierarchy information
  #   tidyr::separate("Hie_Info", c("Hie_COWID", "Hie_Year", "Hie_Type"),
  #                   sep = "_", extra = "drop", fill = "right") |>
  #
  #   # Remove brackets
  #   dplyr::mutate(Hie_Year = gsub("[()]", "", Hie_Year)) |>
  #
  #   # Separate year information
  #   tidyr::separate("Hie_Year", c("Hie_Start", "Hie_End"),
  #                   sep = "-", extra = "drop", fill = "right") |>
  #
  #   # Sort and drop rows with missing year
  #   dplyr::arrange(Entity_Name) |>
  #   dplyr::filter(!is.na(Hie_Start)) |>
  #   dplyr::mutate(Hie_Start = stringr::str_trim(Hie_Start),
  #                 Hie_End = stringr::str_trim(Hie_End))
  #
  #
  # ## Check for years with more or less than 4 digits (or missing)
  # if (dplyr::filter(hierarchy, stringr::str_count(trimws(Hie_Start)) != 4 |
  #                   stringr::str_count(trimws(Hie_End)) != 4 |
  #                   is.na(Hie_Start) |
  #                   is.na(Hie_End)) |>
  #     nrow() > 0) {
  #   cli::cli_abort("Hierarchy start or end dates that are more or less than 4 digits or missing")
  # }
  #
  #
  # # Expand to hierarchy-year data set
  # hierarchy <- hierarchy |>
  #   dplyr::mutate(year = purrr::map2(Hie_Start, Hie_End, seq)) |>
  #   tidyr::unnest(year)
  #
  #
  #
  # # Create polity-year data set with summary of hierarchy information over time
  # hierarchy <- hierarchy |>
  #   dplyr::group_by(Entity_Name, year) |>
  #   dplyr::summarise(
  #     Hie_Type = as.character(paste0(stats::na.omit(Hie_Type), collapse = ";")),
  #     Hie_COWID = as.character(paste0(Hie_COWID, collapse = ";"))) |>
  #   dplyr::ungroup() |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(Hie_Tributary = ifelse(grepl("1", Hie_Type) == TRUE, 1, 0),
  #                 Hie_Dependency = ifelse(grepl("2", Hie_Type) == TRUE, 1, 0))
  #
  #
  # # Match to df
  # data <- data |>
  #   dplyr::left_join(hierarchy, by = c("Entity_Name", "year"))
  #
  # # Recode missing hierarchy to 0
  # data <- data |>
  #   dplyr::mutate(across(.cols = c(Hie_Tributary, Hie_Dependency),
  #                        .fns = ~ ifelse(is.na(.x), 0, .x)))
  #
  #
  # # Create dummies for independent polities (based on hierarchy type)
  # data <- data |>
  #   dplyr::mutate(independent_dependency = ifelse(Hie_Dependency == 1, 0, 1),
  #                 independent_tributary = ifelse(Hie_Tributary == 1, 0, 1))
  #
  # # Create lags and leads for hierarchy dummies
  # data <- data |>
  #   dplyr::arrange(COWNum, year) |>
  #   dplyr::group_by(COWNum) |>
  #   dplyr::mutate(Hie_Dependency_lag = dplyr::lag(Hie_Dependency, 1),
  #                 Hie_Dependency_lead = dplyr::lead(Hie_Dependency, 1),
  #                 Hie_Tributary_lag = dplyr::lag(Hie_Tributary, 1),
  #                 Hie_Tributary_lead = dplyr::lead(Hie_Tributary, 1))
  #
  # # Recode independent dummies based on lags and leads
  # data <- data |>
  #   dplyr::mutate(independent_dependency = ifelse(
  #     (Hie_Dependency == 1 & Hie_Dependency_lag == 0) |
  #       (Hie_Dependency & Hie_Dependency_lead == 0),
  #     1, independent_dependency)) |>
  #   dplyr::mutate(independent_dependency = ifelse(
  #     Hie_Dependency == 1 & is.na(Hie_Dependency_lag), 0, independent_dependency)) |>
  #   dplyr::mutate(independent_dependency = ifelse(
  #     Hie_Dependency == 1 & is.na(Hie_Dependency_lead), 0, independent_dependency))
  #
  # data <- data |>
  #   dplyr::mutate(independent_tributary = ifelse(
  #     (Hie_Tributary == 1 & Hie_Tributary_lag == 0) |
  #       (Hie_Tributary & Hie_Tributary_lead == 0),
  #     1, independent_tributary)) |>
  #   dplyr::mutate(independent_tributary = ifelse(
  #     Hie_Tributary == 1 & is.na(Hie_Tributary_lag), 0, independent_tributary)) |>
  #   dplyr::mutate(independent_tributary = ifelse(
  #     Hie_Tributary == 1 & is.na(Hie_Tributary_lead), 0, independent_tributary))
  #
  # # Create dummy for independence excluding both hierarchy types
  # data <- data |>
  #   dplyr::mutate(independent = ifelse(
  #     independent_dependency == 1 & independent_tributary == 1, 1, 0))


  cli::cli_progress_done()

  ### PREPARE FOR OUTPUT

  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Finishing")


  ## Select variables for output
  data <- data |>
    dplyr::ungroup() |>
    dplyr::mutate(COWNum = as.numeric(COWNum)) |>
    dplyr::select(polity_name = Entity_Name,
                  COWID, COWNUM = COWNum,
                  year,
                  other_names = Other_Names,
                  start_year, end_year,
                  population_10k = Population_10K, autonomy = Autonomy,
                  capital_name = Capital_Name, capital_lon = Capital_Lon_dec, capital_lat = Capital_Lat_dec,
                  capital_raw = Capital,
                  hie_COWID = Hie_COWID, hie_type = Hie_Type,
                  hie_tributary = Hie_Tributary, hie_dependency = Hie_Dependency, state_type, hie_raw = Hie,
                  # ethnonational_group = Ethnonational_group, ## For now
                  EPR_link = EPR_Link,
                  # ACD_actor_link = ACD_Actor_Link, ## For now
                  # end_agent = End_Agent,
                  destination_states_COWNUM = Destination_States, destination_state_1 = Destination_State_1,
                  destination_state_2 = Destination_State_2, destination_state_3 = Destination_State_3,
                  destination_state_4 = Destination_State_4, destination_state_5 = Destination_State_5)

  return(data)

}






