#' Detect potential errors in maps
#'
#' @param shp An sf dataframe containing all geometries
#' @param id_var Name of grouping variable (e.g., COWID)
#' @param capital_data Dataframe containing information on capitals (by year). Should have the same
#' `id_var` and `period_var` names as `shp`.
#' @param period_var Name of year variable
#' @param errors Character vector of which errors to search for. See details.
#' @param dist Minimum allowable distance (in km) for which capitals can be outside polygons.
#' @param parallel Whether to use parallel processing. Default is TRUE.
#' @param ncores Number of cores to use for parallel processing. Default is to use all available cores.
#' @param progress Whether to print progress updates. Default is TRUE.
#' @param report Whether the output should be a report of potential errors. Default is an sf dataframe
#'   with columns for each error detection.
#' @param returnList Whether to return a list containing the full sf data frame AND separate data frames
#'   for each error type (default) or just the full sf data frame. The full sf data frame will either way
#'   include columns indicating potential errors for each type.
#'
#' @import sf
#' @import cli
#' @import future
#' @importFrom furrr future_map
#' @import dplyr
#'
#' @returns
#' Data frame
#'  \item{`id_duplicates`}{Whether COWID and COWNUM do not align, i.e. are there duplicate COWIDs with different COWNUMs?}
#'  \item{`missing_id`}{Whether there are geometries with missing `id_var`.}
#'  \item{`singles`}{Whether there are COWIDs with only one map (may indicate wrong COWID).}
#'  \item{`year`}{Whether there are polygons with years outside the study window (i.e. 1750-1920) or if year is missing.}
#'  \item{`overlap`}{Whether there are COWIDs for which one or more polygons do not overlap with any of the other polygons.}
#'  \item{`capital`}{Whether any of the capitals associated with a COWID falls outside ALL polygons. The option `dist` can be
#'  used to specify a minimum allowable distance.}
#'  \item{`empty`}{Check whether there are empty geometries.}
#'
#' @export




detect_errors <- function(shp,
                          capital_data,
                          id_var,
                          period_var,
                          errors,
                          dist,
                          parallel = TRUE,
                          ncores,
                          progress = TRUE,
                          report = TRUE,
                          returnList = TRUE){


  # # Declare internal variables (to pass R CMD check)
  # internals <- c("year")
  # utils::globalVariables()

  geometry <- COWID <- COWNUM <- year <- lyear <- hyear <- NULL
  year_outside <- year_na <- non_overlap_any <- Cap_Lon <- Cap_Lat <- NULL

  output_args <- c("id_duplicates", "missing_id", "overlap", "capital", "singles", "year", "empty")
  if (missing(errors)) {
    errors <- output_args
  } else {
    if (any(!errors %in% output_args)) {
      cli::cli_alert("{.arg output} contains elements that do not match any summary measure.")
    }
    errors <- match.arg(errors, choices = output_args, several.ok = TRUE)
  }

  if ("capital" %in% errors && missing(capital_data)) {
    cli::cli_abort("{.arg capital data} must be specified if `capital` is selected for error detection.")
  }


  if (missing(id_var)) {
    cli::cli_abort("{.arg id_var} is missing.")
  }

  if (is.character(substitute(id_var))) {
    cli::cli_abort("{.arg id_var} must be a variable, not a character string.")
  }

  if (missing(period_var)) {
    cli::cli_abort("{.arg id_var} is missing.")
  }

  if (is.character(substitute(period_var))) {
    cli::cli_abort("{.arg id_var} must be a variable, not a character string.")
  }

  id_var_str <- deparse(substitute(id_var))
  period_var_str <- deparse(substitute(period_var))

  # Set parallel options
  multicore_support <- future::supportsMulticore()
  future_global <- is(future::plan(), "multisession") | is(future::plan(), "multicore")

  if (is.null(parallel)) {
    if (length(shp_list) < 250) {
      parallel <- FALSE
    } else {
      parallel <- TRUE
    }
  }

  if (!parallel) {
    if (future_global) {
      old_plan <- future::plan("sequential")
      on.exit(future::plan(old_plan), add = TRUE)
      cli::cli_inform(c("Jobs running sequentially.",
                        "i" = "Reverts to original plan after running."
      ))
    }
  }

  if (parallel) {
    if (!future_global) {
      if (missing(ncores)) {
        ncores <- future::availableCores() - 1
      }

      if (ncores < 2) {
        old_plan <- future::plan("sequential")
        on.exit(future::plan(old_plan), add = TRUE)
        cli::cli_inform("Jobs running sequentially since {.arg ncores} is 1.")
      }

      if (ncores >= 2) {
        if (multicore_support) {
          old_plan <- future::plan("multicore", workers = ncores)
          on.exit(future::plan(old_plan), add = TRUE)
          cli::cli_inform(c("Jobs running in parallel using 'multicore' (forking).",
                            "i" = "Reverts to original plan after running."
          ))
        } else {
          old_plan <- future::plan("multisession", workers = ncores)
          on.exit(future::plan(old_plan), add = TRUE)
          cli::cli_inform(c("Jobs running in parallel using 'multisession'.",
                            "i" = "Reverts to original plan after running."
          ))
        }
      }
    }
  }




  steps <- length(errors)
  step <- 0

  error_list <- list()



  ## Check if there are empty geometries
  if ("empty" %in% errors) {
    step <- step + 1
    if (progress) {
      cli::cli_progress_step("{step}/{steps}: Checking for empty geometries")
    }

    shp <- shp |>
      dplyr::mutate(empty_geom = sf::st_is_empty(geometry))

    empty_geom <- shp |>
      dplyr::select({{ id_var }}, empty_geom) |>
      dplyr::filter(empty_geom == TRUE)

    if (nrow(empty_geom) > 0) {
      error_list$empty_geom <- empty_geom
    } else {
      error_list$empty_geom <- NULL
    }

    if (progress) {
      cli::cli_progress_done()
    }
  }



  ## Check if COWID and COWNUM do not align (i.e. COWID duplicates)
  if ("id_duplicates" %in% errors) {
    step <- step + 1
    if (progress) {
      cli::cli_progress_step("{step}/{steps}: COWID duplicates")
    }
    if (length(unique(shp[["COWID"]])) != length(unique(shp[["COWNUM"]]))) {
      # cli::cli_abort("The number of COWIDs and COWNUMs do not match up, indicating that there are duplicates of COWID with the same COWNUM.")

      id_duplicates <- shp |>
        dplyr::group_by(COWID, COWNUM) |>
        dplyr::group_keys() |>
        dplyr::group_by(COWID) |>
        dplyr::mutate(n = n()) |>
        dplyr::filter(n > 1)

    } else {

      id_duplicates <- NULL

    }

    if (nrow(id_duplicates) > 0) {
      error_list$id_duplicates <- id_duplicates
    } else {
      error_list$id_duplicates <- NULL
    }

    if (progress) {
      cli::cli_progress_done()
    }
  }



  ## Check for missing ID
  if ("missing_id" %in% errors) {
    step <- step + 1
    if (progress) {
      cli::cli_progress_step("{step}/{steps}: Missing ID")
    }

    shp <- shp |>
      dplyr::mutate(missing_id = is.na({{ id_var }}) | {{ id_var }} == "99999" | {{ id_var }} == 99999)

    missing_id <- shp |>
      dplyr::select({{ id_var }}, missing_id) |>
      dplyr::filter(missing_id == TRUE)

    if (nrow(missing_id) > 0) {
      error_list$missing_id <- missing_id
    } else {
      error_list$missing_id <- NULL
    }

    if (progress) {
      cli::cli_progress_done()
    }
  }



  ## Check if there are COWIDs with only one map
  if ("singles" %in% errors) {
    step <- step + 1
    if (progress) {
      cli::cli_progress_step("{step}/{steps}: COWIDs with only 1 map")
    }
    shp <- shp |>
      dplyr::group_by({{ id_var }}) |>
      dplyr::mutate(single_map = n() == 1)

    single_map <- shp |>
      dplyr::select(COWID, single_map) |>
      dplyr::filter(single_map == TRUE)

    if (nrow(single_map) > 0) {
      error_list$single_map <- single_map
    } else {
      error_list$single_map <- NULL
    }

    if (progress) {
      cli::cli_progress_done()
    }
  }


  ## Check if there are maps with year outside study period or if there are years missing (including lyear/hyear)
  if ("year" %in% errors) {
    step <- step + 1
    if (progress) {
      cli::cli_progress_step("{step}/{steps}: Missing year or year outside 1750-1920")
    }
    shp <- shp |>
      dplyr::mutate(year_outside = {{ period_var }} < 1750 | {{ period_var }} > 1920) |>
      dplyr::mutate(year_na = is.na({{ period_var }}))

    year_error <- shp |>
      dplyr::select(COWID, year, lyear, hyear, year_outside, year_na) |>
      dplyr::filter(year_outside == TRUE | year_na == TRUE)

    if (nrow(year_error) > 0) {
      error_list$year_error <- year_error
    } else {
      error_list$year_error <- NULL
    }

    if (progress) {
      cli::cli_progress_done()
    }

  }


  ## Check if there are COWIDs with polygons not overlapping
  if ("overlap" %in% errors) {
    step <- step + 1
    if (progress) {
      cli::cli_progress_step("{step}/{steps}: COWIDs with polygons not overlapping")
    }

    shp_list <- split(shp, shp[[id_var_str]])
    shp <- furrr::future_map(shp_list,
                             .f = function(x) {
                               tmp <- sf::st_overlaps(x, sparse = TRUE) |>
                                 sapply(FUN = any) |>
                                 suppressMessages()
                               x$non_overlap <- !tmp
                               return(x)
                             },
                             .options = furrr_options(seed = TRUE),
                             .progress = FALSE
    ) |>
      bind_rows()

    non_overlap <- shp |>
      dplyr::group_by({{ id_var }}) |>
      dplyr::mutate(non_overlap_any = sum(non_overlap) > 0) |>
      dplyr::filter(non_overlap_any == TRUE)

    if (nrow(non_overlap) > 0) {
      error_list$non_overlap <- non_overlap
    } else {
      error_list$non_overlap <- NULL
    }

    if (progress) {
      cli::cli_progress_done()
    }
  }



  ## Check if there are capitals outside polygons
  if ("capital" %in% errors) {
    step <- step + 1
    if (progress) {
      e <- environment()
      msg <- ""
      cli::cli_progress_step("{step}/{steps}: Whether the capital falls outside all polygons{msg}",
                             spinner = TRUE)
    }

    shp_list <- split(shp, 1:nrow(shp))
    shp <- lapply(shp_list,
                 FUN = function(x) {
                   tmp_id <- x[[id_var_str]]
                   tmp_year <- x[[period_var_str]]
                   cap_tmp <- capital_data |>
                     dplyr::filter({{ id_var }} == tmp_id & {{ period_var }} == tmp_year) |>
                     dplyr::filter(!is.na(Cap_Lon))

                   if (nrow(cap_tmp) > 0) {
                     cap_tmp <- cap_tmp |>
                       sf::st_as_sf(coords = c("Cap_Lon", "Cap_Lat"), crs = 4326) |>
                       sf::st_cast("POINT") |>
                       suppressWarnings()

                     cap_within <- sf::st_within(cap_tmp, x, sparse = F) |>
                       sapply(FUN = all) |>
                       all()

                     x$cap_outside <- !cap_within

                     if (!cap_within) {
                       cap_dist <- sf::st_distance(cap_tmp, x) |>
                         units::set_units("km") |>
                         units::drop_units() |>
                         max()
                       x$cap_outside_dist <- cap_dist
                     } else {
                       x$cap_outside_dist <- NA
                     }

                   }else{
                     x$cap_outside <- NA
                     x$cap_outside_dist <- NA
                   }

                   if (progress) {
                     .step_id <- tmp_id
                     e$msg <- glue::glue(": {.step_id}")
                     cli::cli_progress_update(.envir = e)
                   }

                   return(x)
                 }) |>
      bind_rows()

    cap_outside <- shp |>
      filter(cap_outside)

    if (nrow(cap_outside) > 0) {
      error_list$cap_outside <- cap_outside
    } else {
      error_list$cap_outside <- NULL
    }

    if (progress) {
      msg <- ""
      cli::cli_progress_update()
      cli::cli_progress_done()
    }
  }


  if (report) {

    report_str <- c("\n", "POTENTIAL ERRORS:\n")

    if ("id_duplicates" %in% errors) {
      n_id_duplicates <- nrow(error_list$id_duplicates)
      if (is.null(n_id_duplicates)) {
        n_id_duplicates <- 0
      }
      str_id_duplicates <- paste0(n_id_duplicates, " states with potentially duplicate COWIDs.")
      report_str <- c(report_str, "*" = str_id_duplicates)
    }

    if ("empty" %in% errors) {
      n_empty_geom <- nrow(error_list$empty_geom)
      if (is.null(n_empty_geom)) {
        n_empty_geom <- 0
      }
      str_empty_geom <- paste0(n_empty_geom, " shapes with empty geometries.")
      report_str <- c(report_str, "*" = str_empty_geom)
    }

    if ("missing_id" %in% errors) {
      n_missing_id <- nrow(error_list$missing_id)
      if (is.null(n_missing_id)) {
        n_missing_id <- 0
      }
      str_missing_id <- paste0(n_missing_id, " shapes with missing IDs.")
      report_str <- c(report_str, "*" = str_missing_id)
    }

    if ("singles" %in% errors) {
      n_single_map <- nrow(error_list$single_map)
      if (is.null(n_single_map)) {
        n_single_map <- 0
      }
      str_single_map <- paste0(n_single_map, " COWIDs with only a single map.")
      report_str <- c(report_str, "*" = str_single_map)
    }

    if ("year" %in% errors) {
      n_year_error <- nrow(error_list$year_error)
      if (is.null(n_year_error)) {
        n_year_error <- 0
      }
      str_year_error <- paste0(n_year_error, " maps with years missing or outside 1750-1920.")
      report_str <- c(report_str, "*" = str_year_error)
    }

    if ("overlap" %in% errors) {
      n_non_overlap <- nrow(error_list$non_overlap)
      if (is.null(n_non_overlap)) {
        n_non_overlap <- 0
      }
      str_non_overlap <- paste0(n_non_overlap, " maps that do not overlap with other shapes with the same ID.")
      report_str <- c(report_str, "*" = str_non_overlap)
    }

    if ("capital" %in% errors) {
      n_cap_outside <- nrow(error_list$cap_outside)
      if (is.null(n_cap_outside)) {
        n_cap_outside <- 0
      }
      str_cap_outside <- paste0(n_cap_outside, " maps where the capital falls outside the polygon.")
      report_str <- c(report_str, "*" = str_cap_outside)
    }

    cli::cli_inform(report_str)

  }

  if (returnList) {
    out <- list(shp, error_list)
    names(out) <- c("data", "report")
  } else {
    out <- shp
  }

  return(out)


}

