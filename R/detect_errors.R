#' Detect potential errors in maps
#'
#' @param shp An sf dataframe containing all geometries
#' @param id_var Name of grouping variable (e.g., COWID)
#' @param period_var Name of year variable
#' @param errors Character vector of which errors to search for. See details.
#' @param dist Minimum allowable distance (in km) for which capitals can be outside polygons.
#' @param parallel Whether to use parallel processing. Default is TRUE.
#' @param ncores Number of cores to use for parallel processing. Default is to use all available cores.
#' @param report Whether the output should be a report of potential errors. Default is an sf dataframe
#'   with columns for each error detection.
#'
#' @import sf
#' @import cli
#' @import future
#' @import furrr
#' @import dplyr
#'
#' @returns
#' Data frame
#'  \item{`singles`}{Whether there are COWIDs with only one map (may indicate wrong COWID).}
#'  \item{`year`}{Whether there are polygons with years outside the study window (i.e. 1750-1920) or if year is missing.}
#'  \item{`overlap`}{Whether there are COWIDs for which one or more polygons do not overlap with any of the other polygons.}
#'  \item{`capital`}{Whether any of the capitals associated with a COWID falls outside ALL polygons. The option `dist` can be
#'  used to specify a minimum allowable distance.}
#'




detect_errors <- function(shp,
                          id_var,
                          period_var,
                          errors,
                          dist,
                          parallel = TRUE,
                          ncores,
                          report = FALSE){


  # # Declare internal variables (to pass R CMD check)
  # internals <- c("year")
  # utils::globalVariables()

  output_args <- c("overlap", "capital", "distance", "singles", "year")
  if (missing(errors)) {
    errors <- output_args
  } else {
    if (any(!errors %in% output_args)) {
      cli::cli_alert("{.arg output} contains elements that do not match any summary measure.")
    }
    errors <- match.arg(errors, choices = output_args, several.ok = TRUE)
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
  id_var_str <- deparse(substitute(period_var))

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
  step <- step + 1


  ## Check if there are COWIDs with only one map
  if ("singles" %in% errors) {
    cli::cli_progress_step("{step}/{steps}: COWIDs with only 1 map")
    shp <- shp |>
      dplyr::group_by({{ id_var }}) |>
      dplyr::mutate(single_map = n() == 1)
  }



  ## Check if there are maps with year outside study period
  if ("year" %in% errors) {
    cli::cli_progress_step("{step}/{steps}: Missing year or year outside 1750-1920")
    shp <- shp |>
      dplyr::mutate(year_outside = {{ period_var }} < 1750 | {{ period_var }} > 1920) |>
      dplyr::mutate(year_na = is.na({{ period_var }}))
  }


  ## Check if there are COWIDs with polygons not overlapping
  if ("overlap" %in% errors) {
    cli::cli_progress_step("{step}/{steps}: COWIDs with polygons not overlapping")
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
  }


  return(shp)

}

