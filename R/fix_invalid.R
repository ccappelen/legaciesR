#' @title Fix Invalid Geometries
#' @description Rebuilds invalid geometries. If necessary, the functions will iteratively
#'   lower the snapping precision if default options are inadequate.

#' @param shp An sf dataframe.
#' @param max_precision Integer, maximum level of precision for [s2::s2_options()], default is 10^7.
#' @param min_precision Integer, minimum allowed precision for [s2::s2_options()], default is 10. If geometry
#'   is invalid after rebuilding with minimum precision allowed, the function will return an error if
#'   `stop_if_invalid` is `TRUE` and a warning otherwise.
#' @param stop_if_invalid Logical, whether the function should return an error if it fails
#'   to rebuild a valid geometry. The error message will indicate the row number of the invalid geometry.
#' @param progress Logical, whether a progress bar should be displayed. Default is `TRUE.`
#' @param parallel Logical, whether to use parallel processing with `ncores` number of cores.
#'   See details below.
#' @param ncores Integer, number of cores to use for parallel processing. Default is all available cores minus 1.
#' @param report Logical, whether to report the number of valid, rebuilt, and invalid geometries. If
#' `reportColumns` is `FALSE`, no report will be generated.
#' @param reportColumns Logical, whether to return the status of each geometry in new columns
#'   (`geom_valid`, `rebuilt`, and `snap_precision`).

#' @return Returns an sf dataframe with the same number of features as the input. The dataframe includes
#'   three new columns: `geom_valid` indicates whether the original geometry was valid (`TRUE`), `rebuilt`
#'   indicates if the geometry was successfully rebuilt, and `snap_precision` indicates the snapping
#'   precision used to rebuild geometry (if `rebuilt` is `FALSE`, `snap_precision` indicates the minimum snapping
#'   precision that led to invalid geometry).

#' @import sf
# #' @import pbapply
#' @import dplyr
#' @import s2
# #' @import parallel
#' @import cli
#' @import future
#' @importFrom furrr future_map
#' @import progressr

#' @section Support for parallel processing:
#' Parallel processing is is implemented using the [future::future] framework. There are two ways
#' of running jobs in parallel: `multicore` which uses 'forking' to run multiple jobs in parallel with
#' shared memory and `multisession` which launches a set of background R sessions. 'Forking' can be faster
#' than multisession because of the larger overhead associated with copying the active environment to
#' each background R session (whereas forking processes shares memory). However, 'forking' is not
#' supported on Windows platforms and is considered unstable when running from within RStudio
#' (on both Windows and Unix systems such as MacOS). The function will automatically determine
#' whether `multicore` is supported by the platform and choose the appropriate plan.
#'
#' The greater overhead associated with `multisession` is primarily during the first parallel
#' run in a given R session (since the background R sessions stays available for additional
#' parallel jobs). It is possible to define a [future::plan()] in the global environment,
#' which will minimize overhead in subsequent parallel jobs (apart from the first). The function will
#' automatically detect if a `multisession` plan has been set globally and, thus, will not close
#' background sessions after running.

#' @export

fix_invalid <- function(shp,
                        max_precision = 10^7,
                        min_precision = 10,
                        stop_if_invalid = FALSE,
                        progress = TRUE,
                        parallel = NULL,
                        ncores,
                        report = TRUE,
                        reportColumns = TRUE) {

  # Initiate variables (to pass CMD Check)
  geom_valid = rebuilt = rowid = NULL

  # Create unique row number
  shp <- shp |>
    dplyr::mutate(rowid = dplyr::row_number())

  # Split dataframe into list (one item per row)
  shp_list <- split(shp, shp$rowid)

  # Set progress bar options
  if (progress) {
    old_handler <- progressr::handlers(progressr::handler_progress(
      format = ":spin :bar :percent Elapsed: :elapsed, Remaining: ~:eta",
      intrusiveness = 1
    ))
    on.exit(
      if (is.null(old_handler)) progressr::handlers("void") else progressr::handlers(old_handler),
      add = TRUE
    )
  }

  # Set parallel options
  multicore_support <- future::supportsMulticore()
  future_global <- is(future::plan(), "multisession") | is(future::plan(), "multicore")

  if (is.null(parallel)) {
    if (nrow(shp) < 5000) {
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



  # Set parallel options
  # supportedByOS <- ifelse(.Platform$OS.type == "unix", TRUE, FALSE)
  #
  # if (is.null(parallel)) {
  #   if (supportedByOS) {
  #     parallel <- TRUE
  #   } else {
  #     parallel <- FALSE
  #   }
  # }

  # if (parallel) {
  #   if (supportedByOS) {
  #     if(missing(ncores)) {
  #       future::plan("multisession", workers = future::availableCores() - 1)
  #     } else {
  #       if (ncores >= 2) {
  #         future::plan("multisession", workers = ncores)
  #       } else{
  #         future::plan("sequential")
  #         if (progress) cli::cli_inform("Jobs running sequentially since {.arg ncores} is 1.")
  #       }
  #     }
  #     if (progress) cli::cli_inform("Jobs running in parallel using forking (multicore)")
  #   } else {
  #     if (missing(ncores)) {
  #       ncores <- future::availableCores() - 1
  #     }
  #     future::plan("multisession", workers = ncores)
  #     if (progress) cli::cli_inform("Jobs running in parallel using multisession.")
  #   }
  # } else {
  #   future::plan("sequential")
  #   if (progress) cli::cli_inform("Jobs running sequentially.")
  # }
  # on.exit(future::plan("sequential"), add = TRUE)

  ## FRAMEWORK FOR PBAPPLY
  # if (is.null(parallel)) {
  #   if (supportedByOS) {
  #     parallel <- TRUE
  #   } else {
  #     parallel <- FALSE
  #   }
  # }
  #
  # if (parallel) {
  #   if (supportedByOS) {
  #     if (missing(ncores)) {
  #       cl <- parallel::detectCores()-1
  #     } else {
  #       cl <- ncores
  #     }
  #     if (progress) cli::cli_inform("Jobs running in parallel using forking (multicore)")
  #   } else {
  #     if (missing(ncores)) {
  #       ncores <- parallel::detectCores()-1
  #     }
  #     ncores <- ncores
  #     cl <- parallel::makeCluster(ncores)
  #     on.exit(stopCluster(cl))
  #     if (progress) cli::cli_inform("Jobs running in parallel using multisession.")
  #   }
  # } else {
  #   cl <- NULL
  #   if (progress) cli::cli_inform("Jobs running sequentially.")
  # }

  # Write helper function to make valid geometries (iteratively if necessary)
  fun.make.valid <- function(shp1,
                             max_precision,
                             min_precision,
                             stop_if_invalid) {

    if (nrow(shp1) != 1) {
      cli::cli_abort(
        c("Must be only 1 geometry per list item.",
          "i" = "This is likely caused by {.var shp} having the wrong format. Are there observations with multiple geometries?")
        )
    }

    if (st_is_valid(shp1)) {
      if (reportColumns) {
        shp1[["geom_valid"]] <- TRUE
        shp1[["rebuilt"]] <- FALSE
        shp1[["snap_precision"]] <- NA
      }

    } else {

      shp1 <- st_make_valid(shp1)
      if (st_is_valid(shp1)) {
        if (reportColumns) {
          shp1[["geom_valid"]] <- FALSE
          shp1[["rebuilt"]] <- TRUE
          shp1[["snap_precision"]] <- NA
        }

        return(shp1)

      } else {

        shp_invalid <- TRUE
        snap_prec <- max_precision

        while (shp_invalid == TRUE & snap_prec >= min_precision) {
          shp1 <- st_make_valid(shp1, s2_options = s2::s2_options(snap = s2::s2_snap_precision(snap_prec)))
          shp_invalid <- !st_is_valid(shp1)
          snap_prec_last <- snap_prec
          snap_prec <- ifelse(snap_prec > 100, snap_prec / 10, snap_prec - 10)
        }

        if (!st_is_valid(shp1) & stop_if_invalid == TRUE) {
          cli::cli_abort(c(
            paste0("Unable to rebuild valid geometry for row ", shp1[["rowid"]]),
            "i" = "Check violating geometry for errors. Alternatively, set {.arg stop_if_invalid} to {.code FALSE}."
          ))
        }

        if (!st_is_valid(shp1) & stop_if_invalid == FALSE) {
          if (reportColumns) {
            shp1[["geom_valid"]] <- FALSE
            shp1[["rebuilt"]] <- FALSE
            shp1[["snap_precision"]] <- snap_prec_last
          }
        }

        if (st_is_valid(shp1)) {
          if (reportColumns) {
            shp1[["geom_valid"]] <- FALSE
            shp1[["rebuilt"]] <- TRUE
            shp1[["snap_precision"]] <- snap_prec_last
          }
        }
      }
    }
    return(shp1)
  }

  # Run fun.make.valid over all geometries in shp
  # shp_list_valid <- pbapply::pblapply(
  #   shp_list,
  #   FUN = function(x) fun.make.valid(x, max_precision, min_precision, stop_if_invalid),
  #   cl = cl
  # )

  # old_handler <- progressr::handlers(handler_pbcol(
  #   adjust = 1,
  #   pad = 5,
  #   complete = function(s) cli::bg_br_green(cli::col_black(s)),
  #   incomplete = function(s) cli::bg_white(cli::col_black(s)),
  #   intrusiveness = 2))

  progressr::with_progress({
    p <- progressr::progressor(along = shp_list)

    shp_list_valid <- furrr::future_map(
      shp_list,
      .f = function(x) {
        p()
        fun.make.valid(x, max_precision, min_precision, stop_if_invalid)
      },
      .options = furrr_options(seed = TRUE)
    )
  }, interrupts = TRUE)


  # Check list for error messages
  shp_errors <- sapply(shp_list_valid, inherits, what = "try-error")
  if (any(shp_errors) && stop_if_invalid) {
    cli::cli_abort(c(
      "Some items failed to rebuild as valid geometries.",
      "i" = "Set {.arg stop_if_invalid} to {.code FALSE} if you would
        like to proceed with potentially invalid geometries."
    ))
    }


  # Reduce list to sf dataframe
  shp_list_new <- shp_list_valid %>% bind_rows()

  # Reporting options
  if (!reportColumns) {
    report <- FALSE
  }

  if (report) {
    valid_rowids <- shp_list_new |>
      filter(geom_valid == TRUE) |>
      pull(rowid)
    rebuilt_rowids <- shp_list_new |>
      filter(geom_valid == FALSE & rebuilt == TRUE) |>
      pull(rowid)
    invalid_rowids <- shp_list_new %>%
      filter(geom_valid == FALSE & rebuilt == FALSE) |>
      pull(rowid)

    cli::cli_inform(c(
      paste0(length(rebuilt_rowids), " (", round(length(rebuilt_rowids)/nrow(shp_list_new), 3)*100, " %) ",
             "geometries were successfully rebuilt.\n"),
      paste0(length(invalid_rowids), " (", round(length(invalid_rowids)/nrow(shp_list_new), 3)*100, " %) ",
             "geometries failed to rebuild as valid.\n"),
      if (length(invalid_rowids) > 0) {
        ifelse(length(invalid_rowids) <= 10,
               paste0("Invalid geometries: ", paste0(invalid_rowids, collapse = ", ")),
               paste0("Invalid geometries: ",
                      paste0(invalid_rowids[1:10], collapse = ", "),
                      " ..... (more than 10)"))
      }
    ))
  }

  # Remove rowid column
  shp_list_new[["rowid"]] <- NULL

  return(shp_list_new)

}

