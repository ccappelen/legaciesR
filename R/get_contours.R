#' @title Get Contour Polygons Across Groups
#' @description This function creates contour polygons across multiple groups, either by group alone or
#' in combination with a time column.

#' @param shp An sf dataframe containing all geometries (polygons).
#' @param id_var Name of the grouping variable.
#' @param by_period Logical, indicating if shapes should be divided into groups by time in addition to IDs.
#'   Default is `FALSE.`
#' @param period_var Name of the period variable to be used for time grouping.
#'   This argument is necessary if `by_period` is `TRUE.`
#' @param interval Scalar or vector indicating the intervals to group by. If `NULL` (default), the
#'   intervals will automatically be set to 20 (years) and include the full range (i.e. `seq(min, max, 20)`).
#'   For user specification, the value can be either a scalar, indicating the length of intervals
#'   (in which case the full range is used), or a vector specifying the exact breaks, including both the
#'   start of the first interval and end of the last interval.
#' @inheritParams contour_polygons
# @param cuts Integer of length 1 specifying the number of (equally spaced) contour polygons to be returned.
#   For example, if `cuts = 10` the function will return 10 polygons representing the 10 deciles (0-0.1, 0.1-0.2, ...).
#' @param smoothing Logical, whether to apply smoothing after polygonizing the raster. Default is `TRUE`.
#' @param nmap_threshold Integer, indicating the number of shapes required within each group. Default is 5.
#' @param returnList Logical, whether to return a list of geometries by group.
#'   Default is to return an sf dataframe containing all geometries.
#' @param progress Logical, whether to show a progress bar.
#' @param parallel Logical, whether to use parallel processing with `ncores` number of cores.
#'   See details below.
#' @param ncores Integer, the number of cores to use for parallel processing. Default is all available cores minus 1.
#' @param ... Additional arguments passed to [contour_polygons].

#' @return Returns either an sf dataframe (default) or a list of sf dataframes (one list item per group).

#' @importFrom rlang .data
#' @import sf
#' @import dplyr
# #' @import pbapply
# #' @import parallel
#' @import cli
#' @import future
#' @importFrom furrr future_map
#' @importFrom furrr furrr_options
#' @import progressr
#' @importFrom methods is

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

get_contours <- function(shp,
                         id_var,
                         by_period = FALSE,
                         interval = NULL,
                         period_var,
                         cuts = 4,
                         include_higher = TRUE,
                         nmap_threshold = 5,
                         invalid_geom = c("stop", "fix", "exclude"),
                         smoothing = TRUE,
                         returnList = FALSE,
                         progress = TRUE,
                         parallel = NULL,
                         ncores,
                         ...){

  # Check arguments
  if (missing(id_var)) {
    cli::cli_abort("Missing {.arg id_var}")
  }

  if (is.character(substitute(id_var))) {
    cli::cli_abort("{.arg id_var} must be a variable, not a character string.")
  }

  invalid_geom <- match.arg(invalid_geom)

  id_var <- substitute(id_var)
  id_var_str <- deparse(substitute(id_var))
  period_var_str <- deparse(substitute(period_var))

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
  #         future::plan("multicore", workers = ncores)
  #       } else{
  #         future::plan("multicore")
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


  ## CREATE CONTOUR POLYGONS ACROSS ALL PERIODS
  if (by_period == FALSE) {

    shp <- shp |>
      dplyr::group_by({{ id_var }}) |>
      dplyr::filter(n() >= nmap_threshold) |>
      dplyr::ungroup()

    shp_list <- split(shp, shp[[id_var_str]], drop = T)

    period_var <- NULL
    period <- NULL
  }

  ## CREATE CONTOUR POLYGONS BY PERIOD
  if (by_period == TRUE) {

    if (missing(period_var)) {
      cli::cli_abort("Missing {.arg period_var}")
    }

    if (is.character(substitute(period_var))) {
      cli::cli_abort("{.arg period_var} must be a variable, not a character string.")
    }

    ## USE DEFAULT INTERVAL
    if (is.null(interval)) {

      interval_min <- min(shp[[period_var_str]], na.rm = T)
      interval_max <- max(shp[[period_var_str]], na.rm = T)
      interval_length <- 20

      year_seq <- seq(interval_min, interval_max, interval_length)

      if (interval_max - max(year_seq) < interval_length) {
        year_seq[length(year_seq)] <- interval_max
      }

      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp |>
        filter({{ period_var }} >= min(year_seq) & {{ period_var }} <= max(year_seq))

      shp <- shp |>
        dplyr::mutate(
          period = cut({{ period_var }}, breaks = year_seq, labels = year_seq_char, include.lowest = T)
          )

      shp <- shp |>
        dplyr::group_by({{ id_var }}, .data$period) |>
        dplyr::filter(n() >= nmap_threshold) |>
        dplyr::ungroup()

      shp_list <- split(shp, list(shp[[id_var_str]], shp[["period"]]), drop = T)

    }


    ## USE INTERVAL LENGTH DEFINED BY USER
    if (length(interval) == 1) {

      interval_min <- min(shp[[period_var_str]], na.rm = T)
      interval_max <- max(shp[[period_var_str]], na.rm = T)
      interval_length <- interval

      year_seq <- seq(interval_min, interval_max, interval_length)

      if (interval_max - max(year_seq) < interval_length) {
        year_seq[length(year_seq)] <- interval_max
      }

      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp |>
        filter({{ period_var }} >= min(year_seq) & {{ period_var }} <= max(year_seq))

      shp <- shp |>
        dplyr::mutate(
          period = cut({{ period_var }}, breaks = year_seq, labels = year_seq_char, include.lowest = T)
          )

      shp <- shp |>
        dplyr::group_by({{ id_var }}, .data$period) |>
        dplyr::filter(n() >= nmap_threshold) |>
        dplyr::ungroup()

      shp_list <- split(shp, list(shp[[id_var_str]], shp[["period"]]), drop = T)

    }


    ## USE CUSTOM INTERVALS SPECIFIED BY USER
    if (length(interval) > 1) {

      year_seq <- interval
      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp |>
        filter({{ period_var }} >= min(year_seq) & {{ period_var }} <= max(year_seq))

      shp <- shp |>
        dplyr::mutate(
          period = cut({{ period_var }}, breaks = year_seq, labels = year_seq_char, include.lowest = T)
          )

      shp <- shp |>
        dplyr::group_by({{ id_var }}, .data$period) |>
        dplyr::filter(n() >= nmap_threshold) |>
        dplyr::ungroup()

      shp_list <- split(shp, list(shp[[id_var_str]], shp[["period"]]), drop = T)

    }

  }

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


  ## APPLY CONTOUR FUNCTION OVER EACH GROUP OF GEOMETRIES
  if (by_period) {
    progressr::with_progress({
      p <- progressr::progressor(along = shp_list)

      shp_contours <- furrr::future_map(
        shp_list,
        .f = function(x) {
          p()
          contour_polygons(
            x, id_vars = c({{ id_var }}, period),
            nmap_threshold = nmap_threshold,
            smoothing = smoothing,
            invalid_geom = invalid_geom,
            include_higher = include_higher,
            cuts = cuts,
            ...)
        },
        .options = furrr_options(seed = TRUE)
      )
    }, interrupts = TRUE)
  } else {
    progressr::with_progress({
      p <- progressr::progressor(along = shp_list)

      shp_contours <- furrr::future_map(
        shp_list,
        .f = function(x) {
          p()
          contour_polygons(
            x, id_vars = c({{ id_var }}),
            nmap_threshold = nmap_threshold,
            smoothing = smoothing,
            invalid_geom = invalid_geom,
            include_higher = include_higher,
            cuts = cuts,
            ...)
        },
        .options = furrr_options(seed = TRUE)
      )
    }, interrupts = TRUE)
  }



  # shp_contours <- pbapply::pblapply(
  #   shp_list,
  #   FUN = function(x) {
  #     contour_polygons(
  #       x, id_vars = c({{ id_var }}, period),
  #       nmap_threshold = nmap_threshold,
  #       smoothing = smoothing,
  #       ...)
  #   },
  #   cl = cl
  # ) %>% suppressWarnings()

  # Check for errors
  contour_errors <- sapply(shp_contours, inherits, what = "try-error")
  if(any(contour_errors)){
    cli::cli_abort(c(
      "Invalid geometries.",
      "i" = "Some groups encountered an error, likely because of invalid geometries.",
      "i" = "Consider running {.code fix_invalid} first."
    ))
  }

  inv_list <- lapply(shp_contours, FUN = function(x) sf::st_is_valid(x)) |>
    sapply(all) |>
    all()

  if (!inv_list) {
    shp_contours <- lapply(shp_contours,
                           FUN = function(x) st_make_valid(x))
    }

    inv_list <- lapply(shp_contours, FUN = function(x) sf::st_is_valid(x)) |>
      sapply(all) |>
      all()

    if (!inv_list) {
      cli::cli_warn(c(
        "Invalid geometries",
        "i" = "Some contour polygons may not be valid geometries. If you encounter invalid geometries,
        consider lowering resolution or use fewer cuts."
      ))
    }

  ## Return list or dataframe
  if (returnList) {
    shp_contours <- shp_contours
  } else {
    shp_contours <- shp_contours |>  bind_rows()
  }

  return(shp_contours)

}



