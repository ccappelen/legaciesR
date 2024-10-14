#' @title Get Contour Polygons Across Groups
#' @description This function creates contour polygons across multiple groups, either by group alone or
#' in combination with a time column.

#' @param shp An sf dataframe containing all geometries (polygons).
#' @param grp_id Name of the grouping variable.
#' @param by_period Logical, indicating if shapes should be divided into groups by time in addition to IDs.
#'   Default is `FALSE.`
#' @param period_id Name of the period variable to be used for time grouping.
#'   This argument is necessary if `by_period` is `TRUE.`
#' @param interval Scalar or vector indicating the intervals to group by. If `NULL` (default), the
#'   intervals will automatically be set to 20 (years) and include the full range (i.e. `seq(min, max, 20)`).
#'   For user specification, the value can be either a scalar, indicating the length of intervals
#'   (in which case the full range is used), or a vector specifying the exact breaks, including both the
#'   start of the first interval and end of the last interval.
#' @param smoothing Logical, whether to apply smoothing after polygonizing the raster. Default is TRUE.
#' @param nmap_threshold Integer, indicating the number of shapes requires within each group. Default is 5.
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
#' @import pbapply
#' @import parallel
#' @import cli

#' @section Support for parallel processing:
#' There are two ways of running jobs in parallel. Forked R processes or running multiple background
#' R sessions. In the current setup, running multiple background processes (multisession) are slower than
#' running the jobs sequentially due to the overhead associated with opening new R sessions. However,
#' machines running on Microsoft Windows do not support forking (multicore) and will therefore default to
#' a sequential plan unless `parallel` is set to `TRUE.` On Unix platforms (e.g., MacOS), it will default
#' to parallel processing.

#' @export

get_contours <- function(shp,
                         grp_id,
                         by_period = FALSE,
                         interval = NULL,
                         period_id,
                         nmap_threshold = 5,
                         smoothing = TRUE,
                         returnList = FALSE,
                         progress = TRUE,
                         parallel = TRUE,
                         ncores,
                         ...){

  # Check arguments
  if (missing(grp_id)) {
    cli::cli_abort("Missing {.arg grp_id}")
  }

  if (is.character(substitute(grp_id))) {
    cli::cli_abort("{.arg grp_id} must be a variable, not a character string.")
  }

  grp_id_str <- deparse(substitute(grp_id))
  period_id_str <- deparse(substitute(period_id))

  # Set progress bar options
  progress_bar <- ifelse(progress, "timer", "none")
  pboptions(char = "=", style = 1, type = progress_bar)

  # Set parallel options
  supportedByOS <- ifelse(.Platform$OS.type == "unix", TRUE, FALSE)

  if (is.null(parallel)) {
    if (supportedByOS) {
      parallel <- TRUE
    } else {
      parallel <- FALSE
    }
  }

  if (parallel) {
    if (supportedByOS) {
      if (missing(ncores)) {
        cl <- parallel::detectCores()-1
      } else {
        cl <- ncores
      }
      cli::cli_inform("Jobs running in parallel using forking (multicore)")
    } else {
      if (missing(ncores)) {
        ncores <- parallel::detectCores()-1
      }
      ncores <- ncores
      cl <- parallel::makeCluster(ncores)
      on.exit(stopCluster(cl))
      cli::cli_inform("Jobs running in parallel using multisession.")
    }
  } else {
    cl <- 1
    cli::cli_inform("Jobs running sequentially.")
  }


  ## CREATE CONTOUR POLYGONS ACROSS ALL PERIODS
  if (by_period == FALSE) {

    shp <- shp |>
      dplyr::group_by({{ grp_id }}) |>
      dplyr::filter(n() >= nmap_threshold) |>
      dplyr::ungroup()

    shp_list <- split(shp, shp[[grp_id_str]], drop = T)

    period_id <- NULL
    period <- NULL
  }

  ## CREATE CONTOUR POLYGONS BY PERIOD
  if (by_period == TRUE) {

    if (missing(period_id)) {
      cli::cli_abort("Missing {.arg period_id}")
    }

    if (is.character(substitute(period_id))) {
      cli::cli_abort("{.arg period_id} must be a variable, not a character string.")
    }

    ## USE DEFAULT INTERVAL
    if (is.null(interval)) {

      interval_min <- min(shp[[period_id_str]], na.rm = T)
      interval_max <- max(shp[[period_id_str]], na.rm = T)
      interval_length <- 20

      year_seq <- seq(interval_min, interval_max, interval_length)

      if (interval_max - max(year_seq) < interval_length) {
        year_seq[length(year_seq)] <- interval_max
      }

      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp |>
        dplyr::mutate(
          period = cut({{ period_id }}, breaks = year_seq, labels = year_seq_char, include.lowest = T)
          )

      shp <- shp |>
        dplyr::group_by({{ grp_id }}, .data$period) |>
        dplyr::filter(n() >= nmap_threshold) |>
        dplyr::ungroup()

      shp_list <- split(shp, list(shp[[grp_id_str]], shp[["period"]]), drop = T)

    }


    ## USE INTERVAL LENGTH DEFINED BY USER
    if (length(interval) == 1) {

      interval_min <- min(shp[[period_id_str]], na.rm = T)
      interval_max <- max(shp[[period_id_str]], na.rm = T)
      interval_length <- interval

      year_seq <- seq(interval_min, interval_max, interval_length)

      if (interval_max - max(year_seq) < interval_length) {
        year_seq[length(year_seq)] <- interval_max
      }

      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp |>
        dplyr::mutate(
          period = cut({{ period_id }}, breaks = year_seq, labels = year_seq_char, include.lowest = T)
          )

      shp <- shp |>
        dplyr::group_by({{ grp_id }}, .data$period) |>
        dplyr::filter(n() >= nmap_threshold)

      shp_list <- split(shp, list(shp[[grp_id_str]], shp[["period"]]), drop = T)

    }


    ## USE CUSTOM INTERVALS SPECIFIED BY USER
    if (length(interval) > 1) {

      year_seq <- interval
      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp |>
        dplyr::mutate(
          period = cut({{ period_id }}, breaks = year_seq, labels = year_seq_char, include.lowest = T)
          )

      shp_list <- split(shp, list(shp[[grp_id_str]], shp[["period"]]), drop = T)

    }

  }


  ## APPLY CONTOUR FUNCTION OVER EACH GROUP OF GEOMETRIES
  shp_contours <- pbapply::pblapply(
    shp_list,
    FUN = function(x) {
      contour_polygons(
        x, id_vars = c({{ grp_id }}, period),
        nmap_threshold = nmap_threshold,
        smoothing = smoothing,
        ...)
    },
    cl = cl
  ) %>% suppressWarnings()

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



