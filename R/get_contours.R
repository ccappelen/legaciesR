#' @title Get Contour Polygons Across Groups
#' @description This function creates contour polygons across multiple groups, either by group alone or
#' in combination with a time column.
#' @param shp an sf dataframe containing all geometries (polygons).
#' @param grp_id grouping variable.
#' @param by_period logical, indicating if shapes should be divided into groups by time in addition to IDs.
#' Default is \code{FALSE}.
#' @param period_id name of the period variable to be used for time grouping.
#' This argument is necessary if \code{by_period} is \code{TRUE}.
#' @param interval scalar or vector indicating the intervals to group by. If \code{NULL} (default), the
#' intervals will automatically be set 20 and include the full range (i.e. \code{seq(min, max, 20)}).
#' For user specification, the value can be either a scalar, indicating the length of intervals
#' (in which case the full range is used), or a vector specifying the exact breaks, including both the
#' minimum and maximum.
#' @param nmap_threshold scalar indicating the number of shapes requires within each group. Default is 5.
#' @param returnList logical, whether to return a list of geometries by group.
#' Default is to return an sf dataframe containing all geometries.
#' @param progress logical, indicating whether to show a progress bar.
#' @param parallel logical, indicating whether to use parallel processing.
#' @param ncores scalar, indicating the number of cores to use for parallel processing.
#' Default is the number of cores available minus 1.
#' @param ... Additional arguments passed to \code{\link{contour_polygons}}.
#' @return Returns either an sf dataframe (default) or a list of sf dataframes (one list item per group).
#' @importFrom rlang .data
#' @import sf
#' @import dplyr
#' @import pbapply
#' @export


get_contours <- function(shp, grp_id,
                         by_period = FALSE, interval, period_id,
                         nmap_threshold = 5,
                         returnList = F,
                         progress = T,
                         parallel = T,
                         ncores,
                         ...){

  if(is.character(substitute(grp_id))){
    stop("grp_id should be a variable, not a character string.")
  }

  ## CREATE CONTOUR POLYGONS ACROSS ALL PERIODS
  if(by_period == FALSE){

    shp <- shp %>%
      dplyr::group_by({{ grp_id }}) %>%
      dplyr::filter(n() >= nmap_threshold) %>%
      dplyr::ungroup()

    shp_list <- split(shp, shp[[deparse(substitute(grp_id))]], drop = T)

  }

  ## CREATE CONTOUR POLYGONS BY PERIOD
  if(by_period == TRUE){

    if(missing(period_id)){
      stop("Please specify the time variable as a character vector.")
    }

    if(is.character(substitute(grp_id))){
      stop("period_id should be a variable, not a character string.")
    }

    ## USE DEFAULT INTERVAL
    if(missing(interval)){

      interval_min <- min(shp[[deparse(substitute(period_id))]], na.rm = T)
      interval_max <- max(shp[[deparse(substitute(period_id))]], na.rm = T)
      interval_length <- 20

      year_seq <- seq(interval_min, interval_max, interval_length)

      if(interval_max - max(year_seq) < interval_length){
        year_seq[length(year_seq)] <- interval_max
      }

      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp %>%
        dplyr::mutate(
          period = cut({{ period_id }}, breaks = year_seq, labels = year_seq_char, include.lowest = T))

      shp <- shp %>%
        dplyr::group_by({{ grp_id }}, .data$period) %>%
        dplyr::filter(n() >= nmap_threshold) %>%
        dplyr::ungroup()

      shp_list <- split(shp, list(shp[[deparse(substitute(grp_id))]], shp[["period"]]), drop = T)

    }


    ## USE INTERVAL LENGTH DEFINED BY USER (BETWEEN MIN AND MAX)
    if(length(interval) == 1){

      interval_min <- min(shp[[deparse(substitute(period_id))]], na.rm = T)
      interval_max <- max(shp[[deparse(substitute(period_id))]], na.rm = T)
      interval_length <- interval

      year_seq <- seq(interval_min, interval_max, interval_length)

      if(interval_max - max(year_seq) < interval_length){
        year_seq[length(year_seq)] <- interval_max
      }

      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp %>%
        dplyr::mutate(
          period = cut({{ period_id }}, breaks = year_seq, labels = year_seq_char, include.lowest = T))

      shp <- shp %>%
        dplyr::group_by({{ grp_id }}, .data$period) %>%
        dplyr::filter(n() >= nmap_threshold)

      shp_list <- split(shp, list(shp[[deparse(substitute(grp_id))]], shp[["period"]]), drop = T)

    }


    ## USE CUSTOM INTERVALS SPECIFIED BY USER
    if(length(interval) > 1){

      year_seq <- interval
      year_seq_char <- paste0(year_seq[-length(year_seq)], "-", year_seq[-1])

      shp <- shp %>%
        dplyr::mutate(
          period = cut({{ period_id }}, breaks = year_seq, labels = year_seq_char, include.lowest = T))

      shp_list <- split(shp, list(shp[[deparse(substitute(grp_id))]], shp[["period"]]), drop = T)

    }

  }

  ## APPLY CONTOUR FUNCTION OVER EACH GROUP OF GEOMETRIES

  progress_bar <- ifelse(progress, "timer", "none")

  if(parallel == T){
    if(missing(ncores)){
      ncores <- parallel::detectCores()-1
    }else{
      ncores <- ncores
    }
  }else{
    ncores <- 1
  }

  pboptions(char = "=", style = 1, type = progress_bar)

  # if(by_period == FALSE){
  #
  #   shp_contours <- pbapply::pblapply(
  #     shp_list,
  #     FUN = function(x) {
  #       contour_polygons(
  #         x, id_vars = {{ grp_id }})
  #     },
  #     cl = ncores
  #   ) %>% suppressWarnings()
  #
  # }
  #
  # if(by_period == TRUE){
  #
  #   shp_contours <- pbapply::pblapply(
  #     shp_list,
  #     FUN = function(x) {
  #       contour_polygons(
  #         x, id_vars = c({{ grp_id }}, {{ period_id }}))
  #     },
  #     cl = ncores
  #   ) %>% suppressWarnings()
  #
  # }

  shp_contours <- pbapply::pblapply(
    shp_list,
    FUN = function(x) {
      contour_polygons(
        x, id_vars = c({{ grp_id }}, {{ period_id }}),
        ...)
    },
    cl = ncores
  ) %>% suppressWarnings()



  contour_errors <- sapply(shp_contours, inherits, what = "try-error")
  if(any(contour_errors)) stop("Some groups encountered an error, likely because of invalid geometries. Consider trying invalid_geom = 'exclude' or 'fix_exclude' instead.")
  # shp_contours <- shp_contours[!contour_errors]

  inv_list <- lapply(shp_contours, FUN = function(x) sf::st_is_valid(x)) %>%
    sapply(all) %>%
    all()
  if(!inv_list){
    shp_contours <- lapply(shp_contours,
                           FUN = function(x) st_make_valid(x))
  }

  inv_list <- lapply(shp_contours, FUN = function(x) sf::st_is_valid(x)) %>%
    sapply(all) %>%
    all()
  if(!inv_list) warning("Contour polygons may contain invalid geometries. Consider trying different resolution and/or number of cuts.")

  ## Return list or dataframe
  if(returnList){
    shp_contours <- shp_contours
  }else{
    shp_contours <- shp_contours %>% bind_rows()
  }

  return(shp_contours)

}



