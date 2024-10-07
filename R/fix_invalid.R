#' @title Fix Invalid Geometries
#' @description Rebuilds invalid geometries. If necessary, the functions will iteratively
#' lower the snapping precision if default options are inadequate.
#' @param shp a sf dataframe with at least some invalid geometries.
#' @param max_precision maximum level of precision for \code{s2::s2options}, default is \eqn{10^7}.
#' @param min_precision minimum allowed precision for \code{s2::s2options}, default is 10. If geometry
#' is invalid after rebuilding with \code{min_precision}, the function will return an error if
#' \code{stop_if_invalid} is TRUE and a warning otherwise.
#' @param stop_if_invalid logical, whether the function should return an error if it fails
#' to rebuild a valid geometry. The error message will indicate the row number of the invalid geometry.
#' @param progress logical, whether a progress bar should be displayed. Default is TRUE.
#' @param parallel logical, whether to use parallel processing with \code{ncores} number of cores.
#' Default is TRUE.
#' @param ncores number of cores to use for parallel processing. Default is all available cores minus 1.
#' @param report logical, whether to report the number of valid, rebuilt, and invalid geometries.
#' @param reportColumns logical, whether to return the status of each geometry in new columns
#' (geom_valid, rebuilt, and snap_precision)
#' @return Returns an sf dataframe with the same number of features as the input. The dataframe includes
#' three new columns: 'geom_valid' indicates whether the original geometry was valid (TRUE), 'rebuilt'
#' indicates if the geometry was successfully rebuilt, and 'snap_precision' indicates the snapping
#' precision used to rebuild geometry (if 'rebuilt' is FALSE, 'snap_precision' indicates the minimum snapping
#' precision that led to invalid geometry).
#' @import sf
#' @import pbapply
#' @import dplyr
#' @import s2
#' @export


fix_invalid <- function(shp, max_precision = 10^7, min_precision = 10,
                        stop_if_invalid = FALSE,
                        progress = T, parallel = T, ncores,
                        report = T, reportColumns = T){

  if(parallel)
  shp <- shp %>%
    dplyr::mutate(rowid = dplyr::row_number())

  ## Split dataframe into list (one item per row)
  shp_list <- split(shp, shp$rowid)


  ## Write function to make valid (iteratively if necessary)
  fun.make.valid <- function(shp1, max_precision, min_precision,
                             stop_if_invalid){

    if(nrow(shp1) != 1) stop("More than one observations in the list item")

    if(st_is_valid(shp1)){

      if(reportColumns){
        shp1[["geom_valid"]] <- TRUE
        shp1[["rebuilt"]] <- FALSE
        shp1[["snap_precision"]] <- NA
      }

    }else{

      shp1 <- st_make_valid(shp1)

      if(st_is_valid(shp1)){

        if(reportColumns){
          shp1[["geom_valid"]] <- FALSE
          shp1[["rebuilt"]] <- TRUE
          shp1[["snap_precision"]] <- NA
        }

        return(shp1)

      }else{

        shp_invalid <- TRUE
        snap_prec <- max_precision

        while(shp_invalid == TRUE & snap_prec >= min_precision){
          shp1 <- st_make_valid(shp1, s2_options = s2::s2_options(snap = s2::s2_snap_precision(snap_prec)))
          shp_invalid <- !st_is_valid(shp1)
          snap_prec_last <- snap_prec
          snap_prec <- ifelse(snap_prec > 100, snap_prec / 10, snap_prec - 10)
        }

        if(!st_is_valid(shp1) & stop_if_invalid == TRUE){
          stop(paste0("Unable to rebuild valid geometry for row number #: ", shp1[["rowid"]]))
        }

        if(!st_is_valid(shp1) & stop_if_invalid == FALSE){
          # warning(paste0("Unable to rebuild valid geometry for row number #:", shp1[["rowid"]]))
          if(reportColumns){
            shp1[["geom_valid"]] <- FALSE
            shp1[["rebuilt"]] <- FALSE
            shp1[["snap_precision"]] <- snap_prec_last
          }

        }

        if(st_is_valid(shp1)){

          if(reportColumns){
            shp1[["geom_valid"]] <- FALSE
            shp1[["rebuilt"]] <- TRUE
            shp1[["snap_precision"]] <- snap_prec_last
          }

        }

      }

    }

    return(shp1)

  }

  ## Apply fun.make.valid over all items
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

  shp_list_valid <- pbapply::pblapply(
    shp_list,
    FUN = function(x) fun.make.valid(x, max_precision, min_precision, stop_if_invalid),
    cl = ncores
  )

  # shp_list_valid <- pbapply::pblapply(
  #   shp_list,
  #   FUN = function(x) fun.make.valid(x, max_precision = 1e07, min_precision = 10,
  #                                    stop_if_invalid = FALSE),
  #   cl = ncores
  # )

  shp_errors <- sapply(shp_list_valid, inherits, what = "try-error")
  if(any(shp_errors) & stop_if_invalid) stop("Some groups encountered an error")

  shp_list_new <- shp_list_valid %>% bind_rows()

  valid_rowids <- shp_list_new %>%
    filter(geom_valid == TRUE) %>%
    pull(rowid)
  rebuilt_rowids <- shp_list_new %>%
    filter(geom_valid == FALSE & rebuilt == TRUE) %>%
    pull(rowid)
  invalid_rowids <- shp_list_new %>%
    filter(geom_valid == FALSE & rebuilt == FALSE) %>%
    pull(rowid)

  if(report){
  message(paste0(length(rebuilt_rowids), " (", round(length(rebuilt_rowids)/nrow(shp_list_new), 3)*100, " %) ",
                 "geometries were successfully rebuilt.\n"),
          paste0(length(invalid_rowids), " (", round(length(invalid_rowids)/nrow(shp_list_new), 3)*100, " %) ",
                 "geometries failed to rebuild as valid (see row numbers below).\n"),
          ifelse(length(invalid_rowids) <= 10,
                 paste0("Invalid geometries: ", paste0(invalid_rowids, collapse = ", ")),
                 paste0("Invalid geometries: ", paste0(invalid_rowids[1:10], collapse = ", "), " ..... (more than 10)")))
  }

  shp_list_new[["rowid"]] <- NULL

  return(shp_list_new)

}

