#' Create grid summarizing overlapping polygons.
#'
#' Create gridded data by group (or group and year) summarizing the number of
#' polygons covering a given grid cell.
#'
#' @param shp Shape
#' @param ras Raster
#' @param raster_from_shp Logical, if TRUE, using extent of shp to define grid. If FALSE, and `ras` is missing,
#'  grid is generated based on extent of `rnaturalearthdata::countries50`. If `FALSE` and `ras` is not missing,
#'  `ras` will be used.
#' @param res Resolution
#' @param by_period Whether to group by period
#' @param id_var ID variable name
#' @param period_var Period variable name
#' @param interval Scalar or vector indicating the intervals to group by. If `NULL` (default), the
#'   intervals will automatically be set to 20 (years) and include the full range (i.e. `seq(min, max, 20)`).
#'   For user specification, the value can be either a scalar, indicating the length of intervals
#'   (in which case the full range is used), or a vector specifying the exact breaks, including both the
#'   start of the first interval and end of the last interval.
#' @param nmap_threshold Integer, indicating the number of shapes required within each group. Default is 5.
#' @param output Character vector. See details.
#' @param subset A one-sided formula. If provided, the resulting grid data will be based only on observations
#'  defined by this argument. For example, `subset = ~ year > 1850`.
#' @param parallel Logical, whether to use parallel processing with `ncores` number of cores.
#' @param ncores Integer, the number of cores to use for parallel processing. Default is all available cores minus 1.
#' @param updates Logical
#' @param return List or data.frame
#' @param fix_invalid Logical

#' @import dplyr
#' @import dtplyr
#' @importFrom raster raster
#' @importFrom terra rast
#' @importFrom terra as.data.frame
#' @importFrom terra set.names
#' @importFrom tidyr pivot_longer
#' @importFrom stats setNames
#' @import tibble
#' @import rnaturalearthdata
#' @import fasterize
#' @import units
#' @import glue
#' @import cli
# #' @import pbapply
#' @importFrom data.table as.data.table

#' @return
#' Data frame of grid cell-by-state (or grid cell-by-state-by-year) with several summary measures
#'  of the polygons in `shp`. By default, all summary measures are calculated, but it is also possible
#'  to specify which measures to calculate with the argument `output`, which takes a character vector
#'  as input. The following summary measures are available:
#'  \item{`count_across`}{Number of polygons intersecting a grid cell and number of distinct states intersecting a grid cell.}
#'  \item{`share_largest_count`}{Share of polygons for the state with the largest number of polygons in total.}
#'  \item{`share_largest_area`}{Share of polygons for the state with the largest area.}
#'  \item{`share_largest_share`}{Share of polygons for the state with the largest share in a grid cell.}
#'  \item{`share_mean`}{Average share of polygons across all states intersecting a grid cell.}
#'  \item{`borders`}{Total number of state borders intersecting a grid cell and the share of borders relative to
#'  the total number of states intersecting.}
#'  \item{`contested`}{Entropy-based measure of contested territory calculated using the equation \eqn{E = -\sum{}p*ln(p)}, where \eqn{p} is the
#'  state-specific share of polygons intersecting a grid cell.}
#'
#'
#' @export

get_grid <- function(shp, ras,
                     raster_from_shp = TRUE,
                     res = 1/2,
                     by_period = FALSE,
                     id_var,
                     period_var,
                     interval = NULL,
                     nmap_threshold = 5,
                     output,
                     subset = NULL,
                     parallel = TRUE,
                     ncores,
                     updates = TRUE,
                     return = c("list", "data"),
                     fix_invalid = FALSE){

  # Set internals to pass R CMD check
  base = sov_a3 = layer = cname = NULL
  period <- grp <- x <- y <- NULL
  gid <- lon <- lat <- gid_period <- poly_count <- NULL
  grp_id <- polysum_across <- statesum_across <- polysh_largest_count <- NULL
  area <- max_area <- polysh_largest_area <- polysh_largest_share <- NULL
  polysh <- polysh_mean <- polymax_across <- polysh_mean_share <- polysh_across <- NULL
  border_count <- r_border <- polysh_ln <- polyshw <- NULL


  return <- match.arg(return)

  if (missing(id_var)) {
    cli::cli_abort("{.arg id_var} is missing.")
  }

  if (is.character(substitute(id_var))) {
    cli::cli_abort("{.arg id_var} must be a variable, not a character string.")
  }

  id_var_str <- deparse(substitute(id_var))

  if (missing(period_var)) {
    cli::cli_abort("{.arg period_var} is missing.")
  }

  if (is.character(substitute(period_var))) {
    cli::cli_abort("{.arg period_var} must be a variable, not a character string.")
  }

  period_var_str <- deparse(substitute(period_var))



  output_args <- c("count_across", "share_largest_count", "share_largest_area", "share_largest_share", "share_mean", "borders", "contested")
  if (missing(output)) {
    output <- output_args
  } else {
    if (any(!output %in% output_args)) {
      cli::cli_alert("{.arg output} contains elements that do not match any summary measure.")
    }
    output <- match.arg(output, choices = output_args, several.ok = TRUE)
  }

  # Check for invalid geometries
  if (any(!st_is_valid(shp))) {
    if (fix_invalid) {
      shp <- fix_invalid(shp, progress = FALSE, report = FALSE, reportColumns = FALSE)
    } else {
      cli::cli_abort(c("Invalid geometries",
                       "i" = "Fix invalid geometries by setting {.arg fix_invalid} to {.code TRUE}.
                     For more control, consider using the stand-alone function {.fn fix_invalid}."))
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




  ## Create group variable (ID and (if selected) period)
  if(by_period) {

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
        ungroup()

      shp <- shp |>
        dplyr::mutate(grp = paste({{ id_var }}, period, sep = "_")) |>
        dplyr::select({{ id_var }}, {{ period_var }}, period, grp, everything())

      # shp <- shp |>
      #   dplyr::group_by({{ id_var }}, .data$period) |>
      #   dplyr::filter(n() >= nmap_threshold) |>
      #   dplyr::ungroup()
      #
      # shp_list <- split(shp, list(shp[[id_var_str]], shp[["period"]]), drop = T)

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
        ungroup()

      shp <- shp |>
        dplyr::mutate(grp = paste({{ id_var }}, period, sep = "_")) |>
        dplyr::select({{ id_var }}, {{ period_var }}, period, grp, everything())

      # shp <- shp |>
      #   dplyr::group_by({{ id_var }}, .data$period) |>
      #   dplyr::filter(n() >= nmap_threshold)
      #
      # shp_list <- split(shp, list(shp[[id_var_str]], shp[["period"]]), drop = T)

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
        ungroup()

      shp <- shp |>
        dplyr::mutate(grp = paste({{ id_var }}, period, sep = "_")) |>
        dplyr::select({{ id_var }}, {{ period_var }}, period, grp, everything())

      # shp_list <- split(shp, list(shp[[id_var_str]], shp[["period"]]), drop = T)

    }

  } else {

    shp <- shp |>
      dplyr::group_by({{ id_var }}) |>
      dplyr::filter(n() >= nmap_threshold) |>
      ungroup()

    shp <- shp |>
      dplyr::mutate(period = paste(min({{ period_var }}, na.rm = T), max({{ period_var }}, na.rm = T), sep = "-")) |>
      dplyr::mutate(grp = paste({{ id_var }}, period, sep = "_")) |>
      dplyr::select({{ id_var }}, {{ period_var }}, period, grp, everything())

  }

  ## Load shape of modern countries (used to define grid and extract country names)
  rlang::check_installed(
    "rnaturalearthdata",
    reason = "'rnaturalearthdata' must be installed")
  world <- rnaturalearthdata::countries50

  ## Subsetting
  if (!is.null(subset)) {

    if (!"formula" %in% class(subset)) {
      cli::cli_abort("{.arg subset} must be a one-sided formula.")
    }

    if (deparse(subset[[1]]) != "~") {
      cli::cli_abort("{.arg subset} must be a one-sided formula.")
    }

    fml_sub <- subset[[2]]

    shp <- shp |>
      dplyr::filter(fml_sub)

  }


  steps <- 3 + length(output)
  step <- 0
  step <- step + 1
  ### STEP 1: PREPARING GRID
  cli::cli_progress_step("{step}/{steps}: Preparing grid")

  ## Create base grid
  if(raster_from_shp){

    ## Create empty grid from user-provided shp
    r <- terra::rast(shp, resolution = res)

    }else{

    if (missing(ras)) {
      ## Create empty grid (based on world)
      r <- terra::rast(world, resolution = res)
    } else {
      ## Use user-supplied raster
      r <- ras
    }
  }

  ## Create empty dataframe (data.table) with cell id

    period_unique <- unique(shp$period)
    names(period_unique) <- period_unique

    df_temp <- terra::as.data.frame(r, xy = T, na.rm = F) |>
      dplyr::rename(lon = x, lat = y) |>
      tibble::rownames_to_column(var = "gid")  |>
      dplyr::mutate(gid = as.numeric(gid)) |>
      data.table::as.data.table()  |>
      dplyr::select(gid, lon, lat) |>
      suppressWarnings()

    df <- lapply(period_unique,
                 FUN = function(x) {
                   df_temp |>
                     dplyr::mutate(period = x) |>
                     dplyr::select(gid, period, everything())
                 }) |>
      bind_rows(.id = "period") |>
      dplyr::mutate(gid_period = paste(gid, period, sep = "_")) |>
      dplyr::select(gid, period, gid_period, everything()) |>
      suppressWarnings()

    ## ERROR CHECK:
    if (nrow(df) %% terra::ncell(r) != 0) {
      cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
    }

  ## Extract modern country names (iso codes) and to dataframe
  ccode_levels <- levels(factor(world$sov_a3))  |>
    as.data.frame()  |>
    stats::setNames("label") |>
    tibble::rownames_to_column(var = "ccode")
  world <- world  |>
    dplyr::mutate(sov_a3_fct = factor(sov_a3, levels = ccode_levels$label))
  r_ccode <- fasterize::fasterize(world, raster::raster(r), field = "sov_a3_fct")
  ccode <- terra::as.data.frame(r_ccode)  |>
    tibble::rownames_to_column(var = "gid")  |>
    dplyr::mutate(gid = as.numeric(gid)) |>
    data.table::as.data.table()  |>
    dplyr::rename(ccode = layer)
  df <- df  |>
    dplyr::left_join(ccode, by = "gid") %>%
    dplyr::mutate(cname = factor(ccode, levels = ccode_levels$ccode, labels = ccode_levels$label))  |>
    dplyr::mutate(cname = as.character(cname))

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  cli::cli_progress_done()
  ### STEP 2: RASTERIZE SHAPE
  step <- step + 1
  e <- environment()
  msg <- ""
  cli::cli_progress_step("{step}/{steps}: Rasterizing polygons{msg}", spinner = TRUE)


  # r_poly_count <- fasterize::fasterize(shp, raster::raster(r), fun = "count", background = NA, by = deparse(substitute(id_var))) |>
  #   terra::rast()
  #
  # poly_count_df <- as.data.frame(r_poly_count) |>
  #   tibble::rownames_to_column(var = "id") |>
  #   dplyr::mutate(id = as.numeric(id)) |>
  #   dtplyr::lazy_dt() |>
  #   tidyr::pivot_longer(cols = -id, names_to = deparse(substitute(id_var)), values_to = "poly_count") |>
  #   dplyr::mutate({{ id_var }} := stringr::str_replace_all({{ id_var }}, "\\.", " ")) |>
  #   dplyr::filter(!is.na(poly_count)) |>
  #   data.table::as.data.table()

  r_poly <- terra::as.polygons(r) |>
    sf::st_as_sf()

  shp_list <- split(shp, f = shp[["grp"]])

  r_poly_count <- lapply(
    shp_list,
    FUN = function(x) {
      polycount <- as.data.frame(r, na.rm = FALSE) |>
        tibble::rownames_to_column(var = "gid") |>
        dplyr::mutate(gid = as.numeric(gid)) |>
        dplyr::select(gid) |>
        suppressWarnings()
      polycount$count <- sf::st_intersects(r_poly, x) |>
        sapply(FUN = length)
      polycount <- polycount |>
        dplyr::filter(count > 0)
      .step_id <- x[[id_var_str]][1]
      e$msg <- glue::glue(": {.step_id}")
      cli::cli_progress_update(.envir = e)
      return(polycount)
    }) |>
    dplyr::bind_rows(.id = "grp") |>
    dplyr::mutate(period = stringr::str_extract(grp, "(?<=_).*"),
                  # id_period = paste(id, period, sep = "_"),
                  grp_id = stringr::str_extract(grp, ".*(?=_)"))

  msg <- ""
  cli::cli_progress_update()

  # poly_count_df <- as.data.frame(r, na.rm = FALSE) |>
  #   suppressWarnings() |>
  #   tibble::rownames_to_column(var = "id") |>
  #   dplyr::mutate(id = as.numeric(id)) |>
  #   dplyr::select(id) |>
  #   dplyr::left_join(r_poly_count, by = "id") |>
  #   dplyr::rename(poly_count = count)

  poly_count_df <- df |>
    dplyr::select(gid, period, gid_period) |>
    dplyr::left_join(r_poly_count, by = c("gid", "period")) |>
    dplyr::rename(poly_count = count)


  ## List of COWID for each grid cell (-period)
  state_ids <- poly_count_df |>
    dplyr::group_by(gid_period) |>
    dplyr::summarise(COWIDs_combined = paste(grp_id, collapse = ";")) |>
    mutate(COWIDs_combined = dplyr::na_if(COWIDs_combined, "NA"))
  df <- df |>
    dplyr::left_join(state_ids, by = "gid_period")


  ## Total number of polygons for each state (and period)
  max_poly <- shp |>
    sf::st_drop_geometry() |>
    dplyr::group_by(grp) |>
    dplyr::summarise(max_poly = n()) |>
    dplyr::ungroup() |>
    select(grp, max_poly)

  poly_count_df <- poly_count_df |>
    dplyr::left_join(max_poly, by = "grp")
  rm(max_poly)


  cli::cli_progress_done()
  ### STEP 3: CALCULATE SUMMARY MEASURES
  ## Calculate summary statistics for each grid cell
  # if(updates) message("2. Calculating summary measures.")

  ### 3A: Count features and distinct states
  if ("count_across" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - number of features across states and number of distinct states")

    count_across_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::group_by(gid_period) |>
      dplyr::summarise(polysum_across = sum(poly_count, na.rm = T),
                       statesum_across = n_distinct(grp_id)) |>
      dplyr::ungroup() |>
      dplyr::select(gid_period, polysum_across, statesum_across) |>
      dplyr::mutate(statesum_across = ifelse(polysum_across == 0 & statesum_across == 1, 0, statesum_across)) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(count_across_df, by = "gid_period")
    rm(count_across_df)
  }

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  cli::cli_progress_done()
  ### 3B: Share based on largest total number of polygons
  if ("share_largest_count" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - share based on highest total number of polygons")

    max_count_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::group_by(gid_period) |>
      dplyr::filter(max_poly == max(max_poly)) |>
      dplyr::ungroup() |>
      dplyr::mutate(polysh_largest_count = poly_count / max_poly) |>
      dplyr::select(gid_period, name_largest_count = grp_id, polysh_largest_count, polysum_largest_count = poly_count, polymax_largest_count = max_poly) |>
      dplyr::distinct(gid_period, .keep_all = TRUE) |>
      as.data.table() |>
      suppressMessages()

    max_count_list <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(polysh_largest_count = poly_count / max_poly) |>
      dplyr::mutate(polysh_largest_count = round(polysh_largest_count, 3)) |>
      dplyr::group_by(gid_period) |>
      dplyr::arrange(desc(max_poly)) |>
      dplyr::mutate(polysh_largest_count_ranked = paste0(grp_id, "_", polysh_largest_count, collapse = ";")) |>
      dplyr::select(gid_period, polysh_largest_count_ranked) |>
      dplyr::distinct(gid_period, .keep_all = TRUE) |>
      dplyr::mutate(polysh_largest_count_ranked = ifelse(polysh_largest_count_ranked == "NA_NA", NA, polysh_largest_count_ranked)) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(max_count_df, by = "gid_period") |>
      dplyr::left_join(max_count_list, by = "gid_period")
    rm(max_count_df, max_count_list)

  }

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  cli::cli_progress_done()
  ### 3C: Share based on largest area of polygons
  #! How should we operationalize this? (1) Largest of any individual polygons, (2) Largest union area, (3) Largest median area, etc.

  if ("share_largest_area" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - share based on state with largest area")

    shp$area <- sf::st_area(shp) |>
      units::set_units("km2") |>
      units::drop_units()

    # Largest area of any individual polygons
    area_df <- shp |>
      sf::st_drop_geometry() |>
      dplyr::group_by(grp) |>
      dplyr::filter(area == max(area)) |>
      dplyr::ungroup() |>
      dplyr::distinct(grp, .keep_all = TRUE) |>
      dplyr::select(grp, max_area = area)

    max_area_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::left_join(area_df, by = "grp") |>
      dplyr::group_by(gid_period) |>
      dplyr::filter(max_area == max(max_area)) |>
      dplyr::ungroup() |>
      dplyr::mutate(polysh_largest_area = poly_count / max_poly) |>
      dplyr::select(gid_period, name_largest_area = grp_id, polysh_largest_area, polysum_largest_area = poly_count, polymax_largest_area = max_area) |>
      as.data.table() |>
      suppressMessages()

    max_area_list <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::left_join(area_df, by = "grp") |>
      dplyr::mutate(polysh_largest_area = poly_count / max_poly) |>
      dplyr::mutate(polysh_largest_area = round(polysh_largest_area, 3)) |>
      dplyr::group_by(gid_period) |>
      dplyr::arrange(desc(max_area)) |>
      dplyr::mutate(polysh_largest_area_ranked = paste0(grp_id, "_", polysh_largest_area, collapse = ";")) |>
      dplyr::select(gid_period, polysh_largest_area_ranked) |>
      dplyr::distinct(gid_period, .keep_all = TRUE) |>
      dplyr::mutate(polysh_largest_area_ranked = ifelse(polysh_largest_area_ranked == "NA_NA", NA, polysh_largest_area_ranked)) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(max_area_df, by = "gid_period") |>
      dplyr::left_join(max_area_list, by = "gid_period")
    rm(max_area_df, max_area_list)
  }

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  cli::cli_progress_done()
  ### 3D: Share based on state with largest share

  if ("share_largest_share" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - share based on state with largest share of polygons")

    max_share_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(polysh_largest_share = poly_count / max_poly) |>
      dplyr::group_by(gid_period) |>
      dplyr::filter(polysh_largest_share == max(polysh_largest_share)) |>
      dplyr::ungroup() |>
      dplyr::distinct(gid_period, .keep_all = TRUE) |>
      dplyr::select(gid_period, name_largest_share = grp_id, polysh_largest_share, polysum_largest_share = poly_count, polymax_largest_share = max_poly) |>
      as.data.table() |>
      suppressMessages()

    max_share_list <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(polysh_largest_share = poly_count / max_poly) |>
      dplyr::mutate(polysh_largest_share = round(polysh_largest_share, 3)) |>
      dplyr::group_by(gid_period) |>
      dplyr::arrange(desc(polysh_largest_share)) |>
      dplyr::mutate(polysh_largest_share_ranked = paste0(grp_id, "_", polysh_largest_share, collapse = ";")) |>
      dplyr::select(gid_period, polysh_largest_share_ranked) |>
      dplyr::distinct(gid_period, .keep_all = TRUE) |>
      dplyr::mutate(polysh_largest_share_ranked = ifelse(polysh_largest_share_ranked == "NA_NA", NA, polysh_largest_share_ranked)) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(max_share_df, by = "gid_period") |>
      dplyr::left_join(max_share_list, by = "gid_period")
    rm(max_share_df, max_share_list)
  }

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  cli::cli_progress_done()
  ### 3E: Average share of polygons across states in a grid cell

  if ("share_mean" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - average share of polygons across states")

    mean_share_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(polysh = poly_count / max_poly) |>
      dplyr::group_by(gid_period) |>
      dplyr::summarise(polysh_mean = mean(polysh, na.rm = T),
                       polysum_across = sum(poly_count),
                       polymax_across = sum(max_poly)) |>
      dplyr::mutate(polysh_mean_share = ifelse(is.nan(polysh_mean), NA, polysh_mean),
                    polysh_across = polysum_across / polymax_across) |>
      dplyr::ungroup() |>
      dplyr::select(gid_period, polysh_mean_share, polysh_across) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(mean_share_df, by = "gid_period")
    rm(mean_share_df)
  }

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  cli::cli_progress_done()
  ## 3F: Borders

  if ("borders" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - borders{msg}", spinner = TRUE)

    shp_borders <- shp |>
      sf::st_cast("MULTILINESTRING")

    # r_border <- terra::rasterize(terra::vect(shp_borders), r, fun = "count")
    # border_count_df <- as.data.frame(r_border, na.rm = FALSE) |>
    #   tibble::rownames_to_column(var = "id") |>
    #   dplyr::mutate(id = as.numeric(id)) |>
    #   dplyr::rename(border_count = layer) |>
    #   data.table::as.data.table()

    r_poly <- terra::as.polygons(r, aggregate = FALSE) |>
      sf::st_as_sf()

    shp_list <- split(shp_borders, f = shp_borders[["period"]])

    border_count_df <- lapply(
      shp_list,
      FUN = function(x) {
        polycount <- as.data.frame(r, na.rm = FALSE) |>
          tibble::rownames_to_column(var = "gid") |>
          dplyr::mutate(gid = as.numeric(gid)) |>
          dplyr::select(gid) |>
          suppressWarnings()
        polycount$border_count <- sf::st_intersects(r_poly, shp_borders) |>
          sapply(FUN = length)

        .step_id <- x[["period"]][1]
        e$msg <- glue::glue(": {.step_id}")
        cli::cli_progress_update(.envir = e)
        return(polycount)
      }) |>
      dplyr::bind_rows(.id = "period") |>
      dplyr::mutate(gid_period = paste0(gid, period, sep = "_")) |>
      dplyr::select(gid_period, border_count)

    msg <- ""
    cli::cli_progress_update()

    # border_count_df <- as.data.frame(r, na.rm = FALSE) |>
    #   suppressWarnings() |>
    #   tibble::rownames_to_column(var = "gid") |>
    #   dplyr::mutate(gid = as.numeric(gid)) |>
    #   dplyr::select(gid)
    #
    # border_count_df$border_count <- sf::st_intersects(r_poly, shp_borders) |>
    #   sapply(FUN = length)


    df <- df |>
      dplyr::left_join(border_count_df, by = "gid_period") |>
      dplyr::mutate(border_share = border_count / polysum_across)
    rm(border_count_df, r_border, shp_borders)
  }

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  cli::cli_progress_done()
  ## 3G: Contested territory

  if ("contested" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - contested teritory")


    ## Should we normalize based on the number of states? Maybe not - cause more states indicate higher contestednedd (but does the measure also reflect that?)

    contested_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(polysh = poly_count / max_poly) |>
      dplyr::mutate(polysh_ln = ifelse(poly_count == 0, 0, log(polysh))) |>
      dplyr::mutate(polyshw = polysh*polysh_ln) |>
      dplyr::group_by(gid_period) |>
      dplyr::summarise(contested = -sum(polyshw)) |>
      data.table::as.data.table()

    df <- df |>
      dplyr::left_join(contested_df, by = "gid_period")
    rm(contested_df)
  }

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  cli::cli_progress_done()
  ### STEP 4: FINALIZE DATA
  ## Create return list
  # if(updates) message("3. Finalizing data.")
  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Finalizing data")

  if(return == "list"){
    out <- list()
    out$r <- r
    out$info$res <- res
    out$info$id_var <- "COWID"
    out$data <- df |> dplyr::as_tibble()
  }else{
    out <- df |> dplyr::as_tibble()
  }

  ## ERROR CHECK:
  if (nrow(df) %% terra::ncell(r) != 0) {
    cli::cli_abort("Number of rows is not a multiple of the number of grid cells.")
  }

  return(out)

}



