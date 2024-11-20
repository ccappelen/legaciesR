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
#' @param by_grp By group
#' @param id_var ID variable name
#' @param output Character vector. See details.
#' @param subset A one-sided formula. If provided, the resulting grid data will be based only on observations
#'  defined by this argument. For example, `subset = ~ year > 1850`.
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
#' @import tibble
#' @import rnaturalearthdata
#' @import fasterize
#' @import units
#' @import cli
# #' @import pbapply
#' @importFrom data.table as.data.table

#' @return
#' Data frame of grid cell-by-state (or grid cell-by-state-by-year) with several summary measures
#'  of the polygons in `shp`. By default, all summary measures are calculated, but it is also possible
#'  to specify which measures to calculate with the argument `output`, which takes a character vector
#'  as input. The following summary measures are available:
#'  \item{`share_largest_count`}{Share of polygons for the state with the largest number of polygons in total.}
#'  \item{`share_largest_area`}{Share of polygons for the state with the largest area.}
#'
#'
#' @export

get_grid <- function(shp, ras,
                     raster_from_shp = TRUE,
                     res = 1/2,
                     by_grp = TRUE,
                     id_var,
                     output,
                     subset = NULL,
                     updates = T,
                     return = c("list", "data"),
                     fix_invalid = FALSE){

  base = sov_a3 = layer = cname = NULL

  return <- match.arg(return)

  if (missing(id_var)) {
    cli::cli_abort("{.arg id_var} is missing.")
  }

  if (is.character(substitute(id_var))) {
    cli::cli_abort("{.arg id_var} must be a variable, not a character string.")
  }

  # output_args <- c("share_largest_count", "share_largest_area", "share_largest_share")
  output_args <- c("share_largest_count", "share_largest_area")
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
  Sys.sleep(3)

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
  df <- terra::as.data.frame(r, xy = T, na.rm = F) |>
    dplyr::rename(lon = x, lat = y) |>
    tibble::rownames_to_column(var = "id")  |>
    dplyr::mutate(id = as.numeric(id)) |>
    data.table::as.data.table()  |>
    dplyr::select(id, lon, lat) |>
    suppressWarnings()

  ## Extract modern country names (iso codes) and to dataframe
  ccode_levels <- levels(factor(world$sov_a3))  |>
    as.data.frame()  |>
    setNames("label") |>
    tibble::rownames_to_column(var = "ccode")
  world <- world  |>
    dplyr::mutate(sov_a3_fct = factor(sov_a3, levels = ccode_levels$label))
  r_ccode <- fasterize::fasterize(world, raster::raster(r), field = "sov_a3_fct")
  ccode <- terra::as.data.frame(r_ccode)  |>
    tibble::rownames_to_column(var = "id")  |>
    dplyr::mutate(id = as.numeric(id)) |>
    data.table::as.data.table()  |>
    dplyr::rename(ccode = layer)
  df <- df  |>
    dplyr::left_join(ccode, by = "id") %>%
    dplyr::mutate(cname = factor(ccode, levels = ccode_levels$ccode, labels = ccode_levels$label))  |>
    dplyr::mutate(cname = as.character(cname))


  cli::cli_progress_done()
  ### STEP 2: RASTERIZE SHAPE
  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Rasterizing polygons")
  Sys.sleep(3)


  r_poly_count <- fasterize::fasterize(shp, raster::raster(r), fun = "count", background = NA, by = deparse(substitute(id_var))) |>
    terra::rast()

  poly_count_df <- as.data.frame(r_poly_count) |>
    tibble::rownames_to_column(var = "id") |>
    dplyr::mutate(id = as.numeric(id)) |>
    dtplyr::lazy_dt() |>
    tidyr::pivot_longer(cols = -id, names_to = deparse(substitute(id_var)), values_to = "poly_count") |>
    dplyr::mutate(name = stringr::str_replace_all(name, "\\.", " ")) |>
    filter(!is.na(poly_count)) |>
    as.data.table()


  ### STEP 3: CALCULATE SUMMARY MEASURES
  ## Calculate summary statistics for each grid cell
  # if(updates) message("2. Calculating summary measures.")

  cli::cli_progress_done()
  ### 3A: Share based on largest total number of polygons
  if ("share_largest_count" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - share based on highest total number of polygons")
    Sys.sleep(3)


    ## Total number of polygons for each state
    max_poly <- shp |>
      sf::st_drop_geometry() |>
      dplyr::group_by({{ id_var }}) |>
      dplyr::summarise(max_poly = n()) |>
      dplyr::ungroup() |>
      select({{ id_var }}, max_poly)

    max_count_df <- poly_count_df |>
      dplyr::left_join(max_poly, by = deparse(substitute(id_var)))

    max_count_df <- max_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::group_by(id) |>
      dplyr::filter(max_poly == max(max_poly)) |>
      dplyr::ungroup() |>
      dplyr::mutate(polysh_largest_count = poly_count / max_poly) |>
      dplyr::select(id, name_largest_count = name, polysh_largest_count, polysum_largest_count = poly_count, polymax_largest_count = max_poly) |>
      as.data.table()

    df <- df |>
      left_join(max_count_df, by = "id")
    rm(max_count_df)

  }

  cli::cli_progress_done()
  ### 3B: Share based on largest area of polygons
  #! How should we operationalize this? (1) Largest of any individual polygons, (2) Largest union area, (3) Largest median area, etc.

  if ("share_largest_area" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - share based on state with largest area")
    Sys.sleep(3)


    shp$area <- sf::st_area(shp) |>
      units::set_units("km2") |>
      units::drop_units()

    # Largest area of any individual polygons
    area_df <- shp |>
      sf::st_drop_geometry() |>
      dplyr::group_by({{ id_var }}) |>
      dplyr::filter(area == max(area)) |>
      dplyr::ungroup() |>
      dplyr::select({{ id_var }}, max_area = area)

    max_area_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::left_join(area_df) |>
      dplyr::group_by(id) |>
      dplyr::filter(max_area == max(max_area)) |>
      dplyr::ungroup() |>
      dplyr::mutate(polysh_largest_area = poly_count / max_area) |>
      dplyr::select(id, name_largest_area = name, polysh_largest_area, polysum_largest_area = poly_count, polymax_largest_area = max_area) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      left_join(max_area_df, by = "id")
    rm(max_area_df)
  }


  cli::cli_progress_done()
  ### STEP 4: FINALIZE DATA
  ## Create return list
  # if(updates) message("3. Finalizing data.")
  step <- step + 1
  cli::cli_progress_step("{step}/{steps}: Finalizing data")
  Sys.sleep(3)

  if(return == "list"){
    out <- list()
    out$r <- r
    out$info$res <- res
    out$info$id_var <- "COWID"
    out$data <- df |> dplyr::as_tibble()
  }else{
    out <- df |> dplyr::as_tibble()
  }

  return(out)

}


## Progress updates cannot show at the same time as progress bar (the bar overwrites the progress update)
# foo <- function(df, vars) {
#
#   handlers("txtprogressbar")
#   on.exit(handlers("void"), add = T)
#
#   # INITIAL TASK
#   cli::cli_progress_step("Preparing data")
#   df <- df |>
#     mutate(across(.cols = all_of(vars),
#                   .fns = ~ .x^2,
#                   .names = "{.col}_sq"))
#   out <- df |>
#     group_by(group) |>
#     summarise(n = n())
#   Sys.sleep(2)
#
#
#   # FIRST TASK
#   prog <- cli::cli_progress_step("Calculating group means for 'a'", .auto_close = FALSE)
#   Sys.sleep(2)
#   if ("a" %in% vars) {
#     with_progress({
#       temp_ls <- split(df, df$group)
#       p <- progressor(along = temp_ls)
#       tmp_out <- future_map(
#         temp_ls,
#         .f = function(x) {
#           Sys.sleep(runif(1, 0.01, 0.1))
#           p()
#           mean(x[["a"]])
#         },
#         .options = furrr_options(seed = TRUE)
#       ) |>
#         unlist()
#       out <- cbind(out, "a" = tmp_out)
#     })
#
#   }
#
#
#   # SECOND TASK
#   cli::cli_progress_step("Calculating group means for 'b'")
#   Sys.sleep(2)
#   if ("b" %in% vars) {
#     with_progress({
#       temp_ls <- split(df, df$group)
#       p <- progressor(along = temp_ls)
#
#       tmp_out <- future_map(
#         temp_ls,
#         .f = function(x) {
#           Sys.sleep(runif(1, 0.01, .1))
#           p()
#           mean(x[["b"]])
#         },
#         .options = furrr_options(seed = TRUE)
#       ) |>
#         unlist()
#       out <- cbind(out, "b" = tmp_out)
#     })
#
#   }
#
#
#   # SECOND TASK
#   cli::cli_progress_step("Calculating group means for 'c'")
#   Sys.sleep(2)
#   if ("c" %in% vars) {
#     with_progress({
#       temp_ls <- split(df, df$group)
#       p <- progressor(along = temp_ls)
#       tmp_out <- future_map(
#         temp_ls,
#         .f = function(x) {
#           Sys.sleep(runif(1, 0.01, .1))
#           p()
#           mean(x[["c"]])
#         },
#         .options = furrr_options(seed = TRUE)
#       ) |>
#         unlist()
#       out <- cbind(out, "c" = tmp_out)
#     })
#
#   }
#
#   cli::cli_progress_step("Finalizing data")
#   return(out)
#
# }

