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
#'  \item{`count_across`}{Number of polygons intersecting a grid cell and number of distinct states intersecting a grid cell.}
#'  \item{`share_largest_count`}{Share of polygons for the state with the largest number of polygons in total.}
#'  \item{`share_largest_area`}{Share of polygons for the state with the largest area.}
#'  \item{`share_largest_share`}{Share of polygons for the state with the largest share in a grid cell.}
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

  shp_list <- split(shp, f = shp[[deparse(substitute(id_var))]])

  r_poly_count <- lapply(
    shp_list,
    FUN = function(x) {
      polycount <- as.data.frame(r, na.rm = FALSE) |>
        tibble::rownames_to_column(var = "id") |>
        dplyr::mutate(id = as.numeric(id)) |>
        dplyr::select(id) |>
        suppressWarnings()
      polycount$count <- sf::st_intersects(r_poly, x) |>
        sapply(FUN = length)
      polycount <- polycount |>
        dplyr::filter(count > 0)
    }) |>
    bind_rows(.id = deparse(substitute(id_var)))

  poly_count_df <- as.data.frame(r, na.rm = FALSE) |>
    suppressWarnings() |>
    tibble::rownames_to_column(var = "id") |>
    dplyr::mutate(id = as.numeric(id)) |>
    dplyr::select(id) |>
    dplyr::left_join(r_poly_count, by = "id") |>
    dplyr::rename(poly_count = count)


  ## Total number of polygons for each state
  max_poly <- shp |>
    sf::st_drop_geometry() |>
    dplyr::group_by({{ id_var }}) |>
    dplyr::summarise(max_poly = n()) |>
    dplyr::ungroup() |>
    select({{ id_var }}, max_poly)

  poly_count_df <- poly_count_df |>
    dplyr::left_join(max_poly, by = deparse(substitute(id_var)))
  rm(max_poly)


  ### STEP 3: CALCULATE SUMMARY MEASURES
  ## Calculate summary statistics for each grid cell
  # if(updates) message("2. Calculating summary measures.")

  cli::cli_progress_done()
  ### 3A: Count features and distinct states
  if ("count_across" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - number of features across states and number of distinct states")

    count_across_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::group_by(id) |>
      dplyr::summarise(polysum_across = sum(poly_count),
                       statesum_across = n_distinct(deparse(substitute(id_var)))) |>
      dplyr::ungroup() |>
      dplyr::select(id, polysum_across, statesum_across) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(count_across_df, by = "id")
    rm(count_across_df)
  }



  cli::cli_progress_done()
  ### 3B: Share based on largest total number of polygons
  if ("share_largest_count" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - share based on highest total number of polygons")

    max_count_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::group_by(id) |>
      dplyr::filter(max_poly == max(max_poly)) |>
      dplyr::ungroup() |>
      dplyr::mutate(polysh_largest_count = poly_count / max_poly) |>
      dplyr::select(id, name_largest_count = name, polysh_largest_count, polysum_largest_count = poly_count, polymax_largest_count = max_poly) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(max_count_df, by = "id")
    rm(max_count_df)

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
      dplyr::left_join(max_area_df, by = "id")
    rm(max_area_df)
  }


  cli::cli_progress_done()
  ### 3D: Share based on state with largest share

  if ("share_largest_share" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - share based on state with largest share of polygons")

    max_share_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(polysh_largest_share = poly_count / max_poly) |>
      dplyr::group_by(id) |>
      dplyr::filter(polysh_largest_share == max(polysh_largest_share)) |>
      dplyr::ungroup() |>
      dplyr::select(id, name_largest_share = name, polysh_largest_share, polysum_largest_share = poly_count, polymax_largest_share = max_poly) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(max_share_df, by = "id")
    rm(max_share_df)
  }


  cli::cli_progress_done()
  ### 3E: Average share of polygons across states in a grid cell

  if ("share_mean" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - average share of polygons across states")

    mean_share_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(polysh = poly_count / max_poly) |>
      dplyr::group_by(id) |>
      dplyr::summarise(polysh_mean = mean(polysh, na.rm = T),
                       polysum_across = sum(poly_count),
                       polymax_across = sum(max_poly)) |>
      dplyr::mutate(polysh_mean_share = polysh_mean,
                    polysh_across = polysum_across / polymax_across) |>
      dplyr::ungroup() |>
      dplyr::select(id, polysh_mean_share, polysh_across) |>
      as.data.table() |>
      suppressMessages()

    df <- df |>
      dplyr::left_join(mean_share_df, by = "id")
    rm(mean_share_df)
  }



  cli::cli_progress_done()
  ## 3F: Borders

  if ("borders" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - borders")

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

    # shp_list <- split(shp_borders, f = shp[[deparse(substitute(id_var))]])
    #
    # r_border <- lapply(
    #   shp_list,
    #   FUN = function(x) {
    #     polycount <- as.data.frame(r, na.rm = FALSE) |>
    #       tibble::rownames_to_column(var = "id") |>
    #       dplyr::mutate(id = as.numeric(id)) |>
    #       dplyr::select(id) |>
    #       suppressWarnings()
    #     polycount$count <- sf::st_intersects(r_poly, x) |>
    #       sapply(FUN = length)
    #     polycount <- polycount |>
    #       dplyr::filter(count > 0)
    # }) |>
    # bind_rows(.id = deparse(substitute(id_var)))

    border_count_df <- as.data.frame(r, na.rm = FALSE) |>
      suppressWarnings() |>
      tibble::rownames_to_column(var = "id") |>
      dplyr::mutate(id = as.numeric(id)) |>
      dplyr::select(id)

    border_count_df$border_count <- sf::st_intersects(r_poly, shp_borders) |>
      sapply(FUN = length)

    # border_count_df <- as.data.frame(r, na.rm = FALSE) |>
    #   suppressWarnings() |>
    #   tibble::rownames_to_column(var = "id") |>
    #   dplyr::mutate(id = as.numeric(id)) |>
    #   dplyr::select(id) |>
    #   dplyr::left_join(r_border, by = "id") |>
    #   # dplyr::rename(border_count = count) |>
    #   dplyr::group_by(id) |>
    #   dplyr::summarise(border_count = sum(count)) |>
    #   dplyr::ungroup() |>
    #   dplyr::select(id, border_count)

    df <- df |>
      dplyr::left_join(border_count_df, by = "id") |>
      dplyr::mutate(border_share = border_count / polysum_across)
    rm(border_count_df, r_border, shp_borders)
  }



  cli::cli_progress_done()
  ## 3G: Contested territory

  if ("contested" %in% output) {
    step <- step + 1
    cli::cli_progress_step("{step}/{steps}: Calculating summary measures - contested teritory")

    contested_df <- poly_count_df |>
      dtplyr::lazy_dt() |>
      dplyr::mutate(polysh = poly_count / max_poly) |>
      dplyr::mutate(polysh_ln = ifelse(poly_count == 0, 0, log(polysh))) |>
      dplyr::mutate(polyshw = polysh*polysh_ln) |>
      dplyr::group_by(id) |>
      dplyr::summarise(contested = -sum(polyshw)) |>
      data.table::as.data.table()

    df <- df |>
      dplyr::left_join(contested_df, by = "id")
    rm(contested_df)
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

  return(out)

}



