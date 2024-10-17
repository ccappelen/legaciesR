#' @import dplyr
#' @import dtplyr
#' @importFrom raster raster
#' @importFrom terra rast
#' @importFrom terra as.data.frame
#' @importFrom terra set.names
#' @import tibble
#' @import rnaturalearthdata
#' @import fasterize
#' @import pbapply
#' @importFrom data.table as.data.table

#' @export

get_grid <- function(shp, ras,
                     raster_from_shp = F,
                     res = 1/2,
                     by_grp = T, id_var,
                     updates = T,
                     return = c("list", "data")){

  if(raster_from_shp){

    ## Create empty grid from user-provided shp
    r <- terra::rast(shp, resolution = res)

    ## Create empty dataframe (data.table) with cell id
    df <- terra::as.data.frame(r, xy = T, na.rm = F) %>%
      tibble::rownames_to_column(var = "id") %>%
      data.table::as.data.table() %>%
      dplyr::select(-base)

  }else{

    ## Load shape of modern countries (used to define grid and extract country names)
    world <- rnaturalearthdata::countries50

    ## Create empty grid (based on world)
    r <- terra::rast(world, resolution = res) |>
      terra::set.names("base")

    ## Create empty dataframe (data.table) with cell id
    df <- terra::as.data.frame(r, xy = T, na.rm = F) %>%
      tibble::rownames_to_column(var = "id") %>%
      data.table::as.data.table() %>%
      dplyr::select(-base)

    ## Extract modern country names (iso codes) and to dataframe
    ccode_levels <- levels(factor(world$sov_a3)) %>%
      as.data.frame() %>%
      dplyr::rename(label = ".") %>%
      tibble::rownames_to_column(var = "ccode")
    world <- world %>%
      dplyr::mutate(sov_a3_fct = factor(sov_a3, levels = ccode_levels$label))
    r_ccode <- fasterize::fasterize(world, raster::raster(r), field = "sov_a3_fct")
    ccode <- terra::as.data.frame(r_ccode) %>%
      tibble::rownames_to_column(var = "id") %>%
      data.table::as.data.table() %>%
      dplyr::rename(ccode = layer)
    df <- df %>%
      dplyr::left_join(ccode, by = "id") %>%
      dplyr::mutate(cname = factor(ccode, levels = ccode_levels$ccode, labels = ccode_levels$label)) %>%
      dplyr::mutate(cname = as.character(cname))

  }

  ## Rasterize shape (shp): For each group (e.g., COWID), count number of features intersecting grid cell
  if(updates) message("1. Creating grid")



  ## Calculate summary statistics for each grid cell
  if(updates) message("2. Calculating summary measures.")



  ## Create return list
  if(updates) message("3. Finalizing data.")

  if(return == "list"){
    out <- list()
    out$r <- r
    out$info$res <- res
    out$info$id_var <- "COWID"
    out$data <- df
  }else{
    out <- df
  }

  return(out)

}





