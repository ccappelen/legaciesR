#' Create random overlapping polygons
#'
#' @param ext An sf object, within which the polygons will be sampled. If `NULL`, it will be use
#'   a polygon of Africa as the extent extent object (from [rnaturalearthdata]).
#' @param n Integer, the number of groups to create
#' @param nmap_min Integer, minimum number of features within a group
#' @param nmap_max Integer, maximum number of features within a group
#' @param year_min Integer, minimum year
#' @param year_max Integer, maximum year
#' @param n.pnts Integer, number of points to sample for each feature to create a polygon
#' @param progress Logical, whether to show progress bar
#'
#' @return An sf dataframe of random overlapping polygons
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @importFrom smoothr smooth
#' @import sf
#' @import pbapply
#' @import rlang

create_random_polygons_df <- function(
    ext,
    n = 5,
    nmap_min = 50,
    nmap_max = 200,
    year_min = 1800,
    year_max = 1900,
    n.pnts = 10,
    progress = TRUE) {

  if (missing(ext)) {
    rlang::check_installed(
      "rnaturalearthdata",
      reason = "'rnaturalearthdata' must be installed to use the default 'ext' option.")
    ext <- rnaturalearthdata::countries50 |>
      dplyr::filter(continent == "Africa") |>
      sf::st_union()
  } else {
    ext <- ext |>
      sf::st_union()
  }

  # Set progress bar options
  progress_bar <- ifelse(progress, "timer", "none")
  pboptions(char = "=", style = 1, type = progress_bar)

  # List of fictional names
  names_list <- c("Genovia", "Freedonia", "Panem", "Kundu", "Narnia", "Wakanda", "Florin",
                  "Zubrovka", "Gilead", "Wonderland", "Westeros", "Atlantis", "El Dorado",
                  "Neverland", "Qumar", "Absurdistan", "Utopia", "Agrabah", "Zamunda", "Arendelle",
                  "Metropolis", "Oz")
  if (n <= length(names_list)) {
    names <- sample(names_list, size = n, replace = F)
  } else {
    names <- c(
      names_list,
      sapply(
        1:(n-length(names_list)),
        FUN = function(x) paste0(sample(c(LETTERS, LETTERS), 3), collapse = ""))
    )
  }



  id_df <- data.frame(
    id = 1:n,
    name = names,
    nmap = round(runif(n, nmap_min, nmap_max))
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(map_id = list(1:nmap)) |>
    dplyr::ungroup() |>
    tidyr::unnest_longer(col = map_id) |>
    suppressWarnings() |>
    dplyr::mutate(year = round(runif(n(), year_min, year_max)))

  pnts_random <- sf::st_sample(ext, size = n) |>
    sf::st_as_sf()
  buff_random <- sf::st_buffer(pnts_random, dist = runif(nrow(pnts_random), 300e03, 700e03))
  buff_random <- buff_random
  poly_random <- lapply(
    split(buff_random, 1:nrow(buff_random)),
    FUN = function(x) {
      sf::st_geometry(x) <- sf::st_sample(x, 20) |>
        sf::st_union() |>
        sf::st_concave_hull(ratio = 0.8) |>
        smoothr::smooth()
      return(x)
    }
  ) |> bind_rows()

  poly_random <- poly_random |>
    sf::st_intersection(ext) |>
    dplyr::rename(geometry = x) |>
    dplyr::mutate(id = 1:n)

  ext_df <- dplyr::left_join(poly_random, id_df, by = "id")

  sample_polygon <- function(shp, n.pnts) {
    sf::st_geometry(shp) <- shp |>
      sf::st_sample(n.pnts) |>
      sf::st_union() |>
      sf::st_concave_hull(ratio = 0.8) |>
      smoothr::smooth() |>
      sf::st_geometry()
    return(shp)
  }

  df <- pbapply::pblapply(
    split(ext_df, 1:nrow(ext_df)),
    FUN = function(x) sample_polygon(x, n.pnts = 10)
  ) |>
    dplyr::bind_rows()

  return(df)
}


