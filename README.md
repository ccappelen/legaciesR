
<!-- README.md is generated from README.Rmd. Please edit that file -->

# legaciesr

<!-- badges: start -->

<!-- [![Codecov test coverage](https://codecov.io/gh/ccappelen/legaciesR/graph/badge.svg)](https://app.codecov.io/gh/ccappelen/legaciesR) -->

<!-- [![R-CMD-check](https://github.com/ccappelen/legaciesR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ccappelen/legaciesR/actions/workflows/R-CMD-check.yaml) -->

<!-- badges: end -->

The `legaciesr` package provides a set of functions used to aggregate
and summarize the mapping and ID data collected in the
[LEGACIES](https://www.legacies-project.com) project. It allows users
to, i.a., (1) create contour polygons capturing the territorial extent
of historical states at different probability thresholds, (2) create a
grid with various summary measures of historical state presence, and (3)
aggregate data on the number of historical states to modern countries.

## Installation

You can install the development version of legaciesr from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("ccappelen/legaciesR")
```

``` r
library(legaciesr)
#> Warning: replacing previous import 'dplyr::intersect' by 'terra::intersect'
#> when loading 'legaciesr'
#> Warning: replacing previous import 'glue::trim' by 'terra::trim' when loading
#> 'legaciesr'
#> Warning: replacing previous import 'dplyr::union' by 'terra::union' when
#> loading 'legaciesr'
#> Warning: replacing previous import 'terra::extract' by 'tidyr::extract' when
#> loading 'legaciesr'
#> Warning: replacing previous import 'terra::na.omit' by 'stats::na.omit' when
#> loading 'legaciesr'
library(sf) 
library(dplyr)
library(ggplot2)
```

The plots produced below rely on custom functions available in my
personal package which can be loaded with the following code:

``` r
devtools::install_github("ccappelen/cappelenR")
library(cappelenR)
```

## Package overview and workflow

The legaciesr package is composed of a number of functions designed to
process and match the raw mapping and state data collected in the
LEGACIES project. While they can, in most cases, be run independently of
each other, they are mostly meant to be used in the workflow described
in the remainder of this README. In short, it consists of
<!-- (1) fixing invalid geometries in the map data, (2) preprocess the map data and match with information in the ISD state data, (3) detect potential errors in the data set, (4) generate contour polygons from the raw map data, (5) generate a grid cell data set with various summary measures of historical statehood, and (6) match the grid cell data with numerous other data sources commonly used in empirical applications.  -->

1.  Detecting and fixing invalid geometries in the map data.
2.  Cleaning and expanding the ID data to polity-year observations.
3.  Pre-processing the map data and match with information in the ID
    state data.
4.  Detecting potential errors in the map data.
5.  Generating contour polygons from the raw map data.
6.  Generating a grid cell data set with various summary measures of
    historical statehood.
7.  Aggregate data on the number of historical states to the modern
    country level.

Each function (corresponding to each of the seven steps) is further
documented in their respective help documentation. This guide will
outline the basic features of the functions and their intended usage.

## Support for parallel processing:

Some functions enable parallel processing to speed up intensive tasks.
Parallel processing is is implemented using the \[future::future\]
framework. There are two ways of running jobs in parallel: `multicore`
which uses ‘forking’ to run multiple jobs in parallel with shared memory
and `multisession` which launches a set of background R sessions.
‘Forking’ can be faster than multisession because of the larger overhead
associated with copying the active environment to each background R
session (whereas forking processes shares memory). However, ‘forking’ is
not supported on Windows platforms and is considered unstable when
running from within RStudio (on both Windows and Unix systems such as
MacOS). The function will automatically determine whether `multicore` is
supported by the platform and choose the appropriate plan.

The greater overhead associated with `multisession` is primarily during
the first parallel run in a given R session (since the background R
sessions stays available for additional parallel jobs). It is possible
to define a \[future::plan(“multisession”)\] in the global environment,
which will minimize overhead in subsequent parallel jobs (apart from the
first). The function will automatically detect if a `multisession` plan
has been set globally and, thus, will not close background sessions
after running.

It is therefore recommended to start the script by setting a
\[future::plan\]:

``` r
future::plan("multisession", workers = future::availableCores())
## The above code sets up parallel processing on all available cores. 
## This can be changed with the 'workers' option. 
```

## Reading the data

The raw map and ID data is not currently included in the package and
therefore has to be loaded from the user’s own directory. The raw
mapping data is the set of shapefiles called “master_shapefile”, while
the raw ID data isd called “legacies_id_coding.xlsx”:

``` r
shp_folder <- "path to map data folder"
shp_name <- "name of shapefile"
id_path <- "file path to id data"
```

``` r
shp <- st_read(shp_path, shp_name)
rm(shp_folder, shp_name)

id <- readxl::read_xlsx(isd_path)
rm(id_path)
```

## Invalid geometries

Some maps in the raw data may be “invalid” which will result in errors
in many spatial data analyses. This can happen for all kinds of reasons;
typically it is the result of crossing boundaries. `fix_invalid()`
attempts to detect and fix these invalid geometries. It is a wrapper
around the `sf::st_make_valid()` function that attempts to reiteratively
lower the allowed precision to rebuild valid geometries. The function
will return the same data set with fixed geometries (if it was able to
fix them). The returned data set will include three columns describing
the status of the geometry, e.g., whether it was invalid and whether it
was successfully fixed. It will also (by default) print a summary of how
many geometries were invalid, how many were fixed, and how many were
unsuccessful.

Because invalid geometries can happen for many reasons that may point to
other data errors as well, it is recommended to check for valid
geometries and run the `legaciesr::fix_invalid()` to identify potential
issues.

``` r
shp <- fix_invalid(shp)
#> 447 (3.3 %) geometries were successfully rebuilt.
#> 0 (0 %) geometries failed to rebuild as valid.
```

## Prepare ID data

The raw ID data contains information on polities, their capital cities,
and hierarchy status (among other things). `legacies::prepare_id`
expands the data into polity-year format and extracts information on
capital cities and hierarchy status. For capital cities, some
polity-years are coded with multiple capitals in the same year. It is
possible to specify whether all capitals should be included, in which
case each polity-year will be represented by multiple rows
(corresponding to the number of capitals). The default is to keep only
the capital with the longest continuous spell.

``` r
id <- prepare_id(id, multiple_capital = FALSE)
```

## Prepare map data

The next step is to prepare the map data to be in a suitable format for
creating grid data, matching with data from the ISD data, etc. It is
therefore also necessary to provide the ID data (see previous step) in
order to match the two data sets. By default, the function will (1)
expand the data to have one row per year for maps that are assigned a
range of years, (2) fix potential issues such as three-digit years due
to approximate dating, (3) exclude maps without your assignment, (4) add
information on capitals (names and coordinates), (5) crop all geometries
to coastlines\*, (6) exclude maps marked as “incomplete”, (7) exclude
maps of core regions (if they are marked as such)\*\*, and (8) exclude
maps for years in which a state is not considered sovereign (in the ID
data)\*\*\*. Each of these options can be disabled. It is also possible
to exclude maps based on the hierarchy coding of states, i.e., if states
are coded as tributary and/or dependency.

\* The cropped geometries currently result in errors when creating
contour polygons (but work for grid data) \*\* There are currently
errors in the coding of core regions. \*\*\* By default, maps are
included if they fall within a 5-year window of when a state is
considered sovereign. The size of the window can be changed with the
`margin_sovereign` option.

``` r
shp <- prepare_shapes(shp = shp, state_data = id,
                      id_var = COWID, period_var = year,
                      range_min = lyear, range_max = hyear,
                      crop_to_land = FALSE, ## 'get_contours' currently not working when cropped to land
                      exclude_core = FALSE ## Currently errors in 'core' and 'Core.Great' coding.
                      )
```

## Detecing errors

To identify potential errors in the map data, the function
`detect_errors` checks for various issues in the data set, such as the
existence of duplicate COWIDs, missing COWIDs, errors in year assignment
(e.g., outside study range), if there are geometries not overlapping
with other geometries of the same COWID, and if there are capitals
falling outside the geometry of a given COWID. All these CAN indicate
errors, but it does not mean that it is necessarily an error. The
function should therefore be used primarily as an easy way to identify
potential issues with the map data that should be investigated further.

The function returns various objects for easy identification of errors.
First, it prints a report detailing whether and how many potential
issues there are for a given type of error.

``` r
errors <- detect_errors(shp = shp, capital_data = id,
                        id_var = COWID, period_var = year,
                        progress = FALSE)
#> 
#> POTENTIAL ERRORS:
#> • 113 states with potentially duplicate COWIDs.
#> • 8 shapes with empty geometries.
#> • 0 shapes with missing IDs.
#> • 23 COWIDs with only a single map.
#> • 75 maps with years missing or outside 1750-1920.
#> • 2188 maps that do not overlap with other shapes with the same ID.
#> • 4765 maps where the capital falls outside the polygon.
```

Second, it returns a list composed of (1) the original data with columns
indicating potential issues for each error type and (2) separate subsets
of the data set including only observations identified as a potential
issue for each of the error types. For instance, the code below returns
only those COWIDs for which there are geometries not overlapping with
other geometries of the same COWID (which might indicate either
erroneous COWID assignment or erroneous geocoding of the map).

``` r
shp_non_overlap <- errors$report$non_overlap
shp_non_overlap
#> Simple feature collection with 2188 features and 33 fields
#> Active geometry column: geometry (with 8 geometries empty)
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -17.26439 ymin: -29.96989 xmax: 115.7275 ymax: 45.67083
#> Geodetic CRS:  WGS 84
#> # A tibble: 2,188 × 35
#> # Groups:   COWID [95]
#>    COWID COWNUM polity_name map_name     year lyear hyear coder note  coasterror
#>  * <chr>  <int> <chr>       <chr>       <int> <chr> <chr> <chr> <chr>      <int>
#>  1 AGA     4783 agaie       Agaie        1870 1870  1870  cm    <NA>          NA
#>  2 AIR     4361 air         Wuste Agad…  1787 1787  1787  ss    Spli…         NA
#>  3 AIR     4361 air         Agades       1795 1795  1795  ss    <NA>          NA
#>  4 AIR     4361 air         Agades       1790 1790  1790  ss    <NA>          NA
#>  5 AIR     4361 air         Air or Asb…  1874 1874  1874  ss    <NA>          NA
#>  6 AIR     4361 air         Agades       1811 1811  1811  ss    <NA>          NA
#>  7 AIR     4361 air         Agades       1814 1814  1814  ss    <NA>          NA
#>  8 AIR     4361 air         Agades       1818 1818  1818  ss    <NA>          NA
#>  9 AIR     4361 air         Air or Asb…  1855 1855  1855  ss    <NA>          NA
#> 10 AIR     4361 air         Air, Asben   1808 1808  1808  ss    Bord…         NA
#> # ℹ 2,178 more rows
#> # ℹ 25 more variables: cityerror <dbl>, sourcetype <int>, `Core/Great` <int>,
#> #   source <chr>, core <int>, id <dbl>, layer <chr>, path <chr>,
#> #   lyear_str <chr>, core_str <chr>, geometry <GEOMETRY [°]>, geom_valid <lgl>,
#> #   rebuilt <lgl>, snap_precision <dbl>, capital_name <chr>,
#> #   capital_coords <MULTIPOINT [°]>, incomplete <lgl>, in_spell <lgl>,
#> #   empty_geom <lgl>, missing_id <lgl>, single_map <lgl>, year_outside <lgl>, …
```

## Create contour polygons

Contour polygons capture the varying degrees to which a given area is
covered by the digitized maps. The contours divide the region covered by
the union of all maps into custom intervals of the share of maps
covering a given area. By default, the function divides the territory
into four contours (correponding to 0-1, 0.25-1, 0.5-5, and 0.75-1). But
the number of percentiles can be specified with the `cuts` option (see
documentation for details).

\* If the number of maps for a given polity (or polity-period) is equal
to or smaller than the number of `cuts` specified, the function will
return fewer contour polygons for that polity. By default, the function
returns a warning for those cases, but it is also possible to exclude
those cases by setting `threshold_exclude` to `TRUE`.

``` r
df_contour <- get_contours(shp, id_var = COWID)
```

The code below plots the contour polygons for the Sokoto Caliphate:

``` r
# Load map of Africa
afr <- rnaturalearthdata::countries50 |>
  filter(continent == "Africa")

# Plot contour polygons 
df_contour |>
  filter(COWID == "SOK") |>
  ggplot() +
  geom_sf(data = afr, fill = "grey90", color = "grey20") +
  geom_sf(aes(fill = label), color = NA) +
  scale_fill_viridis_d(option = "plasma", direction = 1, na.value = "NA") +
  labs(fill = "Share of polygons") +
  cappelenR::my_maptheme() +
  cappelenR::coord_bbox(df_contour |> filter(COWID == "SOK"),
                        expand_x = 6, expand_y = 3) +
  theme(legend.position = "bottom", legend.justification = "center",
        legend.title.position = "top") 
```

<img src="man/figures/README-plot_contours-1.png" width="100%" />

By default, the function summarizes the maps across the entire period
(i.e. 1750-1920). However, it is also possible to specify the contour
polygons by drawn separately by specified period (see documentation for
further details).

``` r
df_contour_panel <- get_contours(shp, id_var = COWID, by_period = TRUE, period_var = year)

df_contour_panel |>
  filter(COWID == "SOK") |>
  ggplot() +
  geom_sf(data = afr, fill = "grey90", color = "grey20") +
  geom_sf(aes(fill = label), color = NA) +
  scale_fill_viridis_d(option = "plasma", direction = 1, na.value = "NA") +
  labs(fill = "Share of maps") +
  cappelenR::my_maptheme() +
  cappelenR::coord_bbox(df_contour |> filter(COWID == "SOK"),
                        expand_x = 6, expand_y = 3) +
  theme(legend.position = "bottom", legend.justification = "center",
        legend.title.position = "top") +
  facet_wrap(~ period)
```

<img src="man/figures/README-contour_panel-1.png" width="100%" />

## Create grid data

Finally, `get_grid` allows you to create a grid cell data set with
various summary measures of how many maps cover a particular area. By
default, the function creates a grid covering the entire extent of the
provided map data set, but it is also possible to specify a particular
area by providing a raster object with that extent. The function
calculates several different operationalizations of the overall idea of
capturing to what extent a given area was controlled by a state. The
different measures rely on different rules for specyfing what state to
use for summarizing the share of maps if there are more than one state
covering a particular grid cell.

1.  *polysh_largest_count*: The share of all polygons in a grid cell for
    the state with the largest total number of polygons.
2.  *polysh_largest_area*: The share of all polygons in a grid cell for
    the state with the largest single polygon (NB: This could be
    operataionalized differently as based on the area of the union of
    polygons for each state or the median area of polygons for each
    state).
3.  *polysh_largest_share*: The share of all polygons in a grid cell for
    the state with the largest share in that grid cell.
4.  *polysh_across*: The average share of polygons across all states
    intersecting a given grid cell.

In addition to these, there are two measures capturing slightly
different concepts related to borders and contested territory.

5.  *bordersh*: The share of polygons with a border intersecting a given
    grid cell, relative to all polygons intersecting the grid cell
    (across all states). A higher share indicates that the area is more
    likely to be a border region.
6.  *contested*: A measure capturing the idea of contested territory. Is
    is calculated as negative sum of state-specific shares of polygons
    intersecting a grid cell weighted by the logarithm of the same
    share: , where is the state-specific share of polygons intersecting
    a grid cell. (NB: This measure is still in development and the exact
    equation might change.)

``` r
df_grid <- get_grid(shp, id_var = COWID, period_var = year)
```

The output is (by default) a list containing the raster object (`r`)
used for creating the gridded data set along with the actual grid data
set (`df`). To plot a particular variable, you can extract the raster
object and assign the values for a particular variable to that object.
See below:

``` r
# Extract raster
share_largest_count <- df_grid$r

# Assign values to grid cells
terra::values(share_largest_count) <- df_grid$data$polysh_largest_count

# Plot grid
ggplot() +
  geom_sf(data = afr) +
  tidyterra::geom_spatraster(data = share_largest_count, alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = NA) +
  labs(fill = "Share of polygons for the state with the largest \nnumber of polygons in total") +
  guides(fill = guide_colorbar(barwidth = 15)) +
  cappelenR::my_maptheme() +
  cappelenR::coord_bbox(afr) +
  theme(legend.position = "bottom", legend.title.position = "top",
        legend.justification = "center", legend.margin = margin(-2,0,0,0,unit = "cm"),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 12))
```

<img src="man/figures/README-plot_grid-1.png" width="100%" />

Again, it is also possible to create a panel version of the grid data,
divided by specified period (by default it will create a panel for each
20-year period):

``` r
df_grid_panel <- get_grid(shp, id_var = COWID, by_period = TRUE, period_var = year)
```

Plotting the panel data requires creating a different layer for the
raster object for each period and then assigning the values for that
period. The code below provides a function for doing that.

``` r
# Define function for extracting raster grid and values for given variable
# This might be moved to its own proper function in the package
create_grid_panel <- function(x, var) {
  period_temp <- unique(x$data$period)
  r_temp <- x$r
  r_panel <- r_temp
  for (i in seq_along(period_temp[-1])) {
    terra::add(r_panel) <- r_temp
  }
  terra::set.names(r_panel, period_temp)
  for (i in period_temp) {
    terra::values(r_panel[[i]]) <- x$data[x$data$period == i, var]
  }
  return(r_panel)
}

share_largest_count_panel <- create_grid_panel(
  x = df_grid_panel, 
  var = "polysh_largest_count")
```

``` r
# Plot grid for each period
ggplot() +
  geom_sf(data = afr) +
  tidyterra::geom_spatraster(data = share_largest_count_panel, alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = NA) +
  labs(fill = "Share of polygons for the state with the largest \nnumber of polygons in total",
       x = NULL) +
  guides(fill = guide_colorbar(barwidth = 15)) +
  cappelenR::my_maptheme() +
  cappelenR::coord_bbox(afr) +
  theme(legend.position = "bottom", legend.title.position = "top",
        legend.justification = "center",
        legend.title = element_text(size = 12),
        axis.ticks = element_blank()
        ) +
  facet_grid(~ lyr)
```

<img src="man/figures/README-plot_panel_grid-1.png" width="100%" />

## Aggregate ID data

The final two steps allow users to aggregate the ID data to modern
countries (i.e. number of historical states in each modern country).
There are currently three “methods” for aggregating: (1) aggregating
based on destination states as coded in the ID data, (2) aggregating
based on capital city location, and (3) aggregating based on the
intersection of a polity’s contour polygon and the modern country
borders. In the latter case, it is necessary to first match the contour
polygons (created above using the `legaciesr::get_contours()` function).
This is done with the `match_id_contour()` function. It takes as input
the ID data and the contour polygons (e.g., `df_contours`). If the
contour polygons are created for the entire period, then each
polity-year will be matched to the same contour polygon. If the contour
polygons are created as a panel with a certain interval, then the
contour polygons will be matched to the appropriate polity-years. By
default, only the 25-100 % contour polygon will be matched. It is
possible to (1) specify the threshold (e.g., 0.25) of the contour
polygon (as long as it corresponds to one of the break points in the
contour polygons used for matching) and (2) keep all contour polygons
which will result in multiple observations per polity-year. The function
also adds information on land area (for the contour polygon) and
historical population estimates from HYDE (by decade). It is therefore
necessary to specify a file path to the LEGACIES folder which contains
data on population.

``` r
id_contour <- match_id_contour(id_data = id, multiple_levels = FALSE,
                               contour_data = df_contour,
                               covar_path = ".../legacies_project")
```

Finally, `id2country()` takes the ID data and aggregates the number of
historical states to each modern country. If `method = "source"`, the
aggregation is based on the destination states coded in the ID data. It
is possible to specify whether to use only the first destination state
or all destination states with the `multiple` option.

``` r
id_aggregate_source <- id2country(id_data = id_contour, 
                           method = "source", 
                           multiple = FALSE)
```

If `method = "capital"`, the aggregation is based on capital city
locations. In this case, whether to include multiple capitals for a
polity-year is determined by the ID data created with `prepare_id()`.

``` r
id_aggregate_source <- id2country(id_data = id_contour, 
                           method = "capital")
```

If `method = "polygon"`, the aggregation in based on the intersection of
contour polygons and modern country borders. To ensure that a given
polity is counted only when there is more than a trivial amount of
overlap, a given intersection is counted only if either (1) the entire
polity is located within the borders of a given modern country or (2)
the polity overlaps with at least 5 % of the modern country.

``` r
id_aggregate_source <- id2country(id_data = id_contour, 
                           method = "polygon")
#> Warning: attribute variables are assumed to be spatially constant throughout
#> all geometries
```

<!-- ## Adding covariates to grid data -->

<!-- In order to add additional covariates to the grid data, it is necessary to provide the path to the folder containing the relevant data. The folder is located in the LEGACIES OneDrive folder and is called "covariates". Thus, the path should end with the following ".../legacies_project/covariates".  -->

<!-- The function takes the output from `get_grid` and adds the specified covariates to the data set. By default, all covariates are included, but it is possible to specify which covariates to include. At the moment, there are only a few commonly used geographic covariates included, but more will be added.   -->

<!-- ```{r path, eval = TRUE, echo = FALSE} -->

<!-- covar_path <- "/Users/christoffercappelen/Library/CloudStorage/OneDrive-NTNU/legacies_project" -->

<!-- ``` -->

<!-- ```{r path hidden, eval = FALSE} -->

<!-- covar_path <- ".../legacies_project" -->

<!-- ``` -->

<!-- ```{r covars, eval = TRUE} -->

<!-- df_grid <- get_covariates(df_grid, path = covar_path) -->

<!-- ``` -->

<!-- Adding covariates to the panel version of the grid data follows the exact same structure:  -->

<!-- ```{r covars panel, eval = TRUE} -->

<!-- df_grid_panel <- get_covariates(df_grid_panel, path = covar_path) -->

<!-- ``` -->
