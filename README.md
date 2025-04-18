
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
add a range of commonly used covariates to the polygon and grid data.

## Installation

You can install the development version of legaciesr from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("ccappelen/legaciesR")
```

``` r
library(legaciesr)
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
2.  Preprocessing the map data and match with information in the ISD
    state data.
3.  Detecting potential errors in the data set.
4.  Generating contour polygons from the raw map data. (Optional)
5.  Generating a grid cell data set with various summary measures of
    historical statehood.
6.  Matching the grid cell data with numerous other data sources
    commonly used in empirical applications.

Each function (corresponding to each of the six steps) is further
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

The raw map and ISD data is not currently included in the package and
therefore has to be loaded from the user’s own directory:

``` r
shp_folder <- "path to map data folder"
shp_name <- "name of shapefile"
isd_path <- "file path to isd data"
```

``` r
shp <- st_read(shp_path, shp_name)
rm(shp_folder, shp_name)

isd <- readxl::read_xlsx(isd_path)
rm(isd_path)
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

## SUBET DATA TO AFRICA FOR QUICKER COMPUTATION
shp <- shp[st_within(shp,
                     rnaturalearthdata::countries50 |>
                       filter(continent == "Africa") |>
                       st_union(), sparse = F),]
```

## Preprocessing

The next step is to prepare the map data to be in a suitable format for
creating grid data, matching with data from the ISD data, etc. It is
therefore also necessary to provide the ISD data in order to match the
two data sets. By default, the function will (1) expand the data to have
one row per year for maps that are assigned a range of years, (2) fix
potential issues such as three-digit years due to approximate dating,
(3) exclude maps without your assignment, (4) add information on
capitals (names and coordinates), (5) crop all geometries to
coastlines\*, (6) exclude maps marked as “incomplete”, (7) exclude maps
of core regions (if they are marked as such)\*\*, and (8) exclude maps
for years in which a state is not considered sovereign (in the ISD
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
shp <- prepare_shapes(shp = shp, state_data = isd,
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
errors <- detect_errors(shp = shp, capital_data = isd,
                        id_var = COWID, period_var = year,
                        progress = FALSE)
#> 
#> POTENTIAL ERRORS:
#> • 49 states with potentially duplicate COWIDs.
#> • 0 shapes with empty geometries.
#> • 2 shapes with missing IDs.
#> • 36 COWIDs with only a single map.
#> • 1130 maps with years missing or outside 1750-1920.
#> • 817 maps that do not overlap with other shapes with the same ID.
#> • 1583 maps where the capital falls outside the polygon.
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
#> Simple feature collection with 817 features and 32 fields
#> Active geometry column: geometry
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -16.94491 ymin: -31.30053 xmax: 49.33274 ymax: 34.90809
#> Geodetic CRS:  WGS 84
#> # A tibble: 817 × 34
#> # Groups:   COWID [73]
#>     year COWID COWNUM name          lyear hyear coder note  coasterror cityerror
#>  * <dbl> <chr>  <int> <chr>         <dbl> <dbl> <chr> <chr>      <int>     <dbl>
#>  1  1899 563       NA Transvaal      1899  1910 ca    <NA>          NA        10
#>  2  1900 563       NA Transvaal      1899  1910 ca    <NA>          NA        10
#>  3  1910 563       NA Transvaal      1899  1910 ca    <NA>          NA        10
#>  4  1870 99999     NA Liptako        1870  1870 cm    <NA>          NA        12
#>  5  1866 99999     NA Orange Free …  1866  1866 cm    <NA>          NA         6
#>  6  1870 AGA     4783 Agaie          1870  1870 cm    <NA>          NA        16
#>  7  1787 AIR     4361 Wuste Agades   1787  1787 ss    Spli…         NA        NA
#>  8  1795 AIR     4361 Agades         1795  1795 ss    <NA>          NA        NA
#>  9  1790 AIR     4361 Agades         1790  1790 ss    <NA>          NA        NA
#> 10  2019 AIR     4361 Asben          2019  2019 ss    Bord…         NA        NA
#> # ℹ 807 more rows
#> # ℹ 24 more variables: sourcetype <int>, `Core/Great` <int>, source <chr>,
#> #   core <int>, id <dbl>, layer <chr>, path <chr>, lyear_str <chr>,
#> #   core_str <chr>, geometry <GEOMETRY [°]>, geom_valid <lgl>, rebuilt <lgl>,
#> #   snap_precision <dbl>, capital_names <chr>, capital_coords <MULTIPOINT [°]>,
#> #   incomplete <lgl>, in_spell <lgl>, empty_geom <lgl>, missing_id <lgl>,
#> #   single_map <lgl>, year_outside <lgl>, year_na <lgl>, non_overlap <lgl>, …
```

## Create contour polygons

Contour polygons capture the varying degrees to which a given area is
covered by the digitized maps. The contours divide the region covered by
the union of all maps into custom intervals of the share of maps
covering a given area. By default, the function divides the territory
into four contours (correponding to 0-1, 0.25-1, 0.5-5, and 0.75-1). But
the number of percentiles can be specified with the `cuts` option (see
documentation for details).

``` r
df_contour <- get_contours(shp, id_var = COWID)
#> Jobs running sequentially.
#> ℹ Reverts to original plan after running.
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

<img src="man/figures/README-plot contours-1.png" width="100%" />

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

<img src="man/figures/README-contour panel-1.png" width="100%" />

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

<img src="man/figures/README-plot grid-1.png" width="100%" />

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

<img src="man/figures/README-plot panel grid-1.png" width="100%" />

## Adding covariates to grid data

In order to add additional covariates to the grid data, it is necessary
to provide the path to the folder containing the relevant data. The
folder is located in the LEGACIES OneDrive folder and is called
“covariates”. Thus, the path should end with the following
“…/legacies_project/covariates”.

The function takes the output from `get_grid` and adds the specified
covariates to the data set. By default, all covariates are included, but
it is possible to specify which covariates to include. At the moment,
there are only a few commonly used geographic covariates included, but
more will be added.

``` r
covar_path <- ".../legacies_project"
```

``` r
df_grid <- get_covariates(df_grid, path = covar_path)
#> ℹ 1/5: Extracing covariates - Terrain
#> ✔ 1/5: Extracing covariates - Terrain [32.5s]
#> 
#> ℹ 2/5: Extracing covariates - Climate
#> ✔ 2/5: Extracing covariates - Climate [123ms]
#> 
#> ℹ 3/5: Extracing covariates - Crops
#> ✔ 3/5: Extracing covariates - Crops [4ms]
#> 
#> ℹ 4/5: Extracing covariates - Rivers
#> ✔ 4/5: Extracing covariates - Rivers [3m 55.9s]
#> 
#> ℹ 5/5: Extracing covariates - Coast
#> ✔ 5/5: Extracing covariates - Coast [55.6s]
#> 
```

Adding covariates to the panel version of the grid data follows the
exact same structure:

``` r
df_grid_panel <- get_covariates(df_grid_panel, path = covar_path)
#> ℹ 1/5: Extracing covariates - Terrain
#> ✔ 1/5: Extracing covariates - Terrain [30.2s]
#> 
#> ℹ 2/5: Extracing covariates - Climate
#> ✔ 2/5: Extracing covariates - Climate [122ms]
#> 
#> ℹ 3/5: Extracing covariates - Crops
#> ✔ 3/5: Extracing covariates - Crops [5ms]
#> 
#> ℹ 4/5: Extracing covariates - Rivers
#> ✔ 4/5: Extracing covariates - Rivers [3m 19.2s]
#> 
#> ℹ 5/5: Extracing covariates - Coast
#> ✔ 5/5: Extracing covariates - Coast [51.4s]
#> 
```
