
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

## CROP SHP FOR NOW
shp <- shp[st_within(shp,
                     rnaturalearthdata::countries50 |>
                       filter(subregion == "Western Africa") |>
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
#> • 15 states with potentially duplicate COWIDs.
#> • 0 shapes with empty geometries.
#> • 1 shapes with missing IDs.
#> • 26 COWIDs with only a single map.
#> • 465 maps with years missing or outside 1750-1920.
#> • 310 maps that do not overlap with other shapes with the same ID.
#> • 482 maps where the capital falls outside the polygon.
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
#> Simple feature collection with 310 features and 32 fields
#> Active geometry column: geometry
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -16.94491 ymin: 4.957359 xmax: 15.37766 ymax: 20.90528
#> Geodetic CRS:  WGS 84
#> # A tibble: 310 × 34
#> # Groups:   COWID [40]
#>     year COWID COWNUM name         lyear hyear coder note   coasterror cityerror
#>  * <dbl> <chr>  <int> <chr>        <dbl> <dbl> <chr> <chr>       <int>     <dbl>
#>  1  1870 99999     NA Liptako       1870  1870 cm    <NA>           NA        12
#>  2  1870 AGA     4783 Agaie         1870  1870 cm    <NA>           NA        16
#>  3  1790 AIR     4361 Agades        1790  1790 ss    <NA>           NA        NA
#>  4  2019 AIR     4361 Asben         2019  2019 ss    Borde…         NA        NA
#>  5  2019 AIR     4361 Asben         2019  2019 ss    Borde…         NA        NA
#>  6  1874 AIR     4361 Air or Asben  1874  1874 ss    <NA>           NA        37
#>  7  1967 AIR     4361 Aïr           1967  1967 ss    <NA>           NA        NA
#>  8  1811 AIR     4361 Agades        1811  1811 ss    <NA>           NA        NA
#>  9  1814 AIR     4361 Agades        1814  1814 ss    <NA>           NA        NA
#> 10  1818 AIR     4361 Agades        1818  1818 ss    <NA>           NA        NA
#> # ℹ 300 more rows
#> # ℹ 24 more variables: sourcetype <int>, `Core/Great` <int>, source <chr>,
#> #   core <int>, id <dbl>, layer <chr>, path <chr>, lyear_str <chr>,
#> #   core_str <chr>, geometry <MULTIPOLYGON [°]>, geom_valid <lgl>,
#> #   rebuilt <lgl>, snap_precision <dbl>, capital_names <chr>,
#> #   capital_coords <MULTIPOINT [°]>, incomplete <lgl>, in_spell <lgl>,
#> #   empty_geom <lgl>, missing_id <lgl>, single_map <lgl>, year_outside <lgl>, …
```
