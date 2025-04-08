
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

    #> Reading layer `master_shapefile' from data source 
    #>   `/Users/christoffercappelen/Library/Mobile Documents/com~apple~CloudDocs/RESEARCH/GitHub/legaciesR/data_private' 
    #>   using driver `ESRI Shapefile'
    #> Warning in CPL_read_ogr(dsn, layer, query, as.character(options), quiet, : GDAL
    #> Message 1: /Users/christoffercappelen/Library/Mobile
    #> Documents/com~apple~CloudDocs/RESEARCH/GitHub/legaciesR/data_private/master_shapefile.shp
    #> contains polygon(s) with rings with invalid winding order. Autocorrecting them,
    #> but that shapefile should be corrected using ogr2ogr for example.
    #> Simple feature collection with 13484 features and 19 fields (with 6 geometries empty)
    #> Geometry type: MULTIPOLYGON
    #> Dimension:     XY
    #> Bounding box:  xmin: -17.95964 ymin: -35.43061 xmax: 158.122 ymax: 56.78339
    #> Geodetic CRS:  WGS 84

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

``` r
shp <- prepare_shapes(shp = shp, state_data = isd,
                      id_var = COWID, period_var = year,
                      range_min = lyear, range_max = hyear,
                      crop_to_land = FALSE, ## 'get_contours' currently not working when cropped to land
                      exclude_core = FALSE ## Currently errors in 'core' and 'Core.Great' coding.
                      )
#> ! Geometries with missing `id_var`: Geometries with missing `id_var` are assigned to the value '99999'.
#> ℹ 1/6: Fix three-digit years
#> ✔ 1/6: Fix three-digit years [9ms]
#> 
#> ℹ 2/6: Exclude maps with no date
#> ✔ 2/6: Exclude maps with no date [15ms]
#> 
#> ℹ 3/6: Expand range
#> ✔ 3/6: Expand range [621ms]
#> 
#> ℹ 4/6: Matching capitals
#> ✔ 4/6: Matching capitals [30.3s]
#> 
#> ℹ 5/6: Exclude incomplete
#> ✔ 5/6: Exclude incomplete [22ms]
#> 
#> ⠙ 6/6: Exclude non-sovereign maps
#> ⠹ 6/6: Exclude non-sovereign maps: 99999
#> ✔ 6/6: Exclude non-sovereign maps [257ms]
#> 
```

## Detecing errors

``` r
errors <- detect_errors(shp = shp, capital_data = isd,
                        id_var = COWID, period_var = year)
#> ℹ 1/7: Checking for empty geometries
#> ✔ 1/7: Checking for empty geometries [5ms]
#> 
#> ℹ 2/7: COWID duplicates
#> ✔ 2/7: COWID duplicates [7ms]
#> 
#> ℹ 3/7: Missing ID
#> ✔ 3/7: Missing ID [5ms]
#> 
#> ℹ 4/7: COWIDs with only 1 map
#> ✔ 4/7: COWIDs with only 1 map [7ms]
#> 
#> ℹ 5/7: Missing year or year outside 1750-1920
#> ✔ 5/7: Missing year or year outside 1750-1920 [11ms]
#> 
#> ℹ 6/7: COWIDs with polygons not overlapping
#> ✔ 6/7: COWIDs with polygons not overlapping [2.2s]
#> 
#> ⠙ 7/7: Whether the capital falls outside all polygons
#> ⠹ 7/7: Whether the capital falls outside all polygons: 99999
#> ⠸ 7/7: Whether the capital falls outside all polygons: FTO
#> ⠼ 7/7: Whether the capital falls outside all polygons: OGD
#> ⠴ 7/7: Whether the capital falls outside all polygons
#> ✔ 7/7: Whether the capital falls outside all polygons [10s]
#> 
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
