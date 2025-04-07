
<!-- README.md is generated from README.Rmd. Please edit that file -->

# legaciesr

<!-- badges: start -->
<!-- [![Codecov test coverage](https://codecov.io/gh/ccappelen/legaciesR/graph/badge.svg)](https://app.codecov.io/gh/ccappelen/legaciesR) -->
<!-- [![R-CMD-check](https://github.com/ccappelen/legaciesR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ccappelen/legaciesR/actions/workflows/R-CMD-check.yaml) -->
<!-- badges: end -->

The `legaciesr` package provides a set of functions used to aggregate
and summarize the mapping data collected in the
[LEGACIES](https://www.legacies-project.com) project. It allows users
to, i.a., (1) create contour polygons capturing the territorial extent
of historical states at different probability thresholds, (2) create a
grid with various summary measures of historical state presence, and (3)
add a range of commonly used covariates to the polygon and grid data.

NB: The package is still experimental and no functions should be
considered stable.

## Installation

You can install the development version of legaciesr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ccappelen/legaciesR")
```

``` r
library(legaciesr) 
library(sf) 
library(dplyr)
```

## Errors in current master_shapefile

Currently, one geometry (COWID = “EGY”) fails to rebuild due to crossing
edges. However, a few other COWIDs result in invalid geometries when
creating contour polygons. For now, these are omitted:

``` r
df <- df %>% 
  filter(!COWID %in% c("EGY", "LUN", "KUN", "SAF"))
```

## Handling of invalid geometries

The functions in `legaciesr` will (by default) attempt to fix invalid
geometries which would otherwise return an error. The fixes rely on
`legaciesr::fix_invalid()` which is a wrapper around the
`sf::st_make_valid()` function that attempts to reiteratively lower the
allowed precision to rebuild valid geometries. However, because invalid
geometries can happen for many reasons that may point to other data
errors as well, it is recommended to check for valid geometries and run
the `legaciesr::fix_invalid()` to identify potential issues. See package
documentation for further information on the use of this function.

``` r
df <- example_df # Load example data set (based on random data)
df <- fix_invalid(df)
#> 1 (0 %) geometries were successfully rebuilt.
#> 0 (0 %) geometries failed to rebuild as valid.
```

## Create contour polygons for a single group

See documentation for additional arguments (e.g., threshold for number
of maps, the number of contour polygons, resolution of raster, and
handling of invalid geometries).

``` r
df1 <- df[df$name == unique(df$name)[8], ]
contour_polygons(df1, id_vars = name)
#> Simple feature collection with 4 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -10.55116 ymin: 6.086814 xmax: -1.423012 ymax: 13.81136
#> Geodetic CRS:  WGS 84
#>      name prob  label nmaps                       geometry
#> 1 Genovia 0.00    0-1   191 POLYGON ((-4.821082 13.7967...
#> 2 Genovia 0.25 0.25-1   191 POLYGON ((-5.220855 12.9311...
#> 3 Genovia 0.50  0.5-1   191 POLYGON ((-5.220855 12.3317...
#> 4 Genovia 0.75 0.75-1   191 MULTIPOLYGON (((-4.454624 1...
```

## Create contour polygons across all groups

See documentation for additional arguments (e.g., specifying the period
intervals to group by, thresholds for number of maps within a group,
etc.).

``` r
get_contours(df, grp_id = name)
#> Simple feature collection with 88 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -10.55116 ymin: -23.1782 xmax: 46.48245 ymax: 35.56343
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>           name prob  label nmaps                       geometry
#> 1  Absurdistan 0.00    0-1    70 POLYGON ((-0.1282136 15.058...
#> 2  Absurdistan 0.25 0.25-1    70 POLYGON ((-0.5950196 14.426...
#> 3  Absurdistan 0.50  0.5-1    70 POLYGON ((-0.3616166 13.927...
#> 4  Absurdistan 0.75 0.75-1    70 MULTIPOLYGON (((-0.29493 13...
#> 5      Agrabah 0.00    0-1   129 POLYGON ((22.69547 31.91306...
#> 6      Agrabah 0.25 0.25-1   129 POLYGON ((23.49557 30.80692...
#> 7      Agrabah 0.50  0.5-1   129 MULTIPOLYGON (((25.62919 29...
#> 8      Agrabah 0.75 0.75-1   129 MULTIPOLYGON (((25.39583 28...
#> 9    Arendelle 0.00    0-1    70 POLYGON ((7.970548 19.65083...
#> 10   Arendelle 0.25 0.25-1    70 MULTIPOLYGON (((6.934863 15...
```

## Create contour polygons across all groups and by period

``` r
get_contours(df, by_period = T, grp_id = name, period_id = year)
#> Simple feature collection with 440 features and 5 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -10.55116 ymin: -23.1782 xmax: 46.48245 ymax: 35.56343
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>           name    period prob  label nmaps                       geometry
#> 1  Absurdistan 1800-1820 0.00    0-1    17 POLYGON ((0.4913785 15.0530...
#> 2  Absurdistan 1800-1820 0.25 0.25-1    17 MULTIPOLYGON (((-0.541555 1...
#> 3  Absurdistan 1800-1820 0.50  0.5-1    17 MULTIPOLYGON (((0.4913785 1...
#> 4  Absurdistan 1800-1820 0.75 0.75-1    17 MULTIPOLYGON (((-0.00842805...
#> 5      Agrabah 1800-1820 0.00    0-1    37 POLYGON ((22.97841 31.92655...
#> 6      Agrabah 1800-1820 0.25 0.25-1    37 MULTIPOLYGON (((23.21214 30...
#> 7      Agrabah 1800-1820 0.50  0.5-1    37 POLYGON ((24.41419 29.7902,...
#> 8      Agrabah 1800-1820 0.75 0.75-1    37 MULTIPOLYGON (((24.31402 28...
#> 9    Arendelle 1800-1820 0.00    0-1    16 POLYGON ((6.567361 19.49756...
#> 10   Arendelle 1800-1820 0.25 0.25-1    16 MULTIPOLYGON (((7.335773 14...
```

## Support for parallel processing:

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
