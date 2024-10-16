
<!-- README.md is generated from README.Rmd. Please edit that file -->

# legaciesr

<!-- badges: start -->
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
#> Jobs running in parallel using forking (multicore)
#> 0 (0 %) geometries were successfully rebuilt.
#> 0 (0 %) geometries failed to rebuild as valid (see row numbers below).
#> Invalid geometries:
```

## Create contour polygons for a single group

See documentation for additional arguments (e.g., threshold for number
of maps, the number of contour polygons, resolution of raster, and
handling of invalid geometries).

``` r
df1 <- df[df$name == unique(df$name)[13], ]
contour_polygons(df1, id_vars = name)
#> Simple feature collection with 4 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 13.89431 ymin: 22.27268 xmax: 22.82764 ymax: 30.73935
#> Geodetic CRS:  WGS 84
#>      name prob  label nmaps                       geometry
#> 1 Genovia 0.00    0-1   162 POLYGON ((16.33389 30.73935...
#> 2 Genovia 0.25 0.25-1   162 POLYGON ((17.09014 29.80601...
#> 3 Genovia 0.50  0.5-1   162 MULTIPOLYGON (((16.82764 28...
#> 4 Genovia 0.75 0.75-1   162 MULTIPOLYGON (((18.92764 27...
```

## Create contour polygons across all groups

See documentation for additional arguments (e.g., specifying the period
intervals to group by, thresholds for number of maps within a group,
etc.).

``` r
get_contours(df, grp_id = name)
#> Jobs running in parallel using forking (multicore)
#> Simple feature collection with 80 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -17.03699 ymin: -32.94779 xmax: 49.21897 ymax: 30.73935
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>         name prob  label nmaps                       geometry
#> 1    Agrabah 0.00    0-1   162 POLYGON ((21.54533 20.98443...
#> 2    Agrabah 0.25 0.25-1   162 POLYGON ((21.55783 20.01777...
#> 3    Agrabah 0.50  0.5-1   162 MULTIPOLYGON (((24.562 16.9...
#> 4    Agrabah 0.75 0.75-1   162 MULTIPOLYGON (((21.562 18.3...
#> 5  Arendelle 0.00    0-1   162 POLYGON ((47.84397 11.04525...
#> 6  Arendelle 0.25 0.25-1   162 POLYGON ((47.21063 10.37858...
#> 7  Arendelle 0.50  0.5-1   162 MULTIPOLYGON (((47.8523 7.8...
#> 8  Arendelle 0.75 0.75-1   162 MULTIPOLYGON (((46.01897 8....
#> 9   Atlantis 0.00    0-1   162 POLYGON ((23.33081 29.85323...
#> 10  Atlantis 0.25 0.25-1   162 MULTIPOLYGON (((18.66206 26...
```

## Create contour polygons across all groups and by period

``` r
get_contours(df, by_period = T, grp_id = name, period_id = year)
#> Jobs running in parallel using forking (multicore)
#> Simple feature collection with 400 features and 5 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -17.03699 ymin: -32.94779 xmax: 49.21397 ymax: 30.77268
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>         name    period prob  label nmaps                       geometry
#> 1    Agrabah 1800-1820 0.00    0-1    34 POLYGON ((21.54116 20.98206...
#> 2    Agrabah 1800-1820 0.25 0.25-1    34 POLYGON ((21.54325 19.98206...
#> 3    Agrabah 1800-1820 0.50  0.5-1    34 POLYGON ((20.89116 19.38206...
#> 4    Agrabah 1800-1820 0.75 0.75-1    34 MULTIPOLYGON (((21.69116 19...
#> 5  Arendelle 1800-1820 0.00    0-1    31 POLYGON ((47.61402 10.77858...
#> 6  Arendelle 1800-1820 0.25 0.25-1    31 MULTIPOLYGON (((47.21194 10...
#> 7  Arendelle 1800-1820 0.50  0.5-1    31 POLYGON ((46.75986 9.978583...
#> 8  Arendelle 1800-1820 0.75 0.75-1    31 MULTIPOLYGON (((46.11194 9....
#> 9   Atlantis 1800-1820 0.00    0-1    28 MULTIPOLYGON (((18.87412 28...
#> 10  Atlantis 1800-1820 0.25 0.25-1    28 MULTIPOLYGON (((19.40745 28...
```

## Support for parallel processing:

There are two ways of running jobs in parallel. Forked R processes or
running multiple background R sessions. In the current setup, running
multiple background processes (multisession) are slower than running the
jobs sequentially due to the overhead associated with opening new R
sessions. However, machines running on Microsoft Windows do not support
forking (multicore) and will therefore default to a sequential plan
unless `parallel` is set to TRUE. On Unix platforms (e.g., MacOS), it
will default to parallel processing.
