
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
df1 <- df[df$name == unique(df$name)[7], ]
contour_polygons(df1, id_vars = name)
#> Simple feature collection with 4 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 13.2298 ymin: -12.1308 xmax: 18.1298 ymax: -6.497462
#> Geodetic CRS:  WGS 84
#>        name prob  label nmaps                       geometry
#> 1 Freedonia 0.00    0-1    92 POLYGON ((15.13188 -6.49746...
#> 2 Freedonia 0.25 0.25-1    92 POLYGON ((15.38396 -7.09746...
#> 3 Freedonia 0.50  0.5-1    92 POLYGON ((15.31105 -7.53079...
#> 4 Freedonia 0.75 0.75-1    92 POLYGON ((15.07771 -7.89746...
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
#> Bounding box:  xmin: -6.94765 ymin: -31.48974 xmax: 48.82694 ymax: 37.22605
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>           name prob  label nmaps                       geometry
#> 1  Absurdistan 0.00    0-1    92 POLYGON ((14.77997 4.423399...
#> 2  Absurdistan 0.25 0.25-1    92 POLYGON ((13.87997 3.423399...
#> 3  Absurdistan 0.50  0.5-1    92 POLYGON ((14.21331 2.790065...
#> 4  Absurdistan 0.75 0.75-1    92 POLYGON ((13.91539 1.990065...
#> 5      Agrabah 0.00    0-1    92 POLYGON ((9.676273 37.22605...
#> 6      Agrabah 0.25 0.25-1    92 POLYGON ((8.06169 36.62605,...
#> 7      Agrabah 0.50  0.5-1    92 POLYGON ((8.432523 36.39272...
#> 8      Agrabah 0.75 0.75-1    92 POLYGON ((8.403356 36.05939...
#> 9    Arendelle 0.00    0-1    92 POLYGON ((45.39569 -16.3062...
#> 10   Arendelle 0.25 0.25-1    92 POLYGON ((45.38944 -17.2062...
```

## Create contour polygons across all groups and by period

``` r
get_contours(df, by_period = T, grp_id = name, period_id = year)
#> Jobs running in parallel using forking (multicore)
#> Simple feature collection with 320 features and 5 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -6.94765 ymin: -31.48974 xmax: 48.82967 ymax: 37.21565
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>           name    period prob  label nmaps                       geometry
#> 1  Absurdistan 1800-1820 0.00    0-1    24 POLYGON ((15.51037 4.31889,...
#> 2  Absurdistan 1800-1820 0.25 0.25-1    24 POLYGON ((14.07704 3.352223...
#> 3  Absurdistan 1800-1820 0.50  0.5-1    24 POLYGON ((13.79162 2.81889,...
#> 4  Absurdistan 1800-1820 0.75 0.75-1    24 POLYGON ((14.23329 2.21889,...
#> 5      Agrabah 1800-1820 0.00    0-1    16 POLYGON ((8.943135 36.92605...
#> 6      Agrabah 1800-1820 0.25 0.25-1    16 POLYGON ((8.911885 36.69272...
#> 7      Agrabah 1800-1820 0.50  0.5-1    16 POLYGON ((8.180635 36.52605...
#> 8      Agrabah 1800-1820 0.75 0.75-1    16 POLYGON ((8.780635 36.19272...
#> 9    Arendelle 1800-1820 0.00    0-1    23 POLYGON ((45.54461 -16.3603...
#> 10   Arendelle 1800-1820 0.25 0.25-1    23 POLYGON ((45.56336 -17.1936...
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
