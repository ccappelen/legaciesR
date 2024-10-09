
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

Currently, one geometry (COWID = “EGY”) fails to rebuild due to crossing
edges. However, a few other COWIDs result in invalid geometries when
creating contour polygons. For now, these are omitted:

``` r
df <- df %>% 
  filter(!COWID %in% c("EGY", "LUN", "KUN", "SAF"))
```

## Create contour polygons for a single COWID

See documentation for additional arguments (e.g., threshold for number
of maps, the number of contour polygons, resolution of raster, and
handling of invalid geometries).

``` r
sok <- df %>%
  filter(COWID == "SOK")
contour_polygons(sok, id_vars = COWID)
```

## Create contour polygons across all COWIDs

See documentation for additional arguments (e.g., specifying the period
intervals to group by, thresholds for number of maps within a group,
etc.).

``` r
get_contours(df, grp_id = COWID)
```

## Create contour polygons across all COWIDs and by period

``` r
get_contours(df, by_period = T, grp_id = COWID, period_id = year)
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
