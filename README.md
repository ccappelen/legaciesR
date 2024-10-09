
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
df_path <- file.path("../../LEGACIES/Test code/test_shape") # PATH FOR FOLDER OF master_shapefile (user-defined)
df <- read_sf(df_path, "master_shapefile") # LOAD master_shapefile

df <- fix_invalid(df)
#> Jobs running in parallel using forking (multicore)
#> 447 (3.7 %) geometries were successfully rebuilt.
#> 1 (0 %) geometries failed to rebuild as valid (see row numbers below).
#> Invalid geometries: 1297
```

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
#> Simple feature collection with 4 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -1.270313 ymin: 3.541847 xmax: 18.12969 ymax: 19.44185
#> Geodetic CRS:  WGS 84
#>   COWID  cut prob_interval nmaps                       geometry
#> 1   SOK 0.75        0.75-1   159 MULTIPOLYGON (((5.26302 11....
#> 2   SOK 0.50         0.5-1   159 MULTIPOLYGON (((4.329687 13...
#> 3   SOK 0.25        0.25-1   159 POLYGON ((5.14427 14.74185,...
#> 4   SOK 0.00           0-1   159 POLYGON ((8.40052 19.44185,...
```

## Create contour polygons across all COWIDs

See documentation for additional arguments (e.g., specifying the period
intervals to group by, thresholds for number of maps within a group,
etc.).

``` r
get_contours(df, grp_id = COWID)
#> Jobs running in parallel using forking (multicore)
#> Simple feature collection with 1126 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -17.92631 ymin: -32.91223 xmax: 158.0887 ymax: 56.76821
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    COWID  cut prob_interval nmaps                       geometry
#> 1    ABJ 0.75        0.75-1     6 POLYGON ((7.400228 9.312875...
#> 2    ABJ 0.50         0.5-1     6 POLYGON ((7.433561 9.346209...
#> 3    ABJ 0.25        0.25-1     6 POLYGON ((9.362728 10.64621...
#> 4    ABJ 0.00           0-1     6 POLYGON ((9.248145 10.71288...
#> 5    ACH 0.75        0.75-1    26 MULTIPOLYGON (((95.89119 4....
#> 6    ACH 0.50         0.5-1    26 MULTIPOLYGON (((97.35786 3....
#> 7    ACH 0.25        0.25-1    26 MULTIPOLYGON (((94.95786 5....
#> 8    ACH 0.00           0-1    26 POLYGON ((102.3058 6.372775...
#> 9    ADA 0.75        0.75-1    74 POLYGON ((14.1256 10.50851,...
#> 10   ADA 0.50         0.5-1    74 POLYGON ((14.3256 11.07518,...
```

## Create contour polygons across all COWIDs and by period

``` r
get_contours(df, by_period = T, grp_id = COWID, period_id = year)
#> Jobs running in parallel using forking (multicore)
#> Simple feature collection with 1839 features and 5 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -17.84571 ymin: -31.77972 xmax: 158.093 ymax: 56.79681
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    COWID    period  cut prob_interval nmaps                       geometry
#> 1    AIR 1738-1758 0.75        0.75-1     6 MULTIPOLYGON (((8.051684 17...
#> 2    AIR 1738-1758 0.50         0.5-1     6 POLYGON ((8.795434 18.14707...
#> 3    AIR 1738-1758 0.25        0.25-1     6 MULTIPOLYGON (((11.11835 19...
#> 4    AIR 1738-1758 0.00           0-1     6 POLYGON ((10.8996 20.78041,...
#> 5    ALA 1738-1758 0.75        0.75-1     6 MULTIPOLYGON (((1.617851 7....
#> 6    ALA 1738-1758 0.50         0.5-1     6 MULTIPOLYGON (((1.517851 7....
#> 7    ALA 1738-1758 0.25        0.25-1     6 MULTIPOLYGON (((2.084518 8....
#> 8    ALA 1738-1758 0.00           0-1     6 MULTIPOLYGON (((5.051185 9....
#> 9    ALG 1738-1758 0.75        0.75-1    10 POLYGON ((6.309909 37.08083...
#> 10   ALG 1738-1758 0.50         0.5-1    10 MULTIPOLYGON (((8.380742 37...
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
