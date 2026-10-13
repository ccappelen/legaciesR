# Get Contour Polygons Across Groups

This function creates contour polygons across multiple groups, either by
group alone or in combination with a time column.

## Usage

``` r
get_contours(
  shp,
  id_var,
  by_period = FALSE,
  interval = NULL,
  period_var,
  n.breaks = 4,
  breaks = NULL,
  label.breaks = NULL,
  include_higher = TRUE,
  exclude_lower_bound = NA,
  nmap_threshold = 5,
  threshold_exclude = FALSE,
  invalid_geom = c("stop", "fix", "exclude"),
  smoothing = TRUE,
  returnList = FALSE,
  progress = TRUE,
  parallel = NULL,
  ncores,
  ...
)
```

## Arguments

- shp:

  An sf dataframe containing all geometries (polygons).

- id_var:

  Name of the grouping variable.

- by_period:

  Logical, indicating if shapes should be divided into groups by time in
  addition to IDs. Default is `FALSE.`

- interval:

  Scalar or vector indicating the intervals to group by. If `NULL`
  (default), the intervals will automatically be set to 20 (years) and
  include the full range (i.e. `seq(min, max, 20)`). For user
  specification, the value can be either a scalar, indicating the length
  of intervals (in which case the full range is used), or a vector
  specifying the exact breaks, including both the start of the first
  interval and end of the last interval.

- period_var:

  Name of the period variable to be used for time grouping. This
  argument is necessary if `by_period` is `TRUE.`

- n.breaks:

  Integer of length 1 specifying the number of intervals. If `breaks` is
  not specified, the intervals will be evenly spaced. For example, if
  `n.breaks` is 4, the intervals will be 0-0.25, 0.25-0.5, 0.5-0.75,
  0.75-1.

- breaks:

  Numeric vector specifying the interval breaks. Include only the inner
  break values, not the outer values of 0 and 1. For instance, if the
  desired intervals are 0-0.15, 0.15-0.75, and 0.75-1, `breaks` should
  be `c(0.15, 0.75)`. `breaks` supersedes `n.breaks`.

- label.breaks:

  Character vector with optional labels for each interval. Has to be
  same length as `n.breaks` (or one more than the length of `breaks`).

- include_higher:

  Logical, whether the contour polygons should include percentiles above
  current interval. If TRUE (default), the 50 % polygon will include all
  areas covered by at least 50 % of the shapes (and not just within the
  specified interval, e.g., 0.75-1, 0.50-1, 0.25-1...). If FALSE, the
  polygon will include only areas within the specified interval (e.g.,
  0.75-1, 0.50-0.75, 0.25-0.50...).

- exclude_lower_bound:

  numeric, if specified, the areas of least coverage will be excluded
  before generating the contour polygons. Must be a number between 0
  and 1. For example, 0.05 means that areas with less than 5 percent
  coverage will be excluded from the contour polygons. Default is NA,
  meaning all areas are included.

- nmap_threshold:

  Integer, indicating the number of shapes required within each group.
  Default is 5.

- threshold_exclude:

  If the number of maps within a group (polity-period) is smaller than
  the number of `cuts` specified (but stiller above the
  `nmap_threshold`), the function will return fewer cuts for that group
  (and output a warning). If `threshold_exclude` is `TRUE`, these cases
  will be dropped. Default is `FALSE`.

- invalid_geom:

  Character, how to handle invalid geometries. If `"stop"`, returns an
  error if there are invalid geometries; if `"exclude"`, invalid
  geometries will be removed before calculating contours; if `"fix"`,
  invalid geometries will be rebuilt using
  [fix_invalid](https://ccappelen.github.io/legaciesR/reference/fix_invalid.md).
  It is recommended to check for invalid geometries and run
  [fix_invalid](https://ccappelen.github.io/legaciesR/reference/fix_invalid.md)
  separately.

- smoothing:

  Logical, whether to apply smoothing after polygonizing the raster.
  Default is `TRUE`.

- returnList:

  Logical, whether to return a list of geometries by group. Default is
  to return an sf dataframe containing all geometries.

- progress:

  Logical, whether to show a progress bar.

- parallel:

  Logical, whether to use parallel processing with `ncores` number of
  cores. See details below.

- ncores:

  Integer, the number of cores to use for parallel processing. Default
  is all available cores minus 1.

- ...:

  Additional arguments passed to
  [contour_polygons](https://ccappelen.github.io/legaciesR/reference/contour_polygons.md).

## Value

Returns either an sf dataframe (default) or a list of sf dataframes (one
list item per group).

## Support for parallel processing

Parallel processing is is implemented using the
[future::future](https://future.futureverse.org/reference/future.html)
framework. There are two ways of running jobs in parallel: `multicore`
which uses 'forking' to run multiple jobs in parallel with shared memory
and `multisession` which launches a set of background R sessions.
'Forking' can be faster than multisession because of the larger overhead
associated with copying the active environment to each background R
session (whereas forking processes shares memory). However, 'forking' is
not supported on Windows platforms and is considered unstable when
running from within RStudio (on both Windows and Unix systems such as
MacOS). The function will automatically determine whether `multicore` is
supported by the platform and choose the appropriate plan.

The greater overhead associated with `multisession` is primarily during
the first parallel run in a given R session (since the background R
sessions stays available for additional parallel jobs). It is possible
to define a
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
in the global environment, which will minimize overhead in subsequent
parallel jobs (apart from the first). The function will automatically
detect if a `multisession` plan has been set globally and, thus, will
not close background sessions after running.
