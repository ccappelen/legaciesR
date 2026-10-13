# Create Contour Polygons

This function creates contour polygons from multiple overlapping
polygons.

## Usage

``` r
contour_polygons(
  shp,
  n.breaks = 4,
  breaks = NULL,
  label.breaks = NULL,
  id_vars,
  res = 1/30,
  nmap_threshold = 2,
  smoothing = TRUE,
  invalid_geom = c("stop", "fix", "exclude"),
  include_higher = TRUE,
  exclude_lower_bound = NA
)
```

## Arguments

- shp:

  An sf dataframe to be aggregated. Must consist of more than one
  feature.

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

- id_vars:

  Group identifiers.

- res:

  Resolution of the raster (in degrees) used to calculate polygon
  density. Higher resolution (i.e., lower numbers) creates smoother
  borders but also increases processing time. Default is 1/30 degrees.

- nmap_threshold:

  Integer indicating the number of geometries required. Default is 2. If
  threshold is 1, a single geometry will be returned unmodified.

- smoothing:

  Logical, whether to apply smoothing after polygonizing raster. Default
  is `TRUE.`

- invalid_geom:

  Character, how to handle invalid geometries. If `"stop"`, returns an
  error if there are invalid geometries; if `"exclude"`, invalid
  geometries will be removed before calculating contours; if `"fix"`,
  invalid geometries will be rebuilt using
  [fix_invalid](https://ccappelen.github.io/legaciesR/reference/fix_invalid.md).
  It is recommended to check for invalid geometries and run
  [fix_invalid](https://ccappelen.github.io/legaciesR/reference/fix_invalid.md)
  separately.

- include_higher:

  Logical, whether the contour polygons should include percentiles above
  current interval. If TRUE (default), the 50 % polygon will include all
  areas covered by at least 50 % of the shapes (and not just within the
  specified interval, e.g., 0.75-1, 0.50-1, 0.25-1...). If FALSE, the
  polygon will include only areas within the specified interval (e.g.,
  0.75-1, 0.50-0.75, 0.25-0.50...).

- exclude_lower_bound:

  Numeric, if specified, the areas of least coverage will be excluded
  before generating the contour polygons. Must be a number between 0
  and 1. For example, 0.05 means that areas with less than 5 percent
  coverage will be excluded from the contour polygons. Default is NA,
  meaning all areas are included.

## Value

Returns an sf dataframe with the same number of features as specified in
`cuts.` The density, or percentile, is stored in the columns `prob` and
`label.`
