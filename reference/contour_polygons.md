# Create Contour Polygons

This function creates contour polygons from multiple overlapping
polygons.

## Usage

``` r
contour_polygons(
  shp,
  cuts = 4,
  id_vars,
  res = 1/30,
  nmap_threshold = 2,
  smoothing = TRUE,
  invalid_geom = c("stop", "fix", "exclude"),
  include_higher = TRUE
)
```

## Arguments

- shp:

  An sf dataframe to be aggregated. Must consist of more than one
  feature.

- cuts:

  Integer of length 1 specifying the number of (equally spaced) contour
  polygons to be returned. For example, if `cuts = 10` the function will
  return 10 polygons representing the 10 deciles (0-0.1, 0.1-0.2, ...).

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

  logical, whether the contour polygons should include percentiles above
  current interval. If TRUE (default), the 50 % polygon will include all
  areas covered by at least 50 % of the shapes (and not just within the
  specified interval, e.g., 0.75-1, 0.50-1, 0.25-1...). If FALSE, the
  polygon will include only areas within the specified interval (e.g.,
  0.75-1, 0.50-0.75, 0.25-0.50...).

## Value

Returns an sf dataframe with the same number of features as specified in
`cuts.` The density, or percentile, is stored in the columns `prob` and
`label.`
