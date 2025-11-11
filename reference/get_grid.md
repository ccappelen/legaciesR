# Create grid summarizing overlapping polygons.

Create gridded data by group (or group and year) summarizing the number
of polygons covering a given grid cell.

## Usage

``` r
get_grid(
  shp,
  ras,
  raster_from_shp = TRUE,
  res = 1/2,
  by_period = FALSE,
  id_var,
  period_var,
  interval = NULL,
  nmap_threshold = 5,
  output,
  subset = NULL,
  parallel = TRUE,
  ncores,
  updates = TRUE,
  return = c("list", "data"),
  fix_invalid = FALSE
)
```

## Arguments

- shp:

  Shape

- ras:

  Raster

- raster_from_shp:

  Logical, if TRUE, using extent of shp to define grid. If FALSE, and
  `ras` is missing, grid is generated based on extent of
  [`rnaturalearthdata::countries50`](https://docs.ropensci.org/rnaturalearthdata/reference/countries.html).
  If `FALSE` and `ras` is not missing, `ras` will be used.

- res:

  Resolution

- by_period:

  Whether to group by period

- id_var:

  ID variable name

- period_var:

  Period variable name

- interval:

  Scalar or vector indicating the intervals to group by. If `NULL`
  (default), the intervals will automatically be set to 20 (years) and
  include the full range (i.e. `seq(min, max, 20)`). For user
  specification, the value can be either a scalar, indicating the length
  of intervals (in which case the full range is used), or a vector
  specifying the exact breaks, including both the start of the first
  interval and end of the last interval.

- nmap_threshold:

  Integer, indicating the number of shapes required within each group.
  Default is 5.

- output:

  Character vector. See details.

- subset:

  A one-sided formula. If provided, the resulting grid data will be
  based only on observations defined by this argument. For example,
  `subset = ~ year > 1850`.

- parallel:

  Logical, whether to use parallel processing with `ncores` number of
  cores.

- ncores:

  Integer, the number of cores to use for parallel processing. Default
  is all available cores minus 1.

- updates:

  Logical

- return:

  List or data.frame

- fix_invalid:

  Logical

## Value

Data frame of grid cell-by-state (or grid cell-by-state-by-year) with
several summary measures of the polygons in `shp`. By default, all
summary measures are calculated, but it is also possible to specify
which measures to calculate with the argument `output`, which takes a
character vector as input. The following summary measures are available:

- `count_across`:

  Number of polygons intersecting a grid cell and number of distinct
  states intersecting a grid cell.

- `share_largest_count`:

  Share of polygons for the state with the largest number of polygons in
  total.

- `share_largest_area`:

  Share of polygons for the state with the largest area.

- `share_largest_share`:

  Share of polygons for the state with the largest share in a grid cell.

- `share_mean`:

  Average share of polygons across all states intersecting a grid cell.

- `borders`:

  Total number of state borders intersecting a grid cell and the share
  of borders relative to the total number of states intersecting.

- `contested`:

  Entropy-based measure of contested territory calculated using the
  equation \\E = -\sum{}p\*ln(p)\\, where \\p\\ is the state-specific
  share of polygons intersecting a grid cell.
