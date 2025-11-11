# Prepare shapefiles

The function prepares data for further processing with other functions
in the \`legaciesr' package, e.g., expands rows for maps coded with a
range (rather than specific year), excludes certain maps, etc.

## Usage

``` r
prepare_shapes(
  shp,
  state_data,
  id_var,
  period_var,
  range_min,
  range_max,
  fix_year = TRUE,
  year_na = TRUE,
  expand_range = 20L,
  match_capitals = TRUE,
  exclude_source_year = NA,
  exclude_core = TRUE,
  exclude_incomplete = TRUE,
  exclude_sovereign = TRUE,
  margin_sovereign = 5,
  crop_to_land = TRUE
)
```

## Arguments

- shp:

  sf dataframe

- state_data:

  dataframe with state ID data

- id_var:

  ID variable, must be the same in both `shp` and `state_data`

- period_var:

  Year variable, must be the same in both `shp` and `state_data`

- range_min:

  Name of variable for lower year interval

- range_max:

  Name of variable for upper year interval

- fix_year:

  Logical, whether to fix three-digit years. Default is `TRUE`. See
  details.

- year_na:

  Logical, whether to exclude maps with no year indication (exact or
  range)

- expand_range:

  Integer, indicating for maps with year interval (rather than specific
  year) the number of years between repeating maps. If `0`, only the
  start and end date will be included. Otherwise, the specified number
  indicates the how often maps should be repeated. Default is `20`
  years.

- match_capitals:

  Logical, whether to add information on capital cities to the `shp`
  data. Default is `TRUE`. See details.

- exclude_source_year:

  Integer, indicating if maps from sources before a given year should be
  dropped. Default is `NA`, which means all maps, regardless of source
  year, are included.

- exclude_core:

  Exclude maps coded as core (i.e., include only the complete shape of
  both core and periphery). Default is `TRUE`.

- exclude_incomplete:

  Exclude maps with incomplete borders. Default is `TRUE`.

- exclude_sovereign:

  Exclude maps for states that are not sovereign at the time of the map.
  If `exclude_sovereign` is not `none`, then sovereign spells are
  defined by the same hierarchy rule. Otherwise, all polity-years in the
  ID data will be included as sovereign spells. See details.

- margin_sovereign:

  Number of years before or after a state is sovereign that a map is
  still included (if `exclude_sovereign` is `TRUE`). Default is 5 years.

- crop_to_land:

  Logical, whether to crop geometries to land (default is `TRUE`)

## Value

An sf dataframe. Further details:

- `expand_range`:

  Expands rows for maps with interval range rather than specific year.
  The lower and upper limits are always retained. In between the limits,
  rows are only in the specific interval. For example if
  `expand_range = 20`, years divisible by 20 will be included (in
  addition to the lower and upper limits).

- `match_capitals`:

  Capital coordinates are stored as separate geometry. If there are
  multiple capitals in the same year, all of them withh be included. The
  capital names will then be separated by semicolon.

- `exclude_sovereign`:

  Excludes maps for states that are not sovereign. If the year of the
  map is within 5 years of a state being sovereign, the map is still
  included.
