# Detect potential errors in maps

Detect potential errors in maps

## Usage

``` r
detect_errors(
  shp,
  capital_data,
  id_var,
  period_var,
  errors,
  dist,
  parallel = TRUE,
  ncores,
  progress = TRUE,
  report = TRUE,
  returnList = TRUE
)
```

## Arguments

- shp:

  An sf dataframe containing all geometries

- capital_data:

  Dataframe containing information on capitals (by year). Should have
  the same `id_var` and `period_var` names as `shp`.

- id_var:

  Name of grouping variable (e.g., COWID)

- period_var:

  Name of year variable

- errors:

  Character vector of which errors to search for. See details.

- dist:

  Minimum allowable distance (in km) for which capitals can be outside
  polygons.

- parallel:

  Whether to use parallel processing. Default is TRUE.

- ncores:

  Number of cores to use for parallel processing. Default is to use all
  available cores.

- progress:

  Whether to print progress updates. Default is TRUE.

- report:

  Whether the output should be a report of potential errors. Default is
  an sf dataframe with columns for each error detection.

- returnList:

  Whether to return a list containing the full sf data frame AND
  separate data frames for each error type (default) or just the full sf
  data frame. The full sf data frame will either way include columns
  indicating potential errors for each type.

## Value

Data frame

- `id_duplicates`:

  Whether COWID and COWNUM do not align, i.e. are there duplicate COWIDs
  with different COWNUMs?

- `missing_id`:

  Whether there are geometries with missing `id_var`.

- `singles`:

  Whether there are COWIDs with only one map (may indicate wrong COWID).

- `year`:

  Whether there are polygons with years outside the study window (i.e.
  1750-1920) or if year is missing.

- `overlap`:

  Whether there are COWIDs for which one or more polygons do not overlap
  with any of the other polygons.

- `capital`:

  Whether any of the capitals associated with a COWID falls outside ALL
  polygons. The option `dist` can be used to specify a minimum allowable
  distance.

- `empty`:

  Check whether there are empty geometries.
