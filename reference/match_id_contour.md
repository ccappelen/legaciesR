# Match contour polygons to ID data

The function attaches contour polygon geometries to the ID data as well
as data on land area and population.

## Usage

``` r
match_id_contour(
  id_data,
  contour_data,
  contour_level = 0.25,
  multiple_levels = FALSE,
  covar_path
)
```

## Arguments

- id_data:

  A dataframe with ID data (after being processed with the `parepare_id`
  function.)

- contour_data:

  An sf dataframe with contour polygons (from `get_contours`).

- contour_level:

  The probability level to use if matching to a single polygon per
  polity-year. Default is `0.25`.

- multiple_levels:

  Whether to include all contour levels, in which case there will be
  multiple rows per polity-year.

- covar_path:

  File path to folder containing HYDE population data. Typically the
  path will be something like `"~/user/OneDrive-NTNU/legacies-project"`.

## Value

A dataframe with polity-year observations and columns for land area
(based on contour polygons) and decade population estimates based on
HYDE population data (aggregated to contour polygons).
