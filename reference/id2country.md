# Aggregate ID data to modern country

The function aggregates the ID state data (number of historical states)
to modern day country borders. The aggregation method is based on either
destination states from source coding, the location of historical
capital cities, or contour polygons.

## Usage

``` r
id2country(
  id_data,
  method = c("source", "capital", "polygon"),
  cshape_year = "2019",
  multiple = FALSE,
  exclude_hierarchy = c("none", "tributary", "dependency", "all")
)
```

## Arguments

- id_data:

  ID data to be aggregated. If `method` is `"polygon"`, the ID data must
  include contour polygons (using the `match_id_contour` function).

- method:

  Whether to aggregate based on source coding (`"source"`), capital city
  locations (`"capital"`), or contour polygons (`"polygon"`). If
  `method = "polygon"`, polities will only be included in a given
  country if (i) the historical territory falls entirely within the
  modern country or (ii) the historical polity covers at least 5 % of
  the modern country's territory.

- cshape_year:

  Character, the year of modern country borders (`cshapes`). Default is
  `"2019"`, which is currently the latest year available.

- multiple:

  If `method == "source"`, whether to allow polities to be counted
  across multiple countries if the source coding suggests multiple
  destination states. If `FALSE`, the most important destination state
  is used.

- exclude_hierarchy:

  Whether to exclude certain hierarchy types. May have already been
  excluded in earlier functions (e.g., `prepare_id` and
  `prepare_shapes`).

## Value

A dataframe of modern countries with a column for the number of unique
historical states.
