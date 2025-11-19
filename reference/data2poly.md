# Match covariates to polygon data.

Adds a specified list of common covariates to an `sf` polygons
data.frame. The function takes an `sf` dataframe (typically the output
from
[get_contours](https://ccappelen.github.io/legaciesR/reference/get_contours.md))
and returns the same data frame with the specified selection of
covariates.

## Usage

``` r
data2poly(shp, covars)
```

## Arguments

- shp:

  `sf` polygons data.frame, typically the output from
  [get_contours](https://ccappelen.github.io/legaciesR/reference/get_contours.md).

- covars:

  Character vector specifying the covariates to add. Default is the full
  list, i.e. `covars = c("area", "terrain", "malaria", "coast")` (see
  details).

## Value

Returns a dataframe similar to `shp` with columsn added for covariates.
The following covariates are matched by default:

- `area`:

  Area of the polygon in km2

- `terrain`:

  Average elevation in meters and average terrain ruggedness within each
  polygon. Requires installation of the package `elevatr`. Source:

- `malaria`:

  Mean temperature suitability index for malaria within each polygon.
  Requires installation of the package `malariaAtlas`. Source:

- `coast`:

  Distance to nearest coast from the border of polygons (in km).
  Requires installation of the package `rnaturalearthdata`. Source:
