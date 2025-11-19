# Match covariates to raster data.

Adds a specified list of common covariates to the gridded data frame.
The function takes either a SpatRaster object, in which case it returns
a data frame with the specified covariates, or a list object returned
from
[get_grid](https://ccappelen.github.io/legaciesR/reference/get_grid.md),
in which case it returns the same list object with covariates matched to
the data frame.

## Usage

``` r
data2grid(x, covars)
```

## Arguments

- x:

  Either an object of class `SpatRaster` or a list object returned from
  [get_grid](https://ccappelen.github.io/legaciesR/reference/get_grid.md)

- covars:

  Character vector specifying the covariates to add. Default is the full
  list, i.e. `covars = c("area", "terrain", "malaria", "coast")` (see
  details).

## Value

Returns either a dataframe with rows corresponding to grid cells (if `x`
is a `SpatRaster` object) or a the list object with the covariates
matched to the data item (if `x` is the list object returned from
[get_grid](https://ccappelen.github.io/legaciesR/reference/get_grid.md)).
The following covariates are matched by default:

- `area`:

  Area of the grid cells in km2

- `terrain`:

  Average elevation in meters and average terrain ruggedness within each
  grid cell. Requires installation of the package `elevatr`. Source:

- `malaria`:

  Mean temperature suitability index for malaria within each grid cell.
  Requires installation of the package `malariaAtlas`. Source:

- `coast`:

  Distance to nearest coast in meters. Requires installation of the
  package `rnaturalearthdata`. Source:
