# Merge LEGACIES data with covariates

Merge LEGACIES data with covariates

## Usage

``` r
get_covariates(
  x,
  vars,
  grid_id = gid,
  path,
  progress = TRUE,
  parallel = FALSE,
  coords
)
```

## Arguments

- x:

  List with named elements `r`, object of class `SpatRaster`, and
  `data`, object of class `data.frame`, OR a named object of class
  `data.frame`. The list object is returned by the `get_grid` function
  if `return = "list"`.

- vars:

  Covariate data to merge. Default includes all variables. See details
  below.

- grid_id:

  Name of column identifying the grid cell. Default is `gid` which
  follows convention in `get_grid`.

- path:

  Character string specifying the path name for the LEGACIES team
  folder. Depends on the user's computer, but generally should be along
  the lines of `"~/user/OneDrive-NTNU/legacies-project"`.

- progress:

  Logical, whether progress updates should be displayed.

- parallel:

  Logical, whether to parallelize jobs (CURRENTLY NOT USED)

- coords:

  Character vector of length two specifying the names of the coordinate
  (longitude/latitude) variables. Default is `c("lon", "lat")` which
  follows the convention in `get_grid`.

## Value

Data frame or list of data frame and SpatRaster. By default, all
covariates are included, but it is also possible to specify which
covariates to include with the argument `vars`, which takes a character
vector as input. The following data are available:

- `terrain`:

  .

- `climate`:

  .

- `crops`:

  .

- `rivers`:

  .

- `coast`:
