# Fix Invalid Geometries

Rebuilds invalid geometries. If necessary, the functions will
iteratively lower the snapping precision if default options are
inadequate.

## Usage

``` r
fix_invalid(
  shp,
  max_precision = 10^7,
  min_precision = 10,
  stop_if_invalid = FALSE,
  progress = TRUE,
  parallel = NULL,
  ncores,
  report = TRUE,
  reportColumns = TRUE
)
```

## Arguments

- shp:

  An sf dataframe.

- max_precision:

  Integer, maximum level of precision for
  [`s2::s2_options()`](https://r-spatial.github.io/s2/reference/s2_options.html),
  default is 10^7.

- min_precision:

  Integer, minimum allowed precision for
  [`s2::s2_options()`](https://r-spatial.github.io/s2/reference/s2_options.html),
  default is 10. If geometry is invalid after rebuilding with minimum
  precision allowed, the function will return an error if
  `stop_if_invalid` is `TRUE` and a warning otherwise.

- stop_if_invalid:

  Logical, whether the function should return an error if it fails to
  rebuild a valid geometry. The error message will indicate the row
  number of the invalid geometry.

- progress:

  Logical, whether a progress bar should be displayed. Default is
  `TRUE.`

- parallel:

  Logical, whether to use parallel processing with `ncores` number of
  cores. See details below.

- ncores:

  Integer, number of cores to use for parallel processing. Default is
  all available cores minus 1.

- report:

  Logical, whether to report the number of valid, rebuilt, and invalid
  geometries. If `reportColumns` is `FALSE`, no report will be
  generated.

- reportColumns:

  Logical, whether to return the status of each geometry in new columns
  (`geom_valid`, `rebuilt`, and `snap_precision`).

## Value

Returns an sf dataframe with the same number of features as the input.
The dataframe includes three new columns: `geom_valid` indicates whether
the original geometry was valid (`TRUE`), `rebuilt` indicates if the
geometry was successfully rebuilt, and `snap_precision` indicates the
snapping precision used to rebuild geometry (if `rebuilt` is `FALSE`,
`snap_precision` indicates the minimum snapping precision that led to
invalid geometry).

## Support for parallel processing

Parallel processing is is implemented using the
[future::future](https://future.futureverse.org/reference/future.html)
framework. There are two ways of running jobs in parallel: `multicore`
which uses 'forking' to run multiple jobs in parallel with shared memory
and `multisession` which launches a set of background R sessions.
'Forking' can be faster than multisession because of the larger overhead
associated with copying the active environment to each background R
session (whereas forking processes shares memory). However, 'forking' is
not supported on Windows platforms and is considered unstable when
running from within RStudio (on both Windows and Unix systems such as
MacOS). The function will automatically determine whether `multicore` is
supported by the platform and choose the appropriate plan.

The greater overhead associated with `multisession` is primarily during
the first parallel run in a given R session (since the background R
sessions stays available for additional parallel jobs). It is possible
to define a
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
in the global environment, which will minimize overhead in subsequent
parallel jobs (apart from the first). The function will automatically
detect if a `multisession` plan has been set globally and, thus, will
not close background sessions after running.
