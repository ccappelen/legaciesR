% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_invalid.R
\name{fix_invalid}
\alias{fix_invalid}
\title{Fix Invalid Geometries}
\usage{
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
}
\arguments{
\item{shp}{An sf dataframe.}

\item{max_precision}{Integer, maximum level of precision for \code{\link[s2:s2_options]{s2::s2_options()}}, default is 10^7.}

\item{min_precision}{Integer, minimum allowed precision for \code{\link[s2:s2_options]{s2::s2_options()}}, default is 10. If geometry
is invalid after rebuilding with minimum precision allowed, the function will return an error if
\code{stop_if_invalid} is \code{TRUE} and a warning otherwise.}

\item{stop_if_invalid}{Logical, whether the function should return an error if it fails
to rebuild a valid geometry. The error message will indicate the row number of the invalid geometry.}

\item{progress}{Logical, whether a progress bar should be displayed. Default is \code{TRUE.}}

\item{parallel}{Logical, whether to use parallel processing with \code{ncores} number of cores.
See details below.}

\item{ncores}{Integer, number of cores to use for parallel processing. Default is all available cores minus 1.}

\item{report}{Logical, whether to report the number of valid, rebuilt, and invalid geometries. If
\code{reportColumns} is \code{FALSE}, no report will be generated.}

\item{reportColumns}{Logical, whether to return the status of each geometry in new columns
(\code{geom_valid}, \code{rebuilt}, and \code{snap_precision}).}
}
\value{
Returns an sf dataframe with the same number of features as the input. The dataframe includes
three new columns: \code{geom_valid} indicates whether the original geometry was valid (\code{TRUE}), \code{rebuilt}
indicates if the geometry was successfully rebuilt, and \code{snap_precision} indicates the snapping
precision used to rebuild geometry (if \code{rebuilt} is \code{FALSE}, \code{snap_precision} indicates the minimum snapping
precision that led to invalid geometry).
}
\description{
Rebuilds invalid geometries. If necessary, the functions will iteratively
lower the snapping precision if default options are inadequate.
}
\section{Support for parallel processing}{

Parallel processing is is implemented using the \link[future:future]{future::future} framework. There are two ways
of running jobs in parallel: \code{multicore} which uses 'forking' to run multiple jobs in parallel with
shared memory and \code{multisession} which launches a set of background R sessions. 'Forking' can be faster
than multisession because of the larger overhead associated with copying the active environment to
each background R session (whereas forking processes shares memory). However, 'forking' is not
supported on Windows platforms and is considered unstable when running from within RStudio
(on both Windows and Unix systems such as MacOS). The function will automatically determine
whether \code{multicore} is supported by the platform and choose the appropriate plan.

The greater overhead associated with \code{multisession} is primarily during the first parallel
run in a given R session (since the background R sessions stays available for additional
parallel jobs). It is possible to define a \code{\link[future:plan]{future::plan()}} in the global environment,
which will minimize overhead in subsequent parallel jobs (apart from the first). The function will
automatically detect if a \code{multisession} plan has been set globally and, thus, will not close
background sessions after running.
}

