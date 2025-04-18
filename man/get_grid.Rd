% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_grid.R
\name{get_grid}
\alias{get_grid}
\title{Create grid summarizing overlapping polygons.}
\usage{
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
}
\arguments{
\item{shp}{Shape}

\item{ras}{Raster}

\item{raster_from_shp}{Logical, if TRUE, using extent of shp to define grid. If FALSE, and \code{ras} is missing,
grid is generated based on extent of \code{rnaturalearthdata::countries50}. If \code{FALSE} and \code{ras} is not missing,
\code{ras} will be used.}

\item{res}{Resolution}

\item{by_period}{Whether to group by period}

\item{id_var}{ID variable name}

\item{period_var}{Period variable name}

\item{interval}{Scalar or vector indicating the intervals to group by. If \code{NULL} (default), the
intervals will automatically be set to 20 (years) and include the full range (i.e. \code{seq(min, max, 20)}).
For user specification, the value can be either a scalar, indicating the length of intervals
(in which case the full range is used), or a vector specifying the exact breaks, including both the
start of the first interval and end of the last interval.}

\item{nmap_threshold}{Integer, indicating the number of shapes required within each group. Default is 5.}

\item{output}{Character vector. See details.}

\item{subset}{A one-sided formula. If provided, the resulting grid data will be based only on observations
defined by this argument. For example, \code{subset = ~ year > 1850}.}

\item{parallel}{Logical, whether to use parallel processing with \code{ncores} number of cores.}

\item{ncores}{Integer, the number of cores to use for parallel processing. Default is all available cores minus 1.}

\item{updates}{Logical}

\item{return}{List or data.frame}

\item{fix_invalid}{Logical}
}
\value{
Data frame of grid cell-by-state (or grid cell-by-state-by-year) with several summary measures
of the polygons in \code{shp}. By default, all summary measures are calculated, but it is also possible
to specify which measures to calculate with the argument \code{output}, which takes a character vector
as input. The following summary measures are available:
\item{\code{count_across}}{Number of polygons intersecting a grid cell and number of distinct states intersecting a grid cell.}
\item{\code{share_largest_count}}{Share of polygons for the state with the largest number of polygons in total.}
\item{\code{share_largest_area}}{Share of polygons for the state with the largest area.}
\item{\code{share_largest_share}}{Share of polygons for the state with the largest share in a grid cell.}
\item{\code{share_mean}}{Average share of polygons across all states intersecting a grid cell.}
\item{\code{borders}}{Total number of state borders intersecting a grid cell and the share of borders relative to
the total number of states intersecting.}
\item{\code{contested}}{Entropy-based measure of contested territory calculated using the equation \eqn{E = -\sum{}p*ln(p)}, where \eqn{p} is the
state-specific share of polygons intersecting a grid cell.}
}
\description{
Create gridded data by group (or group and year) summarizing the number of
polygons covering a given grid cell.
}
