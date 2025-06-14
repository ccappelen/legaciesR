% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/id2country.R
\name{id2country}
\alias{id2country}
\title{Aggregate ID data to modern country}
\usage{
id2country(
  id_data,
  method = c("source", "capital", "polygon"),
  cshape_year = "2019",
  multiple = FALSE,
  exclude_hierarchy = c("none", "tributary", "dependency", "all")
)
}
\arguments{
\item{id_data}{ID data to be aggregated. If \code{method} is \code{"polygon"}, the ID data must include contour
polygons (using the \code{match_id_contour} function).}

\item{method}{Whether to aggregate based on source coding (\code{"source"}), capital city locations (\code{"capital"}),
or contour polygons (\code{"polygon"}). If \code{method = "polygon"}, polities will only be included in a given country if
(i) the historical territory falls entirely within the modern country or (ii) the historical polity covers at least
5 \% of the modern country's territory.}

\item{cshape_year}{Character, the year of modern country borders (\code{cshapes}). Default is \code{"2019"}, which is currently the
latest year available.}

\item{multiple}{If \code{method == "source"}, whether to allow polities to be counted across multiple countries
if the source coding suggests multiple destination states. If \code{FALSE}, the most important destination state is used.}

\item{exclude_hierarchy}{Whether to exclude certain hierarchy types. May have already been excluded in earlier functions
(e.g., \code{prepare_id} and \code{prepare_shapes}).}
}
\value{
A dataframe of modern countries with a column for the number of unique historical states.
}
\description{
The function aggregates the ID state data (number of historical states) to modern day country borders.
The aggregation method is based on either destination states from source coding, the location of historical
capital cities, or contour polygons.
}
