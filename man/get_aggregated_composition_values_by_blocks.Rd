% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_observations.R
\name{get_aggregated_composition_values_by_blocks}
\alias{get_aggregated_composition_values_by_blocks}
\alias{get_aggregated_composition_values_by_blocks.casal2MPD}
\alias{get_aggregated_composition_values_by_blocks.list}
\title{get_aggregated_composition_values_by_blocks}
\usage{
get_aggregated_composition_values_by_blocks(model, n_year_blocks = 5)

\method{get_aggregated_composition_values_by_blocks}{casal2MPD}(model, n_year_blocks = 5)

\method{get_aggregated_composition_values_by_blocks}{list}(model, n_year_blocks = 5)
}
\arguments{
\item{model}{<casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )}

\item{n_year_blocks}{integer specifying the number of years in a time-block to aggregate over}
}
\value{
data frame with aggregated observed and expected over all years for each observation
}
\description{
An accessor function that returns a data frame of aggregrated composition
}
\author{
Craig Marsh
}
