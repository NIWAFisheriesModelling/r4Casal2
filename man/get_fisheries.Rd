% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fisheries.R
\name{get_fisheries}
\alias{get_fisheries}
\alias{get_fisheries.casal2MPD}
\alias{get_fisheries.list}
\alias{get_fisheries.casal2TAB}
\title{get_fisheries}
\usage{
get_fisheries(model)

\method{get_fisheries}{casal2MPD}(model)

\method{get_fisheries}{list}(model)

\method{get_fisheries}{casal2TAB}(model)
}
\arguments{
\item{model}{<casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions.}
}
\value{
A data frame from Casal2 model output
}
\description{
An accessor function that returns a data frame from a Casal2 model output of process type recruitment
}
\author{
Craig Marsh
}
