% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_derived_quantities.R
\name{plot_derived_quantities}
\alias{plot_derived_quantities}
\alias{plot_derived_quantities.casal2MPD}
\alias{plot_derived_quantities.casal2TAB}
\alias{plot_derived_quantities.list}
\title{plot_derived_quantities default}
\usage{
plot_derived_quantities(model)

\method{plot_derived_quantities}{casal2MPD}(model)

\method{plot_derived_quantities}{casal2TAB}(model)

\method{plot_derived_quantities}{list}(model)
}
\arguments{
\item{model}{<casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )}

\item{plot.it}{Whether to generate a default plot or return the values as a matrix.}

\item{plot_type}{string}
}
\value{
A ggplot object
}
\description{
A plotting function for Casal2 derived_quantities
}
\examples{
\donttest{
library(Casal2)
# plotting Standard Output
data <- extract.mpd(file = system.file("extdata", "estimate.out", package="r4Casal2"))
names(data)
par(mfrow = c(1,2))
plot_derived_quantities(model = data)
}
}
\author{
Craig Marsh
}
