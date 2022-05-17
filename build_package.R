# build package
# this builds all the help files
library(devtools)
library(roxygen2)
document()

devtools::install()
devtools::check()

library(r4Casal2)

?plot.relative_index
