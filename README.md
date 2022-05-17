![Pass Test](https://github.com/NIWAFisheriesModelling/r4Casal2/actions/workflows/CheckDocumentation.yml/badge.svg)

# r4Casal2
An R package that extends the functionality of the base [Casal2](https://github.com/NIWAFisheriesModelling/CASAL2) R package, to aid in visualising, model reporting and diagnostics. It depends on the latest `Casal2` R package which is not on CRAN you can download the latest package from [here](https://github.com/NIWAFisheriesModelling/CASAL2/tree/master/R-libraries) or try and use the command below. The Casal2 base package is used to read and write Casal2 output and configuration files, where as `r4Casal2` is more visualising summarising that we hope other users will contribute to.
```r
devtools::install_github("https://github.com/NIWAFisheriesModelling/CASAL2", subdir="R-libraries/casal2", ref = "HEAD")
```


## Install `r4Casal2` from cloned repository
***It is recommended*** that if you are going to use this package that you clone or fork the github repository. That way you can contribute to the package development, I also find it easier to install.

Once you have cloned the repository (`git clone git@github.com:NIWAFisheriesModelling/Casal2_contrib.git`) navigate to the `r4Casal2` directory. There should be an R-studio project file that you can open. If you have the environment panel open it should have a button to `install and restart`. Otherwise there is an R script called `build_package.R` that you can run to build and install the package.

## Install `r4Casal2` from remote repository
The second option is to install the `r4Casal2` package from the remotely hosted site. This is a recent package so may require package updates if you don't update your `R` environment often. It is recommended to successfully install the following packages before installing `r4Casal2`
```r
install.packages(c("reshape2", "dplyr", "ggplot2", "mvtnorm", "DHARMa","gridExtra","MASS", "knitr", "rmarkdown", "fastmap", "kableExtra"))
```


Once you have successfully installed the above packages, run the following code below. ***Tips*** you will get prompted to update packages, I usually enter the value `2` update only CRAN packages or `3` which wont update any packages.
```r
devtools::install_github("NIWAFisheriesModelling/Casal2_contrib/r4Casal2", build_vignettes  = TRUE)
```
We have found for users with old R versions and R package versions, that the install may fail due to the `vignettes`. If this happens try installing without the vignettes
```r
devtools::install_github("NIWAFisheriesModelling/Casal2_contrib/r4Casal2", build_vignettes  = FALSE)
```
An issue you might find when installing this is the Casal2 R package that is not hosted on CRAN i.e. `library(Casal2)`. The package should be part of the installation or zip package that you recieved with your Casal2 executable, which can be downloaded from here. [here](https://casal2.github.io/casal/).

## Query Functionality
Once the library is installed you can query the functionality `library(help="r4Casal2")` also see the vignette with the command `browseVignettes("r4Casal2")`

## Issues
If you have an issues please create a github issue, or reach out on the [discourse page](https://casal2.discourse.group/) 

