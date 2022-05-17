![Pass Test](https://github.com/NIWAFisheriesModelling/r4Casal2/actions/workflows/CheckDocumentation.yml/badge.svg)

# r4Casal2
An R package that extends the functionality of the base [Casal2](https://github.com/NIWAFisheriesModelling/CASAL2) R package. The purpose of `r4Casal2` is to aid in visualising, intepreting and diagnosing Casal2 models. It depends on the `Casal2` base R package which is not on CRAN you can download the latest package from [here](https://github.com/NIWAFisheriesModelling/CASAL2/tree/master/R-libraries) but it is advised to use the version that is bundled with your Casal2 binary. The Casal2 base package is used to read and write Casal2 output and configuration files, where as `r4Casal2` is more visualising and summarising. We seperated these packages in the hope users will feel more comfortable contribute to the package source code with out messing about with the Casal2 source code.
```r
devtools::install_github("https://github.com/NIWAFisheriesModelling/CASAL2", subdir="R-libraries/casal2", ref = "HEAD")
```


## Install `r4Casal2` from cloned repository
***It is recommended*** that if you are going to use this package that you clone or fork the github repository. That way you can contribute to the package development, I also find it easier to install.

Once you have cloned the repository (`git clone git@github.com:NIWAFisheriesModelling/r4Casal2.git`), there should be an R-studio project file that you can open (double click). If you have the environment panel open, it should have a button to `install and restart`. Otherwise there is an R script called `build_package.R` that you can run to build and install the package.

## Install `r4Casal2` from remote repository
The second option is to install the `r4Casal2` package from the remotely hosted site. This is a recent package so may require package updates if you don't update your `R` environment often. It is recommended to successfully install the following packages before installing `r4Casal2`
```r
install.packages(c("reshape2", "dplyr", "ggplot2", "mvtnorm", "DHARMa","MASS", "knitr"))
```

Once you have successfully installed the above packages, run the following code below. ***Tips*** you will get prompted to update packages, I usually enter the value `2` update only CRAN packages or `3` which wont update any packages.
```r
devtools::install_github("NIWAFisheriesModelling/Casal2_contrib/r4Casal2", build_vignettes  = TRUE)
```
We have found for users with old R versions and R package versions, that the install may fail due to the `vignettes`. If this happens try installing without the vignettes
```r
devtools::install_github("NIWAFisheriesModelling/r4Casal2")
```

## Query Functionality
Once the library is installed you can query the functionality to see the functions `library(help="r4Casal2")` or even better see [**the online book**](https://niwafisheriesmodelling.github.io/r4Casal2/).

## Issues
If you have an issues please create a github issue, or reach out on the [discourse page](https://casal2.discourse.group/) 

