![DeployBook](https://github.com/NIWAFisheriesModelling/r4Casal2/actions/workflows/deploy_bookdown.yml/badge.svg)

# r4Casal2
An R package that extends the functionality of the base [Casal2](https://github.com/NIWAFisheriesModelling/CASAL2) R package. The purpose of `r4Casal2` is to aid in visualising, intepreting and diagnosing Casal2 models. It depends on the `Casal2` base R package which is not on CRAN and can be downloaded from [here](https://github.com/NIWAFisheriesModelling/CASAL2/tree/master/R-libraries). However, it is advised to use the version that is bundled with your Casal2 binary and user manual. The Casal2 base package is used to read and write Casal2 output and configuration files, where as `r4Casal2` is more visualising and summarising. We seperated these packages in the hope users will feel more comfortable to contribute to the package source code with out messing about with the Casal2 source code.
```r
devtools::install_github("https://github.com/NIWAFisheriesModelling/CASAL2", subdir="R-libraries/casal2", ref = "HEAD")
```



## Install `r4Casal2` from cloned repository
***It is recommended*** that if you are going to use this package, that you clone or fork the github repository. That way you can contribute to the package development, I also find it easier to install.

Once you have cloned the repository (`git clone git@github.com:NIWAFisheriesModelling/r4Casal2.git`), there should be an R-studio project file that you can open (double click). If you have the environment panel open, it should have a button to `install and restart`. Otherwise there is an R script called `build_package.R` that you can run to build and install the package.

## Install `r4Casal2` from remote repository
The second option is to install the `r4Casal2` package from the remote hosted site. This is a recent package so may require package updates if you don't update your `R` environment often. It is recommended to successfully install the following packages before installing `r4Casal2`
```r
install.packages(c("reshape2", "dplyr", "ggplot2", "mvtnorm", "DHARMa","MASS", "knitr"))
```

Once you have successfully installed the above packages, run the following code below. ***Tips*** you will get prompted to update packages, I usually enter the value `2` update only CRAN packages or `3` which will not update any packages.

```r
devtools::install_github("NIWAFisheriesModelling/r4Casal2")
```

If you get the following error 
```r
Downloading GitHub repo NIWAFisheriesModelling/r4Casal2@HEAD
Error in utils::download.file(url, path, method = download_method(), quiet = quiet,  : 
  cannot open URL: ....
```
You can follow the suggestion found  [here](https://stackoverflow.com/questions/53845962/having-trouble-getting-devtoolsinstall-github-to-work-in-r-on-win-7-64bit-ma), which suggests the following command for windows 10 machines.

```r
options(download.file.method = "wininet")
```

## Download `r4Casal2`
If you have difficulty downloading installing `r4Casal2` via the above methods. You can download it from [here](https://github.com/NIWAFisheriesModelling/r4Casal2/actions/workflows/deploy_bookdown.yml). **note** you will need to be logged in to your github account to access this link. Click on the top action that has passed, indicated by the green tick. It should have the same label as the latest commit message. Once you have clicked on the top passed action, you should scroll down to the artifacts sections and click on the link labeled `r4Casal2.tar.gz`. This will download `r4Casal2.tar.gz.zip`. Locate where you downloaded it and unzip it, then assuming there is `r4Casal2.X.X.tar.gz` located in `path_to_tar_gz`. You can install it from you R environment, the same way you would install a local package, or run the following R command
```r
remotes::install_local(path = file.path(path_to_tar_gz, "r4Casal2.X.X.tar.gz"))
```

## Query Functionality
Once the library is installed you can query the functionality to see the functions `library(help="r4Casal2")` or even better see [**the online book**](https://niwafisheriesmodelling.github.io/r4Casal2/).

## Issues
If you have an issues please create a github issue, or reach out on the [discourse page](https://casal2.discourse.group/) 


## Contributing to r4Casal2
If you have some R code for Casal2 models or output, the best way to contribute is to clone or fork this repository and push and pull changes. If this falls in the "too hard basket"" and you have code that would be helpful to others. Please email it in a zipped file to the Casal2 development team <mailto:Casal2@niwa.co.nz> and they will add in the **misc** folder until it has been wrapped in formal functions.


