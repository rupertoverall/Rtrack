
Rtrack <img src="man/figures/logo.png" align="right" alt="" width="120" />
=========================================================================

The Rtrack package
-----------------

Rtrack is a package to process raw spatial tracking data (such as from the Morris water maze task) and calculate behavioural strategies. The strategy analysis is performed in a parameter-free manner using pre-trained machine learning models. This makes analysis both fast and reproducible. In addition, the package includes several plotting functions, tools to aid advanced analysis, and export functions for results and raw data.

The export of raw data in a standardised format allows complete experiments to be archived and shared.

Install
-----------------------------------------------------------------------------------------------------------

The package is available from the [package website](http://rupertoverall.net/Rtrack). To install this, run the following lines in [R](https://www.r-project.org/).

First, install any necessary dependencies:

``` r
dependencies = c("crayon", "graphics", "grDevices", "KernSmooth", "methods", "openxlsx", "parallel", "pbapply", "randomForest", "raster", "readxl", "rgeos", "rjson", "snow", "sp", "stats", "tools", "utils")
install.packages(dependencies[!dependencies %in% installed.packages()]) 
```

Then install the package directly from the [package website](http://rupertoverall.net/Rtrack):

``` r
install.packages("http://rupertoverall.net/Rtrack/Rtrack_0.9.0.tar.gz", repos = NULL)
```

Getting started
----------------------------------------------------------------------------------------------------------------

To get started using Rtrack, it is recommended to visit the package website at [http://rupertoverall.net/Rtrack](http://rupertoverall.net/Rtrack)
