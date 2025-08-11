
Rtrack <img src="man/figures/logo.png" align="right" alt="" width="120" />
=========================================================================

The Rtrack package
-----------------

Rtrack is a package to process raw spatial tracking data (such as from the Morris water maze or open field tests) and calculate behavioural strategies. The strategy analysis is performed in a parameter-free manner using pre-trained machine learning models. This makes analysis both fast and reproducible. In addition, the package includes several plotting functions, tools to aid advanced analysis, and export functions for results and raw data.

The export of raw data in a standardised format allows complete experiments to be archived and shared.

Install
-----------------------------------------------------------------------------------------------------------

**From CRAN:** The package is available from the [CRAN repository](https://cran.r-project.org/package=Rtrack). To install it (currently version 2.0.2), run the following line in [R](https://www.r-project.org/).

``` r
install.packages("Rtrack") 
```
**From github:** This github repository contains the development version with the latest features and bug fixes. To install this version (2.0.4), run the following line in [R](https://www.r-project.org/) (you will need to have the [remotes](https://CRAN.R-project.org/package=remotes) package already installed).

``` r
remotes::install_github("rupertoverall/Rtrack") 
```

Getting started
----------------------------------------------------------------------------------------------------------------

To get started using Rtrack, it is recommended to work through the [tutorials](https://rupertoverall.net/Rtrack/articles/Rtrack_tutorials_page.html)
