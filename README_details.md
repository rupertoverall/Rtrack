# 1. System requirements
## Operating system
This package runs on any operating system with R (≥ version 3.5) installed. The package has been tested on the following systems:
(Last updated for Rtrack version 2.0.3 on 2023-08-12 05:52:37 CEST).

|Flavor|Version|Tinstall|Tcheck|Ttotal|Status|Flags|
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
|r-devel-linux-x86_64-debian-clang | 2.0.2 | 66.45 | 325.82 | 392.27 | OK | 
|r-devel-linux-x86_64-debian-gcc | 2.0.2 | 53.72 | 233.69 | 287.41 | OK | 
|r-devel-linux-x86_64-fedora-clang | 2.0.2 | | | 413.22 | NOTE | 
|r-devel-linux-x86_64-fedora-gcc | 2.0.2 | | | 483.95 | OK | 
|r-devel-windows-x86_64 | 2.0.2 | 51.00 | 192.00 | 243.00 | NOTE | 
|r-patched-linux-x86_64 | 1.0.7 | 32.85 | 294.04 | 326.89 | OK | 
|r-release-linux-x86_64 | 1.0.7 | 30.43 | 290.66 | 321.09 | OK | 
|r-release-macos-arm64 | 2.0.2 | | | 113.00 | NOTE | 
|r-release-macos-x86_64 | 2.0.2 | | | 317.00 | NOTE | 
|r-release-windows-x86_64 | 2.0.2 | 55.00 | 267.00 | 322.00 | NOTE | 
|r-oldrel-macos-arm64 | 2.0.2 | | | 106.00 | NOTE | 
|r-oldrel-macos-x86_64 | 1.0.7 | | | 123.00 | OK | 
|r-oldrel-windows-x86_64 | 2.0.2 | 67.00 | 246.00 | 313.00 | NOTE | 

(NOTEs indicate only that the installed size is 5.2 Mb due to the embedded strategy calling models. This is a bit larger than the 5 Mb package size limit preferred by CRAN. However, having the models embedded in the package simplifies installation.)

We can also confirm that the package runs on Linux ARM64 (we test on a Raspberry Pi 5 running Ubuntu 24.04.2 LTS).
 
## R dependencies
Imports: 	crayon, graphics, grDevices, Hmisc, KernSmooth, methods, openxlsx, parallel, pbapply, randomForest, RCurl, readxl, rjson, scales, stats, stringi, terra, tools, utils, zip

Suggests: 	knitr, rmarkdown

## Hardware requirements
No special hardware is required.

# 2. Installation guide
`install.packages("Rtrack")`
Install time is typically < 30 s depending on internet connection speed and the number of dependencies the user already has installed.

# 3. Demo
The core functionality can be demonstrated by the following code which downloads an example dataset, processes track metrics, calls strategies and draws a strategy overview plot.
```
experiment = Rtrack::read_experiment("https://rupertoverall.net/Rtrack/examples/MWM_example.trackxf")
strategies = Rtrack::call_strategy(experiment)
Rtrack::plot_strategies(strategies, experiment, factor = "Strain")
```
This code takes 14 s to complete on an M1 MacBook Pro laptop (using only a single CPU core and including download time).
A detailed walk-through of all functions provided by the package is also available at https://rupertoverall.net/Rtrack/articles/Rtrack_tutorials_page.html

# 4. Instructions for use
The online tutorials https://rupertoverall.net/Rtrack/articles/Rtrack_tutorials_page.html provide step-by-step instructions for running the software and details on how to prepare raw data for analysis.
