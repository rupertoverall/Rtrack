# 1. System requirements
## Operating system
This package runs on any operating system with R (≥ version 3.5) installed. The package has been tested on the following systems:
(Last updated for Rtrack version 2.0.4 on 2025-09-01 17:09:00 CEST).

|Flavor|Version|Tinstall|Tcheck|Ttotal|Status|Flags|
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
r-devel-linux-x86_64-debian-clang | 2.0.4 | 66.58 | 271.97 | 338.55 | OK | | 
r-devel-linux-x86_64-debian-gcc | 2.0.4 | 39.91 | 163.79 | 203.70 | OK | | 
r-devel-linux-x86_64-fedora-clang | 2.0.4 | 		520.54 | OK | | 
r-devel-linux-x86_64-fedora-gcc | 2.0.4 | 		503.29 | OK | | 
r-devel-windows-x86_64 | 2.0.4 | 55.00 | 201.00 | 256.00 | OK | | 
r-patched-linux-x86_64 | 2.0.4 | 59.77 | 250.97 | 310.74 | OK | | 
r-release-linux-x86_64 | 2.0.4 | 58.67 | 252.14 | 310.81 | OK | | 
r-release-macos-arm64 | 2.0.4 | 		112.00 | OK | | 
r-release-macos-x86_64 | 2.0.4 | 		199.00 | OK | | 
r-release-windows-x86_64 | 2.0.4 | 54.00 | 198.00 | 252.00 | OK | | 
r-oldrel-macos-arm64 | 2.0.4 | 		120.00 | NOTE | | 
r-oldrel-macos-x86_64 | 2.0.4 | 		235.00 | NOTE | | 
r-oldrel-windows-x86_64 | 2.0.4 | 73.00 | 280.00 | 353.00 | NOTE | 

(NOTEs indicate only that the installed size is 6.2 Mb due to the embedded strategy calling models. Although this is larger than the 5 Mb package size limit preferred by CRAN, having the models embedded in the package simplifies installation.)

We can also confirm that the package runs on Linux ARM64 (we test on a Raspberry Pi 5 running Ubuntu 24.04.2 LTS).
 
## R dependencies
Imports: 	crayon, graphics, grDevices, Hmisc, KernSmooth, methods, openxlsx, parallel, pbapply, randomForest, readxl, rjson, scales, stats, stringi, terra, tools, utils, zip

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
