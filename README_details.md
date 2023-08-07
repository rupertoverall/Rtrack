# 1. System requirements
## Operating system
This package runs on any operating system with R (≥ version 2.10) installed. The package has been tested on the following systems:
(Last updated on 2020-02-04 14:47:51 CET).

|Flavor|Version|Tinstall|Tcheck|Ttotal|Status|Flags|
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
|r-devel-linux-x86_64-debian-clang |0.9.6 |21.30 |93.51 |114.81 |OK |
|r-devel-linux-x86_64-debian-gcc |0.9.6 |21.08 |72.16 |93.24 |OK |
|r-devel-linux-x86_64-fedora-clang |0.9.6 |||142.89 |OK |
|r-devel-linux-x86_64-fedora-gcc |0.9.6 |||143.48 |OK |
|r-devel-windows-ix86+x86_64 |0.9.6 |46.00 |136.00 |182.00 |OK |
|r-devel-windows-ix86+x86_64-gcc8 |0.9.6 |46.00 |134.00 |180.00 |OK |
|r-patched-linux-x86_64 |0.9.6 |21.80 |81.59 |103.39 |OK |
|r-patched-solaris-x86 |0.9.6 |||205.40 |OK |
|r-release-linux-x86_64 |0.9.6 |20.73 |82.22 |102.95 |OK |
|r-release-windows-ix86+x86_64 |0.9.6 |50.00 |130.00 |180.00 |OK |
|r-release-osx-x86_64 |0.9.6 ||||OK |
|r-oldrel-windows-ix86+x86_64 |0.9.6 |21.00 |86.00 |107.00 |OK |
|r-oldrel-osx-x86_64 |0.9.6 ||||OK |


## R dependencies
Imports: 	crayon, graphics, grDevices, Hmisc, KernSmooth, methods, openxlsx, parallel, pbapply, randomForest, RCurl, readxl, rjson, scales, stats, stringi, terra, tools, utils, zip
Suggests: 	knitr, rmarkdown

## Hardware requirements
No special hardware is required.

# 2. Installation guide
`install.packages("Rtrack")`
Install time is c. 30 s depending on internet connection speed and the number of dependencies the user already has installed.

# 3. Demo
The core functionality can be demonstrated by the following code which downloads an example dataset, processes track metrics, calls strategies and draws a strategy overview plot.
```
experiment = Rtrack::read_experiment("https://rupertoverall.net/Rtrack/examples/MWM_example.trackxf")
strategies = Rtrack::call_strategy(experiment)
Rtrack::plot_strategies(strategies, experiment, factor = "Strain", exclude.probe = TRUE)
```
This code takes 14 s to complete on an M1 MacBook Pro laptop (using only a single CPU core and including download time).
A detailed walk-through of all functions provided by the package is also available at https://rupertoverall.net/Rtrack/articles/Rtrack_tutorials_page.html

# 4. Instructions for use
The online tutorials https://rupertoverall.net/Rtrack/articles/Rtrack_tutorials_page.html provide step-by-step instructions for running the software and details on how to prepare raw data for analysis.
