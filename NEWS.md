# Rtrack 2.0.4

* Improved skew correction of square arenas (OFT and NOR).
* NOR objects may now be arbitrary shapes.
* Implemented 'raw.free.xxx' track file formats, which enable flexible customisation for reading in data from unsupported sources.
* Improved variable plots with options for parametric and non-parametric data.
* Fixed many edge cases in the variable plots.
* Changed the calculation of 'holes.before.goal' in the Barnes maze to make it more robust.
*  Fixed a tricksy bug that made reading an experiment from a URL sometimes not work.
*  Added a patch to allow legacy Rtrack JSON archives to be read in the new system.
*  Added checks to disallow experiments with mixed arena types.
*  Improved customisation of all plots.
*  Improved aesthetics of path and density plots.

# Rtrack 2.0.3

* Fixes for several Windows-specific bugs. 
* Codebase is now routinely tested on MacOS (Apple Silicon and Intel), Ubuntu Linux and Windows 10.
* Fixed bug where custom series colours in the variable plots could be incorrectly ordered.
* Fixed bug where APA zones at certain angles were incorrectly processed.
* Added ability to rotate MWM and Barnes maze arenas to allow alignment.
* Improved support for Topscan 3.0 track files.

# Rtrack 2.0.2

* Major update including support for multiple experiment types.
* Code rewrite to remove dependency on the discontinued sp package.
* Changed arena definitions to require time units.
* New metrics added and exposed to the user a bit differently to be more compatible across the different experiment types.
* MWM strategy caller updated to incorporate the other changes in this release.
* Strategy caller for the Barnes maze added.
* Added support for the new trackxf archive format.

# Rtrack 1.0.7

* Added support for additional input formats.
* Code update to reflect changing dependencies.
* Fixed bug when plotting strategies with user-defined trial boundaries.
* Fixed export to JSON when no experimental factors are present.

# Rtrack 1.0.1

* Added a function to conveniently subset the experiment object.
* It is now possible to plot x-axis ticks in strategy and variable plots either by Day or by Trial.
* Added the ability to suppress titles and x-axis ticks to the strategy and variable plots to enable further customisation.
* The strategy plot now returns cumulative strategy use data to allow custom plots to be built.

# Rtrack 1.0.0

* Fixed the case where plots failed if only one arena was used.
* Added a missing variable in the summary output.

# Rtrack 0.9.10

* Ability to trim track files during reading added.
* Catches case where plotting strategies in the absence of arena boundaries failed.

# Rtrack 0.9.7

* Path interpolation now implemented.
* Bug preventing reading experiment descriptions from CSV or tab-delimited files fixed. 
* `check_experiment` now aware of all supported track formats.
* `identify_track_format` now returns supported formats when called with `NULL` argument.
* Determining format of plain text files now works properly.
* `read_path` now returns a valid `rtrack_path` object of zero path length for empty tracks.
* `calculate_metrics` checks for zero-length paths and skips them (returning `NULL`) to avoid disrupting batch processing.
* Legend placement in variable plots improved.
* Catches cases where path no initial trajectory can be calculated.
* Web links updated to point to new SSL-secured (https) site.

# Rtrack 0.9.6

* Added listing of supported formats when `identify_track_format()` is called with `NULL` argument. 
* Added support for header-free text files.
* Bug fixed in ensuring no perseverance in absence of old goal.
* Catches cases where path is of zero length (no tracking data was recorded). `read_path()` now fails with a warning in such cases.

# Rtrack 0.9.3

* Version initially released to CRAN.

