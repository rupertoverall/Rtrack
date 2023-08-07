#' Read path coordinates from raw data formats.
#'
#' You will normally not need to call this directly. Use
#' \code{\link{read_experiment}} instead.
#'
#' Raw data from several sources, including many commonly-used tracking
#' platforms, can be read in directly. To get a list of the supported formats,
#' use the command \code{Rtrack::identify_track_format()}. The "raw.tab" format
#' is a simple tab-delimited text file containing three columns "Time", "X"
#' and "Y". The timestamp values in "Time" should be in seconds from the start
#' of the trial recording and coordinates should be in real-world units (e.g.
#' cm, in).
#'
#' If \code{interpolate} is set to \code{TRUE}, then the raw data will be
#' cleaned to remove outlier points and ensure that time points are evenly
#' spaced. See the online documentation for details on the method used for each
#' experiment type.
#'
#' The raw path recordings can be truncated if necessary by specifying the
#' \code{time.bounds} parameter. This is a vector of length 2 containing the
#' earliest and latest time points that should be retained for analysis (any
#' points outside these bounds will be discarded). A value of \code{NA}
#' indicates that the path should not be truncated at that end (default is
#' \code{c(NA, NA)} meaning that the path will extend to the start and end of
#' the recorded values). The units used must match the time units in the track
#' files. This option should not normally need to be set, but may be useful if
#' data acquisition begins before, or ends after, the actual experimental trial
#' (e.g. if recording was running during entry and exit from the arena).
#'
#' @param filename A raw data file containing path coordinates. See details for
#'   supported formats.
#' @param arena The \code{arena} object associated with this track. This is
#'   required to calibrate the track coordinates to the coordinate space of the
#'   arena.
#' @param id An optional name for the experiment. Default is to generate this
#'   from the filename provided.
#' @param track.format The format of the raw file.
#' @param track.index Only for formats where multiple tracks are stored in one
#'   file (ignored otherwise). This parameter indicates which section of the
#'   file corresponds to the track to be read. The exact usage depends on the
#'   format being read.
#' @param interpolate Should missing data points be interpolated. Default is
#'   \code{FALSE}. Interpolation is performed at evenly-spaced intervals after
#'   removing outliers.
#' @param time.bounds A vector of length 2 specifying the bounds on the
#'   measurement times (see Details).
#'
#' @return An \code{rtrack_path} object containing the extracted swim path. This
#'   is a list comprised of the components \code{raw.t} (timestamp),
#'   \code{raw.x} (x coordinates), \code{raw.y} (y coordinates),\code{t},
#'   \code{x} and \code{y} (normalised, cleaned and possibly interpolated
#'   coordinates).
#'
#' @seealso \code{\link{read_arena}}, \code{\link{identify_track_format}} to
#'   identify the format code for your raw data, and also
#'   \code{\link{read_experiment}} for processing many tracks at once.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.tab", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "raw.tab")
#'
#' @importFrom readxl read_excel
#' @importFrom methods as
#' @importFrom utils capture.output
#' @importFrom stats median approx
#' @importFrom Hmisc approxExtrap
#' @importFrom terra unwrap vect relate
#'
#' @export
read_path = function(filename, arena, id = NULL, track.format = "none", track.index = NULL, interpolate = FALSE, time.bounds = c(NA, NA)) {
	# Set path id if missing.
	if(is.null(id)){
		id = tools::file_path_sans_ext(basename(filename))
	}
	path = NULL
	if(file.exists(filename)){
		if(arena$description$type == "mwm"){
			path = read_mwm_path(filename, arena, id, track.format, track.index, interpolate, time.bounds)
		}else if(arena$description$type == "oft" | arena$description$type == "nor"){
			path = read_oft_path(filename, arena, id, track.format, track.index, interpolate, time.bounds)
		}else if(arena$description$type == "barnes"){
			path = read_barnes_path(filename, arena, id, track.format, track.index, interpolate, time.bounds)
		}else if(arena$description$type == "apa"){
			path = read_apa_path(filename, arena, id, track.format, track.index, interpolate, time.bounds)
		}
	}else{
		warning(paste0("File '", filename, "' does not exist. Skipping this track."), call. = FALSE)
	}
	if(length(path$t) < 4 | length(path$x) < 4 | length(path$y) < 4){ # 4 points are minimally required by terra::centroids and will crash the session if not present.
		path$t = numeric()
		path$x = numeric()
		path$y = numeric()
		warning(paste0("No valid path data was extracted from the file '", basename(filename), "'. There may be a problem with the track file or the track may just be empty.")) 
	}
	
	# Rounding. This keeps all values consistent with the 4 s.f. output to JSON.
	path$raw.t = signif(path$raw.t, 4)
	path$raw.x = signif(path$raw.x, 4)
	path$raw.y = signif(path$raw.y, 4)
	path$t = signif(path$t, 4)
	path$x = signif(path$x, 4)
	path$y = signif(path$y, 4)

	class(path) = "rtrack_path"
	return(path)
}
