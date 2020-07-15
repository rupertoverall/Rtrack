#' Read path coordinates from raw data formats.
#'
#' The user will normally not need to call this directly. Use
#' \code{\link{read_experiment}} instead.
#'
#' Raw data from several sources can be read in directly. The formats currently supported
#' are 'ethovision.xt.excel' (for swim paths exported from the latest Ethovision
#' software), 'ethovision.3.csv' (for data exported from the older Ethovision version 3)
#' and 'raw.csv'. The 'raw.csv' format is a simple comma-delimited text file containing
#' three columns 'Time', 'X' and 'Y'. The timestamp values in 'Time' should be in seconds
#' from the start of the trial recording and coordinates should be in real-world units
#' (e.g. cm, in).
#'
#' If \code{interpolate} is set to \code{TRUE}, then the raw data will be cleaned to
#' remove outlier points and ensure that time points are evenly spaced. The following
#' procedures are used: 1. any points with missing coordinate data are removed; 2. any
#' points lying outside the arena are removed: 3. any points with excessive inter-point
#' distances (outliers) are removed by first removing points that are more than 1 SD from
#' the mean distance, then recalculating the mean and SD and repeating this step - this is
#' typically sufficient to remove noise from video tracking (such as reflections from a
#' water maze pool); 4. new time intervals are calculated from the first non-missing data
#' point to the last using the sampling rate of the raw data; 5. interpolation of x and y
#' values is performed at the new time points using the 'constant' interpolation method
#' from \code{\link[stats]{approx}}.
#'
#' The raw path recordings can be truncated if necessary by specifying the
#' \code{time.bounds} parameter. This is a vector of length 2 containing the earliest and
#' latest time points that should be retained for analysis (any points outside these
#' bounds will be discarded). A value of \code{NA} indicates that the path should not be
#' truncated at that end (default is \code{c(NA, NA)} meaning that the path will extend to
#' the start and end of the recorded values). The units used must match the time units in
#' the track files. This option should not normally need to be set, but may be useful if
#' data acquisition begins before, or ends after, the actual experimental trial (e.g. if
#' recording was running during entry and exit from the arena).
#'
#' @param filename A raw data file containing path coordinates. See details for supported
#'   formats.
#' @param arena The \code{arena} object associated with this track. This is required to
#'   calibrate the track coordinates to the coordinate space of the arena.
#' @param id An optional name for the experiment. Default is to generate this from the
#'   filename provided.
#' @param track.format The format of the raw file.
#' @param track.index Only for formats where multiple tracks are stored in one file
#'   (ignored otherwise). This parameter indicates which section of the file corresponds
#'   to the track to be read. The exact usage depends on the format being read.
#' @param interpolate Should missing data points be interpolated. Default is \code{FALSE}.
#'   Interpolation is performed at evenly-spaced intervals after removing outliers.
#' @param time.bounds A vector of length 2 specifying the bounds on the measurement times
#'   (see Details).
#'
#' @return An \code{rtrack_path} object containing the extracted swim path. This is a list
#'   comprised of the components \code{raw.t} (timestamp), \code{raw.x} (x coordinates),
#'   \code{raw.y} (y coordinates),\code{t}, \code{x} and \code{y} (normalised, cleaned and
#'   possibly interpolated coordinates).
#'
#' @seealso \code{\link{read_arena}}, \code{\link{identify_track_format}} to identify the
#'   format code for your raw data, and also \code{\link{read_experiment}} for processing
#'   many tracks at once.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.csv", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena_SW.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "ethovision.3.csv")
#'
#' @importFrom readxl read_excel
#' @importFrom methods as
#' @importFrom utils capture.output
#' @importFrom stats median approx
#' @importFrom Hmisc approxExtrap
#'
#' @export
read_path = function(filename, arena, id = NULL, track.format = "none", track.index = NULL, interpolate = FALSE, time.bounds = c(NA, NA)) {
	format.info = strsplit(track.format, "_")[[1]]
	track.format = tolower(format.info[1])
	track.encoding = ifelse(!is.na(format.info[2]), format.info[2], "UTF-8")
	path = list(t = NA, x = NA, y = NA, missing = NA, interpolated = NA)
	if(file.exists(filename)){
		coordinate.data = NULL
		path = list(
			raw.t = NULL,
			raw.x = NULL,
			raw.y = NULL,
			t = NULL,
			x = NULL,
			y = NULL
		)
		if(is.null(id)){
			path$id = tools::file_path_sans_ext(basename(filename))
		}else{
			path$id = id
		}
		if(track.format == "ethovision.xt.excel"){
			# The output capture is an ugly response to a bug in readxl that prints carriage returns to the console (which leads to ugly flashing of the progress bar).
			raw = NULL
			. = capture.output({
				raw = suppressMessages(as.data.frame(readxl::read_excel(filename, col_names = FALSE), stringsAsFactors = FALSE))
			})
			header.lines = as.numeric(raw[grep("header", raw[, 1], ignore.case = TRUE), 2])
			header = as.data.frame(raw[1:header.lines, 1:2])
			coordinate.data = suppressWarnings(as.data.frame(apply(raw[(header.lines + 1):nrow(raw), ], 2, methods::as, "numeric")))
			# Ethovision allows export with units on a separate line as an option.
			# The following check is to work out which row contains the correct header information
			if(length(grep('X\\ center', raw[header.lines, ])) > 0){
				colnames.line = header.lines
			}else if(length(grep('X\\ center', raw[header.lines - 1, ])) > 0){
				colnames.line = header.lines -1
			}else{
				stop("The track file appears to be malformed. Are you sure this has been exported from Ethovision XT correctly?")
			}
			colnames(coordinate.data) = raw[colnames.line, ]
			# The minimal information required is the recording timestamp and X, Y coodinates.
			# Without these, we cannot proceed
			t.col = grep("Recording time", colnames(coordinate.data))
			x.col = grep("X center", colnames(coordinate.data))
			y.col = grep("Y center", colnames(coordinate.data))
			if(length(c(t.col, x.col, y.col)) < 3){
				stop("The track file appears to be malformed. The variables 'Recording time', 'X center' and 'Y center' must be exported.")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,t.col]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,x.col]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,y.col]))
		}else if(track.format == "ethovision.xt.csv"){
			header = as.matrix(suppressMessages(utils::read.table(filename, nrows = 100, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = track.encoding)))
			header.lines = as.numeric(gsub("[^0-9]", "", strsplit(header[1], ",")[[1]][2]))
			raw = suppressMessages(utils::read.csv(filename, header = FALSE, stringsAsFactors = FALSE, skip = header.lines, fileEncoding = track.encoding))
			coordinate.data = suppressWarnings(as.data.frame(apply(raw[(header.lines + 1):nrow(raw), ], 2, methods::as, "numeric")))
			# Ethovision allows export with units on a separate line as an option.
			# The following check is to work out which row contains the correct header information
			if(length(grep('X\\ center', header[header.lines])) > 0){
				colnames.line = header.lines
			}else if(length(grep('X\\ center', header[header.lines - 1])) > 0){
				colnames.line = header.lines - 1
			}else{
				stop("The track file appears to be malformed. Are you sure this has been exported from Ethovision XT correctly?")
			}
			colnames(coordinate.data) = strsplit(gsub('\\"', "", header[colnames.line]), ",")[[1]]
			# The minimal information required is the recording timestamp and X, Y coodinates.
			# Without these, we cannot proceed
			t.col = grep("Recording time", colnames(coordinate.data))
			x.col = grep("X center", colnames(coordinate.data))
			y.col = grep("Y center", colnames(coordinate.data))
			if(length(c(t.col, x.col, y.col)) < 3){
				stop("The track file appears to be malformed. The variables 'Recording time', 'X center' and 'Y center' must be exported.")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,t.col]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, x.col]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, y.col]))
		}else if(track.format == "ethovision.xt.csv2"){
			header = as.matrix(suppressMessages(utils::read.table(filename, nrows = 100, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = track.encoding)))
			header.lines = as.numeric(gsub("[^0-9]", "", strsplit(header[1], ";")[[1]][2]))
			raw = suppressMessages(utils::read.csv2(filename, header = FALSE, stringsAsFactors = FALSE, skip = header.lines, fileEncoding = track.encoding))
			coordinate.data = suppressWarnings(as.data.frame(apply(raw[(header.lines + 1):nrow(raw), ], 2, methods::as, "numeric")))
			# Ethovision allows export with units on a separate line as an option.
			# The following check is to work out which row contains the correct header information
			if(length(grep('X\\ center', header[header.lines])) > 0){
				colnames.line = header.lines
			}else if(length(grep('X\\ center', header[header.lines - 1])) > 0){
				colnames.line = header.lines - 1
			}else{
				stop("The track file appears to be malformed. Are you sure this has been exported from Ethovision XT correctly?")
			}
			colnames(coordinate.data) = strsplit(gsub('\\"', "", header[colnames.line]), ";")[[1]]
			# The minimal information required is the recording timestamp and X, Y coodinates.
			# Without these, we cannot proceed
			t.col = grep("Recording time", colnames(coordinate.data))
			x.col = grep("X center", colnames(coordinate.data))
			y.col = grep("Y center", colnames(coordinate.data))
			if(length(c(t.col, x.col, y.col)) < 3){
				stop("The track file appears to be malformed. The variables 'Recording time', 'X center' and 'Y center' must be exported.")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,t.col]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, x.col]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, y.col]))
		}else if(track.format == "ethovision.3.csv"){
			raw = utils::read.delim(filename, header = F, stringsAsFactors = FALSE, fill = T, fileEncoding = track.encoding)
			header.lines = grep("Sample no\\.", raw[, 1], ignore.case = TRUE) - 1
			coordinate.data = utils::read.csv(filename, header = T, stringsAsFactors = FALSE, skip = header.lines)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "ethovision.3.csv2"){
			raw = utils::read.delim(filename, header = F, stringsAsFactors = FALSE, fill = T, fileEncoding = track.encoding)
			header.lines = grep("Sample no\\.", raw[, 1], ignore.case = TRUE) - 1
			coordinate.data = utils::read.csv2(filename, header = T, stringsAsFactors = FALSE, skip = header.lines)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "actimetrics.watermaze.csv"){
			if(is.null(track.index)){
				stop("The parameter 'track.index' must be set in order to read files of format 'actimetrics.watermaze'.")
			}else{
				raw = utils::read.csv(filename, header = T, skip = 1, stringsAsFactors = FALSE, fileEncoding = track.encoding)
				if(all(((track.index * 3) + (-2:0)) %in% 1:ncol(raw))){
				coordinate.data = as.matrix(raw[, (track.index * 3) + (-2:0)])
				path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,3]))
				path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,1]))
				path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,2]))
				}else{
					stop("The 'track.index' refers to columns not present in the file '", filename, "'.")
				}
			}
		}else if(track.format == "dsnt.wmdat"){
			raw = utils::read.delim(filename, header = F, stringsAsFactors = FALSE, fileEncoding = track.encoding)
			coordinate.data = matrix(raw[, 1], ncol = 3, byrow = T)
			path$raw.t = suppressWarnings(as.numeric(strptime(coordinate.data[ ,3], "%m-%d-%Y %H:%M:%S"))) - suppressWarnings(as.numeric(strptime(coordinate.data[1 ,3], "%m-%d-%Y %H:%M:%S")))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,1]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,2]))
		}else if(track.format == "topscan.txt"){
			raw = utils::read.table(filename, header = F, stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
			# Check the frame rate and catch missing or not found values
			frame.rate = c(as.numeric(strsplit(paste0(raw[which(grepl("Frame", raw[, 1], ignore.case = TRUE) & grepl("Rate", raw[, 2], ignore.case = TRUE)), ], collapse = ""), "\\:")[[1]][2]), 0)[1]   
			frame.rate = ifelse(frame.rate > 1, frame.rate, 1) # In fps or default to units if no other information available
			header.col = grep("CenterX", raw, ignore.case = TRUE)
			header.lines = grep("CenterX", raw[, header.col], ignore.case = TRUE) + 1
			coordinate.data = raw[header.lines:nrow(raw), ]
			headers = gsub("Format\\:", "", raw[header.lines - 1, ])# Not clear that there will always be a space after the 'Format:' tag
			headers = headers[!headers == ""]
			coordinate.data = coordinate.data[, 1:length(headers)]
			colnames(coordinate.data) = headers
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$FrameNum) - min(as.numeric(coordinate.data$FrameNum))) / frame.rate
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ , grep("CenterX", headers)]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ , grep("CenterY", headers)]))
		}else if(track.format == "anymaze.csv"){
			coordinate.data = utils::read.delim(filename, header = F, skip = 1, sep = ",", stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
			path$raw.t = suppressWarnings(as.numeric(strptime(coordinate.data[ ,1], format = "%H:%M:%OS"))) - suppressWarnings(as.numeric(strptime(coordinate.data[1 ,1], format = "%H:%M:%OS")))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,3]))
		}else if(track.format == "anymaze.csv2"){
			coordinate.data = utils::read.delim(filename, header = F, skip = 1, sep = ";", stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,3]))
		}else if(track.format == "anymaze.tab"){
			coordinate.data = utils::read.delim(filename, header = F, skip = 1, sep = "\t", stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,3]))
		# }else if(track.format == "eztrack.csv"){
		# 	coordinate.data = utils::read.delim(filename, header = F, skip = 1, sep = ",", stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
		# 	path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,1]))
		# 	path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,2]))
		# 	path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,3]))
		# }else if(track.format == "timestamp.nh.csv"){
		# 	coordinate.data = utils::read.csv(filename, header = T, stringsAsFactors = FALSE)
		# 	keep = as.character(coordinate.data[, 1]) %in% id
		# 	path$raw.t = suppressWarnings(as.numeric(coordinate.data[keep, 4])) - suppressWarnings(as.numeric(coordinate.data[keep, 4][1]))
		# 	path$raw.x = suppressWarnings(as.numeric(coordinate.data[keep, 2]))
		# 	path$raw.y = suppressWarnings(as.numeric(coordinate.data[keep, 3]))
		}else if(track.format == "raw.csv"){
			coordinate.data = utils::read.csv(filename, header = T, stringsAsFactors = FALSE)
			if(!all(colnames(coordinate.data) %in% c("Time", "X", "Y")) & all(colnames(coordinate.data) %in% c("t", "x", "y"))){
				colnames(coordinate.data)[match(colnames(coordinate.data), c("t", "x", "y"))] = c("Time", "X", "Y")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "raw.csv2"){
			coordinate.data = utils::read.csv2(filename, header = T, stringsAsFactors = FALSE)
			if(!all(colnames(coordinate.data) %in% c("Time", "X", "Y")) & all(colnames(coordinate.data) %in% c("t", "x", "y"))){
				colnames(coordinate.data)[match(colnames(coordinate.data), c("t", "x", "y"))] = c("Time", "X", "Y")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "raw.tab"){
			coordinate.data = utils::read.delim(filename, header = T, stringsAsFactors = FALSE)
			if(!all(colnames(coordinate.data) %in% c("Time", "X", "Y")) & all(colnames(coordinate.data) %in% c("t", "x", "y"))){
				colnames(coordinate.data)[match(colnames(coordinate.data), c("t", "x", "y"))] = c("Time", "X", "Y")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "raw.nh.csv"){
			coordinate.data = utils::read.csv(filename, header = F, stringsAsFactors = FALSE)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[, 1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, 2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, 3]))
		}else if(track.format == "raw.nh.csv2"){
			coordinate.data = utils::read.csv2(filename, header = F, stringsAsFactors = FALSE)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[, 1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, 2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, 3]))
		}else if(track.format == "raw.nh.tab"){
			coordinate.data = utils::read.delim(filename, header = F, stringsAsFactors = FALSE)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[, 1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, 2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, 3]))
		}else{
			stop(paste0("The specified path file format '", track.format, "' is not supported."))
		}
		if(interpolate){
			# 1. Remove missing points
			missing = is.na(path$raw.t) | is.na(path$raw.x) | is.na(path$raw.y)
			if(!all(is.na(time.bounds))){ # The user has specified bounds to the recording
				if(is.na(time.bounds[1]) | time.bounds[1] < 0) time.bounds[1] = 0
				if(is.na(time.bounds[2]) | (time.bounds[2] - time.bounds[1]) > arena$correction$t) time.bounds[2] = arena$correction$t
				missing = missing | (path$raw.t < time.bounds[1] | path$raw.t > time.bounds[2])
			}
			path$t = (path$raw.t / arena$correction$t)[!missing]
			path$x = ((path$raw.x - arena$correction$x) / arena$correction$r)[!missing]
			path$y = ((path$raw.y - arena$correction$y) / arena$correction$r)[!missing]
			if(!all(missing)){ # If track is empty, then interpolation won't help (and will only crash)
				# 2. Remove any points falling outside the arena
				clipped = is.na(sp::over(sp::SpatialPoints(data.frame(x = path$x, y = path$y)), arena$zones$pool))
				if(any(clipped)){
					path$t = path$t[!clipped]
					path$x = path$x[!clipped]
					path$y = path$y[!clipped]
					warning(paste0("For '", path$id, "', some points on the path were outside the arena bounds. These have been removed."))
				}
				# 3. Remove any points with excessive interpoint distances.
				d = sqrt(diff(path$x) ^ 2 + diff(path$y) ^ 2)
				outlier = d > (mean(d) + sd(d)) # Two-pass outlier filter. Remove gross outliers...
				outlier = d > (mean(d[!outlier]) + sd(d[!outlier])) # ... and recalculate.
				path$t = path$t[!outlier]
				path$x = path$x[!outlier]
				path$y = path$y[!outlier]
				# 4. Check the timestamps for any missing intervals in the raw data
				end = path$raw.t[length(path$raw.t)]
				timestep = stats::median(diff(path$raw.t), na.rm = T) 
				new.t = seq(0, end, timestep) / arena$correction$t
				# 5. Replace missing and clipped points by interpolated/extrapolated values
				path$x = Hmisc::approxExtrap(path$t, path$x, xout = new.t, method = "constant", ties = "ordered")$y
				path$y = Hmisc::approxExtrap(path$t, path$y, xout = new.t, method = "constant", ties = "ordered")$y
				path$t = new.t
				# 6. Fix any overzealous extrapolation by bounding to the arena
				clipped = is.na(sp::over(sp::SpatialPoints(data.frame(x = path$x, y = path$y)), arena$zones$pool))
				if(any(clipped)){
					path$x[clipped] = NA
					path$y[clipped] = NA
					path$x = stats::approx(path$t, path$x, xout = new.t, method = "constant", rule = 2, ties = "ordered")$y
					path$y = stats::approx(path$t, path$y, xout = new.t, method = "constant", rule = 2, ties = "ordered")$y
				}
			}
		}else{
			# Clean up by simply removing invalid points
			missing = is.na(path$raw.t) | is.na(path$raw.x) | is.na(path$raw.y)
			if(!all(is.na(time.bounds))){ # The user has specified bounds to the recording
				if(is.na(time.bounds[1]) | time.bounds[1] < 0) time.bounds[1] = 0
				if(is.na(time.bounds[2]) | (time.bounds[2] - time.bounds[1]) > arena$correction$t) time.bounds[2] = arena$correction$t
				missing = missing | (path$raw.t < time.bounds[1] | path$raw.t > time.bounds[2])
			}
			path$t = (path$raw.t / arena$correction$t)[!missing]
			path$x = ((path$raw.x - arena$correction$x) / arena$correction$r)[!missing]
			path$y = ((path$raw.y - arena$correction$y) / arena$correction$r)[!missing]
		}
		# Rounding. This keeps all values consistent with the 4 s.f. output to JSON
		path$t = signif(path$t, 4)
		path$x = signif(path$x, 4)
		path$y = signif(path$y, 4)
	}else{
		warning(paste0("File '", filename, "' does not exist. Skipping this track."), call. = FALSE)
	}
	if(length(path$t) == 0 & length(path$t) == 0 & length(path$t) == 0){
		warning(paste0("No valid path data was extracted from the file '", basename(filename), "'. There may be a problem with the track file or the track may just be empty.")) 
	}
		class(path) = "rtrack_path"
		return(path)
}
