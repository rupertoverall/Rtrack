#' Check the format of a track file.
#'
#' A helper utility to determine the raw data format.
#'
#' Raw data from several sources can be read in directly. A number of formats
#' are supported, but it might not be clear which format code corresponds to
#' your data. This function can be run on a typical file to try to guess your
#' file format. If the format is not recognised, please visit the help page at
#' \url{https://rupertoverall.net/Rtrack/help.html} where it is also possible to
#' request support for new formats.
#'
#' @param filename A raw data file containing path coordinates. If this is NULL
#'   or missing, then a message is given listing all of the possible format
#'   codes.
#'
#' @return The format code as a character string. This code can be used as the
#'   \code{track.format} parameter for \code{\link{read_path}} or in the
#'   \code{_TrackFileFormat} column in the experiment description passed to
#'   \code{\link{read_experiment}}.
#'
#'   If the track format cannot be determined, \code{NA} is returned.
#'
#'   When run without a \code{filename} parameter, a character vector containing
#'   all supported format codes is invisibly returned.
#'
#' @seealso \code{\link{read_path}}, or \code{\link{read_experiment}} to read
#'   the track files.
#'
#' @examples
#' require(Rtrack)
#' track_file = system.file("extdata", "Track_1.tab", package = "Rtrack")
#' identify_track_format(track_file)
#'
#' @importFrom readxl read_excel
#' @importFrom methods as
#'
#' @export
identify_track_format = function(filename = NULL) {
	track.format = NA
	encoding = "UTF-8" # Default = UTF-8
	if(length(filename) == 0){
		supported.formats = c(
			"raw.csv",
			"raw.csv2",
			"raw.tab",
			"raw.free.csv",
			"raw.free.csv2",
			"raw.free.tab",
			"raw.nh.csv",
			"raw.nh.csv2",
			"raw.nh.tab",
			"ethovision.xt.excel",
			"ethovision.xt.csv",
			"ethovision.xt.csv2",
			"ethovision.3.csv",
			"ethovision.3.csv2",
			"actimetrics.watermaze.csv",
			"topscan.txt",
			"anymaze.csv",
			"dsnt.wmdat",
			"tracker.2.dat"
		)
		message(paste(c("Supported formats are:", paste0("   ", supported.formats)), collapse = "\n"))
		invisible(supported.formats)
	}else{
		# Don't rely on the file extension - it might be wrong.
		# Test for each format agnostically.
		testlines = 100 # The number of lines to be tested (make sure this many lines exist).
		#headerlines = 50 # The number of testlines that are allowed to contain non-tabular header information
		raw = tryCatch({suppressMessages(as.data.frame(readxl::read_excel(filename, col_names = FALSE), stringsAsFactors = FALSE))}, error = function(e){FALSE})
		if(length(dim(raw)) > 0){   
			# Looks like a valid Excel document
			header.lines = as.numeric(raw[grep("header", raw[, 1], ignore.case = TRUE), 2])
			if(length(header.lines) > 0){ # Probably in Ethovision format
				header = as.data.frame(raw[1:header.lines, 1:2])
				if(any(grepl("Experiment|Trial\\ name|Trial\\ ID|Arena\\ name", header))){
					track.format = "ethovision.xt.excel"
			}
			}else if(all(unique(as.character(raw[2, ])) %in% c("x", "y", "t")) & ncol(raw) %% 3 == 0){
				track.format = "actimetrics.watermaze.excel"
			}
		}else{ # Not Excel, so text text-based formats
			# Check for UTF-8 encoding
			if(tryCatch({suppressMessages(utils::read.table(filename, nrows = testlines, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")); TRUE}, warning = function(e){FALSE})){
				encoding = "UTF-8"			
			}else if(tryCatch({suppressMessages(utils::read.table(filename, nrows = testlines, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")); TRUE}, warning = function(e){FALSE})){
				encoding = "UTF-8-BOM"			
			}else if(tryCatch({suppressMessages(utils::read.table(filename, nrows = testlines, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-16")); TRUE}, warning = function(e){FALSE})){
				encoding = "UTF-16"			
			}else if(tryCatch({suppressMessages(utils::read.table(filename, nrows = testlines, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-32")); TRUE}, warning = function(e){FALSE})){
				encoding = "UTF-32"			
			}else if(tryCatch({suppressMessages(utils::read.table(filename, nrows = testlines, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1")); TRUE}, warning = function(e){FALSE})){
				encoding = "ISO-8859-1"			
			}else if(tryCatch({suppressMessages(utils::read.table(filename, nrows = testlines, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "ISO-8859-15")); TRUE}, warning = function(e){FALSE})){
				encoding = "ISO-8859-15"			
			}else if(tryCatch({suppressMessages(utils::read.table(filename, nrows = testlines, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "ISO-8859-2")); TRUE}, warning = function(e){FALSE})){
				encoding = "ISO-8859-2"			
			}else{
				warning("This file has an unknown file encoding.")
			}
			suppressWarnings({
				raw = as.matrix(suppressMessages(utils::read.table(filename, nrows = testlines, sep = "\n", fill = TRUE, header = FALSE, stringsAsFactors = FALSE, fileEncoding = encoding)))
			})
		
			# Try reading the standard tabular types.
			testlines = min(nrow(raw), testlines)
			tsv.cols = table(sapply(raw[0:testlines], function(s) length(which(gregexpr("\t", s)[[1]] > 0)) + 1 )) 
			tsv.cols = sort(tsv.cols, decreasing = TRUE)[1]
			csv.cols = table(sapply(raw[0:testlines], function(s) length(which(gregexpr("\\,", s)[[1]] > 0)) + 1 ))
			csv.cols = sort(csv.cols, decreasing = TRUE)[1]
			csv2.cols = table(sapply(raw[0:testlines], function(s) length(which(gregexpr("\\;", s)[[1]] > 0)) + 1 ))
			csv2.cols = sort(csv2.cols, decreasing = TRUE)[1]
			posix.points = table(sapply(raw[0:testlines], function(s) length(which(gregexpr("[0-9]\\:[0-9]", s)[[1]] > 0)) + 1 ))
			posix.points = sort(posix.points, decreasing = TRUE)[1]
			
			if(length(csv.cols) == 1 & as.numeric(names(posix.points)) > 2){ # Anymaze format (with Posix timestamps)
				# Has tabular data that is tab-separated (with or without headers)
				# Is this a headerless table of numeric data?
				raw = utils::read.csv(filename, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, fileEncoding = encoding)
				raw$Time = suppressWarnings(as.numeric(strptime(raw$Time, format = "%H:%M:%OS")))
				if(is.numeric(raw$Time) & is.numeric(raw$Centre.position.X) & is.numeric(raw$Centre.position.Y)){
					track.format = "anymaze.csv"
				}
			}else if(length(tsv.cols) == 1 & as.numeric(names(tsv.cols)) > 2){ # Minimum 3 columns required for t, x, y
				# Has tabular data that is tab-separated (with or without headers)
				# Is this a headerless table of numeric data?
				raw = utils::read.delim(filename, header = FALSE, stringsAsFactors = FALSE, fill = T, fileEncoding = encoding)
				numtest.all = FALSE
				if(ncol(raw) >= 3){ # Catch crazy formats with headers.
					numtest.all = all(apply(raw[, 1:3], 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0 | is.na(row)) ))
				}else{
					numtest.all = FALSE
				}
				if(any(grepl("%%BEGIN_HEADER", raw))  & any(grepl("Tracker version 2", raw))){
					track.format = "tracker.2.dat"
				}else if(ncol(raw) >= 3 & numtest.all){
					track.format = "raw.nh.tab"
				}else if(FALSE){
					# TODO Add an extra test for Topscan here: the format has variants that can trigger this block.
				}else{
					raw = utils::read.delim(filename, header = TRUE, stringsAsFactors = FALSE, fill = T, fileEncoding = encoding)
					numtest.all = all(apply(raw[, 1:3], 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0 | is.na(row)) ))
					if(ncol(raw) == 3 & numtest.all){
						track.format = "raw.tab"
					#}else if(ncol(raw) >= 3 & numtest.all){
					#	track.format = "raw.free.tab"
					#	message("This format has more than 3 columns. Please make sure that the _first_3_ are 'time', 'x', 'y' in that order.")
					}
				}
			}else if(length(csv2.cols) == 1 & as.numeric(names(csv2.cols)) > 2){ 
				# Has tabular data that is semicolon-separated (with or without headers)
				# NOTE that Ethovison XT can use variously "Header Lines:" or "Number of header Lines:", so check for both.
				if(grepl("header\\ lines", raw[1], ignore.case = T) & any(grepl("Experiment|Trial\\ name|Trial\\ ID|Arena\\ name", raw[1:20,]))){
					track.format = "ethovision.xt.csv2"
				}else if(any(grepl("Track\\ file|Object|Samples|Goal\\ Position", raw[1:20,]))){
					track.format = "ethovision.3.csv2"
				}else{
					# Is this a headerless table of numeric data?
					raw = utils::read.csv2(filename, header = FALSE, row.names = NULL, stringsAsFactors = FALSE, fileEncoding = encoding)
					numtest.all = all(apply(raw[, 1:3], 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0 | is.na(row)) ))
					if(numtest.all){
						track.format = "raw.nh.csv"
					}else{
						raw = utils::read.csv2(filename, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, fileEncoding = encoding)
						numtest.all = all(apply(raw, 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0 | is.na(row)) ))
						numtest.sans = all(apply(raw[-1, ], 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0 | is.na(row)) ))
						if(ncol(raw) == 3 & numtest.all){
							track.format = "raw.csv2"
						#}else if(ncol(raw) >= 3 & numtest.all){
						#	track.format = "raw.free.csv2"
						#	message("This format has more than 3 columns. Please make sure that the _first_3_ are 'time', 'x', 'y' in that order.")
						}else if(ncol(raw) %% 3 == 0 & numtest.sans){
							track.format = "actimetrics.watermaze.csv2"
						}
					}
				}
			}else if(length(csv.cols) == 1 & as.numeric(names(csv.cols)) > 2){ 
				# Has tabular data that is comma-separated (with or without headers)
				# NOTE that Ethovison XT can use variously "Header Lines:" or "Number of header Lines:", so check for both.
				if(grepl("header\\ lines", raw[1], ignore.case = T) & any(grepl("Experiment|Trial\\ name|Trial\\ ID|Arena\\ name", raw[1:20]))){
					track.format = "ethovision.xt.csv"
				}else if(any(grepl("Track\\ file|Object|Samples|Goal\\ Position", raw[1:20]))){
					track.format = "ethovision.3.csv"
				}else{
					# Is this a headerless table of numeric data?
					raw = utils::read.csv(filename, header = FALSE, row.names = NULL, stringsAsFactors = FALSE, fileEncoding = encoding)
					numtest.all = all(apply(raw[, 1:3], 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0 | is.na(row)) ))
					if(numtest.all){
						track.format = "raw.nh.csv"
					}else{
						raw = utils::read.csv(filename, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, fileEncoding = encoding)
						numtest.all = all(apply(raw, 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0 | is.na(row)) ))
						numtest.sans = all(apply(raw[-1, ], 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0 | is.na(row)) ))
						if(ncol(raw) == 3 & numtest.all){
							track.format = "raw.csv"
						#}else if(ncol(raw) >= 3 & numtest.all){
						#	track.format = "raw.free.csv"
						#	message("This format has more than 3 columns. Please make sure that the _first_3_ are 'time', 'x', 'y' in that order.")
						}else if(ncol(raw) %% 3 == 0 & numtest.sans){
							track.format = "actimetrics.watermaze.csv"
						}
					}
				}
			}else{ # Not a csv or tsv
				# Check for some known headers
				if(any(grepl("FrameNum", raw) & grepl("CenterX", raw))){
					# Probably Topscan
					track.format = "topscan.txt"
				}else if(any(grepl(":", raw[1:4]))){
					# Probably 'dsnt.wmdat' in which case every 2nd is numeric and every 3rd is a POSIX timestamp (contains':')
					m = matrix(grepl(":", raw[1:(testlines - testlines %% 3)]), nrow = 3)
					if(!any(m[2, ]) & all(m[3, ])){
						track.format = "dsnt.wmdat"
					}
				}
			}
	
		}
	
		# Add encoding info if necessary
		if(!is.na(track.format) & encoding != "UTF-8"){
			track.format = paste(track.format, encoding, sep = "_")
		}
		
		# Give message to user
		if(is.na(track.format)){
			msg = paste0("\u2716", " The format of this track cannot be determined.\n Please visit https://rupertoverall.net/Rtrack/help.html for assistance.")
			message(crayon::red(msg))
		}else{
			msg = paste0("\u2714", " This track seems to be in the format '", track.format, "'.")
			message(crayon::green(msg))
		}
		return(track.format)
	}
}
