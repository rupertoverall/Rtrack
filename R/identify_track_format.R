#' Check the format of a track file.
#'
#' A helper utility to determine the raw data format.
#'
#' Raw data from several sources can be read in directly. A number of formats
#' are supported, but it might not be clear which format code corresponds to
#' your data. This function can be run on a typical file to try to guess your
#' file format. If the format is not recognised, please visit the help page at
#' \url{http://rupertoverall.net/Rtrack/help.html}
#' where it is also possible to request support for new formats.
#'
#' @param filename A raw data file containing path coordinates.
#'
#' @return The format code as a character string. This code can be used as the
#'   \code{track.format} parameter for \code{\link{read_path}} or in the
#'   \code{_TrackFileFormat} column in the experiment description passed to
#'   \code{\link{read_experiment}}.
#'
#'   If the track format cannot be determined, \code{NA} is returned.
#'
#' @seealso \code{\link{read_path}}, or \code{\link{read_experiment}} to read
#'   the track files.
#'
#' @examples
#' require(Rtrack)
#' track_file = system.file("extdata", "Track_1.csv", package = "Rtrack")
#' identify_track_format(track_file)
#'
#' @importFrom readxl read_excel
#' @importFrom methods as
#'
#' @export
identify_track_format = function(filename) {
	track.format = NA
	encoding = "UTF-8" # Default = UTF-8
	# Don't rely on the file extension - it might be wrong.
	# Test for each format agnostically
	testlines = 100 # The number of lines to be tested (make sure this many lines exist)
	headerlines = 50 # The number of testlines that are allowed to contain non-tabular header information
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
	
		# Try reading the standard tabular types (from least to most common non-separator characters)
		# Allow for up to 'headerlines' header lines (there is no way to predict this beforehand)
		tsv.cols = table(sapply(raw[headerlines:testlines], function(s) length(which(gregexpr("\t", s)[[1]] > 0)) + 1 )) 
		csv2.cols = table(sapply(raw[headerlines:testlines], function(s) length(which(gregexpr(";", s)[[1]] > 0)) + 1 ))
		csv.cols = table(sapply(raw[headerlines:testlines], function(s) length(which(gregexpr(",", s)[[1]] > 0)) + 1 ))
		if(length(tsv.cols) == 1 & as.numeric(names(tsv.cols)) > 2){ # Minimum 3 columns required for t, x, y
			# Has tabular data that is tab-separated (with or without headers)
			if(all(grepl("\t", raw))){
				raw = utils::read.delim(filename, header = T, stringsAsFactors = FALSE, fill = T, fileEncoding = encoding)
				coltest = ncol(raw) >= 3
				numtest = tryCatch(is.numeric(colSums(raw, na.rm = TRUE)), error = function(e) FALSE )
				track.format = "raw.tab"
			}
		}else if(length(csv2.cols) == 1 & as.numeric(names(csv2.cols)) > 2){ 
			# Has tabular data that is semicolon-separated (with or without headers)
			if(grepl("Number\\ of\\ header\\ lines", raw[1]) & any(grepl("Experiment|Trial\\ name|Trial\\ ID|Arena\\ name", raw[1:20,]))){
				track.format = "ethovision.xt.csv2"
			}else if(any(grepl("Track\\ file|Object|Samples|Goal\\ Position", raw[1:20,]))){
				track.format = "ethovision.3.csv2"
			}else{
				raw = utils::read.csv2(filename, header = T, row.names = NULL, stringsAsFactors = FALSE, fileEncoding = encoding)
				numtest.all = all(apply(raw, 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0)))
				numtest.sans = all(apply(raw[-1, ], 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0)))
				if(ncol(raw) == 3 & numtest.all){
					track.format = "raw.csv2"
				}else if(ncol(raw) %% 3 == 0 & numtest.sans){
					track.format = "actimetrics.watermaze.csv2"
				}
			}
		}else if(length(csv.cols) == 1 & as.numeric(names(csv.cols)) > 2){ 
			# Has tabular data that is comma-separated (with or without headers)
			if(grepl("Number\\ of\\ header\\ lines", raw[1]) & any(grepl("Experiment|Trial\\ name|Trial\\ ID|Arena\\ name", raw[1:20]))){
				track.format = "ethovision.xt.csv"
			}else if(any(grepl("Track\\ file|Object|Samples|Goal\\ Position", raw[1:20]))){
				track.format = "ethovision.3.csv"
			}else{
				raw = utils::read.csv(filename, header = T, row.names = NULL, stringsAsFactors = FALSE, fileEncoding = encoding)
				numtest.all = all(apply(raw, 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0)))
				numtest.sans = all(apply(raw[-1, ], 1, function(row) all(grepl("[0-9\\-\\.\\NA\\ ]+", row) | nchar(row) == 0)))
				if(ncol(raw) == 3 & numtest.all){
					track.format = "raw.csv"
				}else if(ncol(raw) %% 3 == 0 & numtest.sans){
					track.format = "actimetrics.watermaze.csv"
				}
			}
		}else{ # Not a csv or tsv
			if(any(grepl(":", raw[1:4]))){
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
		msg = paste0("\u2716", " The format of this track cannot be determined.\n Please visit http://rupertoverall.net/Rtrack/help.html for assistance.")
		message(crayon::red(msg))
	}else{
		msg = paste0("\u2714", " This track seems to be in the format '", track.format, "'.")
		message(crayon::green(msg))
	}
	return(track.format)
}
