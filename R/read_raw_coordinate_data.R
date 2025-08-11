read_raw_coordinate_data = function(filename, track.format, track.index){
	format.info = strsplit(track.format, "_")[[1]]
	track.format = tolower(format.info[1])
	track.encoding = ifelse(!is.na(format.info[2]), format.info[2], "UTF-8")
	path = list(t = NA, x = NA, y = NA, missing = NA, interpolated = NA)
	if(file.exists(filename)){
		coordinate.data = NULL
		path = list(
			id = NULL,
			raw.t = NULL,
			raw.x = NULL,
			raw.y = NULL,
			t = NULL,
			x = NULL,
			y = NULL
		)
		if(track.format == "ethovision.xt.excel"){
			# The output capture is an ugly response to a bug in readxl that prints carriage returns to the console (which leads to ugly flashing of the progress bar).
			raw = NULL
			. = capture.output({
				raw = suppressMessages(as.data.frame(readxl::read_excel(filename, col_names = FALSE), stringsAsFactors = FALSE))
			})
			header.lines = as.numeric(raw[grep("header", raw[, 1], ignore.case = TRUE), 2])
			header = as.data.frame(raw[1:header.lines, 1:2])
			coordinate.data = raw[(header.lines + 1):nrow(raw), ]
			for(i in 1:ncol(coordinate.data)) coordinate.data[, i] = suppressWarnings(methods::as(coordinate.data[, i], "numeric"))
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
			coordinate.data = suppressMessages(utils::read.csv(filename, header = FALSE, stringsAsFactors = FALSE, skip = header.lines, fileEncoding = track.encoding))
			for(i in 1:ncol(coordinate.data)) coordinate.data[, i] = suppressWarnings(methods::as(coordinate.data[, i], "numeric"))
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
			coordinate.data = suppressMessages(utils::read.csv2(filename, header = FALSE, stringsAsFactors = FALSE, skip = header.lines, fileEncoding = track.encoding))
			for(i in 1:ncol(coordinate.data)) coordinate.data[, i] = suppressWarnings(methods::as(coordinate.data[, i], "numeric"))
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
			raw = utils::read.delim(filename, header = FALSE, stringsAsFactors = FALSE, fill = T, fileEncoding = track.encoding)
			header.lines = grep("Sample no\\.", raw[, 1], ignore.case = TRUE) - 1
			coordinate.data = utils::read.csv(filename, header = TRUE, stringsAsFactors = FALSE, skip = header.lines)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "ethovision.3.csv2"){
			raw = utils::read.delim(filename, header = FALSE, stringsAsFactors = FALSE, fill = T, fileEncoding = track.encoding)
			header.lines = grep("Sample no\\.", raw[, 1], ignore.case = TRUE) - 1
			coordinate.data = utils::read.csv2(filename, header = TRUE, stringsAsFactors = FALSE, skip = header.lines)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "actimetrics.watermaze.csv"){
			if(is.null(track.index)){
				stop("The parameter 'track.index' must be set in order to read files of format 'actimetrics.watermaze'.")
			}else{
				raw = utils::read.csv(filename, header = TRUE, skip = 1, stringsAsFactors = FALSE, fileEncoding = track.encoding)
				if(all(((track.index[1] * 3) + (-2:0)) %in% 1:ncol(raw))){
				coordinate.data = as.matrix(raw[, (track.index[1] * 3) + (-2:0)])
				path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,3]))
				path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,1]))
				path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,2]))
				}else{
					stop("The 'track.index' refers to columns not present in the file '", filename, "'.")
				}
			}
		}else if(track.format == "dsnt.wmdat"){
			raw = utils::read.delim(filename, header = FALSE, stringsAsFactors = FALSE, fileEncoding = track.encoding)
			coordinate.data = matrix(raw[, 1], ncol = 3, byrow = T)
			path$raw.t = suppressWarnings(as.numeric(strptime(coordinate.data[ ,3], "%m-%d-%Y %H:%M:%S"))) - suppressWarnings(as.numeric(strptime(coordinate.data[1 ,3], "%m-%d-%Y %H:%M:%S")))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,1]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,2]))
		}else if(track.format == "topscan.txt"){
			# Test for column width in this chaotic format.
			raw = readLines(filename)
			ncols = max(sapply(tail(raw), function(s) length(strsplit(s, "\\s")[[1]]) ))
			raw = utils::read.table(filename, header = FALSE, stringsAsFactors = FALSE, col.names = seq_len(ncols), fill = TRUE, fileEncoding = track.encoding)
			# Check the frame rate and catch missing or not found values
			frame.string = paste0(raw[which(grepl("Frame", raw[, 1], ignore.case = TRUE) & grepl("Rate", raw[, 2], ignore.case = TRUE)), ], collapse = "")
			frame.rate = c(as.numeric(gsub("[^0-9.]", "", frame.string)), 0)[1]   
			frame.rate = ifelse(!is.na(frame.rate), frame.rate, 1) # In fps or default to units if no other information available
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
			coordinate.data = utils::read.delim(filename, header = FALSE, skip = 1, sep = ",", stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
			path$raw.t = suppressWarnings(as.numeric(strptime(coordinate.data[ ,1], format = "%H:%M:%OS"))) - suppressWarnings(as.numeric(strptime(coordinate.data[1 ,1], format = "%H:%M:%OS")))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,3]))
		}else if(track.format == "anymaze.csv2"){
			coordinate.data = utils::read.delim(filename, header = FALSE, skip = 1, sep = ";", stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,3]))
		}else if(track.format == "anymaze.tab"){
			coordinate.data = utils::read.delim(filename, header = FALSE, skip = 1, sep = "\t", stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,3]))
		# }else if(track.format == "eztrack.csv"){
		# 	coordinate.data = utils::read.delim(filename, header = FALSE, skip = 1, sep = ",", stringsAsFactors = FALSE, fill = TRUE, fileEncoding = track.encoding)
		# 	path$raw.t = suppressWarnings(as.numeric(coordinate.data[ ,1]))
		# 	path$raw.x = suppressWarnings(as.numeric(coordinate.data[ ,2]))
		# 	path$raw.y = suppressWarnings(as.numeric(coordinate.data[ ,3]))
		# }else if(track.format == "timestamp.nh.csv"){
		# 	coordinate.data = utils::read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
		# 	keep = as.character(coordinate.data[, 1]) %in% id
		# 	path$raw.t = suppressWarnings(as.numeric(coordinate.data[keep, 4])) - suppressWarnings(as.numeric(coordinate.data[keep, 4][1]))
		# 	path$raw.x = suppressWarnings(as.numeric(coordinate.data[keep, 2]))
		# 	path$raw.y = suppressWarnings(as.numeric(coordinate.data[keep, 3]))
		}else if(track.format == "raw.csv"){
			coordinate.data = utils::read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
			if(!all(colnames(coordinate.data) %in% c("Time", "X", "Y")) & all(colnames(coordinate.data) %in% c("t", "x", "y"))){
				colnames(coordinate.data)[match(colnames(coordinate.data), c("t", "x", "y"))] = c("Time", "X", "Y")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "raw.csv2"){
			coordinate.data = utils::read.csv2(filename, header = TRUE, stringsAsFactors = FALSE)
			if(!all(colnames(coordinate.data) %in% c("Time", "X", "Y")) & all(colnames(coordinate.data) %in% c("t", "x", "y"))){
				colnames(coordinate.data)[match(colnames(coordinate.data), c("t", "x", "y"))] = c("Time", "X", "Y")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "raw.tab"){
			coordinate.data = utils::read.delim(filename, header = TRUE, stringsAsFactors = FALSE)
			if(!all(colnames(coordinate.data) %in% c("Time", "X", "Y")) & all(colnames(coordinate.data) %in% c("t", "x", "y"))){
				colnames(coordinate.data)[match(colnames(coordinate.data), c("t", "x", "y"))] = c("Time", "X", "Y")
			}
			path$raw.t = suppressWarnings(as.numeric(coordinate.data$Time))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data$X))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data$Y))
		}else if(track.format == "raw.free.csv"){
			coordinate.data = utils::read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
			if(is.numeric(coordinate.data[, track.index[1]])){
				path$raw.t = coordinate.data[, track.index[1]]
			}else if(is.character(coordinate.data[, track.index[1]])){
				# Try to convert timestamps from '00:00.0' format.
				s = strsplit(coordinate.data[1, track.index[1]], "\\:")[[1]]
				if(length(s) == 1){
					path$raw.t = suppressWarnings(as.numeric(coordinate.data[, track.index[1]]))
				}else if(length(s) == 2){
					tm = do.call(rbind, strsplit(coordinate.data[, track.index[1]], ":"))
					path$raw.t = (as.numeric(tm[, 1]) * 60) + as.numeric(tm[, 2])
				}else if(length(s) == 3){
					tm = do.call(rbind, strsplit(coordinate.data[, track.index[1]], ":"))
					path$raw.t = (as.numeric(tm[, 1]) * 3600) + (as.numeric(tm[, 2]) * 60) + as.numeric(tm[, 3])
				}else{
					stop("The timestamp column does not contain correctly-formatted timestamp data.")
				}
			}else{
				stop("The timestamp column does not contain correctly-formatted timestamp data.")
			}
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, track.index[2]]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, track.index[3]]))
		}else if(track.format == "raw.free.csv2"){
			coordinate.data = utils::read.csv2(filename, header = TRUE, stringsAsFactors = FALSE)
			if(is.numeric(coordinate.data[, track.index[1]])){
				path$raw.t = coordinate.data[, track.index[1]]
			}else if(is.character(coordinate.data[, track.index[1]])){
				# Try to convert timestamps from '00:00.0' format (or 00:00,0 in this file format).
				s = strsplit(coordinate.data[1, track.index[1]], "\\:")[[1]]
				if(length(s) == 1){
					path$raw.t = suppressWarnings(as.numeric(sub("\\,", ".", coordinate.data[, track.index[1]])))
				}else if(length(s) == 2){
					tm = do.call(rbind, strsplit(coordinate.data[, track.index[1]], ":"))
					path$raw.t = (as.numeric(tm[, 1]) * 60) + as.numeric(sub("\\,", ".", tm[, 2]))
				}else if(length(s) == 3){
					tm = do.call(rbind, strsplit(coordinate.data[, track.index[1]], ":"))
					path$raw.t = (as.numeric(tm[, 1]) * 3600) + (as.numeric(tm[, 2]) * 60) + as.numeric(sub("\\,", ".", tm[, 3]))
				}else{
					stop("The timestamp column does not contain correctly-formatted timestamp data.")
				}
			}else{
				stop("The timestamp column does not contain correctly-formatted timestamp data.")
			}
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, track.index[2]]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, track.index[3]]))
		}else if(track.format == "raw.free.tab"){
			coordinate.data = utils::read.delim(filename, header = TRUE, stringsAsFactors = FALSE)
			if(is.numeric(coordinate.data[, track.index[1]])){
				path$raw.t = coordinate.data[, track.index[1]]
			}else if(is.character(coordinate.data[, track.index[1]])){
				# Try to convert timestamps from '00:00.0' format.
				s = strsplit(coordinate.data[1, track.index[1]], "\\:")[[1]]
				if(length(s) == 1){
					path$raw.t = suppressWarnings(as.numeric(coordinate.data[, track.index[1]]))
				}else if(length(s) == 2){
					tm = do.call(rbind, strsplit(coordinate.data[, track.index[1]], ":"))
					path$raw.t = (as.numeric(tm[, 1]) * 60) + as.numeric(tm[, 2])
				}else if(length(s) == 3){
					tm = do.call(rbind, strsplit(coordinate.data[, track.index[1]], ":"))
					path$raw.t = (as.numeric(tm[, 1]) * 3600) + (as.numeric(tm[, 2]) * 60) + as.numeric(tm[, 3])
				}else{
					stop("The timestamp column does not contain correctly-formatted timestamp data.")
				}
			}else{
				stop("The timestamp column does not contain correctly-formatted timestamp data.")
			}
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, track.index[2]]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, track.index[3]]))
		}else if(track.format == "raw.nh.csv"){
			coordinate.data = utils::read.csv(filename, header = FALSE, stringsAsFactors = FALSE)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[, 1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, 2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, 3]))
		}else if(track.format == "raw.nh.csv2"){
			coordinate.data = utils::read.csv2(filename, header = FALSE, stringsAsFactors = FALSE)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[, 1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, 2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, 3]))
		}else if(track.format == "raw.nh.tab"){
			coordinate.data = utils::read.delim(filename, header = FALSE, stringsAsFactors = FALSE)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[, 1]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, 2]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, 3]))
		}else if(track.format == "tracker.2.dat"){
			raw.header = readLines(filename, n = 100)
			header.lines = which(grepl("\\%\\%END_HEADER", raw.header))
			coordinate.data = utils::read.delim(filename, header = FALSE, stringsAsFactors = FALSE, skip = header.lines)
			path$raw.t = suppressWarnings(as.numeric(coordinate.data[, 2]))
			path$raw.x = suppressWarnings(as.numeric(coordinate.data[, 3]))
			path$raw.y = suppressWarnings(as.numeric(coordinate.data[, 4]))
		}else{
			stop(paste0("The specified path file format '", track.format, "' is not supported."))
		}
	}else{
		warning(paste0("File '", filename, "' does not exist. Skipping this track."), call. = FALSE)
	}
	if(length(path$raw.t) == 0){
		warning(paste0("No valid path data was extracted from the file '", basename(filename), "'. There may be a problem with the track file or the track may just be empty.")) 
	}
	return(path)
}
