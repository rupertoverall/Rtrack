#' Export experiment data to a trackxf file.
#'
#' Creates a representation of the experiment data in the trackxf format and
#' writes this to file.
#'
#' The exported trackxf file contains all the raw data and experiment metadata.
#' The trackxf archive contains exactly the same information as if reading from
#' raw data. Calculated metrics are not exported, but can be recreated exactly.
#'
#' A formal description of the trackxf JSON format can be found in the schema
#' file at \url{https://rupertoverall.net/trackxf/trackxf_schema_v0.json}.
#'
#' @param experiment An \code{rtrack_experiment} object from
#'   \code{\link{read_experiment}}.
#' @param file The file to which the archive will be written. This should
#'   ideally use the file extension \code{.trackxf} although other extensions
#'   are also possible. If the filename is given without any extension
#'   (recommended), then the \code{.trackxf} extension will be added
#'   automatically.
#' @param tracks Which tracks should be exported. Default, "all" exports the
#'   entire experiment object. A subset of tracks can be specified using either
#'   numeric indices or a vector of track IDs following usual R standards.
#'
#' @seealso \code{\link{read_experiment}} to import the archive back into an
#'   \code{rtrack_experiment} object.
#'
#' @examples
#' require(Rtrack)
#' experiment.description <- system.file("extdata", "Minimal_experiment.xlsx",
#'   package = "Rtrack")
#' experiment <- read_experiment(experiment.description, format = "excel",
#'   project.dir = system.file("extdata", "", package = "Rtrack"))
#' tempfile <- file.path(tempdir(), "Minimal_experiment.trackxf") # Temporary file
#' export_data(experiment, file = tempfile)
#' imported.experiment <- read_experiment(tempfile, format = "trackxf")
#' # Experiments are identical except for export timestamp/notes
#' all.equal(experiment, imported.experiment)
#' identical(experiment$metrics, imported.experiment$metrics)
#'
#' @importFrom rjson toJSON
#' @importFrom tools file_ext
#' @importFrom pbapply pboptions startpb setpb closepb
#' @importFrom zip zip
#'
#' @export
export_data = function(experiment, file, tracks = "all"){
	tracks.subset = NA
	if((length(tracks) == 1) & (tracks[1] == "all")){
		tracks.subset = names(experiment$metrics)
	}else if(is.numeric(tracks)){
		tracks.subset = names(experiment$metrics)[tracks]
	}else if(is.logical(tracks) & (length(tracks) == length(experiment$metrics))){
		tracks.subset = names(experiment$metrics)[tracks]
	}else if(is.character(tracks)){
		intersection = tracks %in% names(experiment$metrics) 
		tracks.subset = tracks[intersection]
		if(sum(intersection) < length(intersection)) warning("Not all of the specified tracks are present in the experiment.")
	}else{
		stop("Invalid track identifiers, nothing will be exported.")
	}
	
	type = unique(sapply(experiment$metrics[tracks.subset], function(track) track$arena$type ))
	if(length(type) != 1) stop("It is only possible to export tracks of the same experiment type. Please split the experiment into sub-experiments of the same type and export these separately.")
	
	if(tools::file_ext(file) != "trackxf") file = paste0(file, ".trackxf")
	tempfile = paste0(file, ".json") # Write initially to uncompressed JSON file.
	
	progress = function(text){cat("    ", crayon::blue(text), "\n", sep = "")}
	progress("Creating trackxf archive.")
	pbapply::pboptions(type = "timer", txt.width = 50, style = 3, char = "=")
	sections = 1 + length(unique(experiment$factors$`_Arena`)) + length(unique(experiment$factors$`_TargetID`)) + length(tracks.subset)
	section.count = 0
	pb = pbapply::startpb(min = 0, max = sections)
	
	cat('{\n', file = tempfile, append = FALSE)
	cat(' "schema":"https://rupertoverall.net/trackxf/trackxf_schema_v0.json",\n', file = tempfile, append = TRUE)

	experiment.info = list(
		type = type,
		title = "",
		version = "",
		authors = "",
		licensing = "",
		recording = experiment$info$author.note,
		processing = experiment$info$processing.note,
		export = list(
			software = "Rtrack",
			version = as.character(utils::packageVersion("Rtrack")),
			date = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%S%z"), # ISO 8601
			url = "https://rupertoverall.net/Rtrack"
		)
	)
	
	cat(' "info":', file = tempfile, append = TRUE)
	cat(gsub('\n', '\n ',rjson::toJSON(experiment.info, indent = 1)), file = tempfile, append = TRUE)
	cat(',\n', file = tempfile, append = TRUE)

	section.count = section.count + 1
	pbapply::setpb(pb, section.count)

	cat(' "data":{\n', file = tempfile, append = TRUE)
	
	arena.ids = unique(experiment$factors$`_Arena`)
	arena.indices = match(arena.ids, experiment$factors$`_Arena`) # Get first example of each arena.
	arenas = list()
	for(i in seq_along(arena.ids)){
		arena = experiment$metrics[[arena.indices[i]]]$arena
		id = ifelse(!is.null(arena$id), arena$id, arena.ids[i])
		parameters = lapply(colnames(arena$description), function(parameter.name){
			list(
				name = as.character(parameter.name),
				value = as.character(arena$description[, parameter.name])
			)
		})
		zones = lapply(names(arena$zones), function(zone){
			coordinates = terra::crds(terra::unwrap(arena$zones[[zone]]))
			list(
				id = zone,
				x = paste(coordinates[, "x"], collapse = ","),
				y = paste(coordinates[, "y"], collapse = ",")
			)
		})
		arenas[[i]] = list(
			id = id,
			crs = "local",
			parameters = parameters,
			zones = zones
		)
		section.count = section.count + 1
		pbapply::setpb(pb, section.count)
	}
	
		
	cat('  "arenas":', file = tempfile, append = TRUE)
	cat(gsub('\n', '\n  ', gsub(': *', ':', rjson::toJSON(arenas, indent = 1))), file = tempfile, append = TRUE)
			
	subject.ids = unique(experiment$factors$`_TargetID`)

	subjects = list()
	for(i in seq_along(subject.ids)){
		subjects[[i]] = list(
			id = subject.ids[i],
			factors = list()
		)
		section.count = section.count + 1
		pbapply::setpb(pb, section.count)
	}

	cat(',\n', file = tempfile, append = TRUE)
	cat('  "subjects":', file = tempfile, append = TRUE)
	cat(gsub('\n', '\n  ', rjson::toJSON(subjects, indent = 1)), file = tempfile, append = TRUE)
	cat(',\n', file = tempfile, append = TRUE)


	factor.indices = colnames(experiment$factors)[!grepl("^_", colnames(experiment$factors))]
	
	# This block is written manually in order to both provide meaningful progress feedback (there could be very many tracks)
	# and to gain control over stream formatting (very long arrays that should stay on the same line).
	cat('  "tracks":[', file = tempfile, append = TRUE)
	for(i in seq_along(tracks.subset)){
		cat(ifelse(i > 1, ",\n", "\n"), file = tempfile, append = TRUE)
		cat('   {\n', file = tempfile, append = TRUE)
		cat(paste0('    "id":"', experiment$metrics[[i]]$id, '",\n'), file = tempfile, append = TRUE)
		cat(paste0('    "subject":"', experiment$factors[i, "_TargetID"], '",\n'), file = tempfile, append = TRUE)
		cat(paste0('    "session":', experiment$factors[i, "_Trial"], ',\n'), file = tempfile, append = TRUE)
		cat(paste0('    "arena":"', experiment$metrics[[i]]$arena$id, '",\n'), file = tempfile, append = TRUE)
		#
		factors = c(
			list(list(
				name = "_Day", # Retain the leading underscore, as this has special meaning for re-import into Rtrack.
				value = as.character(experiment$factors[i, "_Day"])
			)),
			lapply(factor.indices, function(factor){
				list(
					name = factor,
					value = as.character(experiment$factors[i, factor])
				)
			})
		)
		cat('    "factors":', file = tempfile, append = TRUE)
		cat(gsub('\n', '\n    ', rjson::toJSON(factors, indent = 1)), file = tempfile, append = TRUE)
		cat(',\n', file = tempfile, append = TRUE)
		# Raw data have R-specific 'NA' replaced by empty elements.
		experiment$metrics[[i]]$path$raw.t[is.na(experiment$metrics[[i]]$path$raw.t)] = ""
		experiment$metrics[[i]]$path$raw.x[is.na(experiment$metrics[[i]]$path$raw.x)] = ""
		experiment$metrics[[i]]$path$raw.y[is.na(experiment$metrics[[i]]$path$raw.y)] = ""
		cat('    "coordinates":[\n', file = tempfile, append = TRUE)
		#
		cat('     {\n', file = tempfile, append = TRUE)
		cat(paste0('      "id":"coordinates",\n'), file = tempfile, append = TRUE)
		cat(paste0('      "t":"', paste(experiment$metrics[[i]]$path$t, collapse = ","), '",\n'), file = tempfile, append = TRUE)
		cat(paste0('      "x":"', paste(experiment$metrics[[i]]$path$x, collapse = ","), '",\n'), file = tempfile, append = TRUE)
		cat(paste0('      "y":"', paste(experiment$metrics[[i]]$path$y, collapse = ","), '"\n'), file = tempfile, append = TRUE)
		cat('     },\n', file = tempfile, append = TRUE)
		#
		cat('     {\n', file = tempfile, append = TRUE)
		cat(paste0('      "id":"rawCoordinates",\n'), file = tempfile, append = TRUE)
		cat(paste0('      "t":"', paste(experiment$metrics[[i]]$path$raw.t, collapse = ","), '",\n'), file = tempfile, append = TRUE)
		cat(paste0('      "x":"', paste(experiment$metrics[[i]]$path$raw.x, collapse = ","), '",\n'), file = tempfile, append = TRUE)
		cat(paste0('      "y":"', paste(experiment$metrics[[i]]$path$raw.y, collapse = ","), '"\n'), file = tempfile, append = TRUE)
		cat('     }\n', file = tempfile, append = TRUE)
		#
		cat('    ],\n', file = tempfile, append = TRUE) # End streams.
		# Further streams can be added.
		# Events are currently not implemented in Rtrack but will be in future versions.
		events = list()
		cat('    "events":', file = tempfile, append = TRUE)
		cat(gsub('\n', '\n    ', rjson::toJSON(events, indent = 1)), file = tempfile, append = TRUE)
		cat('\n', file = tempfile, append = TRUE)
		#
		cat('   }', file = tempfile, append = TRUE)
		section.count = section.count + 1
		pbapply::setpb(pb, section.count)
	}

	cat('\n  ]\n }\n}\n', file = tempfile, append = TRUE)

	pbapply::closepb(pb)
	success = file.exists(tempfile)
	
	progress("Compressing trackxf archive.")
	unlink(file)
	zip::zip(file, tempfile, mode = "cherry-pick")
	success = file.exists(file)
	if(success){
		unlink(tempfile)
	}else{
		warning("File compression failed, so an uncompressed '.trackxf.txt' file has been created instead.")
	}

	invisible(success)
}

