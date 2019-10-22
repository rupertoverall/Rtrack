#' Export experiment data to a JSON file.
#'
#' Creates a representation of the experiment data in the JSON format and
#' optionally writes this to file.
#'
#' The exported JSON file contains all the raw data and experiment metadata. The
#' JSON archive contains exactly the same information as if reading from raw
#' data. Calculated metrics are not exported, but can be recreated exactly.
#'
#' A formal description of the JSON format can be found in the schema file at
#' \url{http://rupertoverall.net/Rtrack/Rtrack_schema_v1.json}.
#'
#' @param experiment An \code{rtrack_experiment} object from
#'   \code{\link{read_experiment}}.
#' @param tracks Which tracks should be exported. Default, 'all' exports the
#'   entire experiment object. A subset of tracks can be specified using either
#'   numeric indices or a vector of track IDs following usual R standards.
#' @param file The file to which the JSON data will be written. If \code{NULL}
#'   (the default), nothing will be written.
#'
#' @return This function invisibly returns the JSON data as a character string.
#'
#' @seealso \code{\link{read_experiment}} to import the JSON file back into R.
#'
#' @examples
#' \dontrun{
#' require(Rtrack)
#' experiment.description <- system.file("extdata", "Minimal_experiment.xlsx",
#'   package = "Rtrack")
#' experiment <- read_experiment(experiment.description, format = "excel",
#'   project.dir = system.file("extdata", "", package = "Rtrack"))
#' export_json(experiment, file = "Minimal_experiment.json")
#' imported.experiment <- read_experiment("Minimal_experiment.json",
#'   format = "json")
#' # Experiments are identical except for export timestamp/notes
#' all.equal(experiment, imported.experiment)
#' identical(experiment$metrics, imported.experiment$metrics)
#' }
#'
#' @importFrom rjson toJSON
#'
#' @export
export_json = function(experiment, tracks = "all", file = NULL){
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
	
	schema = "http://rupertoverall.net/Rtrack/Rtrack_schema_v1.json"

	experiment.info = list(
		author.note = experiment$info$author.note,
		processing.note = experiment$info$processing.note,
		export.note = paste0("Experiment exported on ", date(), " by Rtrack (version ", paste0("Rtrack version ", utils::packageVersion("Rtrack")), ") <http://rupertoverall.net/Rtrack>.")
	)
	
	experiment.data = lapply(tracks.subset, function(i){
		arena = experiment$metrics[[i]]$arena
		factor.names = names(experiment$factors)[!grepl("^_", names(experiment$factors))]
		factors = experiment$factors[i, factor.names]
		names(factors) = paste0("factor_", names(factors))
		c(
			list(
				id = experiment$metrics[[i]]$id,
				target = experiment$factors[i, "_TargetID"],
				day = experiment$factors[i, "_Day"],
				trial = experiment$factors[i, "_Trial"],
				arena_name = experiment$factors[i, "_Arena"],
				# Raw data have R-specific 'NA' replaced by empty elements
				raw.t = gsub("NA", "", paste(experiment$metrics[[i]]$path$raw.t, collapse = ",")),
				raw.x = gsub("NA", "", paste(experiment$metrics[[i]]$path$raw.x, collapse = ",")),
				raw.y = gsub("NA", "", paste(experiment$metrics[[i]]$path$raw.y, collapse = ",")),
				t = paste(experiment$metrics[[i]]$path$t, collapse = ","),
				x = paste(experiment$metrics[[i]]$path$x, collapse = ","),
				y = paste(experiment$metrics[[i]]$path$y, collapse = ","),
				arena = as.list(arena$description)
			),
			factors
		)
	})

	json = rjson::toJSON(list(schema = schema, info = experiment.info, data = experiment.data), indent = 1)
	json = gsub('\\"NA\\"', '\\"\\"', json) # Missing data should just be an empty string
	json = gsub(': *', ':', json) # For some reason, there is indentation applied to sublists. I can't be bothered working out if this can be suppressed.
	if(!is.null(file)) write(json, file = file)
	invisible(json)
}
