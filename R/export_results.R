#' Export experiment results to a data.frame or file.
#'
#' Binds experiment data together with analysis results and optionally writes
#' this to file.
#'
#' For convenience, the strategy calls are performed within this function so the
#' user does not necessarily have to have run them previously. A non-thresholded
#' set of strategies will be exported. If only a thresholded subset should be
#' exported, then this can be achieved by performing thresholding separately and
#' passing the rownames to this function as the parameter \code{tracks}.
#'
#' If \code{file} is supplied, the file extension will be used to determine
#' which format to save the file in. The formats ".csv", ".csv2" (see
#' \code{\link[utils]{write.table}} for details of the formats), ".tsv" (
#' tab-delimited text; can also  be written as ".txt" or ".tab") and ".xlsx"
#' (default) are currently supported. If the file extension is not in this list,
#' the data will be written as tab-delimited text with a warning. Note that the
#' Excel '.xlsx' format is supported, but the older '.xls' is not.
#'
#' @param experiment An \code{rtrack_experiment} object from
#'   \code{\link{read_experiment}}.
#' @param tracks Which tracks should be exported. Default, 'all', exports the
#'   entire experiment object. A subset of tracks can be specified using either
#'   numeric indices or a vector of track IDs following usual R standards.
#' @param file The file to which the results will be written. If \code{NULL}
#'   (the default), the data will be returned as a
#'   \code{\link[base]{data.frame}}.
#'
#' @return A \code{data.frame} containing the experimental groups and factors
#'   (as supplied in the original experiment description) together with the
#'   summary metrics and strategy calls). This is returned invisibly if
#'   \code{file} is specified.
#'
#' @examples
#' require(Rtrack)
#' experiment.description <- system.file("extdata", "Minimal_experiment.xlsx",
#'   package = "Rtrack")
#' experiment <- read_experiment(experiment.description, format = "excel",
#'   project.dir = system.file("extdata", "", package = "Rtrack"))
#' # The code below returns a data.frame. 
#' # Use the parameter 'file' to write to a file instead.
#' export_results(experiment)
#'
#' @importFrom utils write.csv write.csv2 write.table packageVersion
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths saveWorkbook
#' @importFrom tools file_ext
#'
#' @export
export_results = function(experiment, tracks = "all", file = NULL){
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
	factors = experiment$factors[tracks.subset, ]
	strategies = Rtrack::call_strategy(experiment$metrics[tracks.subset])$calls
	strategy.caller = paste(Rtrack::call_strategy(experiment$metrics[tracks.subset])$method, Rtrack::call_strategy(experiment$metrics[tracks.subset])$version, sep = "_")
	summary.metrics = as.data.frame(do.call("rbind", lapply(experiment$metrics, "[[", "unscaled.summary")), stringsAsFactors = FALSE)[tracks.subset, ]
	export.note = paste0("Results exported on ", date(), " by Rtrack (version ", paste0("Rtrack version ", utils::packageVersion("Rtrack")), ") <http://rupertoverall.net/Rtrack>.")
	result = cbind(Track_ID = rownames(factors), factors, strategies, summary.metrics)
	
	if(!is.null(file)){
		filetype = tools::file_ext(file)
		if(filetype == "xlsx"){
			workbook = openxlsx::createWorkbook(creator = paste0("Rtrack version ", utils::packageVersion("Rtrack")))
			openxlsx::addWorksheet(workbook, "rtrack_results")
			openxlsx::writeData(workbook, sheet = 1, result)
			headerStyle = openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#577CA5", halign = "center", valign = "center", textDecoration = "bold")
			openxlsx::addStyle(workbook, sheet = 1, headerStyle, rows = 1, cols = 1:ncol(result))
			openxlsx::setColWidths(workbook, sheet = 1, cols = 1:ncol(result), widths = "auto")
			openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
		}else if(filetype == "csv"){
			utils::write.csv(result, file = file)
		}else if(filetype == "csv2"){
			utils::write.csv2(result, file = file)
		}else if(filetype %in% c("txt", "text", "tsv", "tab")){
			utils::write.table(result, file = file, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", row.names = FALSE, col.names = TRUE)
		}else{
			warning(paste0("The file format '", filetype, "' is not supported. File will be written as tab-delimited text instead."))
			utils::write.table(result, file = file, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", row.names = FALSE, col.names = TRUE)
		}
		return(invisible(result))
	}else{
		return(result)
	}
}
