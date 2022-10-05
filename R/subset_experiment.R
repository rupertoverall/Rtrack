#' Subset an experiment.
#'
#' Creates a new experiment object that is a subset of another.
#'
#' @param experiment An \code{rtrack_experiment} object from
#'   \code{\link{read_experiment}}.
#' @param tracks Which tracks should be retained in the subset. Default, 'all', will return the same experiment object. A subset of tracks can be specified using either
#'   numeric indices or a vector of track IDs following usual R standards.
#'
#' @return An \code{rtrack_experiment} containing a subset of the metrics, factors (and strategies if present in the original object).
#'
#' @examples
#' require(Rtrack)
#' experiment.description <- system.file("extdata", "Minimal_experiment.xlsx",
#'   package = "Rtrack")
#' experiment <- read_experiment(experiment.description, format = "excel",
#'   project.dir = system.file("extdata", "", package = "Rtrack"))
#' # The code below returns an experiment object only containing strain "B6". 
#' # Use the parameter 'file' to write to a file instead.
#' b6.tracks = experiment$factors$Strain == "B6"
#' b6.experiment = subset_experiment(experiment, tracks = b6.tracks)
#'
#' @export
subset_experiment = function(experiment, tracks = "all"){
	if(is(experiment, "rtrack_experiment")){
		stop("This function requires an 'rtrack_experiment' object. Did you create this with 'read_experiment'?")
	}else{
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
		result = NULL
		metrics = experiment$metrics[tracks.subset]
		factors = experiment$factors[tracks.subset, ]
		if(length(experiment$strategies) > 0){
			strategies = experiment$strategies[tracks.subset, ]
			result = list(metrics = metrics, factors = factors, strategies = strategies, summary.variables = experiment$summary.variables, info = experiment$info)
		}else{
			result = list(metrics = metrics, factors = factors, summary.variables = experiment$summary.variables, info = experiment$info)
		}
		class(result) = class(experiment)
		return(result)
	}
}
