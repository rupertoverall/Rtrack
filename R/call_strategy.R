#' Calculation of search strategies.
#'
#' Calculates search strategies from path metrics.
#'
#' This function implements a classifier based on a trained random forest model.
#' If the \code{model} parameter is left at "default", then the default model
#' for the appropriate experiment type will be automatically selected. Please
#' note that search strategies are only available for Morris water maze and
#' Barnes maze at this stage. It is expected that other models will be added in
#' the future and your feedback is welcome.
#'
#' @param metrics An \code{rtrack_metrics} object from
#'   \code{\link{calculate_metrics}}, a list of such objects or an
#'   \code{rtrack_experiment} object.
#' @param model The strategy calling model that should be used. Default models
#'   have been implemented for for Morris water maze and Barnes maze
#'   (\code{mwm_rf_v7} and \code{mb_rf_v1} respectively; both trained on mouse
#'   data).
#'
#' @return An \code{rtrack_strategies} object. The \code{calls} element contains
#'   the called strategy/strategies as well as confidence scores for all
#'   possible strategies.
#'
#' @seealso \code{\link{threshold_strategies}}, \code{\link{plot_strategies}}.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.tab", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "raw.tab")
#' metrics <- calculate_metrics(path, arena)
#' strategies <- call_strategy(metrics)
#' # Inspect the strategy call
#' strategies$calls
#'
#' @importFrom methods is
#'
#' @export
call_strategy = function(metrics, model = "default"){
	metrics.list = NULL
	if(methods::is(metrics, "rtrack_metrics")){
		metrics.list = list(metrics)
	}else if(methods::is(metrics, "rtrack_experiment")){
		metrics.list = metrics$metrics
	}else if(methods::is(metrics, "list") & methods::is(metrics[[1]], "rtrack_metrics")){
		metrics.list = metrics
	}else{
		stop("This function requires an 'rtrack_metrics' object or a list of 'rtrack_metrics' objects. Did you create this with 'calculate_metrics' or 'read_experiment'?")
	}
	
	if(metrics.list[[1]]$arena$description$type == "mwm"){
		if(model == "default") model = "mwm_rf_v7"
		return(call_mwm_strategy(metrics.list, model = model))
	}else	if(metrics.list[[1]]$arena$description$type == "barnes"){
		if(model == "default") model = "mb_rf_v2"
		return(call_barnes_strategy(metrics.list, model = model))
	}else	if(metrics.list[[1]]$arena$description$type == "oft"){
		stop("Strategies are not yet implemented for the open field test. Please contact us if you have a use case.")
		#return(call_oft_strategy(metrics, model))
	}else	if(metrics.list[[1]]$arena$description$type == "nor"){
		stop("Strategies are not yet implemented for the novel object recognition test. Please contact us if you have a use case.")
		#return(call_nor_strategy(metrics, model))
	}else	if(metrics.list[[1]]$arena$description$type == "apa"){
		stop("Strategies are not yet implemented for active place avoidance. Please contact us if you have a use case.")
		#return(call_apa_strategy(metrics, model))
	}
}
