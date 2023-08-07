#'Calculation of spatial search path metrics.
#'
#'Calculates a range of metrics from path coordinates.
#'
#'Metrics are calculated based on normalised coordinate data and are made
#'accessible to machine learning algorithms in the \code{features} element of
#'the \code{rtrack_path} object. A relevant selection of metrics (with the same
#'units as the raw data) is also available as the \code{summary} element. These
#'can be useful for custom plots and are also the values exported by
#'\code{\link{export_results}}. Extended metrics are also available as separate
#'elements of the \code{rtrack_metrics} object.
#'
#'@param path An \code{rtrack_path} object as returned by
#'  \code{\link{read_path}}.
#'@param arena An \code{rtrack_arena} object as returned by
#'  \code{\link{read_arena}}.
#'
#'@return An \code{rtrack_metrics} object containing metrics of the search path.
#'  This object is required as input for the \code{\link{call_strategy}} and
#'  \code{\link{plot_path}} functions.
#'
#'@seealso \code{\link{read_path}}, \code{\link{read_arena}}, and also
#'  \code{\link{read_experiment}} for processing many tracks at once.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.tab", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "raw.tab")
#' metrics <- calculate_metrics(path, arena)
#'
#'@importFrom terra vect expanse relate distance wrap unwrap
#'
#'@export
calculate_metrics = function(path, arena){
	metrics = NULL
	if(methods::is(path, "rtrack_path") & methods::is(arena, "rtrack_arena")){
		if(length(path$t) > 0){
			if(arena$description$type == "mwm"){
				metrics = calculate_mwm_metrics(path, arena)
			}else	if(arena$description$type == "barnes"){
				metrics = calculate_barnes_metrics(path, arena)
			}else	if(arena$description$type == "oft"){
				metrics = calculate_oft_metrics(path, arena)
			}else	if(arena$description$type == "nor"){
				metrics = calculate_nor_metrics(path, arena)
			}else	if(arena$description$type == "apa"){
				metrics = calculate_apa_metrics(path, arena)
			}
		}else{
			warning(paste0("The track '", path$id, "' is empty. Metrics cannot be calculated.")) 
		}
	}else{
		warning("The path and/or the arena are not valid Rtrack objects. Metrics cannot be calculated.") 
	}

	return(metrics)
}

