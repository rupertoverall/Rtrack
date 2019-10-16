#' Calculation of search strategies.
#'
#' Calculates Morris water maze strategies from path metrics.
#'
#' This function implements a classifier based on a trained random forest model.
#'
#' @param metrics An \code{rtrack_metrics} object from
#'   \code{\link{calculate_metrics}} or a list of such objects.
#' @param version The strategy calling model that should be used. Currently only
#'   the default \code{mouse_rf_6} is implemented.
#'
#' @return An \code{rtrack_strategies} object. The \code{calls} element contains
#'   the called strategy/strategies as well as confidence scores for all
#'   possible strategies.
#'
#' @seealso \code{\link{threshold_strategies}}, \code{\link{plot_strategies}}.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.csv", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena_SW.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "ethovision.3.csv")
#' metrics <- calculate_metrics(path, arena)
#' strategies <- call_strategy(metrics)
#' # Inspect the strategy call
#' strategies$calls
#'
#' @importFrom stats predict sd
#' @importFrom utils data
#' @import randomForest
#'
#' @export
call_strategy = function(metrics, version = "mouse_rf_6") {
	if(!(class(metrics) == "rtrack_metrics") & !(class(metrics) == "list" & class(metrics[[1]]) == "rtrack_metrics")){
		stop("This function requires a 'rtrack_metrics' object or a list of 'rtrack_metrics' objects. Did you create this with 'calculate_metrics' or 'read.mwm.experiment'?")
	}else{
		strategy.names = c(
			"1" = "thigmotaxis",
			"2" = "circling",
			"3" = "random path",
			"4" = "scanning",
			"5" = "chaining",
			"6" = "directed search",
			"7" = "corrected search",
			"8" = "direct path",
			"9" = "perseverance"
		)
		strategy.colours = c(
			"1" = "#703E3E", 
			"2" = "#B77727", 
			"3" = "#FFB010", 
			"4" = "#FFCF08", 
			"5" = "#FFEF00", 
			"6" = "#99CC00", 
			"7" = "#4C9900",
			"8" = "#006600", 
			"9" = "#5EBDDE"
		)
		summary.metrics = NULL
		if(length(metrics[[1]]) == 1){ # Single track
			summary.metrics = as.data.frame(t(metrics[["summary"]]), stringsAsFactors = F)
		}else{ # Multiple tracks (whole experiment)
			summary.metrics = as.data.frame(t(sapply(metrics, "[[", "summary")), stringsAsFactors = F)
		}
		arena.limit = 2 # This is by definition (normalised radius = 1) the longest distance possible in a circular arena
		# Replace missing data (where either goal not reached, or no goal or old goal present) by suitable values
		summary.metrics$latency.to.goal[is.na(summary.metrics$latency.to.goal)] = 1 # Maximum latency = 1
		summary.metrics$sd.d.goal[is.na(summary.metrics$sd.d.goal)] = 0 
		summary.metrics$mean.d.goal[is.na(summary.metrics$mean.d.goal)] = 2 # Diameter of arena
		summary.metrics$centroid.goal.displacement[is.na(summary.metrics$centroid.goal.displacement)] = 2 # Diameter of arena
		summary.metrics$initial.trajectory.error[is.na(summary.metrics$initial.heading.error)] = 2 # Diameter of arena
		summary.metrics$sd.d.old.goal[is.na(summary.metrics$sd.d.old.goal)] = 0
		summary.metrics$mean.d.old.goal[is.na(summary.metrics$mean.d.old.goal)] = 2 # Diameter of arena
		summary.metrics$centroid.old.goal.displacement[is.na(summary.metrics$centroid.old.goal.displacement)] = 2 # Diameter of arena
		summary.metrics$initial.reversal.error[is.na(summary.metrics$initial.reversal.error)] = 2 # Diameter of arena
		loadNamespace("randomForest")
		# Malformed track metrics are called as NA
		scores = matrix(NA, nrow = nrow(summary.metrics), ncol = length(strategy.names), dimnames = list(rownames(summary.metrics), names(strategy.names)))
		use = which(!is.na(rowSums(summary.metrics)))
		scores[use, ] = stats::predict(rtrack_model, summary.metrics[use, ], type = "prob")
		scores[summary.metrics$mean.d.old.goal == arena.limit * 2, 8] = 0 # Cannot be perseverence if no old goal
		predicted.calls = apply(scores, 1, function(row) order(row, decreasing = T)[1] )
		# Note that a static path (with NA metrics) is called as thigmotaxis with confidence of 0
		matrix.unmap = function (row, col, dim.m){ outofbounds = col < 1 | col > dim.m[2] | row < 1 | row > dim.m[1]; n = ((col - 1) * dim.m[1]) + row; n[outofbounds] = NA; return(n) }
		confidence = scores[matrix.unmap(1:nrow(scores), predicted.calls, dim(scores))]
		confidence[is.na(confidence)] = 0
		calls = as.data.frame(cbind("strategy" = predicted.calls, "name" = strategy.names[predicted.calls], "confidence" = confidence), stringsAsFactors = FALSE)
		calls$confidence = as.numeric(calls$confidence)
		colnames(scores) = strategy.names[colnames(scores)]
		calls = cbind(calls, as.data.frame(scores[use, , drop = FALSE], stringsAsFactors = FALSE))
		strategies = list(
			method = "rtrack",
			version = version,
			parameters = NULL,
			strategy.names = strategy.names,
			strategy.colours = strategy.colours,
			plot.order = c(8:1, 9),
			calls = calls,
			thresholded = FALSE
		)
		class(strategies) = "rtrack_strategies"
		return(strategies)
	}
}
