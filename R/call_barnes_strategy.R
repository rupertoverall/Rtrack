#' @importFrom stats predict sd
#' @importFrom utils data
#' @import randomForest
#'
call_barnes_strategy = function(metrics, model) {
	strategy.names = c(
		"1" = "passive",
		"2" = "chaotic",
		"3" = "random",
		"4" = "serial",
		"5" = "erratic search",
		"6" = "directed search",
		"7" = "corrected path",
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
	features = NULL
	if(length(metrics[[1]]) == 1){ # Single track
		features = as.data.frame(t(metrics[["features"]]), stringsAsFactors = F)
	}else{ # Multiple tracks (whole experiment)
		features = as.data.frame(t(sapply(metrics, "[[", "features")), stringsAsFactors = F)
	}
	arena.limit = 2 # This is by definition (normalised radius = 1) the longest distance possible in a circular arena
	# Replace missing data (where either goal not reached, or no goal or old goal present) by suitable values
	for(i in seq_len(nrow(features))){
		na.features = is.na(features[i, ])
		features[i, grepl("\\.d\\.", names(features)) & na.features] = arena.limit
		features[i, grepl("centroid\\.goal\\.displacement", names(features)) & na.features] = arena.limit
		features[i, grepl("centroid\\.old\\.goal\\.displacement", names(features)) & na.features] = arena.limit
		features[i, grepl("median\\.initial\\.heading\\.error", names(features)) & na.features] = arena.limit
		features[i, grepl("initial\\.trajectory\\.error", names(features)) & na.features] = arena.limit
		features[i, grepl("initial\\.reversal\\.error", names(features)) & na.features] = arena.limit
		features[i, grepl("efficiency", names(features)) & na.features] = 0
		features[i, grepl("holes.before", names(features)) & na.features] = metrics[[i]]$arena$correction$e$n.holes
		features[i, grepl("velocity\\.in", names(features)) & na.features] = 0
		features[i, grepl("immobility\\.in", names(features)) & na.features] = 0
		features[i, grepl("time\\.in", names(features)) & na.features] = 0
		features[i, grepl("latency\\.to", names(features)) & na.features] = 600 # Arbitrarily large value for maximal latency.
		features[i, grepl("\\.zone\\.crossings", names(features)) & na.features] = 0
		features[i, grepl("goal\\.reached", names(features)) & na.features] = 0
	}
	
	loadNamespace("randomForest")
	# Malformed track metrics are called as NA
	scores = matrix(NA, nrow = nrow(features), ncol = length(strategy.names), dimnames = list(rownames(features), names(strategy.names)))
	use = which(!is.na(rowSums(features)))
	calls = as.data.frame(cbind("strategy" = rep(NA, nrow(features)), "name" = rep(NA, nrow(features)), "confidence" = 0, scores), stringsAsFactors = FALSE)
	if(length(use) > 0){
		scores[use, ] = stats::predict(get(eval(model)), features[use, ], type = "prob")
		scores[features$mean.d.old.goal == arena.limit, 9] = 0 # Cannot be perseverence if no old goal
		predicted.calls = apply(scores, 1, function(row) order(row, decreasing = T)[1] )
		matrix.unmap = function (row, col, dim.m){ outofbounds = col < 1 | col > dim.m[2] | row < 1 | row > dim.m[1]; n = ((col - 1) * dim.m[1]) + row; n[outofbounds] = NA; return(n) }
		confidence = scores[matrix.unmap(1:nrow(scores), predicted.calls, dim(scores))]
		confidence[is.na(confidence)] = 0
		calls$strategy[use] = predicted.calls[use]
		calls$name[use] = strategy.names[predicted.calls][use]
		calls$confidence[use] = as.numeric(confidence)[use]
		calls[use, as.character(1:9)] = scores[use, ]
	}
	strategies = list(
		method = "rtrack",
		model = model,
		parameters = NULL,
		strategy.names = strategy.names,
		strategy.colours = strategy.colours,
		plot.order = c(8:1, 9),
		tracks = rownames(features[use, ]),
		calls = calls,
		thresholded = FALSE
	)
	class(strategies) = "rtrack_strategies"
	return(strategies)

}
