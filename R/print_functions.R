print_metrics = function(metrics, called = FALSE){
	is.metrics = FALSE
	is.metrics.list = FALSE
	if(is(metrics, "rtrack_metrics")){
		n.tracks = 1
		n.tracks.string = "a single track"
		is.metrics = TRUE
	}else if(is(metrics, "list") & is(metrics[[1]], "rtrack_metrics")){
		n.tracks = length(metrics)
		if(n.tracks == 1){
			n.tracks.string = "a single track"
		}else{
			n.tracks.string = paste(n.tracks, "tracks")
		}
		is.metrics.list = TRUE
	}else{
		if(called){
			return(0)
		}else{
			stop("This is not a valid rtrack_metrics object. Was it created using the function 'calculate_metrics'?")
		}
	}
	if(is.metrics | is.metrics.list){
		if(is.metrics & n.tracks == 1){
			type = metrics$arena$type
		}else{
			type = unique(sapply(metrics, function(m) m$arena$type ))
			if(length(type) > 1) type = "multiple"
		}
		type.string = ""
		if(type == "mwm"){
			type.string = "a Morris water maze experiment"
		}else if(type == "barnes"){
			type.string = "a Barnes maze experiment"
		}else if(type == "oft"){
			type.string = "an open field test"
		}else if(type == "nor"){
			type.string = "a novel object recognition experiment"
		}else if(type == "apa"){
			type.string = "an active place avoidance experiment"
		}else if(type == "multiple"){
			type.string = "multiple different experiment types"
		}else{
			type.string = "an unknown experiment type"
		}
		if(called){
			return(paste0("containing metrics data for ", n.tracks.string, " from ", type.string))
		}else{
			cat(paste0("  An 'rtrack_metrics' object containing metrics data for ", n.tracks.string, " from ", type.string, ".\n"))
		}
	}
}

print_experiment = function(experiment){
	if(is(experiment, "rtrack_experiment")){
		metrics.string = print_metrics(experiment$metrics, called = TRUE)
		if(metrics.string != 0){
			cat(paste0("  An 'rtrack_experiment' object ", metrics.string, "."), "\n")
		}else{
			stop("This is not a valid rtrack_experiment object. Was it created using the function 'read_experiment'?")
		}
	}else{
			stop("This is not a valid rtrack_experiment object. Was it created using the function 'read_experiment'?")
		}
}

print_path = function(path){
	if(is(path, "rtrack_path")){
		cat(paste0("  An 'rtrack_path' with ",  length(path$t), " coordinate points."), "\n")
	}
}

