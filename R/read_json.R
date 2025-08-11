#' @importFrom utils globalVariables
utils::globalVariables(c("threads", "verbose", "progress"))
read_json = function(filename, threads, verbose, time.units){
	experiment.data = rjson::fromJSON(file = filename, simplify = FALSE)
	schema = experiment.data[[1]]
	experiment.info = experiment.data[[2]]
	experiment.data = experiment.data[[3]]
	if(threads == 1){ # Non-parallel code.
		if(verbose) progress("Processing tracks.")
		pbapply::pboptions(type = "timer", txt.width = 50, style = 3, char = "=")
		pb = pbapply::startpb(min = 0, max = length(experiment.data))
		metrics = lapply(1:length(experiment.data), function(i){
			track = experiment.data[[i]]
			this.path = list(
				raw.t = suppressWarnings(as.numeric(unlist(strsplit(track$raw.t, ",")))),
				raw.x = suppressWarnings(as.numeric(unlist(strsplit(track$raw.x, ",")))),
				raw.y = suppressWarnings(as.numeric(unlist(strsplit(track$raw.y, ",")))),
				t = suppressWarnings(as.numeric(unlist(strsplit(track$t, ",")))),
				x = suppressWarnings(as.numeric(unlist(strsplit(track$x, ",")))),
				y = suppressWarnings(as.numeric(unlist(strsplit(track$y, ",")))),
				id = track$id
			)
			class(this.path) = "rtrack_path"
			arena.description = as.data.frame(track$arena, stringsAsFactors = FALSE)
			rownames(arena.description) = "value"
			arena.description$time.units = time.units
			this.arena = read_arena(filename = track$id, description = arena.description) # 'filename' only used to pass through ID.
			this.metrics = calculate_metrics(this.path, this.arena)
			pbapply::setpb(pb, i)
			return(this.metrics)
		})
		pbapply::closepb(pb)
	}else{
		if(verbose) progress("Initialising cluster.")
		pbapply::pboptions(type = "timer", txt.width = 50, style = 3, char = "=")
		cluster = parallel::makePSOCKcluster(min(threads, nrow(experiment.data)))
		. = parallel::clusterEvalQ(cl = cluster, require("Rtrack"))
		if(verbose) progress(paste0("Processing tracks using ", length(cluster), " threads."))
		metrics = pbapply::pblapply(experiment.data, function(track){
			this.path = list(
				raw.t = suppressWarnings(as.numeric(unlist(strsplit(track$raw.t, ",")))),
				raw.x = suppressWarnings(as.numeric(unlist(strsplit(track$raw.x, ",")))),
				raw.y = suppressWarnings(as.numeric(unlist(strsplit(track$raw.y, ",")))),
				t = suppressWarnings(as.numeric(unlist(strsplit(track$t, ",")))),
				x = suppressWarnings(as.numeric(unlist(strsplit(track$x, ",")))),
				y = suppressWarnings(as.numeric(unlist(strsplit(track$y, ",")))),
				id = track$id
			)
			class(this.path) = "rtrack_path"
			arena.description = as.data.frame(track$arena, stringsAsFactors = FALSE)
			rownames(arena.description) = "value"
			this.arena = read_arena(NULL, description = arena.description)
			this.metrics = calculate_metrics(this.path, this.arena)
			return(this.metrics)
		})
		parallel::stopCluster(cluster)
	}
	names(metrics) = sapply(metrics, "[[", "id")
	
	# A two-step approach. But this is robust against altered ordering of the factors.
	user.factor.names = unique(do.call("c", lapply(experiment.data, function(track) names(track)[grep("^factor_", names(track))] )))
	user.factors = as.data.frame(do.call("cbind", sapply(user.factor.names, function(n) as.character(sapply(experiment.data, "[[", n)) , simplify = FALSE, USE.NAMES = TRUE)), stringsAsFactors = F)
	factors = data.frame(
		"_TargetID" = sapply(experiment.data, "[[", "target"),
		"_Day" = sapply(experiment.data, "[[", "day"),
		"_Trial" = sapply(experiment.data, "[[", "trial"),
		"_Arena" = sapply(experiment.data, "[[", "arena_name"),
		user.factors,
		stringsAsFactors = FALSE, check.names = FALSE)
	colnames(factors) = gsub("^factor_", "", colnames(factors))
	rownames(factors) = sapply(metrics, "[[", "id")
	experiment = list(metrics = metrics, factors = factors, summary.variables = names(metrics[[1]]$summary), info = experiment.info)
	class(experiment) = "rtrack_experiment"
	return(experiment)
}