#' @importFrom stats setNames
#' @importFrom utils globalVariables
utils::globalVariables(c("threads", "verbose", "progress"))
read_trackxf = function(filepath, threads, verbose){
	experiment.data = rjson::fromJSON(file = filepath, simplify = FALSE)
	schema = experiment.data[[1]]
	experiment.info = list(
		author.note = experiment.data[[2]]$recording,
		processing.note = experiment.data[[2]]$processing,
		export.note = experiment.data[[2]]$export
	)
	experiment.data = experiment.data[[3]]
	arenas = lapply(experiment.data$arenas, function(arena){
		parameters = setNames(lapply(arena$parameters, "[[", "value"), sapply(arena$parameters, "[[", "name"))
		valid = sapply(parameters, length) == 1
		arena.description = as.data.frame(parameters[valid], stringsAsFactors = FALSE)
		rownames(arena.description) = "value" # Only for compatibility with native Rtrack experiments.
		read_arena(arena$id, description = arena.description) # The filename is ignored and only used to construct the arena id.
	})
	names(arenas) = sapply(experiment.data$arenas, "[[", "id")
	subjects = as.data.frame(do.call("rbind", lapply(experiment.data$subjects, function(subject){
		factor.names = sapply(subject$factors, "[[", "name")
		factor.values = sapply(subject$factors, "[[", "value")
		setNames(c(subject$id, factor.values), c("SubjectID", factor.names))
	})), stringsAsFactors = FALSE)
	
	progress = function(text){cat("    ", crayon::blue(text), "\n", sep = "")}
	
	if(threads == 1){ # Non-parallel code.
		progress("Processing tracks.")
		pbapply::pboptions(type = "timer", txt.width = 50, style = 3, char = "=")
		pb = pbapply::startpb(min = 0, max = length(experiment.data$tracks))
		metrics = lapply(seq_along(experiment.data$tracks), function(i){
			track = experiment.data$tracks[[i]]
			coordinate.stream.names = sapply(track$coordinates, "[[", "id")
			raw = which(coordinate.stream.names == "rawCoordinates")
			path = which(coordinate.stream.names == "coordinates")
			this.path = list(
				id = track$id,
				raw.t = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[raw]]$t, ","), ",")))),
				raw.x = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[raw]]$x, ","), ",")))),
				raw.y = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[raw]]$y, ","), ",")))),
				t = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[path]]$t, ","), ",")))),
				x = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[path]]$x, ","), ",")))),
				y = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[path]]$y, ","), ","))))
			)
			class(this.path) = "rtrack_path"
			this.arena = arenas[[track$arena]]
			this.metrics = calculate_metrics(this.path, this.arena)
			pbapply::setpb(pb, i)
			return(this.metrics)
		})
		pbapply::closepb(pb)
	}else{
		progress("Initialising cluster.")
		pbapply::pboptions(type = "timer", txt.width = 50, style = 3, char = "=")
		cluster = parallel::makePSOCKcluster(min(threads, length(experiment.data$tracks)))
		parallel::clusterExport(cl = cluster, list("arenas"), envir = environment())
		progress(paste0("Processing tracks using ", length(cluster), " threads."))
		metrics = pbapply::pblapply(experiment.data$tracks, function(track){
			coordinate.stream.names = sapply(track$coordinates, "[[", "id")
			raw = which(coordinate.stream.names == "rawCoordinates")
			path = which(coordinate.stream.names == "coordinates")
			this.path = list(
				id = track$id,
				raw.t = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[raw]]$t, ","), ",")))),
				raw.x = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[raw]]$x, ","), ",")))),
				raw.y = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[raw]]$y, ","), ",")))),
				t = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[path]]$t, ","), ",")))),
				x = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[path]]$x, ","), ",")))),
				y = suppressWarnings(as.numeric(unlist(strsplit(paste0(track$coordinates[[path]]$y, ","), ","))))
			)	
			# Rounding. This keeps all values consistent with the 4 s.f. used in JSON.
			this.path$raw.t = signif(this.path$raw.t, 4)
			this.path$raw.x = signif(this.path$raw.x, 4)
			this.path$raw.y = signif(this.path$raw.y, 4)
			this.path$t = signif(this.path$t, 4)
			this.path$x = signif(this.path$x, 4)
			this.path$y = signif(this.path$y, 4)
			
			class(this.path) = "rtrack_path"
			this.arena = arenas[[track$arena]]
			this.metrics = calculate_metrics(this.path, this.arena)
			return(this.metrics)
		}, cl = cluster)
		parallel::stopCluster(cluster)
	}
	names(metrics) = sapply(metrics, "[[", "id")
	
	# This approach is robust against altered ordering of the factors.
	subject.ids = sapply(experiment.data$tracks, "[[", "subject")
	trackxf.factors = lapply(experiment.data$tracks, "[[", "factors")
	trackxf.factors = as.data.frame(do.call("rbind", lapply(trackxf.factors, function(factor){
		setNames(sapply(factor, "[[", "value"), sapply(factor, "[[", "name"))
	})))
	day =as.numeric(trackxf.factors[, "_Day"]) # This is an Rtrack-specific field that is dumped as a factor in trackxf.
	trackxf.factors[, "_Day"] = NULL # We don't want to import this as a user factor.

	factors = data.frame(
		"_TargetID" = subject.ids,
		"_Day" = day,
		"_Trial" = sapply(experiment.data$tracks, "[[", "session"),
		"_Arena" = sapply(experiment.data$tracks, "[[", "arena"),
		trackxf.factors,
		stringsAsFactors = FALSE, check.names = FALSE)
	factors = factors[1:nrow(factors), ] # Pointless quirk to ensure 'identical()' doesn't fail on attribute reordering.
	rownames(factors) = sapply(metrics, "[[", "id")
	
	experiment = list(metrics = metrics, factors = factors, summary.variables = names(metrics[[1]]$summary), info = experiment.info)
	class(experiment) = "rtrack_experiment"
	return(experiment)
}