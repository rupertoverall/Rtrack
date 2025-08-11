#' Plot a path density map.
#'
#' Plots a density map ("heatmap") of the path.
#'
#' @param metrics An \code{rtrack_metrics} object from
#'   \code{\link{calculate_metrics}}.
#' @param title An optional title for the plot. The default is to use the path
#'   name saved in the \code{rtrack_metrics} object.
#' @param col Colours for the density map. These can be provided as any vector
#'   of colours. The recommended (and default) approach is to use
#'   \code{\link[grDevices]{colorRampPalette}}. The default colouring is a
#'   simple white-to-blue scale.
#' @param legend Should a colour scale legend be drawn? Default is TRUE.
#' @param legend.labels Should the colour scale legend include max/min labels? Default is \code{TRUE}. A value of \code{FALSE} suppresses these labels. Alternatively, a character vector of length 2 can be supplied with user-defined labels.
#' @param cex.legend A numerical value giving the amount by which the legend labels should be magnified relative to the default (default = 1).
#' @param feature.col The colour to plot outlines of arena features (goals,
#'   objects, aversive zone etc., depending on the arena type). Black by
#'   default, but it may be useful to change this if a very dark colour scheme
#'   is used.
#' @param feature.lwd The width of the lines used to plot the feature outlines.
#'   By default this is drawn heavier to make them stand out.
#' @param lwd The thickness of the lines used to draw the arena. Default is 1.
#' @param resolution The resolution of the heatmap in pixels. The default is 600
#'   x 600.
#' @param margins The margins of the plot (see the option \code{mar} in
#'   \code{\link[graphics]{par}}). The defaults should normally not need to be
#'   changed.
#' @param ... Additional arguments to \code{\link[terra]{plot}}.
#'
#' @seealso \code{\link{calculate_metrics}}, \code{\link{plot_path}}.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.tab", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "raw.tab")
#' metrics <- calculate_metrics(path, arena)
#' plot_density(metrics)
#'
#' @importFrom methods is
#' @importFrom graphics par plot lines segments text
#' @importFrom grDevices colorRampPalette
#' @importFrom KernSmooth bkde2D
#' @importFrom terra plot rast mask
#'
#' @export
plot_density = function(metrics, title = NULL, col = grDevices::colorRampPalette(c('#FCFBFD', '#9E9AC8', '#3F007D'))(256), legend = TRUE, legend.labels = TRUE, cex.legend = 1, feature.col = "black", feature.lwd = NA, lwd = 1, resolution = 600, margins = c(0, 0, 3, 0), ...){
 # Accept a 'rtrack_metrics' object or a list of 'rtrack_metrics' objects
	type = ""
	if(methods::is(metrics, 'rtrack_metrics')){
		if(is.null(title)) title = metrics$id
		type = metrics$arena$description$type
		metrics$arena$zones = lapply(metrics$arena$zones, terra::unwrap) # Deserialise arena zones.
	}else if(methods::is(metrics[[1]], 'rtrack_metrics')){
		if(is.null(title)) title = "Multiple paths"
		types = table(sapply(metrics, function(m) m$arena$description$type ))
		if(length(types) == 1){
			type = names(types)
		}else{
			stop("There are multiple arena types in the supplied metrics list. A density plot cannot be created.")
		}
		for(m in metrics){
			m$arena$zones = lapply(m$arena$zones, terra::unwrap) # Deserialise arena zones.
		}
	}else{
		stop("The argument supplied is not an 'rtrack_metrics' object. Did you produce this using 'calculate_metrics' or 'read_experiment'?")
	}
	if(is.na(feature.lwd) | feature.lwd < 1) feature.lwd = max(1, min(par("pin")) / 3)
	
	.parprevious = graphics::par(mar = par("mar"))
	on.exit(par(.parprevious))
	
	valid = TRUE

	if(type == "mwm"){
		plot.path = NULL
		plot.arena = NULL
		plot.goal = NULL
		plot.old.goal = NULL
		if(methods::is(metrics, 'list')){
			plot.arena = terra::unwrap(metrics[[1]]$arena$zones$pool)
			plot.arena.id = metrics[[1]]$arena$id
			plot.goal = terra::unwrap(metrics[[1]]$arena$zones$goal)
			plot.old.goal = terra::unwrap(metrics[[1]]$arena$zones$old.goal)
			same.arena = TRUE
			for(i in seq_along(metrics)){
				this.arena = terra::unwrap(metrics[[i]]$arena$zones$pool)
				this.arena.id = metrics[[i]]$arena$id
				this.goal = terra::unwrap(metrics[[i]]$arena$zones$goal)
				this.old.goal = terra::unwrap(metrics[[i]]$arena$zones$old.goal)
				if(plot.arena.id != this.arena.id){
					same.arena = FALSE
					plot.arena = terra::union(this.arena, plot.arena)
					if(is(plot.arena, "list")) plot.arena = plot.arena[[1]]
					plot.goal = terra::union(this.goal, plot.goal)
					if(is(plot.goal, "list")) plot.goal = plot.goal[[1]]
					plot.old.goal = terra::union(this.old.goal, plot.old.goal)
					if(is(plot.old.goal, "list")) plot.old.goal = plot.old.goal[[1]]
				}
			}
			plot.path = as.data.frame(do.call("rbind", lapply(metrics, function(m){
				cbind(t = m$path$t, x = m$path$x, y = m$path$y)
			})))
			if(!same.arena){
				warning("Multiple arena definitions have been used. A merged plot may not make sense.")
			}
		}else{
			plot.path = as.data.frame(cbind(t = metrics$path$t, x = metrics$path$x, y = metrics$path$y))
			plot.arena = metrics$arena$zones$pool
			plot.goal = metrics$arena$zones$goal
			plot.old.goal = metrics$arena$zones$old.goal
		}
		if(valid){
			density.map = KernSmooth::bkde2D(cbind(x = plot.path$x, y = plot.path$y), range.x = list(c(-1, 1), c(-1, 1)), bandwidth = c(.1, .1), gridsize = c(resolution, resolution))
			rast = terra::rast(ncol = resolution, nrow = resolution, xmin = -1, xmax = 1, ymin = -1, ymax = 1, crs = "local")
			terra::values(rast) = t(floor(density.map$fhat / max(density.map$fhat) * 255) + 1)[rev(seq_len(resolution)), ]
			terra::plot(plot.arena, lwd = lwd, mar = margins, axes = FALSE, main = title, xpd = NA, ...)
			terra::plot(terra::mask(rast, plot.arena), col = col, legend = FALSE, axes = FALSE, add = TRUE)
			if(!is.null(plot.goal)) terra::plot(plot.goal, col = "#FFFFFF00", border = feature.col, lwd = feature.lwd, lty = 1, add = T)
			if(!is.null(plot.old.goal)) terra::plot(plot.old.goal, col = "#FFFFFF00", border = feature.col, lwd = feature.lwd, lty = 3, add = T)
			terra::plot(plot.arena, lwd = lwd, axes = FALSE, add = T)
		}
	}else if(type == "barnes"){
		plot.path = NULL
		plot.arena = NULL
		plot.goal = NULL
		plot.old.goal = NULL
		if(methods::is(metrics, 'list')){
			plot.arena = terra::unwrap(metrics[[1]]$arena$zones$arena)
			plot.arena.id = metrics[[1]]$arena$id
			plot.goal = terra::unwrap(metrics[[1]]$arena$zones$goal)
			plot.old.goal = terra::unwrap(metrics[[1]]$arena$zones$old.goal)
			same.arena = TRUE
			for(i in seq_along(metrics)){
				this.arena = terra::unwrap(metrics[[i]]$arena$zones$arena)
				this.arena.id = metrics[[i]]$arena$id
				this.goal = terra::unwrap(metrics[[i]]$arena$zones$goal)
				this.old.goal = terra::unwrap(metrics[[i]]$arena$zones$old.goal)
				if(plot.arena.id != this.arena.id){
					same.arena = FALSE
					plot.arena = terra::union(this.arena, plot.arena)
					if(is(plot.arena, "list")) plot.arena = plot.arena[[1]]
					plot.goal = terra::union(this.goal, plot.goal)
					if(is(plot.goal, "list")) plot.goal = plot.goal[[1]]
					plot.old.goal = terra::union(this.old.goal, plot.old.goal)
					if(is(plot.old.goal, "list")) plot.old.goal = plot.old.goal[[1]]
				}
			}
			plot.path = as.data.frame(do.call("rbind", lapply(metrics, function(m){
				cbind(t = m$path$t, x = m$path$x, y = m$path$y)
			})))
			if(!same.arena){
				warning("Multiple arena definitions have been used. A merged plot may not make sense.")
			}
		}else{
			plot.path = as.data.frame(cbind(t = metrics$path$t, x = metrics$path$x, y = metrics$path$y))
			plot.arena = metrics$arena$zones$arena
			plot.goal = metrics$arena$zones$goal
			plot.old.goal = metrics$arena$zones$old.goal
		}
		if(valid){
			density.map = KernSmooth::bkde2D(cbind(x = plot.path$x, y = plot.path$y), range.x = list(c(-1, 1), c(-1, 1)), bandwidth = c(.1, .1), gridsize = c(resolution, resolution))
			rast = terra::rast(ncol = resolution, nrow = resolution, xmin = -1, xmax = 1, ymin = -1, ymax = 1, crs = "local")
			terra::values(rast) = t(floor(density.map$fhat / max(density.map$fhat) * 255) + 1)[rev(seq_len(resolution)), ]
			terra::plot(plot.arena, lwd = lwd, mar = margins, axes = FALSE, main = title, xpd = NA, ...)
			terra::plot(terra::mask(rast, plot.arena), col = col, legend = FALSE, axes = FALSE, add = TRUE)
			if(!is.null(plot.goal)) terra::plot(plot.goal, col = "#FFFFFF00", border = feature.col, lwd = feature.lwd, lty = 1, add = T)
			if(!is.null(plot.old.goal)) terra::plot(plot.old.goal, col = "#FFFFFF00", border = feature.col, lwd = feature.lwd, lty = 3, add = T)
			terra::plot(plot.arena, lwd = lwd, axes = FALSE, add = T)
		}
	}else if(type == "oft"){
		plot.path = NULL
		plot.arena = NULL
		if(methods::is(metrics, 'list')){
			plot.arena = terra::unwrap(metrics[[1]]$arena$zones$field)
			plot.arena.id = metrics[[1]]$arena$id
			same.arena = TRUE
			for(i in seq_along(metrics)){
				this.arena = terra::unwrap(metrics[[i]]$arena$zones$field)
				this.arena.id = metrics[[i]]$arena$id
				if(plot.arena.id != this.arena.id){
					same.arena = FALSE
					plot.arena = terra::union(this.arena, plot.arena)
					if(is(plot.arena, "list")) plot.arena = plot.arena[[1]]
				}
			}
			plot.path = as.data.frame(do.call("rbind", lapply(metrics, function(m){
				cbind(t = m$path$t, x = m$path$x, y = m$path$y)
			})))
			if(!same.arena){
				warning("Multiple arena definitions have been used. A merged plot may not make sense.")
			}
		}else{
			plot.path = as.data.frame(cbind(t = metrics$path$t, x = metrics$path$x, y = metrics$path$y))
			plot.arena = metrics$arena$zones$field
		}
		if(valid){
			density.map = KernSmooth::bkde2D(cbind(x = plot.path$x, y = plot.path$y), range.x = list(c(-1, 1), c(-1, 1)), bandwidth = c(.1, .1), gridsize = c(resolution, resolution))
			rast = terra::rast(ncol = resolution, nrow = resolution, xmin = -1, xmax = 1, ymin = -1, ymax = 1, crs = "local")
			terra::values(rast) = t(floor(density.map$fhat / max(density.map$fhat) * 255) + 1)[rev(seq_len(resolution)), ]
			terra::plot(plot.arena, lwd = lwd, mar = margins, axes = FALSE, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = title, xpd = NA, ...)
			terra::plot(terra::mask(rast, plot.arena), col = col, legend = FALSE, axes = FALSE, add = TRUE)
			terra::plot(plot.arena, lwd = lwd, axes = FALSE, add = T)
		}
	}else if(type == "nor"){
		plot.path = NULL
		plot.arena = NULL
		plot.object.1 = NULL
		plot.object.2 = NULL
		if(methods::is(metrics, 'list')){
			plot.arena = terra::unwrap(metrics[[1]]$arena$zones$field)
			plot.arena.id = metrics[[1]]$arena$id
			plot.object.1 = terra::unwrap(metrics[[1]]$arena$zones$object.1)
			plot.object.2 = terra::unwrap(metrics[[1]]$arena$zones$object.2)
			same.arena = TRUE
			for(i in seq_along(metrics)){
				this.arena = terra::unwrap(metrics[[i]]$arena$zones$field)
				this.arena.id = metrics[[i]]$arena$id
				this.object.1 = terra::unwrap(metrics[[i]]$arena$zones$object.1)
				this.object.2 = terra::unwrap(metrics[[i]]$arena$zones$object.2)
				if(plot.arena.id != this.arena.id){
					same.arena = FALSE
					plot.arena = terra::union(this.arena, plot.arena)
					if(is(plot.arena, "list")) plot.arena = plot.arena[[1]]
					plot.object.1 = terra::union(this.object.1, plot.object.1)
					if(is(plot.object.1, "list")) plot.object.1 = plot.object.1[[1]]
					plot.object.2 = terra::union(this.object.2, plot.object.2)
					if(is(plot.object.2, "list")) plot.object.2 = plot.object.2[[1]]
				}
			}
			plot.path = as.data.frame(do.call("rbind", lapply(metrics, function(m){
				cbind(t = m$path$t, x = m$path$x, y = m$path$y)
			})))
			if(!same.arena){
				warning("Multiple arena definitions have been used. A merged plot may not make sense.")
			}
		}else{
			plot.path = as.data.frame(cbind(t = metrics$path$t, x = metrics$path$x, y = metrics$path$y))
			plot.arena = metrics$arena$zones$field
			plot.object.1 = metrics$arena$zones$object.1
			plot.object.2 = metrics$arena$zones$object.2
		}
		if(valid){
			density.map = KernSmooth::bkde2D(cbind(x = plot.path$x, y = plot.path$y), range.x = list(c(-1, 1), c(-1, 1)), bandwidth = c(.1, .1), gridsize = c(resolution, resolution))
			rast = terra::rast(ncol = resolution, nrow = resolution, xmin = -1, xmax = 1, ymin = -1, ymax = 1, crs = "local")
			terra::values(rast) = t(floor(density.map$fhat / max(density.map$fhat) * 255) + 1)[rev(seq_len(resolution)), ]
			terra::plot(plot.arena, lwd = lwd, mar = margins, axes = FALSE, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = title, xpd = NA, ...)
			terra::plot(terra::mask(rast, plot.arena), col = col, legend = FALSE, axes = FALSE, add = TRUE)
			if(!is.null(plot.object.1)) terra::plot(plot.object.1, col = "#FFFFFF00", border = feature.col, lwd = feature.lwd, lty = 1, add = T)
			if(!is.null(plot.object.2)) terra::plot(plot.object.2, col = "#FFFFFF00", border = feature.col, lwd = feature.lwd, lty = 3, add = T)
			terra::plot(plot.arena, lwd = lwd, axes = FALSE, add = TRUE)
		}
	}else if(type == "apa"){
		plot.path = NULL
		plot.arena = NULL
		plot.aversive = NULL
		plot.old.aversive = NULL
		if(methods::is(metrics, 'list')){
			plot.arena = terra::unwrap(metrics[[1]]$arena$zones$arena)
			plot.arena.id = metrics[[1]]$arena$id
			plot.aversive = terra::unwrap(metrics[[1]]$arena$zones$aversive)
			#plot.old.aversive = terra::unwrap(metrics[[1]]$arena$zones$old.aversive)
			same.arena = TRUE
			for(i in seq_along(metrics)){
				this.arena = terra::unwrap(metrics[[i]]$arena$zones$arena)
				this.arena.id = metrics[[i]]$arena$id
				this.aversive = terra::unwrap(metrics[[i]]$arena$zones$aversive)
				this.old.aversive = terra::unwrap(metrics[[i]]$arena$zones$old.aversive)
				if(plot.arena.id != this.arena.id){
					same.arena = FALSE
					plot.arena = terra::union(this.arena, plot.arena)
					if(is(plot.arena, "list")) plot.arena = plot.arena[[1]]
					plot.aversive = terra::union(this.aversive, plot.aversive)
					if(is(plot.aversive, "list")) plot.aversive = plot.aversive[[1]]
					plot.old.aversive = terra::union(this.old.aversive, plot.old.aversive)
					if(is(plot.old.aversive, "list")) plot.old.aversive = plot.old.aversive[[1]]
				}
			}
			plot.path = as.data.frame(do.call("rbind", lapply(metrics, function(m){
				cbind(t = m$path$t, x = m$path$x, y = m$path$y)
			})))
			if(!same.arena){
				warning("Multiple arena definitions have been used. A merged plot may not make sense.")
			}
		}else{
			plot.path = as.data.frame(cbind(t = metrics$path$t, x = metrics$path$x, y = metrics$path$y))
			plot.arena = metrics$arena$zones$arena
			plot.aversive = metrics$arena$zones$aversive.zone
			plot.old.aversive = metrics$arena$zones$old.aversive.zone
		}
		if(valid){
			density.map = KernSmooth::bkde2D(cbind(x = plot.path$x, y = plot.path$y), range.x = list(c(-1, 1), c(-1, 1)), bandwidth = c(.1, .1), gridsize = c(resolution, resolution))
			rast = terra::rast(ncol = resolution, nrow = resolution, xmin = -1, xmax = 1, ymin = -1, ymax = 1, crs = "local")
			terra::values(rast) = t(floor(density.map$fhat / max(density.map$fhat) * 255) + 1)[rev(seq_len(resolution)), ]
			terra::plot(plot.arena, lwd = lwd, mar = margins, axes = FALSE, main = title, xpd = NA, ...)
			terra::plot(terra::mask(rast, plot.arena), col = col, legend = FALSE, axes = FALSE, add = TRUE)
			if(!is.null(plot.aversive)) terra::plot(plot.aversive, col = "#FFFFFF00", border = feature.col, lwd = feature.lwd, lty = 1, add = T)
			if(!is.null(plot.old.aversive)) terra::plot(plot.old.aversive, col = "#FFFFFF00", border = feature.col, lwd = feature.lwd, lty = 3, add = T)
			terra::plot(plot.arena, lwd = lwd, axes = FALSE, add = T)
		}
	}else{
		stop(paste0("The arena type '", type, "' is not supported."))
	}

	if(legend){
		plot.legend.labels = FALSE
		if(legend.labels[1] == TRUE){
			plot.legend.labels = TRUE
			legend.labels = c("max", "min") # Default lables.
		}else if(length(legend.labels) == 2){
			legend.labels = as.character(legend.labels)
			plot.legend.labels = TRUE # User-defined.
		}else{ # No other label systems are supported at this time.
			plot.legend.labels = FALSE
		}
		if(type %in% c("oft", "nor")){ # The square arenas are plotted slightly inset to make space for the legend.
			width = 1 / 10
			height = 1 / length(col) * .5
			breaks = seq(1.2, .6, length.out = length(col) + 1)
			x1 = rep(1.15, length(col)) - width / 2
			x2 = rep(1.15, length(col))
			y1 = head(breaks, length(col))
			y2 = tail(breaks, length(col))
			graphics::rect(x1, y1, x2, y2, col = rev(col), border = rev(col), xpd = NA)
			graphics::rect(x1, .6, x2, 1.2, xpd = NA)
			if(plot.legend.labels){
				graphics::text(x2[1], head(breaks, 1), labels = legend.labels[1], pos = 4, cex = cex.legend, xpd = TRUE)
				graphics::text(x2[1], tail(breaks, 1), labels = legend.labels[2], pos = 4, cex = cex.legend, xpd = TRUE)
			}
		}else{
			width = 1 / 1.2 / 10
			height = 1 / length(col) * .5
			breaks = seq(1, .5, length.out = length(col) + 1)
			x1 = rep(1.15 / 1.2, length(col)) - width / 2
			x2 = rep(1.15 / 1.2, length(col))
			y1 = head(breaks, length(col))
			y2 = tail(breaks, length(col))
			graphics::rect(x1, y1, x2, y2, col = rev(col), border = rev(col), xpd = NA)
			graphics::rect(x1, .5, x2, 1, xpd = NA)
			if(plot.legend.labels){
				graphics::text(x2[1], head(breaks, 1), labels = legend.labels[1], pos = 4, cex = cex.legend, xpd = TRUE)
				graphics::text(x2[1], tail(breaks, 1), labels = legend.labels[2], pos = 4, cex = cex.legend, xpd = TRUE)
			}
		}
	}
}
