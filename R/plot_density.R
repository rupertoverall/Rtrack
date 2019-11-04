#' Plot a path density map.
#'
#' Plots a density map ('heatmap') of the path.
#'
#' @param metrics An \code{rtrack_metrics} object from \code{\link{calculate_metrics}}.
#' @param title An optional title for the plot. The default is to use the
#'   path name saved in the \code{rtrack_metrics} object.
#' @param col Colours for the density map. These can be provided as any vector
#'   of colours. The recommended (and default) approach is to use
#'   \code{\link[grDevices]{colorRampPalette}}. The default colouring is a
#'   simple white-to-blue scale.
#' @param legend Should a colour scale legend be drawn? Default is TRUE.
#' @param goal.col The colour to plot the goal outlines. Black by default, but
#'   it may be useful to change this if a very dark colour scheme is used.
#' @param goal.lwd The width of the lines used to plot the goals. By default
#'   this is drawn heavier to make them stand out.
#' @param resolution The resolution of the heatmap in pixels. The default is 600
#'   x 600.
#' @param margins The margins of the plot (see the option \code{mar} in
#'   \code{\link[graphics]{par}}). The defaults should normally not need to be
#'   changed.
#' @param ... Additional arguments passed to the plot method in the
#'   \code{\link[sp]{SpatialPolygons-class}} to modify plot details.
#'
#' @seealso \code{\link{calculate_metrics}}, \code{\link{plot_path}}.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.csv", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena_SW.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "ethovision.3.csv")
#' metrics <- calculate_metrics(path, arena)
#' plot_density(metrics)
#'
#' @importFrom graphics par plot lines segments
#' @importFrom grDevices colorRampPalette
#' @importFrom sp SpatialPolygons
#' @importFrom rgeos gDifference
#' @importFrom KernSmooth bkde2D
#'
#' @export
plot_density = function(metrics, title = NULL, col = grDevices::colorRampPalette(c('#FCFBFD', '#9E9AC8', '#3F007D'))(256), legend = TRUE, goal.col = "black", goal.lwd = 2, resolution = 600, margins = c(0, 2, 4, 2), ...) {
 # Accept a 'rtrack_metrics' object or a list of 'rtrack_metrics' objects
	if(class(metrics) == 'rtrack_metrics' | class(metrics[[1]]) == 'rtrack_metrics'){
	if(is.null(title)){
		if(class(metrics) == 'rtrack_metrics'){
			title = metrics$id
		}else if(class(metrics[[1]]) == 'rtrack_metrics'){
			title = "Multiple paths"
		}else{
			title = ""
		}
	}
		
		plot.path = NULL
		plot.pool = NULL
		plot.goal = NULL
		plot.old.goal = NULL
		pool.bounding.box = NULL
		if(class(metrics) == 'list'){
			plot.pool = sp::SpatialPolygons(lapply(1:length(metrics), function(i){
				pol = metrics[[i]]$arena$zones$pool@polygons[[1]]
				pol@ID = paste("pool", i, sep = "_")
				return(pol)
			}))
			l = lapply(metrics, function(m) m$arena$zones$pool)
			diff.pool = max(sapply(l, function(li) length(rgeos::gDifference(l[[1]], li)) ))
			plot.goal = sp::SpatialPolygons(lapply(1:length(metrics), function(i){
				pol = metrics[[i]]$arena$zones$goal@polygons[[1]]
				pol@ID = paste("goal", i, sep = "_")
				return(pol)
			}))
			l = lapply(metrics, function(m) m$arena$zones$goal)
			diff.goal = max(sapply(l, function(li) length(rgeos::gDifference(l[[1]], li)) ))
			old.goals = lapply(1:length(metrics), function(i){
				if(length(metrics[[i]]$arena$zones$old.goal@polygons) > 0){
					pol = metrics[[i]]$arena$zones$old.goal@polygons[[1]]
					pol@ID = paste("old.goal", i, sep = "_")
					return(pol)
				}
			})
			old.goals = old.goals[!sapply(old.goals, is.null)]
			if(length(old.goals) > 0) plot.old.goal = sp::SpatialPolygons(old.goals)
			plot.path = as.data.frame(do.call("rbind", lapply(metrics, function(m){
				cbind(t = m$path$t, x = m$path$x, y = m$path$y)
			})))
			l = lapply(metrics, function(m) m$arena$zones$old.goal)
			diff.old.goal = max(sapply(l, function(li) length(rgeos::gDifference(l[[1]], li)) ))
			if(max(diff.pool, diff.goal, diff.old.goal) > 0) warning("Multiple arena definitions have been used. The results may not make sense!")
			pool.bounding.box = plot.pool@bbox
		}else{
			plot.path = as.data.frame(cbind(t = metrics$path$t, x = metrics$path$x, y = metrics$path$y))
			plot.pool = metrics$arena$zones$pool
			plot.goal = metrics$arena$zones$goal
			plot.old.goal = metrics$arena$zones$old.goal
			pool.bounding.box = metrics$arena$zones$pool@bbox
		}
		density.map = KernSmooth::bkde2D(raster::raster(cbind(plot.path$x, plot.path$y)), range.x = as.list(as.data.frame(t(pool.bounding.box))), bandwidth = c(.1, .1), gridsize = c(resolution, resolution))
		density.raster = raster::raster(list(x = density.map$x1, y = density.map$x2, z = density.map$fhat))
		density.raster = raster::mask(density.raster, plot.pool)
		.parprevious = graphics::par(mar = margins, xpd = TRUE)
		on.exit(par(.parprevious))
		sp::plot(plot.pool)
		graphics::rasterImage(raster::as.raster(density.raster, col = col), -1, -1, 1, 1)
		#sp::plot(plot.pool, col = "#FFFFFF00", border = goal.col, add = T, ...)
		sp::plot(plot.goal, col = "#FFFFFF00", border = goal.col, lwd = goal.lwd, lty = 1, add = T)
		if(!is.null(plot.old.goal)) sp::plot(plot.old.goal, col = "#FFFFFF00", border = goal.col, lwd = goal.lwd, lty = 3, add = T)
		sp::plot(plot.pool, col = "#FFFFFF00", add = T, ...)
		if(legend){
			width = diff(pool.bounding.box["x", ]) / 30
			height = diff(pool.bounding.box["y", ]) / 2 / length(col)
			x1 = rep(pool.bounding.box["x", "max"], length(col)) + width
			x2 = rep(pool.bounding.box["x", "max"], length(col)) + 2 * width
			y1 = seq(pool.bounding.box["y", "max"], height, length.out = length(col))
			y2 = y1 - height
			graphics::rect(x1, y1, x2, y2, col = rev(col), border = rev(col))
			graphics::rect(pool.bounding.box["x", "max"] + width, pool.bounding.box["y", "max"], pool.bounding.box["x", "max"] + 2 * width, 0, ...)
		}
		title(main = title)
		invisible(density.raster)
	}else{
		stop("The argument supplied is not an 'rtrack_metrics' object. Did you produce this using 'calculate_metrics' or 'read_experiment'?")
	}
}
