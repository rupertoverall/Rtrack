#' Plot a path.
#'
#' Plots the path together with a representation of the arena.
#'
#' The path is plotted together with the context of the arena. The three
#' concentric zones of the arena (the wall, outer wall and annulus) are drawn in
#' progressively lighter shades of blue. The goal is a filled circle in orange
#' and the old goal is drawn in grey. The direct path to goal is shown as a
#' broken orange line and the "approach corridor" (in transparent orange) is
#' defined as a triangle fanning out from this line by 20 degrees either side.
#' The path itself is drawn in black with the initial path (the section of the
#' path equivalent in length to the distance between the start and the goal)
#' drawn in red if \code{highlight = TRUE}.
#'
#' @param metrics A \code{metrics} object from \code{\link{calculate_metrics}}.
#' @param title An optional title for the plot. The default is to use the path
#'   name saved in the \code{metrics} object.
#' @param quadrants Should the quadrants be marked on the plot. Default is FALSE
#' @param highlight Should key features of the path be highlighted? Default is
#'   TRUE. The type of highlight depends on the plot type: For Morris water maze
#'   and Barnes maze, this will draw the section of the path equivalent in
#'   length to the distance between the start and the goal in red. For active
#'   place avoidance, the perimeter of the arena is annotated with a red point
#'   at the median and a line extending from the lower to the upper quartile.
#'   For other experiment types, this parameter is currently ignored.
#' @param margins The margins of the plot (see the option \code{mar} in
#'   \code{\link[graphics]{par}}). The defaults should normally not need to be
#'   changed.
#' @param path.lwd The thickness of the line used to draw the path. By default
#'   this is drawn heavier to make them stand out.
#' @param lwd The thickness of the lines used to draw the arena. Default is 1.
#' @param ... Additional plotting parameters (passed to \code{\link[terra]{plot}}).
#'
#' @seealso \code{\link{calculate_metrics}}, and also
#'   \code{\link{read_experiment}} for processing many tracks at once.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.tab", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "raw.tab")
#' metrics <- calculate_metrics(path, arena)
#' plot_path(metrics)
#'
#' @importFrom methods is
#' @importFrom graphics par lines segments
#' @importFrom terra plot
#'
#' @export
plot_path = function(metrics, title = NULL, quadrants = FALSE, highlight = TRUE, margins = c(0, 0, 3, 0), path.lwd = NA, lwd = 1, ...) {
	if(methods::is(metrics, 'rtrack_metrics')){
		if(is.null(title)){title = metrics$id}
		if(is.null(title)){title = ""} # Empty title string in case using metrics$id failed
		if(is.na(path.lwd) | path.lwd < 1) path.lwd = max(1, min(par("pin")) / 2)
		.parprevious = graphics::par(mar = par("mar"))
		on.exit(graphics::par(.parprevious))

		metrics$arena$zones = lapply(metrics$arena$zones, terra::unwrap) # Deserialise arena zones.
		
		if(metrics$arena$description$type == "mwm"){
			terra::plot(metrics$arena$zones$pool, lwd = lwd, mar = margins, axes = FALSE, main = title, ..., xpd = NA)
			terra::plot(metrics$arena$zones$wall, lwd = path.lwd, col = "#C0E0FFFF", add = T, border = NA)
			terra::plot(metrics$arena$zones$far.wall, lwd = path.lwd, col = "#C0E0FFA0", add = T, border = NA)
			terra::plot(metrics$arena$zones$annulus, lwd = path.lwd, col = "#C0E0FF40", add = T, border = NA)
			if(quadrants){
				terra::plot(metrics$arena$zones$n.quadrant, lwd = path.lwd, col = "#A0A0A040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$e.quadrant, lwd = path.lwd, col = "#F0F0F040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$s.quadrant, lwd = path.lwd, col = "#A0A0A040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$w.quadrant, lwd = path.lwd, col = "#F0F0F040", add = T, border = "#00000020")
			}
			if(!is.null(metrics$arena$zones$goal)){
				terra::plot(metrics$arena$zones$goal.corridor, lwd = path.lwd, col = "#D0A00080", add = T, border = "#00000040", xpd = NA)
				terra::plot(metrics$arena$zones$goal, lwd = path.lwd, col = "#F0A020F0", add = T)
			}
			if(!is.null(metrics$arena$zones$old.goal)){
				terra::plot(metrics$arena$zones$old.goal, lwd = path.lwd, col = "#80808080", add = T)
			}
			graphics::lines(metrics$path$x, metrics$path$y, lwd = path.lwd, col = "#000000FF", xpd = NA)
			if(highlight){
				graphics::lines(metrics$initial.path$x, metrics$initial.path$y, lwd = (path.lwd * 1.1), col = "#FF6000FF", xpd = NA)
			}
			if(!is.null(metrics$arena$zones$goal)) graphics::segments(metrics$path$x[1], metrics$path$y[1], metrics$arena$goal$x, metrics$arena$goal$y, lty = 3, lwd = path.lwd, col = "#F0A020F0")
			terra::plot(metrics$arena$zones$pool, lwd = path.lwd, border = "#606060FF", add = T)
		}else if(metrics$arena$description$type == "barnes"){
			terra::plot(metrics$arena$zones$arena, lwd = lwd, mar = margins, axes = FALSE, main = title, ..., xpd = NA)
			terra::plot(metrics$arena$zones$annulus, lwd = path.lwd, col = "#C0E0FF80", add = T, border = NA)
			terra::plot(metrics$arena$zones$centre, lwd = path.lwd, col = "#C0E0FF80", add = T, border = NA)
			if(quadrants){
				terra::plot(metrics$arena$zones$n.quadrant, lwd = path.lwd, col = "#A0A0A040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$e.quadrant, lwd = path.lwd, col = "#F0F0F040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$s.quadrant, lwd = path.lwd, col = "#A0A0A040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$w.quadrant, lwd = path.lwd, col = "#F0F0F040", add = T, border = "#00000020")
			}
			for(hole in grep("^hole_", names(metrics$arena$zones), value = TRUE)){
				terra::plot(metrics$arena$zones[[hole]], col = "#C0E0FFFF", add = T, border = NA)
			}
			if(!is.null(metrics$arena$zones$goal)){
				terra::plot(metrics$arena$zones$goal.corridor, lwd = path.lwd, col = "#D0A00080", add = T, border = "#00000040", xpd = NA)
				terra::plot(metrics$arena$zones$goal, lwd = path.lwd, col = "#E0A000FF", add = T)
			}
			if(!is.null(metrics$arena$zones$old.goal)){
				terra::plot(metrics$arena$zones$old.goal, lwd = path.lwd, col = "#80808080", add = T)
			}
			graphics::lines(metrics$path$x, metrics$path$y, lwd = path.lwd, col = "#000000FF", xpd = NA)
			if(highlight){
				graphics::lines(metrics$initial.path$x, metrics$initial.path$y, lwd = (path.lwd * 1.1), col = "#FF6000FF", xpd = NA)
			}
			if(!is.null(metrics$arena$zones$goal)) graphics::segments(metrics$path$x[1], metrics$path$y[1], metrics$arena$goal$x, metrics$arena$goal$y, lty = 3, lwd = path.lwd, col = "#F0A020F0")
			terra::plot(metrics$arena$zones$arena, lwd = path.lwd, border = "#606060FF", add = T)
		}else if(metrics$arena$description$type == "oft"){
			terra::plot(metrics$arena$zones$field, lwd = lwd, mar = margins, axes = FALSE, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = title, ..., xpd = NA)
			terra::plot(metrics$arena$zones$centre, col = "#C0E0FF80", add = T, border = NA)
			terra::plot(metrics$arena$zones$wall, col = "#C0E0FF80", add = T, border = NA)
			terra::plot(metrics$arena$zones$corner, col = "#C0E0FFFF", add = T, border = NA)
			graphics::lines(metrics$path$x, metrics$path$y, lwd = path.lwd, col = "#000000FF", xpd = NA)
			terra::plot(metrics$arena$zones$field, lwd = path.lwd, border = "#606060FF", add = T)
		}else if(metrics$arena$description$type == "nor"){
			terra::plot(metrics$arena$zones$field, lwd = lwd, mar = margins, axes = FALSE, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = title, ..., xpd = NA)
			terra::plot(metrics$arena$zones$wall, col = "#C0E0FF80", add = T, border = NA)
			terra::plot(metrics$arena$zones$corner, col = "#C0E0FFFF", add = T, border = NA)
			if(!metrics$arena$novel$object.1){
				terra::plot(metrics$arena$zones$object.1.vicinity, col = "#A0A0A080", add = T, border = NA)
				terra::plot(metrics$arena$zones$object.1, lwd = lwd, col = "#A0A0A0F0", add = T, border = NA)
			}
			if(!metrics$arena$novel$object.2){
				terra::plot(metrics$arena$zones$object.2, lwd = lwd, col = "#A0A0A0F0", add = T, border = NA)
				terra::plot(metrics$arena$zones$object.2.vicinity, col = "#A0A0A080", add = T, border = NA)
			}
			if(metrics$arena$novel$object.1 | metrics$arena$novel$object.2){
				terra::plot(metrics$arena$zones$novel.object.vicinity, col = "#D0A00080", add = T, border = NA)
				terra::plot(metrics$arena$zones$novel.object, lwd = lwd, col = "#E0A000FF", add = T, border = NA)
			}
			graphics::lines(metrics$path$x, metrics$path$y, lwd = path.lwd, col = "#000000FF", xpd = NA)
			terra::plot(metrics$arena$zones$field, lwd = path.lwd, border = "#606060FF", add = T)
		}else if(metrics$arena$description$type == "apa"){
			terra::plot(metrics$arena$zones$arena, lwd = lwd, mar = margins, axes = FALSE, main = title, ..., xpd = NA)
			# Make an arc for the angle-from-aversive. If no aversive, then ignore (NOT from old.aversive).
			if(!is.null(metrics$arena$zones$aversive.zone)){
				midaversive = (((metrics$arena$aversive.zone$end.angle + 360) + (metrics$arena$aversive.zone$start.angle + 360)) / 2) %% 360
				halfwidth = (((metrics$arena$aversive.zone$end.angle + 360) - (metrics$arena$aversive.zone$start.angle + 360)) / 2) %% 360
				startaversive = midaversive - (halfwidth * sign(metrics$arena$correction$e$arena.rotation))
				start.angle = startaversive + as.numeric(metrics$features["lower.angle.from.aversive.zone"]) 
				mid.angle = startaversive + as.numeric(metrics$features["median.angle.from.aversive.zone"])
				end.angle = startaversive + as.numeric(metrics$features["upper.angle.from.aversive.zone"])
				ring = terra::vect(circle(0, 0, 1.02), type = "polygons", crs = "local")
				centre.theta = deg2rad(start.angle - 90)
				corner.theta = deg2rad(start.angle)
				pre.sqcentre = c(x = sin(centre.theta), y = cos(centre.theta)) * 1.1 + c(0, 0) 
				pre.sqcorner = c(x = sin(corner.theta), y = cos(corner.theta)) * 1.1 + c(0, 0)
				centre.theta = deg2rad(end.angle + 90)
				corner.theta = deg2rad(end.angle)
				post.sqcentre = c(x = sin(centre.theta), y = cos(centre.theta)) * 1.1 + c(0, 0)
				post.sqcorner = c(x = sin(corner.theta), y = cos(corner.theta)) * 1.1 + c(0, 0)
				negative.pre = terra::vect(square(pre.sqcentre['x'], pre.sqcentre['y'], pre.sqcorner['x'], pre.sqcorner['y']), type = "polygons", crs = "local") # Square blocking arena before sector
				negative.post = terra::vect(square(post.sqcentre['x'], post.sqcentre['y'], post.sqcorner['x'], post.sqcorner['y']), type = "polygons", crs = "local") # Square blocking arena after sector
				if(end.angle - start.angle > 180){
					block = terra::intersect(negative.pre, negative.post)
				}else{
					block = terra::union(negative.pre, negative.post)
				}
				sector = terra::erase(
					terra::erase(
						ring, 
						block
					),
					metrics$arena$zones$arena
				)
				if(!is.null(metrics$arena$zones$aversive.zone)) terra::plot(sector, col = "#FF6000FF", add = T, border = NA, xpd = NA)
				if(!is.null(metrics$arena$zones$aversive.zone)) points(x = sin(deg2rad(mid.angle)) * 1.01, y = cos(deg2rad(mid.angle)) * 1.01, pch = 20, cex = 1.5 * path.lwd, col = "#FF6000FF", xpd = NA)
			}
			# Plot rest of arena and path.
			terra::plot(metrics$arena$zones$wall, lwd = path.lwd, col = "#C0E0FFA0", add = T, border = NA)
			terra::plot(metrics$arena$zones$centre, lwd = path.lwd, col = "#C0E0FFA0", add = T, border = NA)
			if(quadrants){
				terra::plot(metrics$arena$zones$n.quadrant, lwd = path.lwd, col = "#A0A0A040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$e.quadrant, lwd = path.lwd, col = "#F0F0F040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$s.quadrant, lwd = path.lwd, col = "#A0A0A040", add = T, border = "#00000020")
				terra::plot(metrics$arena$zones$w.quadrant, lwd = path.lwd, col = "#F0F0F040", add = T, border = "#00000020")
			}
			if(!is.null(metrics$arena$zones$aversive.zone)) terra::plot(metrics$arena$zones$aversive.zone, col = "#D0A00080", add = T)
			if(!is.null(metrics$arena$zones$old.aversive.zone)) terra::plot(metrics$arena$zones$old.aversive.zone, col = "#80808080", add = T)
			graphics::lines(metrics$path$x, metrics$path$y, lwd = path.lwd, col = "#000000FF", xpd = NA)
			terra::plot(metrics$arena$zones$arena, lwd = path.lwd, border = "#606060FF", add = T, xpd = NA)

			# Add an arrow for direction of rotation.
			lines(x = sin(deg2rad(-26:-64)) * 1.1, y = cos(deg2rad(-26:-64)) * 1.1, lwd = 1.5 * path.lwd, col = "#A0A0A0FF", xpd = NA)
			if(sign(metrics$arena$correction$e$arena.rotation) == 1){ # Clockwise.
				polygon(
					x = c(sin(deg2rad(-25)) * 1.1, sin(deg2rad(-27)) * 1.075, sin(deg2rad(-27)) * 1.125, sin(deg2rad(-25)) * 1.1),
					y = c(cos(deg2rad(-25)) * 1.1, cos(deg2rad(-27)) * 1.075, cos(deg2rad(-27)) * 1.125, cos(deg2rad(-25)) * 1.1),
					col = "#A0A0A0FF",
					border = NA,
					xpd = NA
				)
			}else{ # Counterclockwise.
				polygon(
					x = c(sin(deg2rad(-65)) * 1.1, sin(deg2rad(-63)) * 1.075, sin(deg2rad(-63)) * 1.125, sin(deg2rad(-65)) * 1.1),
					y = c(cos(deg2rad(-65)) * 1.1, cos(deg2rad(-63)) * 1.075, cos(deg2rad(-63)) * 1.125, cos(deg2rad(-65)) * 1.1),
					col = "#A0A0A0FF",
					border = NA,
					xpd = NA
				)
			}
		}

		invisible()
	}else{
		stop("The argument supplied is not an 'rtrack_metrics' object. Did you produce this using 'calculate_metrics' or 'read_experiment'?")
	}
}
