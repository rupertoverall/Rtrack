#' Plot a path.
#'
#' Plots the path together with a representation of the arena. These plots are
#' useful for diagnosis of classification problems.
#'
#' The path is plotted together with the context of the arena. The three
#' concentric zones of the arena (the wall, outer wall and annulus) are drawn in
#' progressively lighter shades of blue. The goal is a filled circle in orange
#' and the old goal is drawn in grey. The direct path to goal is shown as a
#' broken orange line and the 'approach corridor' (in transparent orange) is
#' defined as a triangle fanning out from this line by 20 degrees either side.
#' The path itself is drawn in black with grey sections corresponding to
#' interpolated data (if \code{highlight.interpolated = TRUE}, the parameter
#' \code{interpolate = TRUE} was set in \code{\link{read_path}} and there were
#' missing data points in the raw track file). The initial path (the section of
#' the path equivalent in length to the distance between the start and the goal)
#' is drawn in red if \code{highlight.initial = TRUE}.
#'
#' @param metrics A \code{metrics} object from \code{\link{calculate_metrics}}.
#' @param title An optional title for the plot. The default is to use the path
#'   name saved in the \code{metrics} object.
#' @param quadrants Should the quadrants be marked on the plot. Default is FALSE
#' @param highlight.interpolated Should interpolated sections of the path be
#'   highlighted (in grey). Default is TRUE.
#' @param highlight.initial Should the initial section of the path be
#'   highlighted (in red). Default is TRUE. This is the section of the path
#'   equivalent in length to the distance between the start and the goal.
#' @param margins The margins of the plot (see the option \code{mar} in
#'   \code{\link[graphics]{par}}). The defaults should normally not need to be
#'   changed.
#' @param ... Additional arguments passed to the plot method in the
#'   \code{\link[sp]{SpatialPolygons-class}} to modify plot details.
#'
#' @seealso \code{\link{calculate_metrics}}, and also
#'   \code{\link{read_experiment}} for processing many tracks at once.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.csv", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena_SW.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "ethovision.3.csv")
#' metrics <- calculate_metrics(path, arena)
#' plot_path(metrics)
#'
#' @importFrom graphics par plot lines segments
#'
#' @export
plot_path = function(metrics, title = NULL, quadrants = FALSE, highlight.interpolated = TRUE, highlight.initial = TRUE, margins = c(0, 2, 4, 2), ...) {
	if(class(metrics) == 'rtrack_metrics'){
		if(is.null(title)){title = metrics$id}
		if(is.null(title)){title = ""} # Empty title string in case using metrics$id failed
		.parprevious = graphics::par(no.readonly = TRUE)
		parameters = c(...)
		path.lwd = as.numeric(parameters[which(names(parameters) == "lwd")])
		if(length(path.lwd) == 0 ) path.lwd = 2
		graphics::par(mar = margins)
		sp::plot(metrics$arena$zones$pool, lwd = (path.lwd / 2), main = title)
		sp::plot(metrics$arena$zones$wall, lwd = path.lwd, col = "#C0E0FFFF", add = T, border = NA)
		sp::plot(metrics$arena$zones$far.wall, lwd = path.lwd, col = "#C0E0FFA0", add = T, border = NA)
		sp::plot(metrics$arena$zones$annulus, lwd = path.lwd, col = "#C0E0FF40", add = T, border = NA)
		if(quadrants){
			sp::plot(metrics$arena$zones$n.quadrant, add = T, lwd = path.lwd, col = "#A0A0A040", border = "#00000020")
			sp::plot(metrics$arena$zones$e.quadrant, add = T, lwd = path.lwd, col = "#F0F0F040", border = "#00000020")
			sp::plot(metrics$arena$zones$s.quadrant, add = T, lwd = path.lwd, col = "#A0A0A040", border = "#00000020")
			sp::plot(metrics$arena$zones$w.quadrant, add = T, lwd = path.lwd, col = "#F0F0F040", border = "#00000020")
		}
		sp::plot(metrics$arena$zones$goal.corridor, add = T, lwd = path.lwd, col = "#D0A00080", border = "#00000040")
		sp::plot(metrics$arena$zones$goal, lwd = path.lwd, col = "#F0A020F0", add = T)
		if(!is.null(metrics$arena$zones$old.goal)) sp::plot(metrics$arena$zones$old.goal, lwd = path.lwd, col = "#00000080", add = T)
		graphics::lines(metrics$path$x, metrics$path$y, lwd = path.lwd, col = "#000000FF")
		if(highlight.initial){
			graphics::lines(metrics$initial.path$x, metrics$initial.path$y, lwd = (path.lwd * 1.1), col = "#FF6000FF")
		}
		if(highlight.interpolated){
			interpolated = metrics$path$t %in% metrics$path$interpolated
			graphics::lines(metrics$path$x[interpolated], metrics$path$y[interpolated], lwd = (path.lwd * 1.1), col = "#808080FF")
		}
		graphics::segments(metrics$path$x[1], metrics$path$y[1], metrics$arena$goal$x, metrics$arena$goal$y, lty = 3, lwd = path.lwd, col = "#F0A020F0")
		sp::plot(metrics$arena$zones$pool, lwd = path.lwd, add = T)
		graphics::par(mar = .parprevious$mar)
		invisible()
	}else{
		stop("The argument supplied is not an 'rtrack_metrics' object. Did you produce this using 'calculate_metrics' or 'read_experiment'?")
	}
}
