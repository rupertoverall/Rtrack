#' Read an arena description.
#'
#' The user will normally not need to call this function directly. Use
#' \code{\link{read_experiment}} instead.
#'
#' Every path must be accompanied by a description of the 'arena'. This
#' description includes arena size, goal coordinates etc. and is unique for
#' every combination of these (i.e. a different arena description file is
#' required for goal reversal trials).
#'
#' The \code{type} parameter specifies the type of arena used. Currently only
#' 'mwm' (for Morris water maze) is supported but other types will be added in
#' the future. For the water maze, this package currently assumes a circular
#' pool and circular platforms (the commonly-used square platforms are
#' approximated by a circle of a diameter equal to the width of the square. This
#' is because the rotational orientation of square patforms is seldom recorded
#' (the behaviour of the package regarding this detail may be changed in future
#' versions).
#'
#' This function does not need to be explicitly called if
#' \code{\link{read_experiment}} is being used (in that case, specify the arena
#' file names in the column "_Arena").
#'
#' Quadrants are defined such that the goal is centred around the north
#' quadrant. Note that this means that the quadrant assignment will change in
#' the case of a goal reversal experiment. This simplifies the experiment set-up
#' considerably without imposing restrictions on more complex (e.g. multiple
#' reversal) study designs.
#'
#' @param filename A file specifying the arena.
#' @param description A data.frame containing parameters specifying the arena.
#'   If supplied, the \code{filename} argument will be ignored. This is intended
#'   for internal use only and can be ignored.
#'
#' @return An \code{rtrack_arena} object containing a representation of the
#'   arena, which can be passed to \code{\link{read_path}}.
#'
#' @seealso \code{\link{read_path}}, and also \code{\link{read_experiment}} for
#'   processing many tracks at once.
#'
#' @examples
#' require(Rtrack)
#' arena_description <- system.file("extdata", "Arena_SW.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#'
#' @importFrom utils read.delim
#' @importFrom sp SpatialPolygons Polygons Polygon
#' @importFrom raster intersect
#'
#' @export
read_arena = function(filename, description = NULL){
	if(is.null(description)){
		description = tryCatch(
			as.data.frame(t(utils::read.delim(filename, header = F, sep = "=", strip.white = T, comment.char = "#", stringsAsFactors = F, row.names = 1)), stringsAsFactors = F),
			error = function(e) stop(paste0("Cannot open file '", filename, "'. Is the path to this file correct?"))
		)
		rownames(description) = "value"
	}
	if(description$type == "mwm"){
		raw.pool = unlist(strsplit(description$arena.bounds, "\\s+"))
		raw.goal = unlist(strsplit(description$goal, "\\s+"))
		raw.old.goal = rep(NA, 4)
		if(!is.null(description$old.goal)) raw.old.goal = unlist(strsplit(description$old.goal, "\\s+"))
		# Pool dimensions are normalised using 'correction' values. Zero-centred and with a radius of 1
		# (Paths are normalised to match)
		correction = list(
			t = as.numeric(description$trial.length),
			x = as.numeric(raw.pool[2]),
			y =  as.numeric(raw.pool[3]),
			r = as.numeric(raw.pool[4])
		)
		pool = list(x = 0, y = 0, radius = 1, shape = "circle") # Defined as unit size
		goal = list(
			shape = as.character(raw.goal[1]),
			x = (as.numeric(raw.goal[2]) - correction$x) / correction$r, 
			y = (as.numeric(raw.goal[3]) - correction$y) / correction$r, 
			radius = as.numeric(raw.goal[4]) / correction$r
		)
		old.goal = list(
			shape = as.character(raw.old.goal[1]),
			x = (as.numeric(raw.old.goal[2]) - correction$x) / correction$r, 
			y = (as.numeric(raw.old.goal[3]) - correction$y) / correction$r, 
			radius = as.numeric(raw.old.goal[4]) / correction$r
		)

		# Annulus is ring defined by goal bounds
		goal.centre.distance = sqrt(goal$x ^ 2 + goal$y ^ 2)
		wall.radius = pool$radius * 0.8 # Wall zone width is 10 % of pool diameter
		annulus.outer.radius = goal.centre.distance + goal$radius
		annulus.inner.radius = goal.centre.distance - goal$radius
		if(is.na(goal.centre.distance)){ # If no goal (e.g. probe trial)...
			goal.centre.distance = sqrt(old.goal$x ^ 2 + old.goal$y ^ 2) # ...the annulus is defined on the old goal
			annulus.outer.radius = goal.centre.distance + old.goal$radius
			annulus.inner.radius = goal.centre.distance - old.goal$radius
		}
		wall = list(x = 0, y = 0, outer.radius = pool$radius, inner.radius = wall.radius, shape = "circle")
		far.wall = list(x = 0, y = 0, outer.radius = wall$inner.radius, inner.radius = annulus.outer.radius, shape = "circle")
		annulus = list(x = 0, y = 0, outer.radius = annulus.outer.radius, inner.radius = annulus.inner.radius, shape = "circle")
		arena = list(type = as.character(description$type), description = description, correction = correction, pool = pool, goal = goal, old.goal = old.goal)

		# Create polygons representing arena components
		arena$zones = list()
		arena$zones$pool = sp::SpatialPolygons(list(sp::Polygons( list(
			sp::Polygon(circle(pool$x, pool$y, pool$radius))
			), "pool")))
		arena$zones$wall = sp::SpatialPolygons(list(sp::Polygons( list(
			sp::Polygon(circle(wall$x, wall$y, wall$outer.radius)),
			sp::Polygon(circle(wall$x, wall$y, wall$inner.radius), hole = T)
			), "wall")))
		arena$zones$far.wall = sp::SpatialPolygons(list(sp::Polygons( list(
			sp::Polygon(circle(far.wall$x, far.wall$y, far.wall$outer.radius)),
			sp::Polygon(circle(far.wall$x, far.wall$y, far.wall$inner.radius), hole = T)
			), "far.wall")))
		arena$zones$annulus = sp::SpatialPolygons(list(sp::Polygons( list(
			sp::Polygon(circle(annulus$x, annulus$y, annulus$outer.radius)),
			sp::Polygon(circle(annulus$x, annulus$y, annulus$inner.radius), hole = T)
			), "annulus")))
		if(!is.na(goal$x) & !is.na(goal$y)){
			arena$zones$goal = sp::SpatialPolygons(list(sp::Polygons( list(
				sp::Polygon(circle(goal$x, goal$y, goal$radius))
			), "goal")))
		}else{
			arena$zones$goal = sp::SpatialPolygons(list())
		}
		if(!is.na(old.goal$x) & !is.na(old.goal$y)){
			arena$zones$old.goal = sp::SpatialPolygons(list(sp::Polygons( list(
				sp::Polygon(circle(old.goal$x, old.goal$y, old.goal$radius))
			), "old.goal")))
		}else{
			arena$zones$old.goal = sp::SpatialPolygons(list())
		}
	
		# Quadrants are defined such that goal (or old goal if goal absent) is in centre of north quadrant
		goal.angle = atan(goal$x / goal$y)
		if(is.na(goal.angle)) goal.angle = atan(old.goal$x / old.goal$y)
		if(is.na(goal.angle)) goal.angle = 0 # Can happen if goal is exactly in centre of arena and no old goal present
		n.point = c(x = sin(goal.angle - pi * 0.25) * pool$radius, y = cos(goal.angle - pi * 0.25) * pool$radius)
		e.point = c(x = sin(goal.angle + pi * 0.25) * pool$radius, y = cos(goal.angle + pi * 0.25) * pool$radius)
		s.point = c(x = sin(goal.angle + pi * 0.75) * pool$radius, y = cos(goal.angle + pi * 0.75) * pool$radius)
		w.point = c(x = sin(goal.angle - pi * 0.75) * pool$radius, y = cos(goal.angle - pi * 0.75) * pool$radius)
		arena$zones$n.quadrant = raster::intersect(arena$zones$pool, sp::SpatialPolygons(list(sp::Polygons( list(sp::Polygon(data.frame(x = c(0, n.point["x"], e.point["x"] + n.point["x"], e.point["x"]), y = c(0, n.point["y"], e.point["y"] + n.point["y"], e.point["y"])))), "N.quadrant"))))
		arena$zones$e.quadrant = raster::intersect(arena$zones$pool, sp::SpatialPolygons(list(sp::Polygons( list(sp::Polygon(data.frame(x = c(0, e.point["x"], s.point["x"] + e.point["x"], s.point["x"]), y = c(0, e.point["y"], s.point["y"] + e.point["y"], s.point["y"])))), "E.quadrant"))))
		arena$zones$s.quadrant = raster::intersect(arena$zones$pool, sp::SpatialPolygons(list(sp::Polygons( list(sp::Polygon(data.frame(x = c(0, s.point["x"], w.point["x"] + s.point["x"], w.point["x"]), y = c(0, s.point["y"], w.point["y"] + s.point["y"], w.point["y"])))), "S.quadrant"))))
		arena$zones$w.quadrant = raster::intersect(arena$zones$pool, sp::SpatialPolygons(list(sp::Polygons( list(sp::Polygon(data.frame(x = c(0, w.point["x"], n.point["x"] + w.point["x"], n.point["x"]), y = c(0, w.point["y"], n.point["y"] + w.point["y"], n.point["y"])))), "W.quadrant"))))
	
		class(arena) = "rtrack_arena"
		return(arena)
	}else{
		stop("The arena file '", filename, "' does not have a valid 'type' definition.")
	}
}
