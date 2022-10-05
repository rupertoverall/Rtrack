#' Calculation of spatial search path metrics.
#'
#' Calculates a range of metrics from path coordinates.
#'
#' Metrics are calculated based on normalised coordinate data and are available
#' as the \code{summary} element of the \code{rtrack_path} object. Unnormalised
#' values (with the same units as the raw data) are also available as the
#' \code{unscaled.summary} element. These can be useful for custom plots and are
#' also the values exported by \code{\link{export_results}}. Extended metrics
#' are available as separate elements of the \code{rtrack_path} object.
#'
#' @param path An \code{rtrack_path} object as returned by
#'   \code{\link{read_path}}.
#' @param arena An \code{rtrack_arena} object as returned by
#'   \code{\link{read_arena}}.
#'
#' @return An \code{rtrack_metrics} object containing metrics of the search path.
#'   This object is required as input for the \code{\link{call_strategy}} and
#'   \code{\link{plot_path}} functions.
#'
#' @seealso \code{\link{read_path}}, \code{\link{read_arena}}, and also
#'   \code{\link{read_experiment}} for processing many tracks at once.
#'
#' @examples
#' require(Rtrack)
#' track_file <- system.file("extdata", "Track_1.csv", package = "Rtrack")
#' arena_description <- system.file("extdata", "Arena_SW.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#' path <- read_path(track_file, arena, track.format = "ethovision.3.csv")
#' metrics <- calculate_metrics(path, arena)
#'
#' @importFrom sp Polygon Polygons SpatialPoints SpatialPolygons over
#' @importFrom rgeos gArea
#' @importFrom utils tail
#' @importFrom stats sd
#'
#' @export
calculate_metrics = function(path, arena){
	metrics = NULL
	if(is(path, "rtrack_path") & is(arena, "rtrack_arena")){
		if(length(path$t) > 0){
			# Area covered by the swim path as a fraction of the total arena
			path.coverage = sp::Polygons( list(sp::Polygon(cbind(path$x, path$y))), "coverage")
			coverage = path.coverage@area / arena$zones$pool@polygons[[1]]@area
			path.centroid = colMeans(path.coverage@Polygons[[1]]@coords)
			d.centroid = ((path$x - path.centroid[1]) ^ 2 + (path$y - path.centroid[2]) ^ 2) ^ .5
			d.goal = ((path$x - arena$goal$x) ^ 2 + (path$y - arena$goal$y) ^ 2) ^ .5
			d.old.goal = ((path$x - arena$old.goal$x) ^ 2 + (path$y - arena$old.goal$y) ^ 2) ^ .5
			d.origin = ((path$x) ^ 2 + (path$y) ^ 2) ^ .5
		
			# If the start point is given as 'a', the target as 'b' and the path as 'c',
			# this gives the angle as the absolute deviation from the line ab (i.e. the direct line from start to target)
			path.values = data.frame(x = path$x, y = path$y)
			start = data.frame(x = path$x[1], y = path$y[1])
			angle.from.points = function(a, b, c, degrees = FALSE){
				# Calculates angle abc
				ba = sqrt((b$x - a$x) ^ 2 + (b$y - a$y) ^ 2)
				ca = sqrt((c$x - a$x) ^ 2 + (c$y - a$y) ^ 2)
				bc = sqrt((b$x - c$x) ^ 2 + (b$y - c$y) ^ 2)
				angle = suppressWarnings(acos( (ba ^ 2 + ca ^ 2 - bc ^ 2) / (2 * ba * ca) )) # cosine rule
				angle[!is.finite(angle)] = NA # Undefined angles will just be ignored downstraem (but they sometimes throw harmless warnings which I have suppressed)
				if(degrees) angle = angle / pi * 180
				return(angle)
			}
		
			# Heading errors
			target.distance = sqrt((arena$goal$x - start$x) ^ 2 + (arena$goal$y - start$y) ^ 2)
			path.segment.lengths = sqrt(diff(path.values$x) ^ 2 + diff(path.values$y) ^ 2)
			path.length = sum(path.segment.lengths)
			cumulative.distance = cumsum(path.segment.lengths)
			initial = which(cumulative.distance < target.distance)
		
			# Initial heading error
			# Angle calculated for each interval and mean taken over all
			# Defined over path length equal to distance to target
			angles = angle.from.points(start, arena$goal, path.values[initial, ], degrees=T)
			initial.heading.error = list(angles = angles, mean = mean(angles, na.rm=T))
			# Initial displacement error
			# Displacement from projected path calculated and mean taken over all
			# Defined over path length equal to distance to target
			theta = atan2(arena$goal$y - start$y, arena$goal$x - start$x)
			projected.path = data.frame(x = start$x + cos(theta) * cumulative.distance[initial], y = start$y + sin(theta) * cumulative.distance[initial])
			displacement.distance = sqrt(rowSums(path.values[initial, c("x", "y")] - projected.path) ^ 2)
			initial.displacement.error = list(projected.path = projected.path, displacement = displacement.distance, mean = mean(displacement.distance, na.rm=T))
			# Initial trajectory error
			# Displacement from target at point on path equal to target distance
			initial.trajectory.end = path.values[initial[length(initial)], ] # Last point _nearer_ than the target distance
			error = sqrt((arena$goal$x - initial.trajectory.end$x) ^ 2 + (arena$goal$y - initial.trajectory.end$y) ^ 2)
			initial.trajectory.error = list(endpoint = initial.trajectory.end, error = error)
		
			# The 'alpha' as used by the Garthe classifier is slightly different
			# It measures the running difference between the path angle and the angle to the goal
			# If the current point is given as 'a', the target as 'b' and the next point on the path as 'c'
			# This can yield NaN (and a warning) when the subject does not move during one segment
			# (i.e. the angle is thus 0). These sections are just ignored.
			alpha = suppressWarnings(angle.from.points(path.values[-nrow(path.values), ], arena$goal, path.values[-1, ], degrees=T))
			# The full heading error is the running difference in heading angle compared to the direct start-goal path
			# This is used in the Garthe classifier
			heading.error = angle.from.points(start, arena$goal, path.values, degrees=T)
			
			# Heading error to old goal (same length as for goal)
			initial.reversal.error = sqrt((arena$old.goal$x - initial.trajectory.end$x) ^ 2 + (arena$old.goal$y - initial.trajectory.end$y) ^ 2)
		
			# 'Turning' behaviour measured by angular displacement at each timepoint
			dx = path.values[-1, 'x'] - path.values[-nrow(path.values), 'x']
			dy = path.values[-1, 'y'] - path.values[-nrow(path.values), 'y']
			angle.from.xaxis = atan2(dx, dy)
			turning = c(NA, NA, angle.from.xaxis[-1] - angle.from.xaxis[-length(angle.from.xaxis)])
		
			# Define the goal-oriented (triangular) 'corridor' that is 20 deg either side of optimal path
			corridor.angle = 20 / 180 * pi
			corridor.end.length = tan(corridor.angle) * target.distance # The width of the triangle base at the goal: the hypotenuse for the following calculations
			corridor.edge.distance = sqrt(corridor.end.length ^ 2 + target.distance ^ 2)
			corridor.end.right.x = start$x + cos(theta + corridor.angle) * corridor.edge.distance
			corridor.end.right.y = start$y + sin(theta + corridor.angle) * corridor.edge.distance
			corridor.end.left.x = start$x + cos(theta - corridor.angle) * corridor.edge.distance
			corridor.end.left.y = start$y + sin(theta - corridor.angle) * corridor.edge.distance
			# Create a polygon covering the triangle from start to goal
			arena$zones$goal.corridor = sp::SpatialPolygons(list())
			if(!is.na(corridor.end.length)){ # Only if there is a goal
				arena$zones$goal.corridor = sp::SpatialPolygons(list(sp::Polygons( list(sp::Polygon(data.frame(x = c(start$x, corridor.end.left.x, corridor.end.right.x, start$x), y = c(start$y, corridor.end.left.y, corridor.end.right.y, start$y)))), "goal.corridor")))
			}
			
			efficiency = NA
			if(length(angles) > 0) efficiency = length(which(angles < 15)) / length(angles) * 100
			# Swim speed and latency
			total.time = utils::tail(path$t, 1) - utils::head(path$t, 1)
			latency.to.goal = path$t[which(!is.na(sp::over(sp::SpatialPoints(path.values), arena$zones$goal)))[1]]
			average.swim.speed = utils::tail(cumulative.distance, 1) / utils::tail(path$t, 1)
			velocity = path.segment.lengths / (path$t[-1] - path$t[-length(path$t)])
			velocity[is.infinite(velocity) | is.nan(velocity)] = 0
			
			# Roaming entropy
			gridsize = 50
			freq = KernSmooth::bkde2D(raster::raster(cbind(path$x, path$y)), range.x = as.list(as.data.frame(t(arena$zones$pool@bbox))), bandwidth = c(1 / gridsize, 1 / gridsize), gridsize = c(gridsize, gridsize))$fhat
			sequence = seq(-1, 1, length.out = gridsize + 2)[-c(1, gridsize + 2)]
			outsideArena = is.na(sp::over(sp::SpatialPoints(data.frame(x = rep(sequence, times = gridsize), y = rep(sequence, each = gridsize))), arena$zones$pool))
			sum = sum(freq, na.rm = T)
			p = (freq / sum)[freq > 0 & !outsideArena]
			roaming.entropy = -sum(p * log(p)) / log(length(which(!outsideArena)))
		
			# Here the time spent in each defined zone is calculated
			time.in.zone = lapply(arena$zones, function(zone) sum(sp::over(sp::SpatialPoints(path.values), zone), na.rm = T) / length(path$t) )
			zone.crossings = lapply(arena$zones, function(zone){
				presence = sp::over(sp::SpatialPoints(path.values), zone)
				ceiling(length(which(is.na(presence[-length(presence)]) != is.na(presence[-1]))) / 2) # Half the transitions between 'in'/'out' are 'crossings'
			})
		
			metrics = list(
				id = path$id,
				arena = arena,
				area = sapply(arena$zones, rgeos::gArea),
				path = path,
				path.length = path.length,
				velocity = velocity,
				total.time = total.time,
				latency.to.goal = latency.to.goal,
				goal.crossings = zone.crossings$goal,
				old.goal.crossings = zone.crossings$old.goal,
				coverage = coverage,
				outliers = time.in.zone$pool - time.in.zone$goal.corridor, # Path not in approach corridor (% time)
				initial.path = path.values[initial, ],
				initial.heading.error = initial.heading.error,
				initial.displacement.error = initial.displacement.error,
				initial.trajectory.error = initial.trajectory.error,
				efficiency = efficiency,
				roaming.entropy = roaming.entropy,
				alpha = alpha,
				heading.error = heading.error,
				time.in.zone = time.in.zone,
				summary = c(
					path.length = path.length,
					mean.velocity = mean(velocity),
					sd.velocity = stats::sd(velocity),
					total.time = total.time,
					latency.to.goal = latency.to.goal,
					goal.crossings = zone.crossings$goal,
					old.goal.crossings = zone.crossings$old.goal,
					coverage = coverage,
					mean.d.centroid = mean(d.centroid),
					mean.d.goal = mean(d.goal),
					mean.d.old.goal = mean(d.old.goal),
					mean.d.origin = mean(d.origin),
					sd.d.centroid = stats::sd(d.centroid),
					sd.d.goal = stats::sd(d.goal),
					sd.d.old.goal = stats::sd(d.old.goal),
					sd.d.origin = stats::sd(d.origin),
					centroid.goal.displacement = sqrt((path.centroid[1] - arena$goal$x) ^ 2 + (path.centroid[2] - arena$goal$y) ^ 2),
					centroid.old.goal.displacement = sqrt((path.centroid[1] - arena$old.goal$x) ^ 2 + (path.centroid[2] - arena$old.goal$y) ^ 2),
					mean.initial.heading.error = initial.heading.error$mean,
					initial.trajectory.error = initial.trajectory.error$error[1],
					initial.reversal.error = initial.reversal.error[1],
					turning = mean(turning, na.rm = T),
					turning.absolute = mean(abs(turning), na.rm = T),
					efficiency = efficiency,
					roaming.entropy = roaming.entropy,
					time.in.zone.pool = time.in.zone$pool,
					time.in.zone.wall = time.in.zone$wall,
					time.in.zone.far.wall = time.in.zone$far.wall,
					time.in.zone.annulus = time.in.zone$annulus,
					time.in.zone.goal = time.in.zone$goal,
					time.in.zone.old.goal = time.in.zone$old.goal,
					time.in.zone.n.quadrant = time.in.zone$n.quadrant,
					time.in.zone.e.quadrant = time.in.zone$e.quadrant,
					time.in.zone.s.quadrant = time.in.zone$s.quadrant,
					time.in.zone.w.quadrant = time.in.zone$w.quadrant
				),
				unscaled.summary = c(
					path.length = path.length * arena$correction$r,
					mean.velocity = mean(velocity) * arena$correction$r / arena$correction$t,
					sd.velocity = stats::sd(velocity) * arena$correction$r / arena$correction$t,
					total.time = total.time * arena$correction$t,
					latency.to.goal = latency.to.goal * arena$correction$t,
					goal.crossings = zone.crossings$goal,
					old.goal.crossings = zone.crossings$old.goal,
					coverage = coverage,
					mean.d.centroid = mean(d.centroid) * arena$correction$r,
					mean.d.goal = mean(d.goal) * arena$correction$r,
					mean.d.old.goal = mean(d.old.goal) * arena$correction$r,
					mean.d.origin = mean(d.origin) * arena$correction$r,
					sd.d.centroid = stats::sd(d.centroid) * arena$correction$r,
					sd.d.goal = stats::sd(d.goal) * arena$correction$r,
					sd.d.old.goal = stats::sd(d.old.goal) * arena$correction$r,
					sd.d.origin = stats::sd(d.origin) * arena$correction$r,
					centroid.goal.displacement = sqrt((path.centroid[1] - arena$goal$x) ^ 2 + (path.centroid[2] - arena$goal$y) ^ 2) * arena$correction$r,
					centroid.old.goal.displacement = sqrt((path.centroid[1] - arena$old.goal$x) ^ 2 + (path.centroid[2] - arena$old.goal$y) ^ 2) * arena$correction$r,
					mean.initial.heading.error = initial.heading.error$mean,
					initial.trajectory.error = initial.trajectory.error$error[1] * arena$correction$r,
					initial.reversal.error = initial.reversal.error[1] * arena$correction$r,
					turning = mean(turning, na.rm = T),
					turning.absolute = mean(abs(turning), na.rm = T),
					efficiency = efficiency,
					roaming.entropy = roaming.entropy,
					time.in.zone.pool = time.in.zone$pool * arena$correction$t,
					time.in.zone.wall = time.in.zone$wall * arena$correction$t,
					time.in.zone.far.wall = time.in.zone$far.wall * arena$correction$t,
					time.in.zone.annulus = time.in.zone$annulus * arena$correction$t,
					time.in.zone.goal = time.in.zone$goal * arena$correction$t,
					time.in.zone.old.goal = time.in.zone$old.goal * arena$correction$t,
					time.in.zone.n.quadrant = time.in.zone$n.quadrant * arena$correction$t,
					time.in.zone.e.quadrant = time.in.zone$e.quadrant * arena$correction$t,
					time.in.zone.s.quadrant = time.in.zone$s.quadrant * arena$correction$t,
					time.in.zone.w.quadrant = time.in.zone$w.quadrant * arena$correction$t
				)
			)
			class(metrics) = "rtrack_metrics"
		}else{
			warning(paste0("The track '", path$id, "' is empty. Metrics cannot be calculated.")) 
		}
	}else{
		warning("The path and/or the arena are not valid Rtrack objects. Metrics cannot be calculated.") 
	}
	return(metrics)
}

