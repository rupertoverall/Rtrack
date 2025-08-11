#' @importFrom terra unwrap vect convHull expanse crds centroids relate distance wrap
#' @importFrom KernSmooth bkde2D
#' @importFrom stats quantile
#' @importFrom utils globalVariables
utils::globalVariables(c("velocity.quantile", "d.centroid.quantile", "d.goal.quantile", "d.old.goal.quantile", "d.origin.quantile" ))

calculate_mwm_metrics = function(path, arena){
	# Deserialise arena zones, but retain wrapped version to return.
	wrapped.arena.zones = arena$zones
	arena$zones = lapply(arena$zones, terra::unwrap)
	
	# Check for presence of optional zones (terra cannot handle missing data).
	goal.present = !is.null(arena$zones$goal)
	old.goal.present = !is.null(arena$zones$old.goal)
	
	# Area covered by the swim path as a fraction of the total arena
	path.values = cbind(x = path$x, y = path$y)
	pathpoints = terra::vect(path.values, type = "points", crs = "local") # With closed path.
	pathpoly = terra::vect(path.values, type = "polygons", crs = "local")
	coverage = terra::expanse(terra::convHull(pathpoly), transform = FALSE) / terra::expanse(arena$zones$pool, transform = FALSE) # Arena circle is actually a 360-point polygon approximation of a circle (area < pi r^2).
	path.centroid = terra::crds(terra::centroids(pathpoly))
	d.centroid = ((path$x - path.centroid[1]) ^ 2 + (path$y - path.centroid[2]) ^ 2) ^ .5
	d.origin = ((path$x) ^ 2 + (path$y) ^ 2) ^ .5

	# Distance from goals
	d.goal = NA
	d.old.goal = NA
	if(goal.present) d.goal = as.numeric(terra::distance(arena$zones$goal, pathpoints))
	if(old.goal.present) d.old.goal = as.numeric(terra::distance(arena$zones$old.goal, pathpoints))
	
	# If the start point is given as 'a', the target as 'b' and the path as 'c',
	# this gives the angle as the absolute deviation from the line ab (i.e. the direct line from start to target)
	path.values = as.data.frame(path.values)
	start = path.values[1, ]
	angle.from.points = function(a, b, c, degrees = FALSE){
		# Calculates angle abc
		ba = sqrt((b$x - a$x) ^ 2 + (b$y - a$y) ^ 2)
		ca = sqrt((c$x - a$x) ^ 2 + (c$y - a$y) ^ 2)
		bc = sqrt((b$x - c$x) ^ 2 + (b$y - c$y) ^ 2)
		angle = suppressWarnings(acos( (ba ^ 2 + ca ^ 2 - bc ^ 2) / (2 * ba * ca) )) # cosine rule
		angle[!is.finite(angle)] = NA # Undefined angles will just be ignored downstream (but they sometimes throw harmless warnings which I have suppressed)
		if(degrees) angle = angle / pi * 180
		return(angle)
	}

	# Heading errors
	path.segment.lengths = sqrt(diff(path.values$x) ^ 2 + diff(path.values$y) ^ 2)
	path.length = sum(path.segment.lengths)
	cumulative.distance = cumsum(path.segment.lengths)
	target.distance = sqrt((arena$goal$x - start$x) ^ 2 + (arena$goal$y - start$y) ^ 2)
	initial = which(cumulative.distance < target.distance)

	# Initial heading error
	# Angle calculated for each interval and median taken over all
	# Defined over path length equal to distance to target
	angles = angle.from.points(start, arena$goal, path.values[initial, ], degrees=T)
	initial.heading.error = list(angles = angles, median = median(angles, na.rm=T))
	# Initial displacement error
	# Displacement from projected path calculated and median taken over all
	# Defined over path length equal to distance to target
	theta = atan2(arena$goal$y - start$y, arena$goal$x - start$x)
	projected.path = data.frame(x = start$x + cos(theta) * cumulative.distance[initial], y = start$y + sin(theta) * cumulative.distance[initial])
	displacement.distance = sqrt(rowSums(path.values[initial, c("x", "y")] - projected.path) ^ 2)
	initial.displacement.error = list(projected.path = projected.path, displacement = displacement.distance, median = median(displacement.distance, na.rm=T))
	# Initial trajectory error
	# Displacement from target at point on path equal to target distance
	initial.trajectory.end = path.values[initial[length(initial)], ] # Last point _nearer_ than the target distance
	error = sqrt((arena$goal$x - initial.trajectory.end$x) ^ 2 + (arena$goal$y - initial.trajectory.end$y) ^ 2)
	initial.trajectory.error = list(endpoint = initial.trajectory.end, error = error)

	alpha = suppressWarnings(angle.from.points(path.values[-nrow(path.values), ], arena$goal, path.values[-1, ], degrees=T))

	heading.error = angle.from.points(start, arena$goal, path.values, degrees=T)
	
	# Heading error to old goal
	reverse.target.distance = sqrt((arena$old.goal$x - start$x) ^ 2 + (arena$old.goal$y - start$y) ^ 2)
	reverse.initial = which(cumulative.distance < reverse.target.distance)
	initial.reversal.end = path.values[reverse.initial[length(reverse.initial)], ] # Last point _nearer_ than the reversal target distance
	reversal.error = sqrt((arena$old.goal$x - initial.reversal.end$x) ^ 2 + (arena$old.goal$y - initial.reversal.end$y) ^ 2)
	initial.reversal.error = list(endpoint = initial.reversal.end, error = reversal.error)

	# Define the goal-oriented (triangular) 'corridor' that is 20 deg either side of optimal path
	if(goal.present){ # Only if there is a goal
		corridor.angle = 20 / 180 * pi
		corridor.end.length = tan(corridor.angle) * target.distance # The width of the triangle base at the goal = the hypotenuse for the following calculations
		corridor.edge.distance = sqrt(corridor.end.length ^ 2 + target.distance ^ 2)
		corridor.end.right.x = start$x + cos(theta + corridor.angle) * corridor.edge.distance
		corridor.end.right.y = start$y + sin(theta + corridor.angle) * corridor.edge.distance
		corridor.end.left.x = start$x + cos(theta - corridor.angle) * corridor.edge.distance
		corridor.end.left.y = start$y + sin(theta - corridor.angle) * corridor.edge.distance
		# Create a polygon covering the triangle from start to goal
		goal.corridor = terra::vect(cbind(x = c(start$x, corridor.end.left.x, corridor.end.right.x, start$x), y = c(start$y, corridor.end.left.y, corridor.end.right.y, start$y)), type = "polygons", crs = "local")
		arena$zones$goal.corridor = goal.corridor
	}

	in.zones = lapply(arena$zones, function(zone) as.logical(rowSums(terra::relate(pathpoints, zone, relation = "intersects"))) )
	
	efficiency = NA
	if(length(angles) > 0) efficiency = length(which(angles < 15)) / length(angles) * 100
	# Speed and latency
	total.time = diff(range(path$t, na.rm = T))
	intervals = diff(path$t)
	negligible = intervals == 0 # This can happen due to rounding precision.
	velocity = path.segment.lengths[!negligible] / intervals[!negligible]
	velocity[is.infinite(velocity) | is.nan(velocity)] = NA
	velocity.in.zone = lapply(in.zones, function(in.zone){
		median(velocity[which(in.zone[!negligible])], na.rm = TRUE)
	})
	if(!goal.present){
		velocity.in.zone$goal = NA
		velocity.in.zone$corridor = NA
	}
	if(!old.goal.present) velocity.in.zone$old.goal = NA
	
	# Latency to enter zone
	latency.to.zone = lapply(in.zones, function(in.zone){
		path$t[which(in.zone)[1]] - path$t[1]
	})
	if(!goal.present) latency.to.zone$goal = NA
	if(!old.goal.present) latency.to.zone$old.goal = NA
	
	# 'Turning' behaviour measured by angular displacement at each timepoint
	dx = path.values[-1, 'x'] - path.values[-nrow(path.values), 'x']
	dy = path.values[-1, 'y'] - path.values[-nrow(path.values), 'y']
	angle.from.xaxis = atan2(dy, dx)
	angle.from.xaxis[is.na(angle.from.xaxis)] = 0 # In the case that the path point is _on_ the origin.
	turning = c(NA, NA, angle.from.xaxis[-1] - angle.from.xaxis[-length(angle.from.xaxis)])
	
	# Roaming entropy
	gridsize = 50
	freq = KernSmooth::bkde2D(path.values, range.x = list(c(-1, 1), c(-1, 1)), bandwidth = c(1 / gridsize, 1 / gridsize), gridsize = c(gridsize, gridsize))$fhat
	sequence = seq(-1, 1, length.out = gridsize)
	grid = terra::vect(cbind(x = rep(sequence, times = gridsize), y = rep(sequence, each = gridsize)), type = "points")
	sum = sum(freq, na.rm = T)
	p = (freq / sum)[freq > 0]
	roaming.entropy = -sum(p * log(p)) / log(length(p))
	
	# Here the time spent in each defined zone is calculated
	time.in.zone = lapply(in.zones, function(in.zone) sum(in.zone, na.rm = T) / length(path$t) )
	if(!goal.present){
		time.in.zone$goal = NA
		time.in.zone$corridor = NA
	}
	if(!old.goal.present) time.in.zone$old.goal = NA
	zone.crossings = lapply(in.zones, function(in.zone){
		ceiling(length(which(in.zone[-length(in.zone)] != in.zone[-1])) / 2) # Half the transitions between 'in'/'out' are 'crossings'
	})
	if(!goal.present) zone.crossings$goal = NA
	if(!old.goal.present) zone.crossings$old.goal = NA
	
	# Was the goal/old goal reached at all? Only look at where the subject _ended_ its path.
	goal.reached = NA
	if(goal.present) goal.reached = any(tail(in.zones$goal))
	old.goal.reached = NA
	if(old.goal.present) old.goal.reached = any(tail(in.zones$old.goal))
	
	velocity.quantile = unname(stats::quantile(velocity, na.rm = TRUE))
	d.centroid.quantile = unname(stats::quantile(d.centroid, na.rm = TRUE))
	d.goal.quantile = unname(stats::quantile(d.goal, na.rm = TRUE))
	d.old.goal.quantile = unname(stats::quantile(d.old.goal, na.rm = TRUE))
	d.origin.quantile = unname(stats::quantile(d.origin, na.rm = TRUE))
	angle.quantile = unname(stats::quantile(angle.from.xaxis, na.rm = TRUE))
	
	outliers = NA
	if(goal.present) outliers = time.in.zone$pool - time.in.zone$goal.corridor # Path not in approach corridor (% time)
	centroid.goal.displacement = NA
	if(goal.present) centroid.goal.displacement = sqrt((path.centroid[1] - arena$goal$x) ^ 2 + (path.centroid[2] - arena$goal$y) ^ 2)
	centroid.old.goal.displacement = NA
	if(old.goal.present) centroid.old.goal.displacement = sqrt((path.centroid[1] - arena$old.goal$x) ^ 2 + (path.centroid[2] - arena$old.goal$y) ^ 2)
	median.initial.heading.error = NA
	if(goal.present) median.initial.heading.error = initial.heading.error$median
	initial.trajectory.error.error = NA
	if(goal.present) initial.trajectory.error.error = initial.trajectory.error$error[1]
	initial.reversal.error.error = NA
	if(old.goal.present) initial.reversal.error.error = initial.reversal.error$error[1]
	
	metrics = list(
		id = path$id,
		arena = arena,
		area = sapply(arena$zones, terra::expanse, transform = FALSE),
		path = path,
		path.length = path.length,
		velocity = velocity,
		total.time = total.time,
		goal.crossings = zone.crossings$goal,
		old.goal.crossings = zone.crossings$old.goal,
		coverage = coverage,
		angle.from.xaxis = angle.from.xaxis,
		outliers = outliers,
		initial.path = path.values[initial, ],
		initial.heading.error = initial.heading.error,
		initial.displacement.error = initial.displacement.error,
		initial.trajectory.error = initial.trajectory.error,
		initial.reversal.error = initial.reversal.error,
		efficiency = efficiency,
		roaming.entropy = roaming.entropy,
		alpha = alpha,
		heading.error = heading.error,
		velocity.in.zone = velocity.in.zone,
		latency.to.zone = latency.to.zone,
		time.in.zone = time.in.zone,
		features = c(
			path.length = path.length,
			lower.velocity = velocity.quantile[2],
			median.velocity = velocity.quantile[3],
			upper.velocity = velocity.quantile[4],
			total.time = total.time,
			coverage = coverage,
			outliers = outliers,
			turning = sum(turning, na.rm = T) / total.time,
			absolute.turning = sum(abs(turning), na.rm = T) / total.time,
			lower.angle = angle.quantile[2],
			median.angle = angle.quantile[3],
			upper.angle = angle.quantile[4],
			lower.d.centroid = d.centroid.quantile[2],
			median.d.centroid = d.centroid.quantile[3],
			upper.d.centroid = d.centroid.quantile[4],
			lower.d.goal = d.goal.quantile[2],
			median.d.goal = d.goal.quantile[3],
			upper.d.goal = d.goal.quantile[4],
			lower.d.old.goal = d.old.goal.quantile[2],
			median.d.old.goal = d.old.goal.quantile[3],
			upper.d.old.goal = d.old.goal.quantile[4],
			lower.d.origin = d.origin.quantile[2],
			median.d.origin = d.origin.quantile[3],
			upper.d.origin = d.origin.quantile[4],
			centroid.goal.displacement = centroid.goal.displacement,
			centroid.old.goal.displacement = centroid.old.goal.displacement,
			median.initial.heading.error = median.initial.heading.error,
			initial.trajectory.error = initial.trajectory.error.error,
			initial.reversal.error = initial.reversal.error.error,
			efficiency = efficiency,
			roaming.entropy = roaming.entropy,
			velocity.in.wall.zone = velocity.in.zone$wall,
			velocity.in.far.wall.zone = velocity.in.zone$far.wall,
			velocity.in.annulus.zone = velocity.in.zone$annulus,
			velocity.in.goal.zone = velocity.in.zone$goal,
			velocity.in.goal.corridor = velocity.in.zone$goal.corridor,
			velocity.in.old.goal.zone = velocity.in.zone$old.goal,
			velocity.in.n.quadrant = velocity.in.zone$n.quadrant,
			velocity.in.e.quadrant = velocity.in.zone$e.quadrant,
			velocity.in.s.quadrant = velocity.in.zone$s.quadrant,
			velocity.in.w.quadrant = velocity.in.zone$w.quadrant,
			latency.to.wall.zone = latency.to.zone$wall,
			latency.to.far.wall.zone = latency.to.zone$far.wall,
			latency.to.annulus.zone = latency.to.zone$annulus,
			latency.to.goal.zone = latency.to.zone$goal,
			latency.to.old.goal.zone = latency.to.zone$old.goal,
			latency.to.n.quadrant = latency.to.zone$n.quadrant,
			latency.to.e.quadrant = latency.to.zone$e.quadrant,
			latency.to.s.quadrant = latency.to.zone$s.quadrant,
			latency.to.w.quadrant = latency.to.zone$w.quadrant,
			time.in.wall.zone = time.in.zone$wall,
			time.in.far.wall.zone = time.in.zone$far.wall,
			time.in.annulus.zone = time.in.zone$annulus,
			time.in.goal.zone = time.in.zone$goal,
			time.in.goal.corridor = time.in.zone$goal.corridor,
			time.in.old.goal.zone = time.in.zone$old.goal,
			time.in.n.quadrant = time.in.zone$n.quadrant,
			time.in.e.quadrant = time.in.zone$e.quadrant,
			time.in.s.quadrant = time.in.zone$s.quadrant,
			time.in.w.quadrant = time.in.zone$w.quadrant,
			wall.zone.crossings = zone.crossings$wall,
			far.wall.zone.crossings = zone.crossings$far.wall,
			annulus.zone.crossings = zone.crossings$annulus,
			goal.zone.crossings = zone.crossings$goal,
			goal.corridor.crossings = zone.crossings$goal.corridor,
			old.goal.zone.crossings = zone.crossings$old.goal,
			n.quadrant.crossings = zone.crossings$n.quadrant,
			e.quadrant.crossings = zone.crossings$e.quadrant,
			s.quadrant.crossings = zone.crossings$s.quadrant,
			w.quadrant.crossings = zone.crossings$w.quadrant,
			goal.reached = goal.reached,
			old.goal.reached = old.goal.reached
		),
		summary = c(
			path.length = path.length * arena$correction$r * arena$correction$d,
			total.time = total.time,
			velocity = velocity.quantile[3] * arena$correction$r * arena$correction$d / arena$correction$t,
			distance.from.goal = d.goal.quantile[3] * arena$correction$r * arena$correction$d,
			distance.from.old.goal = d.old.goal.quantile[3] * arena$correction$r * arena$correction$d,
			initial.heading.error = initial.heading.error$median,
			initial.trajectory.error = initial.trajectory.error.error * arena$correction$r * arena$correction$d,
			initial.reversal.error = initial.reversal.error.error * arena$correction$r * arena$correction$d,
			efficiency = efficiency,
			roaming.entropy = roaming.entropy,
			latency.to.goal = latency.to.zone$goal * arena$correction$t,
			latency.to.old.goal = latency.to.zone$old.goal * arena$correction$t,
			time.in.wall.zone = time.in.zone$wall * total.time,
			time.in.far.wall.zone = time.in.zone$far.wall * total.time,
			time.in.annulus.zone = time.in.zone$annulus * total.time,
			time.in.goal.zone = time.in.zone$goal * total.time,
			time.in.old.goal.zone = time.in.zone$old.goal * total.time,
			time.in.n.quadrant = time.in.zone$n.quadrant * total.time,
			time.in.e.quadrant = time.in.zone$e.quadrant * total.time,
			time.in.s.quadrant = time.in.zone$s.quadrant * total.time,
			time.in.w.quadrant = time.in.zone$w.quadrant * total.time,
			goal.crossings = zone.crossings$goal,
			old.goal.crossings = zone.crossings$old.goal
		)
	)

	# Replace serialised arena zones and add additionals.
	metrics$arena$zones = wrapped.arena.zones
	if(goal.present) metrics$arena$zones$goal.corridor = terra::wrap(goal.corridor)
	
	class(metrics) = "rtrack_metrics"
	return(metrics)
}	