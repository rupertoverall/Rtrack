#' @importFrom terra unwrap vect convHull expanse crds centroids relate distance
#' @importFrom KernSmooth bkde2D
#' @importFrom stats quantile
#' @importFrom utils globalVariables
utils::globalVariables(c("velocity.quantile", "d.centroid.quantile", "d.origin.quantile", "d.wall.quantile", "d.corner.quantile", "d.centre.quantile" ))

calculate_oft_metrics = function(path, arena){
	# Deserialise arena zones, but retain wrapped version to return.
	wrapped.arena.zones = arena$zones
	arena$zones = lapply(arena$zones, terra::unwrap)
	
	# Area covered by the path as a fraction of the total arena
	path.values = cbind(x = path$x, y = path$y)
	pathpoints = terra::vect(path.values, type = "points", crs = "local") # With closed path.
	pathpoly = terra::vect(path.values, type = "polygons", crs = "local")
	coverage = terra::expanse(terra::convHull(pathpoly), transform = FALSE) / terra::expanse(arena$zones$field, transform = FALSE) # Arena circle is actually a 360-point polygon approximation of a circle (area < pi r^2).
	path.centroid = terra::crds(terra::centroids(pathpoly))
	d.centroid = ((path$x - path.centroid[1]) ^ 2 + (path$y - path.centroid[2]) ^ 2) ^ .5
	d.origin = ((path$x) ^ 2 + (path$y) ^ 2) ^ .5

	in.zones = lapply(arena$zones, function(zone) as.logical(rowSums(terra::relate(pathpoints, zone, relation = "intersects"))) )
	
	# Distance from wall, corner
	d.wall = terra::distance(arena$zones$wall, pathpoints)
	d.corner = terra::distance(arena$zones$corner, pathpoints)
	d.centre = terra::distance(arena$zones$centre, pathpoints)
	
	# Path length
	path.segment.lengths = sqrt(diff(path.values[, "x"]) ^ 2 + diff(path.values[, "y"]) ^ 2)
	path.length = sum(path.segment.lengths)
	cumulative.distance = cumsum(path.segment.lengths)

	# 'Turning' behaviour measured by angular displacement at each timepoint
	dx = path.values[-1, 'x'] - path.values[-nrow(path.values), 'x']
	dy = path.values[-1, 'y'] - path.values[-nrow(path.values), 'y']
	angle.from.xaxis = atan2(dy, dx)
	turning = c(NA, NA, angle.from.xaxis[-1] - angle.from.xaxis[-length(angle.from.xaxis)])

	# Speed in wall, corner, centre zones (total time using normalised units)
	total.time = diff(range(path$t, na.rm = T))
	intervals = diff(path$t)
	negligible = intervals == 0 # This can happen due to rounding precision.
	velocity = path.segment.lengths[!negligible] / intervals[!negligible]
	velocity[is.infinite(velocity) | is.nan(velocity)] = NA
	velocity.in.zone = lapply(in.zones, function(in.zone){
		median(velocity[which(in.zone[!negligible])], na.rm = TRUE)
	})
	
	# Immobility in each zone.
	immobile = c(velocity < 0.001, FALSE) # Additional dummy interval as padding.
	immobility = sum(intervals[which(immobile)], na.rm = TRUE)
	immobility.in.zone = lapply(in.zones, function(in.zone){
		sum(intervals[which(immobile & in.zone[!negligible])], na.rm = TRUE)
	})
	
	# Latency to enter zone
	latency.to.zone = lapply(in.zones, function(in.zone){
		path$t[which(in.zone)[1]] - path$t[1]
	})
	
	# Roaming entropy
	gridsize = 50
	freq = KernSmooth::bkde2D(path.values, range.x = list(c(-1, 1), c(-1, 1)), bandwidth = c(1 / gridsize, 1 / gridsize), gridsize = c(gridsize, gridsize))$fhat
	sequence = seq(-1, 1, length.out = gridsize)
	sum = sum(freq, na.rm = T)
	p = (freq / sum)[freq > 0]
	roaming.entropy = -sum(p * log(p)) / log(length(p))
	
	# Here the time spent in each defined zone is calculated
	time.in.zone = lapply(in.zones, function(in.zone) sum(in.zone, na.rm = T) / length(path$t) )
	zone.crossings = lapply(in.zones, function(in.zone){
		ceiling(length(which(in.zone[-length(in.zone)] != in.zone[-1])) / 2) # Half the transitions between 'in'/'out' are 'crossings'
	})

	velocity.quantile = unname(stats::quantile(velocity, na.rm = TRUE))
	d.centroid.quantile = unname(stats::quantile(d.centroid, na.rm = TRUE))
	d.origin.quantile = unname(stats::quantile(d.origin, na.rm = TRUE))
	d.wall.quantile = unname(stats::quantile(d.wall, na.rm = TRUE))
	d.corner.quantile = unname(stats::quantile(d.corner, na.rm = TRUE))
	d.centre.quantile = unname(stats::quantile(d.centre, na.rm = TRUE))

	metrics = list(
		id = path$id,
		arena = arena,
		area = sapply(arena$zones, terra::expanse, transform = FALSE),
		path = path,
		path.length = path.length,
		velocity = velocity,
		immobile = immobile,
		immobile.x = path$x[immobile],
		immobile.y = path$y[immobile],
		immobility = immobility,
		total.time = total.time,
		coverage = coverage,
		turning = turning,
		d.centroid = d.centroid,
		d.origin = d.origin,
		d.wall = d.wall,
		d.corner = d.corner,
		d.centre = d.centre,
		roaming.entropy = roaming.entropy,
		velocity.in.zone = velocity.in.zone,
		immobility.in.zone = immobility.in.zone,
		latency.to.zone = latency.to.zone,
		time.in.zone = time.in.zone,
		zone.crossings = zone.crossings,
		features = c(
			path.length = path.length,
			total.time = total.time,
			lower.velocity = velocity.quantile[2],
			median.velocity = velocity.quantile[3],
			upper.velocity = velocity.quantile[4],
			immobility = immobility,
			turning = sum(turning, na.rm = T) / total.time,
			absolute.turning = sum(abs(turning), na.rm = T) / total.time,
			coverage = coverage,
			lower.d.centroid = d.centroid.quantile[2],
			median.d.centroid = d.centroid.quantile[3],
			upper.d.centroid = d.centroid.quantile[4],
			lower.d.origin = d.origin.quantile[2],
			median.d.origin = d.origin.quantile[3],
			upper.d.origin = d.origin.quantile[4],
			lower.d.wall = d.wall.quantile[2],
			median.d.wall = d.wall.quantile[3],
			upper.d.wall = d.wall.quantile[4],
			lower.d.corner = d.corner.quantile[2],
			median.d.corner = d.corner.quantile[3],
			upper.d.corner = d.corner.quantile[4],
			lower.d.centre = d.centre.quantile[2],
			median.d.centre = d.centre.quantile[3],
			upper.d.centre = d.centre.quantile[4],
			roaming.entropy = roaming.entropy,
			velocity.in.centre.zone = velocity.in.zone$centre,
			velocity.in.wall.zone = velocity.in.zone$wall,
			velocity.in.corner.zone = velocity.in.zone$corner,
			immobility.in.centre.zone = immobility.in.zone$centre,
			immobility.in.wall.zone = immobility.in.zone$wall,
			immobility.in.corner.zone = immobility.in.zone$corner,
			latency.to.centre.zone = latency.to.zone$centre,
			latency.to.wall.zone = latency.to.zone$wall,
			latency.to.corner.zone = latency.to.zone$corner,
			time.in.centre.zone = time.in.zone$centre,
			time.in.wall.zone = time.in.zone$wall,
			time.in.corner.zone = time.in.zone$corner,
			centre.zone.crossings = zone.crossings$centre,
			wall.zone.crossings = zone.crossings$wall,
			corner.zone.crossings = zone.crossings$corner
		),
		summary = c(
			path.length = path.length * arena$correction$r * arena$correction$d,
			total.time = total.time,
			velocity = velocity.quantile[3] * arena$correction$r * arena$correction$d / arena$correction$t,
			immobility = immobility * arena$correction$t,
			coverage = coverage,
			distance.from.centre = d.centre.quantile[3] * arena$correction$r * arena$correction$d,
			distance.from.wall = d.wall.quantile[3] * arena$correction$r * arena$correction$d,
			distance.from.corner = d.corner.quantile[3] * arena$correction$r * arena$correction$d,
			roaming.entropy = roaming.entropy,
			velocity.in.centre.zone = velocity.in.zone$centre * arena$correction$r * arena$correction$d / arena$correction$t,
			velocity.in.wall.zone = velocity.in.zone$wall * arena$correction$r * arena$correction$d / arena$correction$t,
			velocity.in.corner.zone = velocity.in.zone$corner * arena$correction$r * arena$correction$d / arena$correction$t,
			immobility.in.centre.zone = immobility.in.zone$centre * arena$correction$t,
			immobility.in.wall.zone = immobility.in.zone$wall * arena$correction$t,
			immobility.in.corner.zone = immobility.in.zone$corner * arena$correction$t,
			latency.to.centre.zone = latency.to.zone$centre * arena$correction$t,
			latency.to.wall.zone = latency.to.zone$wall * arena$correction$t,
			latency.to.corner.zone = latency.to.zone$corner * arena$correction$t,
			time.in.centre.zone = time.in.zone$centre * total.time,
			time.in.wall.zone = time.in.zone$wall * total.time,
			time.in.corner.zone = time.in.zone$corner * total.time,
			centre.zone.crossings = zone.crossings$centre,
			wall.zone.crossings = zone.crossings$wall,
			corner.zone.crossings = zone.crossings$corner
		)
	)

	# Replace serialised arena zones.
	metrics$arena$zones = wrapped.arena.zones

	class(metrics) = "rtrack_metrics"
	return(metrics)
}	

