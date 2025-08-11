#' @importFrom terra unwrap vect convHull expanse relate distance
#' @importFrom KernSmooth bkde2D
#' @importFrom stats quantile
#' @importFrom utils globalVariables
utils::globalVariables(c("velocity.quantile", "d.object.quantile", "d.novel.object.quantile", "d.origin.quantile" ))

calculate_nor_metrics = function(path, arena){
	# Deserialise arena zones, but retain wrapped version to return.
	wrapped.arena.zones = arena$zones
	arena$zones = lapply(arena$zones, terra::unwrap)

	# Check for presence of optional zones (terra cannot handle missing data).
	novel.object.present = arena$novel$object.1 | arena$novel$object.2
	
	# Area covered by the path as a fraction of the total arena
	path.values = cbind(x = path$x, y = path$y)
	pathpoints = terra::vect(path.values, type = "points", crs = "local") # With closed path.
	pathpoly = terra::vect(path.values, type = "polygons", crs = "local")
	coverage = terra::expanse(terra::convHull(pathpoly), transform = FALSE) / terra::expanse(arena$zones$field, transform = FALSE) # Arena circle is actually a 360-point polygon approximation of a circle (area < pi r^2).
	d.origin = ((path$x) ^ 2 + (path$y) ^ 2) ^ .5
	
	in.zones = lapply(arena$zones, function(zone) as.logical(rowSums(terra::relate(pathpoints, zone, relation = "intersects"))) )
	
	# Distance from wall, corner
	d.wall = terra::distance(arena$zones$wall, pathpoints)
	d.corner = terra::distance(arena$zones$corner, pathpoints)
	d.object.1 = terra::distance(arena$zones$object.1, pathpoints)
	d.object.2 = terra::distance(arena$zones$object.2, pathpoints)
	d.object = terra::distance(arena$zones$object, pathpoints)
	if(novel.object.present){
		d.novel.object = terra::distance(arena$zones$novel.object, pathpoints)
	}else{
		d.novel.object = NA
	}

	# Path length
	path.segment.lengths = sqrt(diff(path.values[, "x"]) ^ 2 + diff(path.values[, "y"]) ^ 2)
	path.length = sum(path.segment.lengths)
	cumulative.distance = cumsum(path.segment.lengths)

	# 'Turning' behaviour measured by angular displacement at each timepoint
	dx = path.values[-1, 'x'] - path.values[-nrow(path.values), 'x']
	dy = path.values[-1, 'y'] - path.values[-nrow(path.values), 'y']
	angle.from.xaxis = atan2(dy, dx)
	turning = c(NA, NA, angle.from.xaxis[-1] - angle.from.xaxis[-length(angle.from.xaxis)])

	# Speed in wall, corner, object zones (total time using normalised units)
	total.time =  diff(range(path$t, na.rm = T))
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
	d.object.quantile = unname(stats::quantile(d.object, na.rm = TRUE))
	d.novel.object.quantile = unname(stats::quantile(d.novel.object, na.rm = TRUE))
	d.origin.quantile = unname(stats::quantile(d.origin, na.rm = TRUE))
	d.wall.quantile = unname(stats::quantile(d.wall, na.rm = TRUE))
	d.corner.quantile = unname(stats::quantile(d.corner, na.rm = TRUE))
	
	metrics = list(
		id = path$id,
		arena = arena,
		area = sapply(arena$zones, terra::expanse, transform = FALSE),
		path = path,
		path.length = path.length,
		total.time = total.time,
		velocity = velocity,
		immobile = immobile,
		immobile.x = path$x[immobile],
		immobile.y = path$y[immobile],
		immobility = immobility,
		coverage = coverage,
		d.origin = d.origin,
		d.wall = d.wall,
		d.corner = d.corner,
		d.object.1 = d.object.1,
		d.object.2 = d.object.2,
		d.object = d.object,
		d.novel.object = d.novel.object,
		path.length = path.length,
		turning = turning,
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
			lower.d.origin = d.origin.quantile[2],
			median.d.origin = d.origin.quantile[3],
			upper.d.origin = d.origin.quantile[4],
			lower.d.wall = d.wall.quantile[2],
			median.d.wall = d.wall.quantile[3],
			upper.d.wall = d.wall.quantile[4],
			lower.d.corner = d.corner.quantile[2],
			median.d.corner = d.corner.quantile[3],
			upper.d.corner = d.corner.quantile[4],
			lower.d.object = d.object.quantile[2],
			median.d.object = d.object.quantile[3],
			upper.d.object = d.object.quantile[4],
			lower.d.novel.object = d.novel.object.quantile[2],
			median.d.novel.object = d.novel.object.quantile[3],
			upper.d.novel.object = d.novel.object.quantile[4],
			roaming.entropy = roaming.entropy,
			velocity.in.wall.zone = velocity.in.zone$wall,
			velocity.in.corner.zone = velocity.in.zone$corner,
			velocity.in.object.1.vicinity = velocity.in.zone$object.1.vicinity,
			velocity.in.object.2.vicinity = velocity.in.zone$object.2.vicinity,
			velocity.in.object.vicinity = velocity.in.zone$object.vicinity,
			velocity.in.novel.object.vicinity = ifelse(novel.object.present, velocity.in.zone$novel.object.vicinity, NA),
			immobility.in.wall.zone = immobility.in.zone$wall,
			immobility.in.corner.zone = immobility.in.zone$corner,
			immobility.in.object.1.vicinity = immobility.in.zone$object.1.vicinity,
			immobility.in.object.2.vicinity = immobility.in.zone$object.2.vicinity,
			immobility.in.object.vicinity = immobility.in.zone$object.vicinity,
			immobility.in.novel.object.vicinity = ifelse(novel.object.present, immobility.in.zone$novel.object.vicinity, NA),
			latency.to.wall.zone = latency.to.zone$wall,
			latency.to.corner.zone = latency.to.zone$corner,
			latency.to.object.1.vicinity = latency.to.zone$object.1.vicinity,
			latency.to.object.2.vicinity = latency.to.zone$object.2.vicinity,
			latency.to.object.vicinity = latency.to.zone$object.vicinity,
			latency.to.novel.object.vicinity = ifelse(novel.object.present, latency.to.zone$novel.object.vicinity, NA),
			time.in.wall.zone = time.in.zone$wall,
			time.in.corner.zone = time.in.zone$corner,
			time.in.object.1.vicinity = time.in.zone$object.1.vicinity,
			time.in.object.2.vicinity = time.in.zone$object.2.vicinity,
			time.in.object.vicinity = time.in.zone$object.vicinity,
			time.in.novel.object.vicinity = ifelse(novel.object.present, time.in.zone$novel.object.vicinity, NA),
			wall.zone.crossings = zone.crossings$wall,
			corner.zone.crossings = zone.crossings$corner,
			object.1.vicinity.crossings = zone.crossings$object.1.vicinity,
			object.2.vicinity.crossings = zone.crossings$object.2.vicinity,
			object.vicinity.crossings = zone.crossings$object.vicinity,
			novel.object.vicinity.crossings = ifelse(novel.object.present, zone.crossings$novel.object.vicinity, NA)
		),
		summary = c(
			path.length = path.length * arena$correction$r * arena$correction$d,
			total.time = total.time,
			velocity = velocity.quantile[3] * arena$correction$r * arena$correction$d / arena$correction$t,
			immobility = immobility * arena$correction$t,
			coverage = coverage,
			distance.from.object = median(d.object, na.rm = TRUE) * arena$correction$r * arena$correction$d,
			distance.from.novel.object = median(d.novel.object, na.rm = TRUE) * arena$correction$r * arena$correction$d,
			roaming.entropy = roaming.entropy,
			latency.to.object.vicinity = latency.to.zone$object.vicinity * arena$correction$t,
			latency.to.novel.object.vicinity = ifelse(novel.object.present, latency.to.zone$novel.object.vicinity * arena$correction$t, NA),
			time.in.wall.zone = time.in.zone$wall * total.time,
			time.in.corner.zone = time.in.zone$corner * total.time,
			time.in.object.vicinity = time.in.zone$object.vicinity * total.time,
			time.in.novel.object.vicinity = ifelse(novel.object.present, time.in.zone$novel.object.vicinity * total.time, NA),
			object.vicinity.crossings = zone.crossings$object.vicinity,
			novel.object.vicinity.crossings = ifelse(novel.object.present, zone.crossings$novel.object.vicinity, NA)
		)
	)

	# Replace serialised arena zones.
	metrics$arena$zones = wrapped.arena.zones

	class(metrics) = "rtrack_metrics"
	return(metrics)
}	

