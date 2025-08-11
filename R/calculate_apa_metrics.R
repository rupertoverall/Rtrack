#' @importFrom terra unwrap vect convHull expanse crds centroids relate
#' @importFrom KernSmooth bkde2D
#' @importFrom stats quantile
#' @importFrom utils globalVariables
utils::globalVariables(c("velocity.quantile", "immobility.quantile", "angle.from.aversive.zone.quantile"))

calculate_apa_metrics = function(path, arena){
	# Deserialise arena zones, but retain wrapped version to return.
	wrapped.arena.zones = arena$zones
	arena$zones = lapply(arena$zones, terra::unwrap)
	
	# Check for presence of optional zones (terra cannot handle missing data).
	aversive.zone.present = !is.null(arena$zones$aversive.zone)
	old.aversive.zone.present = !is.null(arena$zones$old.aversive.zone)

	# APA has two frames of reference. Calculate local (subject) frame coordinates.
	rotated.angle = arena$correction$e$arena.rotation * (path$t - path$t[1]) # Rotated angle changes over time.
	rotated.points = spin(path$x, path$y, deg2rad(-rotated.angle))
	path$local.x = rotated.points$x
	path$local.y = rotated.points$y

	# Movement metrics are calculated in either the local frame (of the subject) or the global frame (of the room).
	# Area covered by the swim path as a fraction of the total arena
	local.path.values = cbind(x = path$local.x, y = path$local.y)
	local.pathpoints = terra::vect(local.path.values, type = "points", crs = "local") # With closed path.
	global.path.values = cbind(x = path$x, y = path$y)
	global.pathpoints = terra::vect(global.path.values, type = "points", crs = "local") # With closed path.
	pathpoly = terra::vect(local.path.values, type = "polygons", crs = "local")
	coverage = terra::expanse(terra::convHull(pathpoly), transform = FALSE) / terra::expanse(arena$zones$arena, transform = FALSE) # Arena circle is actually a 360-point polygon approximation of a circle (area < pi r^2).
	path.centroid = terra::crds(terra::centroids(pathpoly))
	d.centroid = ((path$x - path.centroid[1]) ^ 2 + (path$y - path.centroid[2]) ^ 2) ^ .5
	d.origin = ((path$x) ^ 2 + (path$y) ^ 2) ^ .5

	in.zones = lapply(arena$zones, function(zone) as.logical(rowSums(terra::relate(local.pathpoints, zone, relation = "intersects"))) )
	if(aversive.zone.present) in.zones$aversive.zone = as.logical(rowSums(terra::relate(global.pathpoints, arena$zones$aversive.zone, relation = "intersects"))) # In GLOBAL frame.
	if(old.aversive.zone.present) in.zones$old.aversive.zone = as.logical(rowSums(terra::relate(global.pathpoints, arena$zones$old.aversive.zone, relation = "intersects"))) # In GLOBAL frame.
	
	# Path length
	path.segment.lengths = sqrt(diff(local.path.values[, "x"]) ^ 2 + diff(local.path.values[, "y"]) ^ 2)
	path.length = sum(path.segment.lengths)

	# 'Turning' behaviour measured by angular displacement at each timepoint
	dx = diff(local.path.values[, "x"])
	dy = diff(local.path.values[, "y"])
	angle.from.xaxis = atan2(dy, dx)
	turning = diff(angle.from.xaxis)

	# Angle from aversive zone.
	x = global.path.values[, "x"]
	y = global.path.values[, "y"]
	angle.from.xaxis = ((atan2(y, x) * sign(arena$correction$e$arena.rotation) + pi) / (2 * pi) * 360 + 180) %% 360
	angle.from.aversive.zone = NA
	if(aversive.zone.present){
		aversive.zone.width = ((arena$aversive.zone$end.angle + 360) - arena$aversive.zone$start.angle) %% 360
		angle.from.aversive.zone = 0
		if(sign(arena$correction$e$arena.rotation) == -1){
			angle.from.aversive.zone = ((angle.from.xaxis + 90 + 360 - arena$aversive.zone$start.angle) %% 360) - aversive.zone.width
			angle.from.aversive.zone[angle.from.aversive.zone < 0] = 0 # Inside aversive zone = zero distance.
		}else{
			angle.from.aversive.zone = ((arena$aversive.zone$end.angle + 360 - angle.from.xaxis + 90) %% 360) - aversive.zone.width
			angle.from.aversive.zone[angle.from.aversive.zone < 0] = 0 # Inside aversive zone = zero distance.
		}
	}
	angle.from.old.aversive.zone = NA
	if(old.aversive.zone.present){
		old.aversive.zone.width = ((arena$old.aversive.zone$end.angle + 360) - arena$old.aversive.zone$start.angle) %% 360
		angle.from.old.aversive.zone = 0
		if(sign(arena$correction$e$arena.rotation) == -1){
			angle.from.old.aversive.zone = ((angle.from.xaxis + 90 + 360 - arena$old.aversive.zone$start.angle) %% 360) - old.aversive.zone.width
			angle.from.old.aversive.zone[angle.from.old.aversive.zone < 0] = 0 # Inside aversive zone = zero distance.
		}else{
			angle.from.old.aversive.zone = ((arena$old.aversive.zone$end.angle + 360 - angle.from.xaxis + 90) %% 360) - old.aversive.zone.width
			angle.from.old.aversive.zone[angle.from.old.aversive.zone < 0] = 0 # Inside aversive zone = zero distance.
		}
	}
	
	# Speed in zones (total time using normalised units)
	total.time = diff(range(path$t, na.rm = T))
	intervals = diff(path$t)
	negligible = intervals == 0 # This can happen due to rounding precision.
	velocity = path.segment.lengths[!negligible] / intervals[!negligible]
	velocity[is.infinite(velocity) | is.nan(velocity)] = NA
	velocity.in.zone = lapply(in.zones, function(in.zone){
		median(velocity[which(in.zone[!negligible])], na.rm = TRUE)
	})
	if(!aversive.zone.present) velocity.in.zone$aversive.zone = NA
	if(!old.aversive.zone.present) velocity.in.zone$old.aversive.zone = NA
	
	# Immobility in each zone.
	immobile = c(velocity < 0.001, FALSE) # Additional dummy interval as padding.
	immobility = sum(intervals[which(immobile)], na.rm = TRUE)
	immobility.in.zone = lapply(in.zones, function(in.zone){
		sum(intervals[which(immobile & in.zone[!negligible])], na.rm = TRUE)
	})
	if(!aversive.zone.present) immobility.in.zone$aversive.zone = NA
	if(!old.aversive.zone.present) immobility.in.zone$old.aversive.zone = NA
	
	# Latency to enter zone
	latency.to.zone = lapply(in.zones, function(in.zone){
		path$t[which(in.zone)[1]] - path$t[1]
	})
	if(!aversive.zone.present) latency.to.zone$aversive.zone = NA
	if(!old.aversive.zone.present) latency.to.zone$old.aversive.zone = NA
	
	# Roaming entropy
	gridsize = 50
	freq = KernSmooth::bkde2D(local.path.values, range.x = list(c(-1, 1), c(-1, 1)), bandwidth = c(1 / gridsize, 1 / gridsize), gridsize = c(gridsize, gridsize))$fhat
	sequence = seq(-1, 1, length.out = gridsize)
	grid = terra::vect(cbind(x = rep(sequence, times = gridsize), y = rep(sequence, each = gridsize)), type = "points")
	sum = sum(freq, na.rm = T)
	p = (freq / sum)[freq > 0]
	roaming.entropy = -sum(p * log(p)) / log(length(p))
	
	# Here the time spent in each defined zone is calculated
	time.in.zone = lapply(in.zones, function(in.zone) sum(in.zone, na.rm = T) / length(path$t) )
	if(!aversive.zone.present) time.in.zone$aversive.zone = NA
	if(!old.aversive.zone.present) time.in.zone$old.aversive.zone = NA
	zone.crossings = lapply(in.zones, function(in.zone){
		ceiling(length(which(in.zone[-length(in.zone)] != in.zone[-1])) / 2) # Half the transitions between 'in'/'out' are 'crossings'
	})
	if(!aversive.zone.present) zone.crossings$aversive.zone = NA
	if(!old.aversive.zone.present) zone.crossings$old.aversive.zone = NA
	
	velocity.quantile = unname(stats::quantile(velocity, na.rm = TRUE))
	immobility.quantile = unname(stats::quantile(immobility, na.rm = TRUE))
	angle.from.aversive.zone.quantile = rep(NA, 5)
	if(!is.null(arena$zones$aversive.zone)) angle.from.aversive.zone.quantile = unname(stats::quantile(angle.from.aversive.zone, na.rm = TRUE))
	angle.from.old.aversive.zone.quantile = rep(NA, 5)
	if(!is.null(arena$zones$old.aversive.zone)) angle.from.old.aversive.zone.quantile = unname(stats::quantile(angle.from.old.aversive.zone, na.rm = TRUE))
	
	metrics = list(
		id = path$id,
		arena = arena,
		area = sapply(arena$zones, terra::expanse, transform = FALSE),
		path = path, # This path has local coordinates embedded (use for plotting).
		path.length = path.length,
		velocity = velocity,
		immobile = immobile,
		immobile.x = path$x[immobile],
		immobile.y = path$y[immobile],
		immobility = immobility,
		total.time = total.time,
		angle.from.aversive.zone = angle.from.aversive.zone,
		angle.from.old.aversive.zone = angle.from.old.aversive.zone,
		goal.crossings = zone.crossings$aversive.zone,
		coverage = coverage,
		roaming.entropy = roaming.entropy,
		velocity.in.zone = velocity.in.zone,
		immobility.in.zone = immobility.in.zone,
		latency.to.zone = latency.to.zone,
		time.in.zone = time.in.zone,
		zone.crossings = zone.crossings,
		features = c(
			path.length = path.length,
			lower.velocity = velocity.quantile[2],
			median.velocity = velocity.quantile[3],
			upper.velocity = velocity.quantile[4],
			lower.immobility = immobility.quantile[2],
			median.immobility = immobility.quantile[3],
			upper.immobility = immobility.quantile[4],
			coverage = coverage,
			turning = sum(turning, na.rm = T) / total.time,
			absolute.turning = sum(abs(turning), na.rm = T) / total.time,
			lower.angle.from.aversive.zone = angle.from.aversive.zone.quantile[2],
			median.angle.from.aversive.zone = angle.from.aversive.zone.quantile[3],
			upper.angle.from.aversive.zone = angle.from.aversive.zone.quantile[4],
			lower.angle.from.old.aversive.zone = angle.from.old.aversive.zone.quantile[2],
			median.angle.from.old.aversive.zone = angle.from.old.aversive.zone.quantile[3],
			upper.angle.from.old.aversive.zone = angle.from.old.aversive.zone.quantile[4],
			roaming.entropy = roaming.entropy,
			velocity.in.centre.zone = velocity.in.zone$centre,
			velocity.in.wall.zone = velocity.in.zone$wall,
			velocity.in.aversive.zone =  ifelse(!is.null(velocity.in.zone$aversive.zone), velocity.in.zone$aversive.zone, NA),
			velocity.in.old.aversive.zone = ifelse(!is.null(velocity.in.zone$old.aversive.zone), velocity.in.zone$old.aversive.zone, NA),
			immobility.in.centre.zone = immobility.in.zone$centre,
			immobility.in.wall.zone = immobility.in.zone$wall,
			immobility.in.aversive.zone =  ifelse(!is.null(immobility.in.zone$aversive.zone), immobility.in.zone$aversive.zone, NA),
			immobility.in.old.aversive.zone = ifelse(!is.null(immobility.in.zone$old.aversive.zone), immobility.in.zone$old.aversive.zone, NA),
			latency.to.centre.zone = latency.to.zone$centre,
			latency.to.wall.zone = latency.to.zone$wall,
			latency.to.aversive.zone =  ifelse(!is.null(latency.to.zone$aversive.zone), latency.to.zone$aversive.zone, NA),
			latency.to.old.aversive.zone = ifelse(!is.null(latency.to.zone$old.aversive.zone), latency.to.zone$old.aversive.zone, NA),
			time.in.centre.zone = time.in.zone$centre,
			time.in.wall.zone = time.in.zone$wall,
			time.in.aversive.zone = ifelse(!is.null(time.in.zone$aversive.zone), time.in.zone$aversive.zone, NA),
			time.in.old.aversive.zone = ifelse(!is.null(time.in.zone$old.aversive.zone), time.in.zone$old.aversive.zone, NA),
			centre.zone.crossings = zone.crossings$centre,
			wall.zone.crossings = zone.crossings$wall,
			aversive.zone.crossings =  ifelse(!is.null(zone.crossings$aversive.zone), zone.crossings$aversive.zone, NA),
			old.aversive.zone.crossings = ifelse(!is.null(zone.crossings$old.aversive.zone), zone.crossings$old.aversive.zone, NA)
		),
		summary = c(
			path.length = path.length * arena$correction$r * arena$correction$d,
			total.time = total.time,
			velocity = velocity.quantile[3] * arena$correction$r * arena$correction$d / arena$correction$t,
			immobility = immobility * arena$correction$t,
			angle.from.aversive.zone = angle.from.aversive.zone.quantile[3],
			angle.from.old.aversive.zone = angle.from.old.aversive.zone.quantile[3],
			roaming.entropy = roaming.entropy,
			latency.to.aversive.zone = ifelse(!is.null(latency.to.zone$aversive.zone), latency.to.zone$aversive.zone, NA) * arena$correction$t,
			latency.to.old.aversive.zone = ifelse(!is.null(latency.to.zone$old.aversive.zone), latency.to.zone$old.aversive.zone, NA) * arena$correction$t,
			time.in.centre.zone = time.in.zone$centre * total.time,
			time.in.wall.zone = time.in.zone$wall * total.time,
			time.in.aversive.zone = ifelse(!is.null(time.in.zone$aversive.zone), time.in.zone$aversive.zone, NA) * total.time,
			time.in.old.aversive.zone = ifelse(!is.null(time.in.zone$old.aversive.zone), time.in.zone$old.aversive.zone, NA) * total.time,
			aversive.zone.crossings = ifelse(!is.null(zone.crossings$aversive.zone), zone.crossings$aversive.zone, NA),
			old.aversive.zone.crossings = ifelse(!is.null(zone.crossings$old.aversive.zone), zone.crossings$old.aversive.zone, NA)
		)
	)

	# Replace serialised arena zones and add additionals.
	metrics$arena$zones = wrapped.arena.zones

	class(metrics) = "rtrack_metrics"
	return(metrics)
}	

