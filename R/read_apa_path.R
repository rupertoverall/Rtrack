#' @importFrom utils globalVariables tail
utils::globalVariables("read_apa_path")
read_apa_path = function(filename, arena, id, track.format, track.index, interpolate, time.bounds){
	path = read_raw_coordinate_data(filename, track.format, track.index)
	path$id = id
	
	# 0. Encode missing points.
	# There is a special case for 'tracker.2.dat' data, which uses 0, 0 to encode missing data points.
	# We set these here to NA.
	fixed.x = path$raw.x
	fixed.y = path$raw.y
	if(track.format == "tracker.2.dat"){
		origin = path$raw.x == 0 & path$raw.y == 0
		fixed.x[origin] = NA
		fixed.y[origin] = NA
	}
	
	# 1. Remove missing points
	missing = is.na(path$raw.t) | is.na(fixed.x) | is.na(fixed.y)
	if(!all(is.na(time.bounds))){ # The user has specified bounds to the recording
		missing = missing | (path$raw.t < time.bounds[1] | path$raw.t > time.bounds[2])
	}
	path$t = path$raw.t[!missing] / arena$correction$t
	path$x = ((fixed.x - arena$correction$x) / arena$correction$r)[!missing]
	path$y = ((fixed.y - arena$correction$y) / arena$correction$r)[!missing]

	if(interpolate){
		if(!all(missing)){ # If track is empty, then interpolation won't help (and will only crash)
			field = terra::unwrap(arena$zones$arena)
			# 2. Remove any points falling outside the arena
			pathpoints = terra::vect(cbind(x = path$x, y = path$y), type = "points", crs = "local")
			clipped = !as.logical(terra::relate(pathpoints, field, relation = "intersects"))
			if(any(clipped)){
				path$t = path$t[!clipped]
				path$x = path$x[!clipped]
				path$y = path$y[!clipped]
				warning(paste0("For '", id, "', some points on the path were outside the arena bounds. These have been removed."))
			}
			# 3. Outlier removal is not done for APA.
			timestep = stats::median(diff(path$t), na.rm = T) 
			new.t = seq(path$t[1], utils::tail(path$t, 1), timestep)
			# 5. Replace missing and clipped points by interpolated/extrapolated values
			path$x = Hmisc::approxExtrap(path$t, path$x, xout = new.t, method = "constant", ties = "ordered")$y
			path$y = Hmisc::approxExtrap(path$t, path$y, xout = new.t, method = "constant", ties = "ordered")$y
			path$t = new.t
			# 6. Fix any overzealous extrapolation by bounding to the arena
			pathpoints = terra::vect(cbind(x = path$x, y = path$y), type = "points", crs = "local")
			clipped = !as.logical(terra::relate(pathpoints, field, relation = "intersects"))
			if(any(clipped)){
				path$x[clipped] = NA
				path$y[clipped] = NA
				path$x = stats::approx(path$t, path$x, xout = new.t, method = "constant", rule = 2, ties = "ordered")$y
				path$y = stats::approx(path$t, path$y, xout = new.t, method = "constant", rule = 2, ties = "ordered")$y
			}
		}
	}
	
	# 7. Invert y-axis for formats reporting pixel position.
	if(track.format %in% c("tracker.2.dat")){
		path$y = path$y * -1
	}

	return(path)
}
		