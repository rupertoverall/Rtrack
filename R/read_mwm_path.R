#' @importFrom utils globalVariables tail
utils::globalVariables("read_mwm_path")
read_mwm_path = function(filename, arena, id, track.format, track.index, interpolate, time.bounds){
	path = read_raw_coordinate_data(filename, track.format, track.index)
	path$id = id

	# 1. Remove missing points
	missing = is.na(path$raw.t) | is.na(path$raw.x) | is.na(path$raw.y)
	if(!all(is.na(time.bounds))){ # The user has specified bounds to the recording
		missing = missing | (path$raw.t < time.bounds[1] | path$raw.t > time.bounds[2])
	}
	path$t = path$raw.t[!missing] / arena$correction$t
	path$x = ((path$raw.x - arena$correction$x) / arena$correction$r)[!missing]
	path$y = ((path$raw.y - arena$correction$y) / arena$correction$r)[!missing]
	
	if(interpolate){
		if(!all(missing)){ # If track is empty, then interpolation won't help (and will only crash)
			field = terra::unwrap(arena$zones$pool)
			# 2. Remove any points falling outside the arena
			pathpoints = terra::vect(cbind(x = path$x, y = path$y), type = "points", crs = "local")
			clipped = !as.logical(terra::relate(pathpoints, field, relation = "intersects"))
			if(any(clipped)){
				path$t = path$t[!clipped]
				path$x = path$x[!clipped]
				path$y = path$y[!clipped]
				warning(paste0("For '", id, "', some points on the path were outside the arena bounds. These have been removed."))
			}
			# 3. Outlier removal is no longer done for MWM
			# 4. Check the timestamps for any missing intervals in the raw data
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

	return(path)
}
		