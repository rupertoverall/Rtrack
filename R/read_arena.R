#' Read an arena description.
#'
#' The user will normally not need to call this function directly. Use
#' \code{\link{read_experiment}} instead.
#'
#' Every path must be accompanied by a description of the "arena". This
#' description includes arena size, goal coordinates etc.{} and is unique for
#' every combination of these (i.e. a different arena description file is
#' required for goal reversal trials).
#'
#' The \code{type} parameter specifies the type of experiment. Current options
#' are \code{mwm} (for Morris water maze), \code{barnes} (Barnes maze),
#' \code{oft} (open field test), \code{nor} (novel object recognition task) and
#' \code{apa} (active place avoidance, also known as the carousel maze). For the
#' water maze, Barnes maze and APA, the pool/arena and goal platforms/holes are
#' restricted to being circular (the square platforms sometimes used for MWM are
#' approximated by a circle of a diameter equal to the width of the square. This
#' is because the rotational orientation of square platforms is seldom recorded
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
#' arena_description <- system.file("extdata", "Arena.txt", package = "Rtrack")
#' arena <- read_arena(arena_description)
#'
#' @importFrom utils read.delim
#' @importFrom tools file_path_sans_ext
#' @importFrom terra vect erase intersect buffer wrap
#'
#' @export
read_arena = function(filename, description = NULL){
	if(is.null(description)){
		description.mat = tryCatch(
			utils::read.delim(filename, header = F, sep = "=", strip.white = T, comment.char = "#", stringsAsFactors = F),
			error = function(e) stop(paste0("Cannot open file '", filename, "'. Is the path to this file correct?"))
		)
		description = as.data.frame(t(description.mat[, 2]), stringsAsFactors = F)
		objects = which(description.mat[, 1] == "object")
		if(length(objects) > 1) description.mat[objects, 1] = paste0(description.mat[objects, 1], 1:length(objects))
		colnames(description) = make.names(description.mat[, 1], unique = T)
		rownames(description) = "value"
	}
	
	# Generic checks. Other experiment type-specific checks are done by each  experiment type block below.
	missing.required = setdiff(c("type", "time.units", "arena.bounds"), colnames(description))
	if(length(missing.required) == 1){
		stop(paste0("One or more arena description files are missing the parameter '", missing.required[1], "'."))
	}else if(length(missing.required) > 1){
		stop(paste0("One or more arena description files are missing the following parameters: '", paste(missing.required, collapse = "', '"), "'."))
	}

	# Convert time.units info into seconds.
	if(description$time.units == "us" | description$time.units == "micros"){
		description$time.units = 1e6
	}else if(description$time.units == "ms"){
		description$time.units = 1000
	}else if(description$time.units == "s"){
		description$time.units = 1
	}else if(description$time.units == "min"){
		description$time.units = 1 / 60
	}else if(description$time.units == "h"){
		description$time.units = 1 / 3600
	}else if(description$time.units == "d"){
		description$time.units = 1 / 86400
	}else if(description$time.units == "y"){
		description$time.units = 1 / 31536000
	}else{
		x = eval(parse(text = description$time.units))
		if(is.numeric(x)){
			description$time.units = x
		}else{
			stop("The 'time.units' parameter is not valid. Please check the documentation for creating the arena file.")
		}
	}
	
	id = ifelse(!is.null(description$name), description$name, tools::file_path_sans_ext(basename(filename)))
	
	arena = NULL
	if(description$type == "mwm"){
		## ----- MWM -----
		raw.pool = unlist(strsplit(description$arena.bounds, "\\s+"))
		if(raw.pool[1] != "circle") stop("The water maze pool must be circular.")
		raw.goal = rep(NA, 4)
		if(!is.null(description$goal)) raw.goal = unlist(strsplit(description$goal, "\\s+"))
		raw.old.goal = rep(NA, 4)
		if(!is.null(description$old.goal)) raw.old.goal = unlist(strsplit(description$old.goal, "\\s+"))
		if(is.null(description$goal) & is.null(description$old.goal)) stop("At least one of 'goal' or 'old.goal' must be provided in the arena description.")
		# Pool dimensions are normalised using 'correction' values. Zero-centred and with a radius of 1.
		# (Paths are normalised to match).
		correction = list(
			t = as.numeric(description$time.units),
			x = as.numeric(raw.pool[2]),
			y =  as.numeric(raw.pool[3]),
			r = as.numeric(raw.pool[4]),
			a = c(as.numeric(description$angle), 0)[1]
		)
		pool = list(x = 0, y = 0, radius = 1, shape = "circle") # Defined as unit size.
		if(!is.null(description$goal)){
				goal = list(
				shape = as.character(raw.goal[1]),
				x = (as.numeric(raw.goal[2]) - correction$x) / correction$r, 
				y = (as.numeric(raw.goal[3]) - correction$y) / correction$r, 
				radius = as.numeric(raw.goal[4]) / correction$r
			)
			if(correction$a != 0){
				spun = spin(goal$x, goal$y, deg2rad(correction$a))
				goal$x = spun$x
				goal$y = spun$y
			}
		}
		if(!is.null(description$old.goal)){
			old.goal = list(
				shape = as.character(raw.old.goal[1]),
				x = (as.numeric(raw.old.goal[2]) - correction$x) / correction$r, 
				y = (as.numeric(raw.old.goal[3]) - correction$y) / correction$r, 
				radius = as.numeric(raw.old.goal[4]) / correction$r
			)
			if(correction$a != 0){
				spun = spin(old.goal$x, old.goal$y, deg2rad(correction$a))
				old.goal$x = spun$x
				old.goal$y = spun$y
			}
		}

		# Annulus is ring defined by goal bounds.
		wall.radius = pool$radius * 0.8 # Wall zone width is 10 % of pool diameter.
		annulus.outer.radius = NULL
		annulus.inner.radius = NULL
		if(!is.null(description$goal)){
			goal.centre.radius = sqrt(goal$x ^ 2 + goal$y ^ 2)
			annulus.outer.radius = goal.centre.radius + goal$radius
			annulus.inner.radius = goal.centre.radius - goal$radius
		}else if(!is.null(description$old.goal)){
			goal.centre.radius = sqrt(old.goal$x ^ 2 + old.goal$y ^ 2) # ...the annulus is defined on the old goal.
			annulus.outer.radius = goal.centre.radius + old.goal$radius
			annulus.inner.radius = goal.centre.radius - old.goal$radius
		}	
		wall = list(x = 0, y = 0, outer.radius = pool$radius, inner.radius = wall.radius, shape = "circle")
		far.wall = list(x = 0, y = 0, outer.radius = wall$inner.radius, inner.radius = annulus.outer.radius, shape = "circle")
		annulus = list(x = 0, y = 0, outer.radius = annulus.outer.radius, inner.radius = annulus.inner.radius, shape = "circle")
		arena = list(id = id, type = as.character(description$type), description = description, correction = correction, pool = pool)
		if(!is.null(description$goal)) arena$goal = goal
		if(!is.null(description$old.goal)) arena$old.goal = old.goal

		# Create polygons representing arena components.
		arena$zones = list()
		arena$zones$pool = terra::vect(circle(pool$x, pool$y, pool$radius), type = "polygons", crs = "local")
		arena$zones$wall = terra::erase(
			terra::vect(circle(wall$x, wall$y, wall$outer.radius), type = "polygons", crs = "local"),
			terra::vect(circle(wall$x, wall$y, wall$inner.radius), type = "polygons", crs = "local")
		)
		arena$zones$far.wall = terra::erase(
			terra::vect(circle(far.wall$x, far.wall$y, far.wall$outer.radius), type = "polygons", crs = "local"),
			terra::vect(circle(far.wall$x, far.wall$y, far.wall$inner.radius), type = "polygons", crs = "local")
		)
		arena$zones$annulus = terra::erase(
			terra::vect(circle(annulus$x, annulus$y, annulus$outer.radius), type = "polygons", crs = "local"),
			terra::vect(circle(annulus$x, annulus$y, annulus$inner.radius), type = "polygons", crs = "local")
		)
		if(!is.null(description$goal)){
			arena$zones$goal = terra::vect(circle(goal$x, goal$y, goal$radius), type = "polygons", crs = "local")
		}
		if(!is.null(description$old.goal)){
			arena$zones$old.goal = terra::vect(circle(old.goal$x, old.goal$y, old.goal$radius), type = "polygons", crs = "local")
		}
	
		# Quadrants are defined such that goal (or old goal if goal absent) is in centre of north quadrant.
		goal.angle = NA
		if(!is.null(description$goal)) goal.angle = atan(goal$x / goal$y)
		if(!is.null(description$old.goal)) goal.angle = atan(old.goal$x / old.goal$y)
		if(is.na(goal.angle)) goal.angle = 0 # Can happen if goal is exactly in centre of arena and no old goal present.
		n.point = c(x = sin(goal.angle - pi * 0.25), y = cos(goal.angle - pi * 0.25))
		e.point = c(x = sin(goal.angle + pi * 0.25), y = cos(goal.angle + pi * 0.25))
		s.point = c(x = sin(goal.angle + pi * 0.75), y = cos(goal.angle + pi * 0.75))
		w.point = c(x = sin(goal.angle - pi * 0.75), y = cos(goal.angle - pi * 0.75))
		arena$zones$n.quadrant = terra::intersect(arena$zones$pool, terra::vect(cbind(x = c(0, n.point["x"], e.point["x"] + n.point["x"], e.point["x"], 0), y = c(0, n.point["y"], e.point["y"] + n.point["y"], e.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$e.quadrant = terra::intersect(arena$zones$pool, terra::vect(cbind(x = c(0, e.point["x"], s.point["x"] + e.point["x"], s.point["x"], 0), y = c(0, e.point["y"], s.point["y"] + e.point["y"], s.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$s.quadrant = terra::intersect(arena$zones$pool, terra::vect(cbind(x = c(0, s.point["x"], w.point["x"] + s.point["x"], w.point["x"], 0), y = c(0, s.point["y"], w.point["y"] + s.point["y"], w.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$w.quadrant = terra::intersect(arena$zones$pool, terra::vect(cbind(x = c(0, w.point["x"], n.point["x"] + w.point["x"], n.point["x"], 0), y = c(0, w.point["y"], n.point["y"] + w.point["y"], n.point["y"], 0)), type = "polygons", crs = "local"))
	}else	if(description$type == "barnes"){
		## ----- Barnes maze -----
		raw.field = unlist(strsplit(description$arena.bounds, "\\s+"))
		if(raw.field[1] != "circle") stop("The Barnes maze arena must be circular.")
		raw.goal = rep(NA, 4)
		if(!is.null(description$goal)) raw.goal = unlist(strsplit(description$goal, "\\s+"))
		raw.old.goal = rep(NA, 4)
		if(!is.null(description$old.goal)) raw.old.goal = unlist(strsplit(description$old.goal, "\\s+"))
		if(all(is.na(raw.goal)) & all(is.na(raw.old.goal))) stop("At least one of 'goal' or 'old.goal' must be provided in the arena description.")
		hole.definitions = description[, grep("^hole[^s]?", colnames(description))]
		n.holes = 0
		if(!is.null(dim(hole.definitions))){
			n.holes = ncol(hole.definitions)
			if(!is.null(description$holes)) warning("Only use the 'holes' OR the 'hole' parameter. Both are present, so the individual 'hole' definitions will be used (and the 'holes' definition will be ignored).")
		}else{ # No hole defined. Use the autogenerate function.
			if(!is.null(description$holes)){
				n.holes = as.numeric(unlist(strsplit(description$holes, "\\s+"))[1]) # The number of holes to generate.
			}else{
				stop("The holes must be defined. Use either the 'hole' keyword to define each hole, or the 'holes' parameter to autogenerate them based on the position of one hole.")
			}
		}

		# Arena dimensions are normalised using 'correction' values. Zero-centred and with a radius of 1.
		# (Paths are normalised to match).
		correction = list(
			t = as.numeric(description$time.units),
			x = as.numeric(raw.field[2]),
			y =  as.numeric(raw.field[3]),
			r = as.numeric(raw.field[4]),
			a = c(as.numeric(description$angle), 0)[1],
			e = list(
				n.holes = as.numeric(n.holes)
			)
		)
		# The region 'field' is renamed to 'arena' in zones list below
		field = list(x = 0, y = 0, radius = 1, shape = "circle") # Defined as unit size.
		if(!is.null(description$goal)) {
			goal = list(
				shape = as.character(raw.goal[1]),
				x = (as.numeric(raw.goal[2]) - correction$x) / correction$r, 
				y = (as.numeric(raw.goal[3]) - correction$y) / correction$r, 
				radius = as.numeric(raw.goal[4]) / correction$r
			)
			if(correction$a != 0){
				spun = spin(goal$x, goal$y, deg2rad(correction$a))
				goal$x = spun$x
				goal$y = spun$y
			}
		}
		if(!is.null(description$old.goal)) {
			old.goal = list(
				shape = as.character(raw.old.goal[1]),
				x = (as.numeric(raw.old.goal[2]) - correction$x) / correction$r, 
				y = (as.numeric(raw.old.goal[3]) - correction$y) / correction$r, 
				radius = as.numeric(raw.old.goal[4]) / correction$r
			)
			if(correction$a != 0){
				spun = spin(goal$x, goal$y, deg2rad(correction$a))
				goal$x = spun$x
				goal$y = spun$y
			}
		}
		
		centre.radius = field$radius * 0.2 # Centre zone radius is 10 % of arena diameter.
		# Set up holes.
		hole.centre.x = rep(NA, n.holes)
		hole.centre.y = rep(NA, n.holes)
		hole.radius = rep(NA, n.holes)
		if(!is.null(dim(hole.definitions))){ # Holes are individually defined.
			for(i in seq_along(hole.definitions)){
				def = unlist(strsplit(hole.definitions[, i], "\\s+"))
				hole.centre.x[i] = (as.numeric(def[2]) - correction$x) / correction$r
				hole.centre.y[i] = (as.numeric(def[3]) - correction$y) / correction$r
				hole.radius[i] = as.numeric(def[4]) / correction$r
				if(correction$a != 0){
					spun = spin(hole.centre.x[i], hole.centre.y[i], deg2rad(correction$a))
					hole.centre.x[i] = spun$x
					hole.centre.y[i] = spun$y
				}
			}
		}else{ # 'n.holes' equally-spaced holes need to be generated.
			def = unlist(strsplit(description$holes, "\\s+"))[-1]
			hole.centre.x = (as.numeric(def[2]) - correction$x) / correction$r
			hole.centre.y = (as.numeric(def[3]) - correction$y) / correction$r
			hole.radius = as.numeric(def[4]) / correction$r
			hole.centre.angle = pi / 2 - atan2(hole.centre.y, hole.centre.x) # Shift 0 to the top of the vertical.
			hole.centre.radius = sqrt(hole.centre.x ^ 2 + hole.centre.y ^ 2)
			# Calculate centres of holes.
			hole.centre.angles = hole.centre.angle + ((2 * pi) / n.holes) * (seq_len(n.holes) - 1)
			hole.centre.x = hole.centre.radius * sin(hole.centre.angles)
			hole.centre.y = hole.centre.radius * cos(hole.centre.angles)
			hole.radius = rep(hole.radius, n.holes)
			if(correction$a != 0){
				spun = spin(hole.centre.x[i], hole.centre.y[i], deg2rad(correction$a))
				hole.centre.x[i] = spun$x
				hole.centre.y[i] = spun$y
			}
		}
		# The annulus is a ring centred on the arena centre and containing  all holes plus a 5 % buffer either side (clipped by the arena bounds of course).
		hole.centre.radius = sqrt(hole.centre.x ^ 2 + hole.centre.y ^ 2)
		annulus.outer.radius = max(hole.centre.radius) + (max(hole.radius) + field$radius * 0.05)
		annulus.inner.radius = min(hole.centre.radius) - (max(hole.radius) + field$radius * 0.05)
		
		centre = list(x = 0, y = 0, radius = centre.radius, shape = "circle")
		annulus = list(x = 0, y = 0, outer.radius = annulus.outer.radius, inner.radius = annulus.inner.radius, shape = "circle")
		arena = list(id = id, type = as.character(description$type), description = description, correction = correction, arena = field)
		if(!is.null(description$goal)) arena$goal = goal
		if(!is.null(description$old.goal)) arena$old.goal = old.goal
		
		# Create polygons representing arena components
		arena$zones = list()
		arena$zones$arena = terra::vect(circle(field$x, field$y, field$radius), type = "polygons", crs = "local")
		arena$zones$centre = terra::vect(circle(centre$x, centre$y, centre$radius), type = "polygons", crs = "local")
		arena$zones$annulus = terra::intersect(
			terra::erase(
				terra::vect(circle(annulus$x, annulus$y, annulus$outer.radius), type = "polygons", crs = "local"),
				terra::vect(circle(annulus$x, annulus$y, annulus$inner.radius), type = "polygons", crs = "local")
			),
			arena$zones$arena # Clip to arena extent.
		)
		# The 'vicinity' buffer is added because animals do not _enter_ the holes, but rather investigate from the edge and centre-of-mass tracking will place them outside the actual target.
		if(!is.null(description$goal)){
			arena$zones$goal = terra::vect(circle(goal$x, goal$y, goal$radius), type = "polygons", crs = "local")
			arena$zones$goal.vicinity = terra::buffer(arena$zones$goal, 0.05, quadsegs = 90)
		}
		if(!is.null(description$old.goal)){
			arena$zones$old.goal = terra::vect(circle(old.goal$x, old.goal$y, old.goal$radius), type = "polygons", crs = "local")
			arena$zones$old.goal.vicinity = terra::buffer(arena$zones$old.goal, 0.05, quadsegs = 90)
		}
		# Add individual holes (for counting unique visits) and then combine with the arena.
		holes = setNames(lapply(seq_len(n.holes), function(n){
			terra::vect(circle(hole.centre.x[n], hole.centre.y[n], hole.radius[n]), type = "polygons", crs = "local")
		}), paste0("hole_", seq_len(n.holes)))
		# Add separate vicinities.
		hole.vicinities = setNames(lapply(seq_len(n.holes), function(n){
			terra::buffer(holes[[n]], 0.05, quadsegs = 90)
		}), paste0("hole_", seq_len(n.holes), "_vicinity"))
		arena$zones = c(arena$zones, holes, hole.vicinities)
		arena$zones$hole.vicinity = terra::buffer(Reduce(terra::union, holes), 0.05, quadsegs = 90)
		# Add special zones for goal-adjacent holes.
		if(!is.null(description$goal)){
			adjacency.to.goal = order(sqrt((hole.centre.x - goal$x) ^ 2 + (hole.centre.y - goal$y) ^ 2), decreasing = FALSE)
			arena$zones$goal.adjacent = terra::union(arena$zones$hole.vicinity[adjacency.to.goal[2]], arena$zones$hole.vicinity[adjacency.to.goal[3]]) # Most adjacent is goal itself.
		}
		if(!is.null(description$old.goal)){
			adjacency.to.old.goal = order(sqrt((hole.centre.x - old.goal$x) ^ 2 + (hole.centre.y - old.goal$y) ^ 2), decreasing = FALSE)
			arena$zones$old.goal.adjacent = terra::union(arena$zones$hole.vicinity[adjacency.to.old.goal[2]], arena$zones$hole.vicinity[adjacency.to.old.goal[3]]) # Most adjacent is goal itself.
		}
		
		# Quadrants are defined such that goal (or old goal if goal absent) is in centre of the north quadrant.
		goal.angle = NA
		if(!is.null(description$goal)) goal.angle = atan(goal$x / goal$y)
		if(!is.null(description$old.goal)) goal.angle = atan(old.goal$x / old.goal$y)
		if(is.na(goal.angle)) goal.angle = 0 # Can happen if goal is exactly in centre of arena and no old goal present.
		n.point = c(x = sin(goal.angle - pi * 0.25), y = cos(goal.angle - pi * 0.25))
		e.point = c(x = sin(goal.angle + pi * 0.25), y = cos(goal.angle + pi * 0.25))
		s.point = c(x = sin(goal.angle + pi * 0.75), y = cos(goal.angle + pi * 0.75))
		w.point = c(x = sin(goal.angle - pi * 0.75), y = cos(goal.angle - pi * 0.75))
		arena$zones$n.quadrant = terra::intersect(arena$zones$arena, terra::vect(cbind(x = c(0, n.point["x"], e.point["x"] + n.point["x"], e.point["x"], 0), y = c(0, n.point["y"], e.point["y"] + n.point["y"], e.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$e.quadrant = terra::intersect(arena$zones$arena, terra::vect(cbind(x = c(0, e.point["x"], s.point["x"] + e.point["x"], s.point["x"], 0), y = c(0, e.point["y"], s.point["y"] + e.point["y"], s.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$s.quadrant = terra::intersect(arena$zones$arena, terra::vect(cbind(x = c(0, s.point["x"], w.point["x"] + s.point["x"], w.point["x"], 0), y = c(0, s.point["y"], w.point["y"] + s.point["y"], w.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$w.quadrant = terra::intersect(arena$zones$arena, terra::vect(cbind(x = c(0, w.point["x"], n.point["x"] + w.point["x"], n.point["x"], 0), y = c(0, w.point["y"], n.point["y"] + w.point["y"], n.point["y"], 0)), type = "polygons", crs = "local"))
	}else	if(description$type == "oft"){
		## ----- OFT -----
		raw.field = unlist(strsplit(description$arena.bounds, "\\s+"))
		if(raw.field[1] != "square") stop("The open field arena must be square.")
		# Coordinates will be converted into a consistent square polygon.
		raw.coordinates = NULL
		if(raw.field[1] == "square"){
			if(length(raw.field) == 4){
				raw.coordinates = square(raw.field[2], raw.field[3], raw.field[4])
				transform.model = standardise_square(raw.coordinates)
			}else if(length(raw.field) == 5){
				raw.coordinates = square(raw.field[2], raw.field[3], raw.field[4], raw.field[5])
				transform.model = standardise_square(raw.coordinates)
			}else if(length(raw.field) == 9){
				raw.coordinates = square(raw.field[2], raw.field[3], raw.field[4], raw.field[5], raw.field[6], raw.field[7], raw.field[8], raw.field[9])
				transform.model = standardise_square(raw.coordinates)
			}else{
				stop("The arena definition is incorrect. Please check the manual for the accepted formats.")
			}
		}else{
			stop("Only square or rectangular arenas are supported at this stage.")
		}
		# (Paths are normalised to match).
		correction = list(
			t = as.numeric(description$time.units),
			x1 = raw.coordinates[1, "x"],
			y1 = raw.coordinates[1, "y"],
			x2 = raw.coordinates[2, "x"],
			y2 = raw.coordinates[2, "y"],
			x3 = raw.coordinates[3, "x"],
			y3 = raw.coordinates[3, "y"],
			x4 = raw.coordinates[4, "x"],
			y4 = raw.coordinates[4, "y"],
			r = standardised_radius_square(raw.coordinates),
			a = c(as.numeric(description$angle), 0)[1],
			model = transform.model
		)
		field = list(x = 0, y = 0, radius = 1, shape = "square") # Defined as unit size ('radius' is 1/2 width)

		# Define zones
		arena = list(id = id, type = as.character(description$type), description = description, correction = correction, field = field)
		centre.radius = field$radius * 0.2 # Centre zone width is 20 % of arena width.
		wall.radius = field$radius * 0.6 # Wall zone width is 20 % of arena width.
		wall = list(x = 0, y = 0, outer.radius = field$radius, inner.radius = wall.radius, shape = "square")
		corner.centre = 1 - ((1 - wall.radius) / 2)
		corner_topleft = list(x = -corner.centre, y = corner.centre, radius = 1 - corner.centre, shape = "square")
		corner_topright = list(x = corner.centre, y = corner.centre, radius = 1 - corner.centre, shape = "square")
		corner_bottomright = list(x = corner.centre, y = -corner.centre, radius = 1 - corner.centre, shape = "square")
		corner_bottomleft = list(x = -corner.centre, y = -corner.centre, radius = 1 - corner.centre, shape = "square")

		# Create polygons representing arena components.
		arena$zones = list()
		arena$zones$field = terra::vect(square(field$x, field$y, field$radius), type = "polygons", crs = "local")
		arena$zones$centre = terra::vect(square(wall$x, wall$y, centre.radius), type = "polygons", crs = "local")
		arena$zones$wall = terra::erase(
			terra::vect(square(wall$x, wall$y, wall$outer.radius), type = "polygons", crs = "local"),
			terra::vect(square(wall$x, wall$y, wall$inner.radius), type = "polygons", crs = "local")
		)
		corner_topleft = terra::vect(square(corner_topleft$x, corner_topleft$y, corner_topleft$radius), type = "polygons", crs = "local")
		corner_topright = terra::vect(square(corner_topright$x, corner_topright$y, corner_topright$radius), type = "polygons", crs = "local")
		corner_bottomright = terra::vect(square(corner_bottomright$x, corner_bottomright$y, corner_bottomright$radius), type = "polygons", crs = "local")
		corner_bottomleft = terra::vect(square(corner_bottomleft$x, corner_bottomleft$y, corner_bottomleft$radius), type = "polygons", crs = "local")
		arena$zones$corner = rbind(corner_topleft, corner_topright, corner_bottomright, corner_bottomleft)
	}else	if(description$type == "nor"){
		## ----- NOR -----
		raw.field = unlist(strsplit(description$arena.bounds, "\\s+"))
		if(raw.field[1] != "square") stop("The novel object recognition task arena must be square.")
		# Coordinates will be converted into a consistent square polygon.
		raw.coordinates = square(raw.field[2], raw.field[3], raw.field[4], raw.field[5], raw.field[6], raw.field[7], raw.field[8], raw.field[9])
		transform.model = standardise_square(raw.coordinates)
		# (Paths are normalised to match).
		correction = list(
			t = as.numeric(description$time.units),
			x1 = raw.coordinates[1, "x"],
			y1 = raw.coordinates[1, "y"],
			x2 = raw.coordinates[2, "x"],
			y2 = raw.coordinates[2, "y"],
			x3 = raw.coordinates[3, "x"],
			y3 = raw.coordinates[3, "y"],
			x4 = raw.coordinates[4, "x"],
			y4 = raw.coordinates[4, "y"],
			r = standardised_radius_square(raw.coordinates),
			a = c(as.numeric(description$angle), 0)[1],
			model = transform.model
		)
		field = list(x = 0, y = 0, radius = 1, shape = "square") # Defined as unit size ('radius' is 1/2 width)

		# Objects
		object.1.is.novel = object.2.is.novel = FALSE
		count.object.1 = sum(!is.null(description$object.1), (!is.null(description$novel.object.1)) * 2)
		if(count.object.1 == 0){
			stop(paste0("At least one of 'object.1' or 'novel.object.1' must be specified. Please check the arena file '", filename, "'."))
		}else if(count.object.1 == 1){ # Old object.
			raw.object.1 = unlist(strsplit(description$object.1, "\\s+"))
			object.1.coordinates = unlist(c(square_transform(as.numeric(raw.object.1[2]), as.numeric(raw.object.1[3]), transform.model),
			square_transform(as.numeric(raw.object.1[4]), as.numeric(raw.object.1[5]), transform.model),
			square_transform(as.numeric(raw.object.1[6]), as.numeric(raw.object.1[7]), transform.model),
			square_transform(as.numeric(raw.object.1[8]), as.numeric(raw.object.1[9]), transform.model)))
		}else if(count.object.1 == 2){ # New object.
			raw.object.1 = unlist(strsplit(description$novel.object.1, "\\s+"))
			object.1.coordinates = unlist(c(square_transform(as.numeric(raw.object.1[2]), as.numeric(raw.object.1[3]), transform.model),
			square_transform(as.numeric(raw.object.1[4]), as.numeric(raw.object.1[5]), transform.model),
			square_transform(as.numeric(raw.object.1[6]), as.numeric(raw.object.1[7]), transform.model),
			square_transform(as.numeric(raw.object.1[8]), as.numeric(raw.object.1[9]), transform.model)))
			object.1.is.novel = TRUE
		}else if(count.object.1 == 3){
			stop(paste0("Only one of 'object.1' or 'novel.object.1' may be specified. Please check the arena file '", filename, "'."))
		}
		count.object.2 = sum(!is.null(description$object.2), (!is.null(description$novel.object.2)) * 2)
		if(count.object.2 == 0){
			stop(paste0("At least one of 'object.2' or 'novel.object.2' must be specified. Please check the arena file '", filename, "'."))
		}else if(count.object.2 == 1){ # Old object.
			raw.object.2 = unlist(strsplit(description$object.2, "\\s+"))
			object.2.coordinates = unlist(c(square_transform(as.numeric(raw.object.2[2]), as.numeric(raw.object.2[3]), transform.model),
			square_transform(as.numeric(raw.object.2[4]), as.numeric(raw.object.2[5]), transform.model),
			square_transform(as.numeric(raw.object.2[6]), as.numeric(raw.object.2[7]), transform.model),
			square_transform(as.numeric(raw.object.2[8]), as.numeric(raw.object.2[9]), transform.model)))
		}else if(count.object.2 == 2){ # New object.
			raw.object.2 = unlist(strsplit(description$novel.object.2, "\\s+"))
			object.2.coordinates = unlist(c(square_transform(as.numeric(raw.object.2[2]), as.numeric(raw.object.2[3]), transform.model),
			square_transform(as.numeric(raw.object.2[4]), as.numeric(raw.object.2[5]), transform.model),
			square_transform(as.numeric(raw.object.2[6]), as.numeric(raw.object.2[7]), transform.model),
			square_transform(as.numeric(raw.object.2[8]), as.numeric(raw.object.2[9]), transform.model)))
			object.2.is.novel = TRUE
		}else if(count.object.2 == 3){
			stop(paste0("Only one of 'object.2' or 'novel.object.2' may be specified. Please check the arena file '", filename, "'."))
		}
		
		# Define zones
		arena = list(id = id, type = as.character(description$type), description = description, correction = correction, field = field)
		wall.radius = field$radius * 0.6 # Wall zone width is 20 % of arena width.
		wall = list(x = 0, y = 0, outer.radius = field$radius, inner.radius = wall.radius, shape = "square")
		corner.centre = 1 - ((1 - wall.radius) / 2)
		corner_topleft = list(x = -corner.centre, y = corner.centre, radius = 1 - corner.centre, shape = "square")
		corner_topright = list(x = corner.centre, y = corner.centre, radius = 1 - corner.centre, shape = "square")
		corner_bottomright = list(x = corner.centre, y = -corner.centre, radius = 1 - corner.centre, shape = "square")
		corner_bottomleft = list(x = -corner.centre, y = -corner.centre, radius = 1 - corner.centre, shape = "square")
		object.vicinity.width = field$radius * 0.2 # Half width of wall zone.
		
		# Create polygons representing arena components.
		arena$zones = list()
		arena$zones$field = terra::vect(square(field$x, field$y, field$radius), type = "polygons", crs = "local")
		arena$zones$wall = terra::erase(
			terra::vect(square(wall$x, wall$y, wall$outer.radius), type = "polygons", crs = "local"),
			terra::vect(square(wall$x, wall$y, wall$inner.radius), type = "polygons", crs = "local")
		)
		corner_topleft = terra::vect(square(corner_topleft$x, corner_topleft$y, corner_topleft$radius), type = "polygons", crs = "local")
		corner_topright = terra::vect(square(corner_topright$x, corner_topright$y, corner_topright$radius), type = "polygons", crs = "local")
		corner_bottomright = terra::vect(square(corner_bottomright$x, corner_bottomright$y, corner_bottomright$radius), type = "polygons", crs = "local")
		corner_bottomleft = terra::vect(square(corner_bottomleft$x, corner_bottomleft$y, corner_bottomleft$radius), type = "polygons", crs = "local")
		arena$zones$corner = rbind(corner_topleft, corner_topright, corner_bottomright, corner_bottomleft)
		if(raw.object.1[1] == "square"){
			if(length(raw.object.1) == 4) object.1.coordinates[3] = as.numeric(raw.object.1[4]) / correction$r # Drop in radius if required.
			arena$zones$object.1 = terra::vect(square(object.1.coordinates[1], object.1.coordinates[2], object.1.coordinates[3], object.1.coordinates[4], object.1.coordinates[5], object.1.coordinates[6], object.1.coordinates[7], object.1.coordinates[8]), type = "polygons", crs = "local")
		}else if(raw.object.1[1] == "circle"){
			arena$zones$object.1 = terra::vect(circle(
				x = object.1.coordinates["x"], 
				y = object.1.coordinates["y"], 
				radius = as.numeric(raw.object.1[4]) / correction$r
			), type = "polygons", crs = "local")
		}else if(raw.object.1[1] == "polygon"){
			arena$zones$object.1 = terra::vect(polygon(object.1.coordinates), type = "polygons", crs = "local")
		}
		if(raw.object.2[1] == "square"){
			if(length(raw.object.2) == 4) object.2.coordinates[3] = as.numeric(raw.object.2[4]) / correction$r # Drop in radius if required.
			arena$zones$object.2 = terra::vect(square(object.2.coordinates[1], object.2.coordinates[2], object.2.coordinates[3], object.2.coordinates[4], object.2.coordinates[5], object.2.coordinates[6], object.2.coordinates[7], object.2.coordinates[8]), type = "polygons", crs = "local")
		}else if(raw.object.2[1] == "circle"){
			arena$zones$object.2 = terra::vect(circle(
				x = object.2.coordinates["x"], 
				y = object.2.coordinates["y"], 
				radius = as.numeric(raw.object.2[4]) / correction$r
			), type = "polygons", crs = "local")
		}else if(raw.object.2[1] == "polygon"){
			arena$zones$object.2 = terra::vect(polygon(object.2.coordinates), type = "polygons", crs = "local")
		}
		# Test that object zones are valid. If not, then the following operations can crash R.
		if(!is.na(sum(range(terra::ext(arena$zones$object.1)))) & !is.na(sum(range(terra::ext(arena$zones$object.2))))){
			## Create a region surrounding the objects
			object.1.vicinity = terra::buffer(arena$zones$object.1, object.vicinity.width, quadsegs = 90)
			object.2.vicinity = terra::buffer(arena$zones$object.2, object.vicinity.width, quadsegs = 90)
			# Erase any overlap between the object buffers and clip to field.
			arena$zones$object.1.vicinity =  terra::intersect(
				terra::erase(
					object.1.vicinity,
					object.2.vicinity
				),
				arena$zones$field
			)
			arena$zones$object.2.vicinity =  terra::intersect(
				terra::erase(
					object.2.vicinity,
					object.1.vicinity
				),
				arena$zones$field
			)
			zone.names = setNames(, names(arena$zones))
			if(!is.null(arena$zones$novel.object.1)) zone.names[c("object.1", "object.1.vicinity")] = c("novel.object.1", "novel.object.1.vicinity")
			if(!is.null(arena$zones$novel.object.2)) zone.names[c("object.2", "object.2.vicinity")] = c("novel.object.2", "novel.object.2.vicinity")
		}else{
			stop("The objects are not correctly entered in the arena description file. Please check the documentation carefully.")
		}
		# Create a new zone including all/any objects.
		arena$zones$object = terra::union(object.1.vicinity, object.2.vicinity)
		arena$zones$object.vicinity = terra::buffer(arena$zones$object, object.vicinity.width, quadsegs = 90)
		#
		# Create a new zone including all/any novel objects. This may be empty.
		if(object.1.is.novel | object.2.is.novel){
			arena$zones$novel.object = switch(
				object.1.is.novel + (object.2.is.novel * 2), 
				arena$zones$object.1,
				arena$zones$object.2,		 
				terra::union(object.1.vicinity, object.2.vicinity)
			)
			arena$zones$novel.object.vicinity = terra::buffer(arena$zones$novel.object, object.vicinity.width, quadsegs = 90)
		}
		#
		arena$novel$object.1 = object.1.is.novel
		arena$novel$object.2 = object.2.is.novel
	}else	if(description$type == "apa"){
		## ----- APA -----
		raw.arena = unlist(strsplit(description$arena.bounds, "\\s+"))
		if(raw.arena[1] != "circle") stop("The active place avoidance arena must be circular.")
		raw.aversive.zone = rep(NA, 4)
		if(!is.null(description$aversive.zone)){
			raw.aversive.zone = unlist(strsplit(description$aversive.zone, "\\s+"))
			if(as.numeric(raw.aversive.zone[3]) <= 0){
				warning("Aversive zone with is not greater than zero so is considered absent.")
				raw.aversive.zone = rep(NA, 4) # Reset to empty if zone description is invalid.
			}
		}
		raw.old.aversive.zone = rep(NA, 4)
		if(!is.null(description$old.aversive.zone)){
			raw.old.aversive.zone = unlist(strsplit(description$old.aversive.zone, "\\s+"))
			if(as.numeric(raw.old.aversive.zone[3]) <= 0){
				warning("Old aversive zone with is not greater than zero so is considered absent.")
				raw.old.aversive.zone = rep(NA, 4) # Reset to empty if zone description is invalid.
			}
		}
		if(all(is.na(raw.aversive.zone)) & all(is.na(raw.old.aversive.zone))) stop("At least one of 'aversive.zone' or 'old.aversive.zone' must be provided in the arena description.")
		raw.reference.rotation = unlist(strsplit(description$arena.rotation, "\\s+"))
		
		# Arena dimensions are normalised using 'correction' values. Zero-centred and with a radius of 1
		# (Paths are normalised to match)
		correction = list(
			t = as.numeric(description$time.units),
			x = as.numeric(raw.arena[2]),
			y =  as.numeric(raw.arena[3]),
			r = as.numeric(raw.arena[4]),
			a = c(as.numeric(description$angle), 0)[1],
			e = list(
				arena.rotation = as.numeric(raw.reference.rotation)
			)
		)
		# The region 'field' is renamed to 'arena' in zones list below
		field = list(x = 0, y = 0, radius = 1, shape = "circle") # Defined as unit size.
		centre.radius = field$radius * 0.2 # Centre zone width is 20 % of arena diameter.
		centre = list(x = 0, y = 0, radius = centre.radius, shape = "circle")
		wall.radius = field$radius * 0.6 # Wall zone width is 20 % of arena diameter.
		wall = list(x = 0, y = 0, outer.radius = field$radius, inner.radius = wall.radius, shape = "circle")
		aversive.centre = as.numeric(raw.aversive.zone[2])
		aversive.width = as.numeric(raw.aversive.zone[3])
		aversive.zone = list(
			shape = as.character(raw.aversive.zone[1]),
			x = 0, 
			y = 0, 
			start.radius = ifelse(length(raw.aversive.zone) == 5, as.numeric(raw.aversive.zone[4]) / correction$r, 0),
			end.radius = ifelse(length(raw.aversive.zone) == 5, as.numeric(raw.aversive.zone[5]) / correction$r, 1),
			start.angle = (aversive.centre - (aversive.width / 2)) %% 360,
			end.angle = (aversive.centre + (aversive.width / 2)) %% 360
		)
		old.aversive.centre = as.numeric(raw.old.aversive.zone[2])
		old.aversive.width = as.numeric(raw.old.aversive.zone[3])
		old.aversive.zone = list(
			shape = as.character(raw.old.aversive.zone[1]),
			x = 0, 
			y = 0, 
			start.radius = ifelse(length(raw.old.aversive.zone) == 5, as.numeric(raw.old.aversive.zone[4]) / correction$r, 0),
			end.radius = ifelse(length(raw.old.aversive.zone) == 5, as.numeric(raw.old.aversive.zone[5]) / correction$r, 1),
			start.angle = (old.aversive.centre - (old.aversive.width / 2)) %% 360,
			end.angle = (old.aversive.centre + (old.aversive.width / 2)) %% 360
		)
		
		# Define zones
		arena = list(id = id, type = as.character(description$type), description = description, correction = correction, arena = field, centre = centre, wall = wall)
		if(!is.null(description$aversive.zone)) arena$aversive.zone = aversive.zone
		if(!is.null(description$old.aversive.zone)) arena$old.aversive.zone = old.aversive.zone

		# Build aversive sector from components
		centre.theta = deg2rad(aversive.zone$start.angle - 90)
		corner.theta = deg2rad(aversive.zone$start.angle)
		pre.sqcentre = c(x = sin(centre.theta), y = cos(centre.theta)) * aversive.zone$end.radius + c(aversive.zone$x, aversive.zone$y) 
		pre.sqcorner = c(x = sin(corner.theta), y = cos(corner.theta)) * aversive.zone$end.radius + c(aversive.zone$x, aversive.zone$y)
		# Square blocking arena after sector
		centre.theta = deg2rad(aversive.zone$end.angle + 90)
		corner.theta = deg2rad(aversive.zone$end.angle)
		post.sqcentre = c(x = sin(centre.theta), y = cos(centre.theta)) * aversive.zone$end.radius + c(aversive.zone$x, aversive.zone$y)
		post.sqcorner = c(x = sin(corner.theta), y = cos(corner.theta)) * aversive.zone$end.radius + c(aversive.zone$x, aversive.zone$y)

		# Build old aversive sector from components
		centre.theta = deg2rad(old.aversive.zone$start.angle - 90)
		corner.theta = deg2rad(old.aversive.zone$start.angle)
		old.pre.sqcentre = c(x = sin(centre.theta), y = cos(centre.theta)) * old.aversive.zone$end.radius + c(old.aversive.zone$x, old.aversive.zone$y) 
		old.pre.sqcorner = c(x = sin(corner.theta), y = cos(corner.theta)) * old.aversive.zone$end.radius + c(old.aversive.zone$x, old.aversive.zone$y)
		# Square blocking arena after sector
		centre.theta = deg2rad(old.aversive.zone$end.angle + 90)
		corner.theta = deg2rad(old.aversive.zone$end.angle)
		old.post.sqcentre = c(x = sin(centre.theta), y = cos(centre.theta)) * old.aversive.zone$end.radius + c(old.aversive.zone$x, old.aversive.zone$y)
		old.post.sqcorner = c(x = sin(corner.theta), y = cos(corner.theta)) * old.aversive.zone$end.radius + c(old.aversive.zone$x, old.aversive.zone$y)
		
		# Create polygons representing arena components
		arena$zones = list()
		arena$zones$arena = terra::vect(circle(field$x, field$y, field$radius), type = "polygons", crs = "local")
		arena$zones$centre = terra::vect(circle(centre$x, centre$y, centre$radius), type = "polygons", crs = "local")
		arena$zones$wall = terra::erase(
			terra::vect(circle(wall$x, wall$y, wall$outer.radius), type = "polygons", crs = "local"),
			terra::vect(circle(wall$x, wall$y, wall$inner.radius), type = "polygons", crs = "local")
		)
		if(!is.null(description$aversive.zone)){
			negative.pre = terra::vect(square(pre.sqcentre['x'], pre.sqcentre['y'], pre.sqcorner['x'], pre.sqcorner['y']), type = "polygons", crs = "local") # Square blocking arena before sector
			negative.post = terra::vect(square(post.sqcentre['x'], post.sqcentre['y'], post.sqcorner['x'], post.sqcorner['y']), type = "polygons", crs = "local") # Square blocking arena after sector
			negative.inner = terra::vect(circle(aversive.zone$x, aversive.zone$y, aversive.zone$start.radius), type = "polygons", crs = "local") # Inner circle
	    negative.outer = terra::vect(circle(aversive.zone$x, aversive.zone$y, aversive.zone$end.radius), type = "polygons", crs = "local") # Outer circle
	    if(aversive.width >= 360){
	    	warning("The aversive zone occupies the entire arena - this is presumably an error.")
	    	arena$zones$aversive.zone = arena$zones$arena
	    }else if(aversive.width > 180){
				arena$zones$aversive.zone = terra::intersect(
					terra::erase(
						arena$zones$arena, 
						terra::intersect(negative.pre, negative.post)
					),
					terra::erase(negative.outer, negative.inner)
				)
	    }else{
	    	arena$zones$aversive.zone = terra::intersect(
	    		terra::erase(
	    			arena$zones$arena, 
	    			rbind(negative.pre, negative.post)
	    		),
	    		terra::erase(negative.outer, negative.inner)
	    	)
	    }
		}
		if(!is.null(description$old.aversive.zone)){
			negative.pre = terra::vect(square(old.pre.sqcentre['x'], old.pre.sqcentre['y'], old.pre.sqcorner['x'], old.pre.sqcorner['y']), type = "polygons", crs = "local") # Square blocking arena before sector
			negative.post = terra::vect(square(old.post.sqcentre['x'], old.post.sqcentre['y'], old.post.sqcorner['x'], old.post.sqcorner['y']), type = "polygons", crs = "local") # Square blocking arena after sector
			negative.inner = terra::vect(circle(old.aversive.zone$x, old.aversive.zone$y, old.aversive.zone$start.radius), type = "polygons", crs = "local") # Inner circle
			negative.outer = terra::vect(circle(old.aversive.zone$x, old.aversive.zone$y, old.aversive.zone$end.radius), type = "polygons", crs = "local") # Outer circle
			if(old.aversive.width >= 360){
				warning("The old aversive zone occupies the entire arena - this is presumably an error.")
				arena$zones$old.aversive.zone = arena$zones$arena
			}else if(old.aversive.width > 180){
				arena$zones$old.aversive.zone = terra::intersect(
					terra::erase(
						arena$zones$arena, 
						terra::intersect(negative.pre, negative.post)
					),
					terra::erase(negative.outer, negative.inner)
				)
			}else{
				arena$zones$old.aversive.zone = terra::intersect(
					terra::erase(
						arena$zones$arena, 
						rbind(negative.pre, negative.post)
					),
					terra::erase(negative.outer, negative.inner)
				)
			}
		}
		
		# Quadrants are defined such that the centre of the aversive zone is in centre of the north quadrant (by definition straight upwards on the page).
		aversive.zone.angle = deg2rad((aversive.zone$start.angle %% 360 + aversive.zone$end.angle %% 360) / 2)
		if(is.na(aversive.zone.angle)) aversive.zone.angle = deg2rad((old.aversive.zone$start.angle %% 360 + old.aversive.zone$end.angle %% 360) / 2)
		n.point = c(x = sin(aversive.zone.angle - pi * 0.25), y = cos(aversive.zone.angle - pi * 0.25))
		e.point = c(x = sin(aversive.zone.angle + pi * 0.25), y = cos(aversive.zone.angle + pi * 0.25))
		s.point = c(x = sin(aversive.zone.angle + pi * 0.75), y = cos(aversive.zone.angle + pi * 0.75))
		w.point = c(x = sin(aversive.zone.angle - pi * 0.75), y = cos(aversive.zone.angle - pi * 0.75))
		arena$zones$n.quadrant = terra::intersect(arena$zones$arena, terra::vect(cbind(x = c(0, n.point["x"], e.point["x"] + n.point["x"], e.point["x"], 0), y = c(0, n.point["y"], e.point["y"] + n.point["y"], e.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$e.quadrant = terra::intersect(arena$zones$arena, terra::vect(cbind(x = c(0, e.point["x"], s.point["x"] + e.point["x"], s.point["x"], 0), y = c(0, e.point["y"], s.point["y"] + e.point["y"], s.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$s.quadrant = terra::intersect(arena$zones$arena, terra::vect(cbind(x = c(0, s.point["x"], w.point["x"] + s.point["x"], w.point["x"], 0), y = c(0, s.point["y"], w.point["y"] + s.point["y"], w.point["y"], 0)), type = "polygons", crs = "local"))
		arena$zones$w.quadrant = terra::intersect(arena$zones$arena, terra::vect(cbind(x = c(0, w.point["x"], n.point["x"] + w.point["x"], n.point["x"], 0), y = c(0, w.point["y"], n.point["y"] + w.point["y"], n.point["y"], 0)), type = "polygons", crs = "local"))
	}else{
		stop("The arena file '", filename, "' does not have a valid 'type' definition.")
	}

	# Serialise terra objects to enable parallelisation.
	arena$zones = lapply(arena$zones, terra::wrap)

	class(arena) = "rtrack_arena"
	return(arena)

}
