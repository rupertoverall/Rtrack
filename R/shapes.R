#' @importFrom stats coef lm
#' @importFrom utils data
#' @import randomForest

deg2rad = function(deg){(deg * pi) / (180)}

circle = function(x, y, radius, res = 360){
	shape = cbind(x = (cos((0:res) * 2 * pi / res) * radius) + x, y = (sin((0:res) * 2 * pi / res) * radius) + y)
	shape[nrow(shape), ] = shape[1, ] # Ensure the polygon is closed
	return(signif(shape, 4))
}

square = function(centre.x, centre.y, corner.x, corner.y = NA, corner.x2 = NA, corner.y2 = NA, corner.x3 = NA, corner.y3 = NA){
	if(is.na(corner.y)){ # This must be a centre/radius definition
		centre.x = as.numeric(centre.x)
		centre.y = as.numeric(centre.y)
		corner.x = as.numeric(corner.x)
		radius = corner.x
		shape = cbind(x = c(centre.x - radius, centre.x + radius, centre.x + radius, centre.x - radius), y = c(centre.y + radius, centre.y + radius, centre.y - radius, centre.y - radius))
		shape = rbind(shape, shape[1, ]) # Ensure the polygon is closed
		colnames(shape) = c("x", "y")
		return(signif(shape, 4))
	}else if(is.na(corner.x2)){ # This must be a centre/corner definition
		centre.x = as.numeric(centre.x)
		centre.y = as.numeric(centre.y)
		corner.x = as.numeric(corner.x)
		corner.y = as.numeric(corner.y)
		radius = sqrt(sum(c(corner.x - centre.x, corner.y - centre.y) ^ 2))
		theta = atan2(corner.y - centre.y, corner.x - centre.x)
		shape = t(sapply(c(pi/2, pi, 1.5 * pi, 2 * pi), function(p) c(cos(theta + p), sin(theta + p))) * radius + c(centre.x, centre.y))
		shape = rbind(shape, shape[1, ]) # Ensure the polygon is closed
		colnames(shape) = c("x", "y")
		return(signif(shape, 4))
	}else{ # Assume a four-corners definition
		raw.corners = cbind(x = as.numeric(c(centre.x, corner.x, corner.x2, corner.x3)), y = as.numeric(c(centre.y, corner.y, corner.y2, corner.y3)))
		# Correct ordering of the input coordinates cannot be guaranteed.
		# This orients the scaled space as best as possible to the orientation and direction of the raw data.
		y.order = order(raw.corners[, "y"])
		bottom.corners = y.order[1:2]
		top.corners = y.order[3:4]
		x.order = order(raw.corners[, "x"])
		left.corners = x.order[1:2]
		right.corners = x.order[3:4]
		shape = raw.corners[c(intersect(top.corners, left.corners), intersect(top.corners, right.corners), intersect(bottom.corners, right.corners), intersect(bottom.corners, left.corners)), ]
		shape = rbind(shape, shape[1, ]) # Ensure the polygon is closed
		colnames(shape) = c("x", "y")
		return(signif(shape, 4))
	}
}

polygon = function(coords){
	if (length(coords) %% 2 != 0) {
		stop("Please provide an even number of coordinates.")
	}

	x_coords = as.numeric(coords[seq(1, length(coords), by = 2)])
	y_coords = as.numeric(coords[seq(2, length(coords), by = 2)])

	raw.corners = cbind(x = x_coords, y = y_coords)

	shape = raw.corners
	shape = rbind(shape, shape[1, ]) # Ensure the polygon is closed
	colnames(shape) = c("x", "y")
	
	# Use convHull to ensure the order of the points yields a convex polygon
	shape = terra::crds(terra::convHull(terra::vect(shape)))

	return(signif(shape, 4))
}

standardise_square = function(raw.corners){
	# Get a model describing the affine transform of a square to the standardised space
	colnames(raw.corners) = c("x", "y")
	corners = cbind(x = c(-1, 1, 1, -1, -1), y = c(1, 1, -1, -1, 1))
	return(stats::lm(corners ~ raw.corners[, "x"] + raw.corners[, "y"]))
}

standardised_radius_square = function(raw.corners){
	# Standardise the raw coordinates, then get 'radius' in averaged coordinate space
	colnames(raw.corners) = c("x", "y")
	corners = cbind(x = c(-1, 1, 1, -1, -1), y = c(1, 1, -1, -1, 1))
	model = stats::lm(raw.corners ~ corners[, "x"] + corners[, "y"])
	x = c(0, -1, 0, 1, 0, 0)
	y = c(0, 0, 1, 0, -1, 0)
	new.x = (x * stats::coef(model)['corners[, "x"]', "x"]) + (y * stats::coef(model)['corners[, "y"]', "x"]) + stats::coef(model)["(Intercept)", "x"]
	new.y = (x * stats::coef(model)['corners[, "x"]', "y"]) + (y * stats::coef(model)['corners[, "y"]', "y"]) + stats::coef(model)["(Intercept)", "y"]
	radius = mean((((new.x - new.x[1]) ^ 2 + (new.y - new.y[1]) ^ 2) ^ .5)[-1])
	return(signif(radius, 4))
}

square_transform = function(x, y, model){
	# Convert a point from raw to standardised space based on an affine model
	new.x = (x * stats::coef(model)['raw.corners[, "x"]', "x"]) + (y * stats::coef(model)['raw.corners[, "y"]', "x"]) + stats::coef(model)["(Intercept)", "x"]
	new.y = (x * stats::coef(model)['raw.corners[, "x"]', "y"]) + (y * stats::coef(model)['raw.corners[, "y"]', "y"]) + stats::coef(model)["(Intercept)", "y"]
	return(list(x = signif(new.x, 4), y = signif(new.y, 4)))
}

rotate = function(x, angle){
	x * matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2, 2)
}

spin = function(x, y, angle){
	data.frame(
		x = x * cos(angle) + y * sin(angle),
		y = -x * sin(angle) + y * cos(angle)
	)
}

transform_object = function(raw.obj, model) {
	# Transform all points of a raw object by calling `square_transform` on all its points

	# Remove the object type to get just the coordinates
	raw.obj.coords = raw.obj[-1]

	# If specifying a polygon, make sure there is an even number of coords (as x,y pairs)
	if ((length(raw.obj.coords) >= 8) && (length(raw.obj.coords) %% 2 != 0)) {
		stop("Please provide an even number of coordinates for the object.")
	}

	# Count the number of points and round it up to 4 so we always end up with a vector
	# of at least 8 coordinates (even if they are NA)
	n_points = ceiling(length(raw.obj.coords)/2)
	if (n_points <= 4) n_points = 4;

	# Perform the transformation to end up with a named vector
	transformed.obj.coords = unlist(
		mapply(
			function(x, y) square_transform(as.numeric(x), as.numeric(y), model),
			raw.obj.coords[seq(1, by = 2, length.out = n_points)], raw.obj.coords[seq(2, by = 2, length.out = n_points)],
			SIMPLIFY = TRUE, USE.NAMES = FALSE
		)
	)

	# Set "x" and "y" names
	obj.coords = setNames(transformed.obj.coords, rep(c("x", "y"), length.out = length(transformed.obj.coords)))

	# Return the result
	return(obj.coords)
}