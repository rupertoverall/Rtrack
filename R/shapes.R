#' @importFrom stats coef lm
#' @importFrom utils data
#' @import randomForest

deg2rad = function(deg){(deg * pi) / (180)}

circle = function(x, y, radius, res = 360){
	x = as.numeric(x)
	y = as.numeric(y)
	radius = as.numeric(radius)
	shape = cbind(x = (cos((0:res) * 2 * pi / res) * radius) + x, y = (sin((0:res) * 2 * pi / res) * radius) + y)
	shape[nrow(shape), ] = shape[1, ] # Ensure the polygon is closed
	return(signif(shape, 4))
}

square = function(centre.x, centre.y, corner.x, corner.y = NA, corner.x2 = NA, corner.y2 = NA, corner.x3 = NA, corner.y3 = NA){
	if(is.na(corner.y)){ # This must be a centre/radius definition
		centre.x = as.numeric(centre.x)
		centre.y = as.numeric(centre.y)
		radius = as.numeric(corner.x)
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
		shape = rbind(shape, shape[1, ]) # Ensure the polygon is closed.
		shape = terra::crds(terra::convHull(terra::vect(shape))) # Use convHull to ensure the order of the points yields a convex polygon.
		colnames(shape) = c("x", "y")
		return(signif(shape, 4))
	}
}

polyshape = function(vertices){
	shape = rbind(vertices, vertices[1, ]) # Ensure the polygon is closed.
	shape = terra::crds(terra::convHull(terra::vect(shape))) # Use convHull to ensure the order of the points yields a convex polygon.
	return(signif(shape, 4))
}

standardise_square = function(raw.corners){
	# Get a projection matrix describing the transform of a potentially skewed arena to the standardised space.
	clean.corners = raw.corners[-nrow(raw.corners), ] # Ignore the replicate last row (that just keeps the polygon closed).
	colnames(clean.corners) = c("x", "y")
	corners = cbind(x = c(-1, 1, 1, -1), y = c(1, 1, -1, -1))
	A = matrix(0, nrow = 2 * nrow(clean.corners), ncol = 8)
	b = matrix(0, nrow = 2 * nrow(clean.corners), ncol = 1)
	for(i in seq_len(nrow(clean.corners))){ 
		ii = 2 * i
		A[ii - 1, ] = c(clean.corners[i, 1], clean.corners[i, 2], 1, 0, 0, 0, -clean.corners[i, 1] * corners[i, 1], -clean.corners[i, 2] * corners[i, 1])
		A[ii,] = c(0, 0, 0, clean.corners[i, 1], clean.corners[i, 2], 1, -clean.corners[i, 1] * corners[i, 2], -clean.corners[i, 2] * corners[i, 2])
		b[ii - 1] = corners[i, 1]
		b[ii] = corners[i, 2]
	}
	return(matrix(c(solve(A, b), 1), nrow = 3, ncol = 3, byrow = TRUE))
}

standardised_radius_square = function(raw.corners){
	# Standardise the raw coordinates, then get 'radius' in averaged coordinate space.
	# This 'radius' is half the width/height of the square.
	clean.corners = raw.corners[-nrow(raw.corners), ] # Ignore the replicate last row (that just keeps the polygon closed).
	colnames(clean.corners) = c("x", "y")
	corners = cbind(x = c(-1, 1, 1, -1), y = c(1, 1, -1, -1))
	model = stats::lm(clean.corners ~ corners[, "x"] + corners[, "y"])
	x = c(0, -1, 0, 1, 0)
	y = c(0, 0, 1, 0, -1)
	new.x = (x * stats::coef(model)['corners[, "x"]', "x"]) + (y * stats::coef(model)['corners[, "y"]', "x"]) + stats::coef(model)["(Intercept)", "x"]
	new.y = (x * stats::coef(model)['corners[, "x"]', "y"]) + (y * stats::coef(model)['corners[, "y"]', "y"]) + stats::coef(model)["(Intercept)", "y"]
	radius = mean((((new.x - new.x[1]) ^ 2 + (new.y - new.y[1]) ^ 2) ^ .5)[-1])
	return(signif(radius, 4))
}

project = function(x, y, model){
	# Project points from raw to standardised space.
	if(length(x) != length(y)) stop("Error when transforming points: coordinates do not match.")
	points = cbind(x, y, 1)
	transformed.points = t(apply(points, 1, function(row){
		transformed = model %*% row
		return(transformed[1:2] / transformed[3])
	}))
	return(list(x = signif(transformed.points[, 1], 4), y = signif(transformed.points[, 2], 4)))
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

