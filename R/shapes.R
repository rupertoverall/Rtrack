circle = function(x, y, radius, res=100){
	shape = cbind(x = (cos((0:res) * 2 * pi / res) * radius) + x, y = (sin((0:res) * 2 * pi / res) * radius) + y)
	shape[nrow(shape), ] = shape[1, ] # Ensure the polygon is closed
	return(shape)
}
square = function(centre.x, centre.y, corner.x, corner.y){
	radius = sqrt(sum(c(corner.x - centre.x, corner.y - centre.y) ^ 2))
	theta = atan2(corner.y - centre.y, corner.x - centre.x)
	shape = t(sapply(c(pi/2, pi, 1.5 * pi, 2 * pi), function(p) c(cos(theta + p), sin(theta + p))) * radius + c(centre.x, centre.y))
	shape = rbind(shape, shape[1, ]) # Ensure the polygon is closed
	return(shape)
}

