approxExtrap = function (x, y, xout, method = "linear", n = 50, rule = 2, f = 0, ties = "ordered", na.rm = FALSE){
	if(is.list(x)){
		y = x[[2]]
		x = x[[1]]
	}
	if(na.rm){
		d = !is.na(x + y)
		x = x[d]
		y = y[d]
	}
	x = as.numeric(x)
	y = as.numeric(y)
	d = !duplicated(x)
	x = x[d]
	y = y[d]
	d = order(x)
	x = x[d]
	y = y[d]
	w = approx(x, y, xout = xout, method = method, n = n, rule = 2, f = f, ties = ties)$y
	r = range(x)
	d = xout < r[1]
	if(any(is.na(d))) stop("NAs not allowed in xout")
	if(any(d)) w[d] = (y[2] - y[1])/(x[2] - x[1]) * (xout[d] - x[1]) + y[1]
	d = xout > r[2]
	n = length(y)
	if(any(d)) w[d] = (y[n] - y[n - 1])/(x[n] - x[n - 1]) * (xout[d] - x[n - 1]) + y[n - 1]
	list(x = xout, y = w)
}