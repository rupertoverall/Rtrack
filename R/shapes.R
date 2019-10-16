circle = function(x, y, radius, res=100){
	data.frame(x = (cos((0:res) * 2 * pi / res) * radius) + x, y = (sin((0:res) * 2 * pi / res) * radius) + y)
}
