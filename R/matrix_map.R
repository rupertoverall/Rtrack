matrix.map = function(n, dim.m){
  col = ceiling(n / dim.m[1])
  row = n - ((col - 1) * dim.m[1])
  outofbounds = col < 1 | col > dim.m[2] | row < 1 | row > dim.m[1]
  col[outofbounds] = NA
  row[outofbounds] = NA
  return(list(row=row, col=col))
}
