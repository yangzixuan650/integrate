trapezoid <- function(f, range, n){
  res <- 0
  for(i in 1:n){
    a <- range[1]
    b <- range[2]
    res <- res + (f(a+(i/n) + f(a+(i+1)/n)))*(b - a)/(2*n)
  }
  return(res)
}
