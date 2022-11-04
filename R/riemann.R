riemann <- function(f, range, n){
  # approximate a function by Riemann sum
  res <- 0
  for(i in 1:n){
    a <- range[1]
    b <- range[2]
    xi <- a + (i-0.5)*(b-a)/n
    res <- res + f(xi)*(b - a)/n
  }
  return(res)
}
