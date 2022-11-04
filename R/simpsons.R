simpsons <- function(f, range, n){
  a <- range[1]
  b <- range[2]
  res <- 0
  for(i in 1:n){
    m <- a + (i-1)/n
    k <- a + i/n
    res <- res + (k-m)*(f(m) + 4*f((m+k)/2) + f(k))/6
  }
  return(res)
}
