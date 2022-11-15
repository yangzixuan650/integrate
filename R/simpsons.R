#' Simpsons method
#'
#' @param f a function to be integrated
#' @param range integration range
#' @param n number of intervals
#'
#' @return integration result
#' @export
#'
#' @examples
#' # A function to be integrated:
#' f <- function(x) {return(sqrt(x))}
#' res <- simpsons(f, c(0,1), 10)
#'
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
