#' Trapezoid rule
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
#' res <- trapezoid(f, c(0,1), 10)
#'
trapezoid <- function(f, range, n){
  res <- 0
  a <- range[1]
  b <- range[2]
  for(i in 1:n){
    res <- res + (f(a+(i/n) + f(a+(i+1)/n)))*(b - a)/(2*n)
  }
  return(res)
}
