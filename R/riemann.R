#' Approximate an integrable function by Riemann sum
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
#' res <- riemann(f, c(0,1), 10)
#'
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
