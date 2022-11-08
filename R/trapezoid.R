#' Title
#'
#' @param f
#' @param range
#' @param n
#'
#' @return
#' @export
#'
#' @examples
trapezoid <- function(f, range, n){
  res <- 0
  a <- range[1]
  b <- range[2]
  for(i in 1:n){
    res <- res + (f(a+(i/n) + f(a+(i+1)/n)))*(b - a)/(2*n)
  }
  return(res)
}
