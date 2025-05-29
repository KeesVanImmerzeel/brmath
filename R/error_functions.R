# 3.1. Error functions and related functions, page 857.

#' Polder function P
#'
#' @param x First argument in Polder function
#' @param y Second argument in Polder function
#' @references Formula 25 page 862.
#' @return Result of Polder function
#'
P <- function(x, y) {
  0.5*exp(2*x)*pracma::erfc(x/y+y) + 0.5*exp(-2*x)*pracma::erfc(x/y-y)
}

