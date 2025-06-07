################################################################################
# Part B, III Functions, chapter 3.1. Error functions and related functions,
# page 857.
################################################################################

################################################################################
# Chapter 3.1.1 Error function
################################################################################

################################################################################
# Chapter 3.1.2 Polder Function
################################################################################

#' Polder function, Chapter 3.1.2, formula 24, page 862.
#'
#' @param x First argument in the Polder function
#' @param y Second argument in the Polder function
#' @references Formula 24 page 862.
#' @return Result of the Polder function
#' @export
P <- function(x, y) {
  0.5*exp(2*x)*pracma::erfc(x/y+y) + 0.5*exp(-2*x)*pracma::erfc(x/y-y)
}

#' Conjugate Polder function, Chapter 3.1.2, formula 25, page 862.
#'
#' @param x First argument in the Polder function
#' @param y Second argument in the Polder function
#' @references Formula 25 page 862.
#' @return Result of Conjugate the Polder function
#' @export
Pconj <- function(x, y) {
  0.5*exp(2*x)*pracma::erfc(x/y+y) - 0.5*exp(-2*x)*pracma::erfc(x/y-y)
}

#' Derivative of the Polder function with respect to x, Chapter 3.1.2,
#' formula 29, page 863.
#'
#' @param x First argument in the Polder function
#' @param y Second argument in the Polder function
#' @references Formula 29 page 863.
#' @return Derivative of the Polder function with respect to x
#' @export
dPdx <- function(x, y) {
  2*Pconj(x,y) - 2*(-(x/y)^2-y^2)/(sqrt(pi)*y)
}

#' Derivative of the Polder function with respect to y, Chapter 3.1.2,
#' formula 30, page 863.
#'
#' @param x First argument in the Polder function
#' @param y Second argument in the Polder function
#' @references Formula 30 page 863.
#' @return Derivative of the Polder function with respect to y
#' @export
dPdy <- function(x, y) {
  2*x*exp(-(x/y)^2-y^2)/(sqrt(pi)*y^2)
}

#' Derivative of the Conjugate Polder function with respect to x, Chapter 3.1.2,
#' formula 31, page 863.
#'
#' @param x First argument in the Polder function
#' @param y Second argument in the Polder function
#' @references Formula 31 page 863.
#' @return Derivative of the Conjugate Polder function with respect to x
#' @export
dPconjdx <- function(x, y) {
  2*P(x,y)
}

#' Derivative of the Conjugate Polder function with respect to y, Chapter 3.1.2,
#' formula 32, page 863.
#'
#' @param x First argument in the Polder function
#' @param y Second argument in the Polder function
#' @references Formula 32 page 863.
#' @return Derivative of the Conjugate Polder function with respect to y
#' @export
dPconjdy <- function(x, y) {
  -2*(-(x/y)^2-y^2)/sqrt(pi)
}

################################################################################
# Chapter 3.1.3 Resistance function
################################################################################

#' Resistance function, Chapter 3.1.3, formula 41, page 865.
#'
#' @param x First argument in the Resistance function
#' @param y Second argument in the Resistance function
#' @references Formula 41 page 865.
#' @return Result of the Resistance function
#' @export
R <- function(x, y) {
  exp(2*x+y^2)*pracma::erfc(x/y+y)
}


################################################################################
# Chapter 3.1.4 The M-function
################################################################################

#' M-function, Chapter 3.1.4, formula 49, page 869.
#'
#' @param u First argument in the M-function function
#' @param Alpha Second argument in the M-function
#' @references Formula 49 page 869.
#' @return Result of the M-function
#' @export
M <- function(u, Alpha) {
  integrand <- function(y) {(1/y)*exp(-y)*pracma::erf(Alpha*sqrt(y))}
  res <- stats::integrate(integrand, lower = u, upper = Inf)
  return(res$value)
}

