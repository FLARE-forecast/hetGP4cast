#' fit_hetgp
#'
#' @param X matrix of inputs
#' @param Z vector of responses (temperatures)
#'
#' @return a hetGP model fit object
#' @export
#'
#' @examples
#'
fit_hetgp <- function(X, Y){

  het_gp_fit <- hetGP::mleHetGP(X, Y, covtype =  "Gaussian")
  het_gp_fit <- hetGP::rebuild(het_gp_fit, robust = TRUE)

  return(het_gp_fit)

}



