#' predict_hetgp
#'
#' @param Xnew a prediction matrix
#' @param het_gp_fit a hetGP model fit object (from fit_hetgp)
#'
#' @return a vector of predictions
#' @export
#'
#' @examples
#'
predict_hetgp <- function(het_gp_object, save_covmat = FALSE){
  het_gp_fit = het_gp_object$het_gp_fit
  Xmat = het_gp_fit$Xmat
  df = het_gp_fit$df
  Xnew <- as.matrix(Xnew)
  preds <- predict(Xnew)
  return(preds)
}
