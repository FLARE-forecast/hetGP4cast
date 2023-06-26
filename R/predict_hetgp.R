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
predict_hetgp <- function(Xnew, het_gp_fit){
  Xnew <- as.matrix(Xnew)
  preds <- predict(Xnew)
  return(preds)
}
