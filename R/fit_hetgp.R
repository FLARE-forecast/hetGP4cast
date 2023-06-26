#' Title
#'
#' @param X 
#' @param Z
#'
#' @return
#' @export
#'
#' @examples
#'
fit_hetgp = function(X, Z){

  het_gp_fit = mleHetGP(X, Z, covtype =  "Gaussian")
  het_gp_fit = rebuild(het_gp_fit, robust = TRUE)

  return(df)

}

predict_hetgp = function(Xnew, het_gp_fit){
  Xnew = as.matrix(Xnew)
  preds = predict(Xnew)
  return(preds)
}


