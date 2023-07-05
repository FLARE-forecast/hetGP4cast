#' predict_hetgp
#'
#' @param het_gp_fit a hetGP model fit object (from fit_hetgp())
#' @param save_covmat boolean: should the predictive covariance matrix between prediction locations be saved?
#'
#' @return a data.frame in standard format
#' @export
#'
#' @examples preds <- predict_hetgp(het_gp_object = het_object)
#'
predict_hetgp <- function(het_gp_object, save_covmat = FALSE){
  het_gp_fit = het_gp_object$het_gp_fit
  Xmat = het_gp_object$Xmat
  df = het_gp_object$df
  Xnew <- as.matrix(Xmat)

  if (save_covmat){
    preds <- predict(x = Xnew, xprime = Xnew, object = het_gp_fit)
    covmat = preds$cov
  }else{
    preds <- predict(x = Xnew, object = het_gp_fit)
    covmat = NULL
  }

  Mean = preds$mean
  sd_with_Nug = sqrt(preds$sd2 + preds$nugs)
  family = "normal"
  model_id = "hetGP"
  reference_datetime = "NA"

  meandf = df[, c("datetime", "site_id", "variable")]
  sddf = meandf

  meandf$parameter = "mu"
  meandf$prediction = Mean

  sddf$parameter = "sigma"
  sddf$prediction = sd_with_Nug

  df = rbind(meandf, sddf)
  df$model_id = model_id
  df$family = family
  df$reference_datetime = reference_datetime

  return(list(pred_df = df, covmat = covmat))
}
