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
  Xnew <- matrix[1:366]

  if (save_covmat){
    preds <- predict(x = Xnew, xprime = Xnew, object = het_gp_fit)
    covmat = preds$cov
  }else{
    preds <- predict(x = Xnew, object = het_gp_fit)
    covmat = NULL
  }
  pred_df = data.frame(Mean = preds$mean, sd = sqrt(preds$sd2 + preds$nugs), DOY = df$DOY)
  pred_df = pred_df[!(duplicated(pred_df$DOY)), ]

  # get df where preds are means
  df2 = df[,  c("reference_datetime", "site_id" ,"variable")]
  meandf = pred_df[, c("Mean", "DOY")]
  dflist = list(length = 30)
  for (i in 1:30){
    tempdf = df2
    tempdf$datetime = as.POSIXct( tempdf$reference_datetime ) + lubridate::days(i)
    tempdf$horizon = i
    print(tempdf)
    dflist[[i]] = tempdf
  }

  resdf = data.table::rbindlist(dflist)

  resdf$DOY = as.integer(format(resdf$datetime, "%j"))
  resdf2 = resdf[resdf$DOY %in% meandf$DOY, ]
  resdf2 = merge(resdf2, meandf, by = "DOY")
  resdf2$DOY = NULL
  resdf2$horizon = NULL
  resdf2$parameter = "mu"
  resdf2$family = family
  resdf2$model_id = model_id
  final_mean_df = resdf2
  colnames(final_mean_df)[7] = "prediction"

  # now do sd's
  sddf = pred_df[, c("sd", "DOY")]
  dflist = list(length = 30)
  for (i in 1:30){
    tempdf = df2
    tempdf$datetime = as.POSIXct( tempdf$reference_datetime ) + lubridate::days(i)
    dflist[[i]] = tempdf
  }

  resdf = data.table::rbindlist(dflist)
  head(resdf)
  resdf$DOY = as.integer(format(resdf$datetime, "%j"))
  resdf2 = resdf[resdf$DOY %in% meandf$DOY, ]
  resdf2 = merge(resdf2, meandf, by = "DOY")
  resdf2$DOY = NULL
  resdf2$parameter = "sigma"
  resdf2$family = family
  resdf2$model_id = model_id
  final_sd_df = resdf2
  colnames(final_sd_df)[7] = "prediction"

  finaldf = rbind(final_mean_df, final_sd_df)

  return(list(pred_df = finaldf, covmat = covmat))
}
