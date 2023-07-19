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
  Xnew <- matrix(1:365)
  model_id = "hetGP"
  family = "normal"

  if (save_covmat){
    preds <- predict(x = Xnew, xprime = Xnew, object = het_gp_fit)
    covmat = preds$cov
  }else{
    preds <- predict(x = Xnew, object = het_gp_fit)
    covmat = NULL
  }
  pred_df = data.frame(Mean = preds$mean, sd = sqrt(preds$sd2 + preds$nugs), DOY = 1:365)


  # get df where preds are means
  df2 = df[,  c("datetime", "site_id" ,"variable")]
  colnames(df2)[1] = "reference_datetime"
  meandf = pred_df[, c("Mean", "DOY")]
  dflist = list(length = 30)
  for (i in 1:30){
    tempdf = df2
    tempdf$datetime = as.POSIXct(tempdf$reference_datetime, tz = "GMT")
    tempdf$datetime = tempdf$reference_datetime + lubridate::days(i)
    tempdf$horizon = i
    #print(tempdf)
    dflist[[i]] = tempdf
  }

  resdf = data.table::rbindlist(dflist)
  # convert datetime (date of forecast) to DOY
  resdf$DOY = as.integer(format(resdf$datetime, "%j"))

  resdf2 = merge(resdf, meandf, by = "DOY")
  resdf2 = resdf2[complete.cases(resdf2), ]

  # get rid of DOY/horizon, because they are not in the standard format
  resdf2$DOY = NULL
  resdf2$horizon = NULL

  resdf2$parameter = "mu"
  resdf2$family = family
  resdf2$model_id = model_id
  final_mean_df = resdf2
  idx = which(colnames(final_mean_df) == "Mean")
  colnames(final_mean_df)[idx] = "prediction"

  # now do sd's
  sddf = pred_df[, c("sd", "DOY")]
  dflist = list(length = 30)
  for (i in 1:30){
    tempdf = df2
    tempdf$datetime = as.POSIXct(tempdf$reference_datetime, tz = "GMT")
    tempdf$datetime = tempdf$reference_datetime + lubridate::days(i)
    tempdf$horizon = i
    #print(tempdf)
    dflist[[i]] = tempdf
  }

  resdf = data.table::rbindlist(dflist)

  resdf$DOY = as.integer(format(resdf$datetime, "%j"))


  resdf2 = merge(resdf, sddf, by = "DOY")
  resdf2$DOY = NULL
  resdf2$horizon = NULL

  resdf2$parameter = "sigma"
  resdf2$family = family
  resdf2$model_id = model_id
  final_sd_df = resdf2
  idx = which(colnames(final_sd_df) == "sd")
  colnames(final_sd_df)[idx] = "prediction"
  finaldf = rbind(final_mean_df, final_sd_df)

  return(list(pred_df = finaldf, covmat = covmat))
}
