#' predict_hetgp
#'
#' @param het_gp_fit a hetGP model fit object (from fit_hetgp())
#' @param save_covmat boolean: should the predictive covariance matrix between prediction locations be saved?
#'
#' @return a data.frame in standard format
#' @export
#'
#' @examples preds <- predict_hetgp(het_gp_object = het_object, reference_date = as.Date("2022-10-05"))
#'
reference_date = as.Date("2022-09-13")
predict_hetgp <- function(het_gp_object,
                          save_covmat = FALSE,
                          reference_date,
                          depths = 1:10){

  # check if reference date is in correct format
  date_check = as.integer(grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", reference_date, value=F))
  x = integer(0)
  if (identical(x, date_check)){
    stop("invalid time format. Please enter date 'YYYY-MM-DD' ")
  }

  # convert to Date object
  if (!inherits(reference_date, c("Date", "POSIXt"))){
    reference_date = as.Date(reference_date)

  }

  # check if depths argument is correct (must be numeric and greater than 0)
  if (setequal(1:10, depths)){
    if (!is.numeric(depths)){
      stop("depths must be a numeric vector >= 0")
    }
    if (sum(depths < 0) >= 1 ){
      stop("depths must be >= 0")
    }
  }

  het_gp_fit = het_gp_object$het_gp_fit
  df = het_gp_object$df
  include_depth = het_gp_object$include_depth
  variable = het_gp_object$Y_resp

  model_id = "hetGP"
  family = "normal"
  variable = "temperature"

  date_times = reference_date + lubridate::days(1:35)
  doys = as.integer(format(date_times, "%j"))

  # DOY is only covariate
  if (!include_depth){
    Xnew <- matrix(1:365)

  if (save_covmat){
    preds <- predict(x = Xnew, xprime = Xnew, object = het_gp_fit)
    covmat = preds$cov
  }else{
    preds <- predict(x = Xnew, object = het_gp_fit)
    covmat = NULL
  }

  pred_df = data.frame(Mean = preds$mean, sd = sqrt(preds$sd2 + preds$nugs), DOY = 1:365)

  mypreds = pred_df[pred_df$DOY %in% doys, ]
  mean_preds = mypreds$Mean
  sd_preds = mypreds$sd

  final_mean_df = data.frame(reference_date = rep(reference_date, nrow(mypreds)), datetime = date_times,
                             prediction = mean_preds, model_id = model_id, family = family,
                             parameter = "mu", variable = variable)

  final_sd_df = data.frame(reference_date = rep(reference_date, nrow(mypreds)), datetime = date_times,
                             prediction = sd_preds, model_id = model_id, family = family,
                             parameter = "sd", variable = variable)

  finaldf = rbind(final_mean_df, final_sd_df)

  return(list(pred_df = finaldf, covmat = covmat, df= df))

  # DOY and depth are covariates
  }else{
    Xnew = data.frame(DOY=rep(1:365, length(depths)), depth = rep(depths, each = 365))
    Xnew = as.matrix(Xnew)

    if (save_covmat){
      preds <- predict(x = Xnew, xprime = Xnew, object = het_gp_fit)
      covmat = preds$cov
    }else{
      preds <- predict(x = Xnew, object = het_gp_fit)
      covmat = NULL
    }

    pred_df = data.frame(Mean = preds$mean, sd = sqrt(preds$sd2 + preds$nugs),
                         DOY=rep(1:365, length(depths)),
                         depth = rep(depths, each = 365))


    mypreds = pred_df[pred_df$DOY %in% doys, ]

    dummydf = data.frame(DOY = doys, datetime = date_times)
    mypreds = merge(mypreds, dummydf, by = "DOY")

    mean_preds = mypreds$Mean
    sd_preds = mypreds$sd

    final_mean_df = data.frame(reference_date = reference_date, datetime = mypreds$datetime,
                               prediction = mean_preds, model_id = model_id, family = family,
                               parameter = "mu", variable = variable, depth = mypreds$depth)

    final_sd_df = data.frame(reference_date = reference_date, datetime = mypreds$datetime,
                             prediction = sd_preds, model_id = model_id, family = family,
                             parameter = "sd", variable = variable, depth = mypreds$depth)

    finaldf = rbind(final_mean_df, final_sd_df)

    return(list(pred_df = finaldf, covmat = covmat, df = df))
  }
}

preds = predict_hetgp(het_gp_object = het_gp_object, reference_date = "2022-09-01")

