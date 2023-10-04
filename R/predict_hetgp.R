#' Obtain posterior predictive distribution from a heteroscedastic Gaussian process model fit
#'
#' @param het_gp_object a hetGP model fit object (from fit_hetgp())
#' @param save_covmat boolean: should the predictive covariance matrix between prediction locations be saved?
#' @param reference_datetime character or Date, POSIXt class date formatted as YYYY-MM-DD
#' @param max_horizon forecast horizon in days (default is 35)
#' @param PI prediction interval coverage (numeric between 0 and 1) (default is 1:10)
#' @return a list containing a data.frame in standard format containing forecasts;
#' a predictive covariance matrix if save_covmat = TRUE (otherwise this will be NULL),
#' the original df used for fitting; preds4plotting, a data.frame containing the mean, sd and upper and lower bounds (easier for plotting);
#' include_covar, a boolean value denoting whether another covariate was used, covar_name; covar_levels, the values of the additional covariate
#' at which forecasts were made;covar_name, the name of the additional covariate (or DOY if DOY is the only covariate),
#' and Yname, the name of the response variable
#' @export
#'
#' @examples
#' data(sample_lake_data_1mdepth)
#' mod1 = fit_hetgp(X = "DOY", Y = "temperature",site_id = "FCR", df = sample_lake_data_1mdepth)
#' preds <- predict_hetgp(het_gp_object = mod1, reference_datetime = as.Date("2022-10-05"))
#'
#' \dontrun{data(lake_data_depth)
#' modeld = fit_hetgp(X = c("DOY","depth"), Y = "temperature", site_id = "BARC", df = lake_data_depth)
#' modeld_preds = predict_hetgp(het_gp_object = modeld, reference_date = "2023-09-01")
#' plot_hetGPpreds(predObject = modeld_preds)
#' }
#'
#' @references
#' Binois, Mickael, and Robert B. Gramacy. "hetgp: Heteroskedastic Gaussian process modeling and sequential design in R." (2021).
predict_hetgp <- function(het_gp_object,
                          save_covmat = FALSE,
                          reference_datetime,
                          max_horizon = 35,
                          PI = .90){

  # check if reference date is in correct format
  date_check = as.integer(grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", reference_datetime, value=F))
  x = integer(0)
  if (identical(x, date_check)){
    stop("invalid time format. Please enter date 'YYYY-MM-DD' ")
  }

  # convert to Date object
  if (!inherits(reference_datetime, c("Date", "POSIXt"))){
    reference_datetime = as.Date(reference_datetime)

  }

  if ((PI < 0) || (PI >= 1) ){
    stop("PI must be a value in [0,1)")
  }

  alpha1 = (1 - PI) / 2
  alpha2 = 1 - alpha1

  het_gp_fit = het_gp_object$het_gp_fit
  df = het_gp_object$df
  include_covar = het_gp_object$include_covar
  Y_resp = het_gp_object$variable
  Yname = het_gp_object$Yname
  pred_width = PI

  model_id = "hetGP"
  family = "normal"
  variable = "temperature"

  date_times = reference_datetime + lubridate::days(1:max_horizon)
  doys = as.integer(format(date_times, "%j"))

  # DOY is only covariate
  if (!include_covar){
    covar_name = het_gp_object$covar_name
    Xnew <- matrix(1:365)
    Xnew_doy = matrix(doys)

    # predict on all DOYs but below, only the relevant ones are extracted
    if (save_covmat){
      preds <- predict(x = Xnew, object = het_gp_fit)

      predscov = predict(x = Xnew_doy, xprime = Xnew_doy, object = het_gp_fit)
      covmat = predscov$cov
      diag(covmat) = diag(covmat) + predscov$nugs
    }else{
      preds <- predict(x = Xnew, object = het_gp_fit)
      covmat = NULL
    }

    pred_df = data.frame(Mean = preds$mean, sd = sqrt(preds$sd2 + preds$nugs), DOY = 1:365)

    mypreds = pred_df[pred_df$DOY %in% doys, ]
    mean_preds = mypreds$Mean
    sd_preds = mypreds$sd

    final_mean_df = data.frame(reference_datetime = rep(reference_datetime, nrow(mypreds)), datetime = date_times,
                               prediction = mean_preds, model_id = model_id, family = family,
                               parameter = "mu", variable = variable)

    final_sd_df = data.frame(reference_datetime = rep(reference_datetime, nrow(mypreds)), datetime = date_times,
                             prediction = sd_preds, model_id = model_id, family = family,
                             parameter = "sd", variable = variable)

    finaldf = rbind(final_mean_df, final_sd_df)


    mypreds$Lower <- qnorm(alpha1, mypreds$Mean, mypreds$sd)
    mypreds$Upper <- qnorm(alpha2, mypreds$Mean, mypreds$sd)
    mylist = list(pred_df = finaldf, covmat = covmat, df = df, preds4plotting = mypreds,
                  include_covar = include_covar, Yname = Yname, pred_width = pred_width,
                  covar_name = covar_name)
    class(mylist) = "hetGPpreds"
    return(mylist)
    # DOY and other covariate are covariates
  }else{
    covar_name = het_gp_object$covar_name
    covar_levels = het_gp_object$covar_levels
    Xnewdf = data.frame(DOY=rep(1:365, length(covar_levels)), temp_name = rep(covar_levels, each = 365))
    idx = which(colnames(Xnewdf) == "temp_name")
    colnames(Xnewdf)[idx] = covar_name
    Xnew = as.matrix(Xnewdf)

    Xnew_doy = as.matrix(Xnewdf[Xnewdf$DOY %in% doys, ])
    if (save_covmat){
      preds <- predict(x = Xnew, object = het_gp_fit)

      predscov = predict(x = Xnew_doy, xprime = Xnew_doy, object = het_gp_fit)
      covmat = predscov$cov
      diag(covmat) = diag(covmat) + predscov$nugs
    }else{
      preds <- predict(x = Xnew, object = het_gp_fit)
      covmat = NULL
    }

    pred_df = data.frame(Mean = preds$mean, sd = sqrt(preds$sd2 + preds$nugs),
                         DOY=rep(1:365, length(covar_levels)),
                         temp_name = rep(covar_levels, each = 365))

    idx = which(colnames(pred_df) == "temp_name")
    colnames(pred_df)[idx] = covar_name

    mypreds = pred_df[pred_df$DOY %in% doys, ]

    dummydf = data.frame(DOY = doys, datetime = date_times)
    mypreds = merge(mypreds, dummydf, by = "DOY")

    mean_preds = mypreds$Mean
    sd_preds = mypreds$sd

    final_mean_df = data.frame(reference_datetime = reference_datetime, datetime = mypreds$datetime,
                               prediction = mean_preds, model_id = model_id, family = family,
                               parameter = "mu", variable = variable, temp_name = mypreds$depth)

    final_sd_df = data.frame(reference_datetime = reference_datetime, datetime = mypreds$datetime,
                             prediction = sd_preds, model_id = model_id, family = family,
                             parameter = "sd", variable = variable, temp_name = mypreds$depth)

    finaldf = rbind(final_mean_df, final_sd_df)

    idx = which(colnames(finaldf) == "temp_name")
    colnames(finaldf)[idx] = covar_name

    df$DOY = NULL

    mypreds$Lower = qnorm(alpha1, mypreds$Mean, mypreds$sd)
    mypreds$Upper = qnorm(alpha2, mypreds$Mean, mypreds$sd)
    mylist = list(pred_df = finaldf, covmat = covmat, df = df, preds4plotting = mypreds,
                  include_covar = include_covar, Yname = Yname, pred_width = pred_width,
                  covar_levels = covar_levels, covar_name = covar_name)
    class(mylist) = "hetGPpreds"
    return(mylist)
  }
}


#' Plot predictive mean and prediction intervals of a forecast
#'
#' @param predObject an object of class "hetGPpreds"
#' @return it makes a plot
#' @export
#'
#' @examples
#' data(sample_lake_data_1mdepth)
#' mod1 =  fit_hetgp(X = "DOY", Y = "temperature", site_id = "FCR", df = sample_lake_data_1mdepth)
#' preds = predict_hetgp(het_gp_object = mod1, reference_datetime = "2023-09-01")
#' plot_hetGPpreds(predObject=preds)
plot_hetGPpreds = function(predObject){
  include_covar = predObject$include_covar

  Yname = predObject$Yname
  percent_width = predObject$pred_width * 100

  if(!include_covar){
    plotdf = predObject$preds4plotting
    ymin = min(plotdf$Lower)
    ymax = max(plotdf$Upper)
    x = predObject$preds4plotting$DOY
    plot(x, plotdf$Mean, xlab = "DOY",
         ylab = Yname, type = "l", ylim = c(ymin, ymax))
    lines(x, plotdf$Upper, lty = 2)
    lines(x, plotdf$Lower, lty = 2)
    legend("topright", legend = c(Yname, paste(percent_width, "%", "pred intervals")), lwd=2, lty = c(1,2))
  }else{
    plotdf = predObject$preds4plotting
    ymin = min(plotdf$Lower)
    ymax = max(plotdf$Upper)

    covar_name = predObject$covar_name

    covar_levels = predObject$covar_levels
    covar_L = length(covar_levels)
    numCols = ceiling(covar_L / 2)
    if (numCols == 1){
      par(mfrow = c(1,2))
    }else{
      par(mfrow = c(2, numCols))
    }
    for (i in 1:length(covar_levels)){
      my_value = covar_levels[i]
      temp = plotdf[plotdf[[covar_name]] == my_value, ]
      x = temp$DOY
      plot(x, temp$Mean, xlab = "DOY",
           ylab = Yname, type = "l", ylim = c(ymin, ymax),
           main = paste(covar_name, my_value))
      lines(x, temp$Upper, lty = 2)
      lines(x, temp$Lower, lty = 2)
      if (i == 1){
        legend("topright", legend = c(Yname, paste(percent_width, "%", "pred intervals")), lwd=2, lty = c(1,2))
      }
    }
  }
}
