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
#'
fit_hetgp <- function(X, Y, site_id, df){
  
  # Only a df of standard format will be accepted. It must have correct variable names. If there are missing names
  # function will stop and state which ones are missing/extra. Not sure what to do about the depth column...since
  # that is not a column in the 'standard' format
  # df_names = colnames(df)
  df_names = colnames(df)
  #df_names = c("model_id","datetime","reference_datetime", "site_id","family","parameter","variable", "DOY")
  standard_names = c("model_id","datetime","reference_datetime","site_id",
                     "family","parameter","variable","prediction")
  
  names_in_df = df_names %in% standard_names
  if (setequal(df_names, standard_names)){
    print("data frame is in correct format")
  }else{
    
    missing_names = standard_names[! (standard_names %in% intersect(df_names, standard_names))]
    extra_names = df_names[!(df_names %in% (intersect(df_names, standard_names)))]
    
    if (length(missing_names)){
      warning("You are missing the following column names in df: ", paste(missing_names, collapse = ' '))
      stop("please ensure df is in the correct format before proceeding")
    }
    if (length(extra_names)){
      warning("The following extra columns in df were detected: ", paste(extra_names, collapse = ' '))
      stop("please ensure df is in the correct format before proceeding")
    }
    
  }
  # check if names from X, Y are in standard names
  # Now df is in correct format
  # check Y
  # Y must be character vector of length 1 and must be any one of the variable names
  
  if (!is.character(Y)){
    stop("The response, Y, must be a string. Accepted responses are", paste(accepted_vars,collapse=' '))
  }
  if(length(Y)>1){
    stop("response, Y, should be a string")
  }
  
  accepted_vars = unique(df$variable)
  #accepted_vars = c("temperature", "salt", "algae")
  
  
  if (!(Y %in% accepted_vars)){
    stop("The only supported names for the response variable, Y, are : ", paste(accepted_vars,collapse=' '))
  }
  
  if (Y != "temperature"){
    warning("If response Y is 0 censored, a log transform is recommended.")
  }
  # check X's
  # must be character
  # be be Depth, DOY or c(Depth, DOY)
  accepted_Xs = c("Depth", "DOY")
  
  if (!is.character(X)){
    stop("Inputs, X, must be strings. Accepted inputs are ", paste(accepted_Xs, collapse = ' '))
  }
  if (length(X) == length(accepted_Xs)){
    if(anyNA(match(X, accepted_Xs))){
      stop("Inputs, X, must be one or both of : ", paste(accepted_Xs, collapse = ' '))
    }
  }else{
    if ( is.na(match(X, accepted_Xs))){
      stop("Inputs, X, must be one or both of : ", paste(accepted_Xs, collapse = ' '))
    }
  }
  
  # also check dates before converting to julian day
  if (!is.Date(df$datetime)){
    stop("datetime column is not a date format")
  }
  if (!is.Date(df$reference_datetime)){
    stop("reference_datetime column is not a date format")
  }
  
  # if Depth is NOT a col in df, but Depth is requested as a covariate, throw error
  if (!("Depth" %in% df_names) & ("Depth" %in% X)){
    stop("Depth is requested as a covariate but is not a column in df!")
  }
  
  # check site_id
  accepted_sites = unique(df$site_id)
  
  if (!is.character(site_id)){
    stop("site_id must be a string. Accepted site_ids are ", paste(accepted_sites, collapse = ' '))
  }
  
  
  good_sites = intersect(site_id, accepted_sites)
  if (!setequal(site_id, good_sites)){
    stop("incorrect value for site_id. Accepted site_ids are ", paste(accepted_sites, collapse = ' '))
  }
  # bad input for site_id
  if (length(good_sites) == 0){
    stop("incorrect value for site_id. Accepted site_ids are ", paste(accepted_sites, collapse = ' '))
  }
  
  
  # if more than one site_id are entered ask user if this is really what they want to do
  if (length(good_sites) > 1){
    print("you selected multiple site_ids. This will pool data from multiple site_ids.
            Do you wish to continue?
            Press any key to continue or N to stop")
    user_input = readline()
    if (user_input == "N" || user_input == "n"){
      return("You have chosen to end the program and edit your input for site_id")
    }
  }
  
  # check if model_id and family just have one value
  # but wtf is parameter
  models = unique(df$model_id)
  families = unique(df$model_id)
  
  if (length(models) > 1){
    print("More than one model type is present in df. Do you wish to continue fitting?
          Press any key to continue or N to stop")
    user_input = readline()
    if (user_input == "N" || user_input == "n"){
      return("quitting")
    }
  }
  
  if (length(families) > 1){
    print("More than one family type is present in df. Do you wish to continue fitting?
          Press any key to continue or N to stop")
    user_input = readline()
    if (user_input == "N" || user_input == "n"){
      return("quitting")
    }
  }
  
  df = as.data.frame(df)
  df = df[(df$site_id == site_id) & (df$variable == Y), ]
  
  # convert datetime to Julian Day--DOY,
  df$DOY = as.integer(format(df$datetime, "%j"))
  df$DOY_refdate = as.integer(format(df$reference_datetime, "%j"))
  
  # extract X's
  X_idx = which(colnames(df) == X)
  
  Xmat = df[, X_idx]
  
  # get Y
  Y_resp = df$prediction
  
  # fit model
  print("fitting model. For large datasets (>10,000 rows), this could take some time!")
  #return("passes")
  #het_gp_fit <- hetGP::mleHetGP(Xmat, Y_resp, covtype =  "Gaussian")
  
  #het_gp_fit <- hetGP::rebuild(het_gp_fit, robust = TRUE)
  
  #return(het_gp_fit)
  
  return(list(het_gp_fit = het_gp_fit, df = df, Xmat = Xmat))
  
}
table(df$DOY)
head(df)
df$dummy=1

unique(df$site_id)
modfit=fit_hetgp(X = "DOY", Y = "temperature", site_id = "BARC", df = df)
modfit$
  
  X = c("Depth", "DOY")
df = data.table::fread("aquatics-2023-03-20-xgboost_parallel.csv.gz")
df2 = filter(df, variable == "temperature")
plot(df2$datetime, df2$prediction)
stuff = readline()

stuff = filter(df, site_id == "BARC")
table(stuff$)

unique(df$parameter)
