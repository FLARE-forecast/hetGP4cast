#' fit_hetgp
#'
#' @param X matrix of inputs
#' @param Z vector of responses (temperatures)
#'
#' @return a list containing a hetGP model fit object, the original df, and the X matrix
#' The list should be input to predict_hetgp()
#' @export
#'
#' @examples
#' # put in a sample dataset and read it in here for the example
#' test = fit_hetgp(X = "DOY", Y = "temperature", site_id = "BARC", df = exampleDF)
#'
fit_hetgp <- function(X, Y, site_id, df){
  use_depth = FALSE
  # Only a df of standard format will be accepted. It must have correct variable names. If there are missing names
  # function will stop and state which ones are missing/extra. Not sure what to do about the depth column...since
  # that is not a column in the 'standard' format
  # df_names = colnames(df)
  df_names = colnames(df)
 # df_names = c("datetime","x", "site_id","variable", "yY")
  
  # but prediction would be replaced by something else?
  standard_names = c("datetime", "site_id", "variable", "observation")
  standard_names_depth = c("datetime", "site_id", "variable", "observation", "depth")
  names_in_df = df_names %in% standard_names
  names_in_df_depth = df_names %in% standard_names_depth

  # data sets are in correct format, set flag for using depth
  if (setequal(df_names, standard_names) || setequal(df_names, standard_names_depth)){
    if (length(df_names) == 5){
      use_depth = TRUE
    }
  }else{
    
      missing_names = standard_names_depth[! (standard_names_depth %in% intersect(df_names, standard_names_depth))]
      extra_names = df_names[!(df_names %in% (intersect(df_names, standard_names_depth)))]
      
      # missing and extra names
      if (length(missing_names)){
        if (length(missing_names)){
          warning("You are missing the following column names in df (depth is optional): ",
                  paste(missing_names, collapse = ' '))
          warning("The following extra columns in df were detected: ", paste(extra_names, collapse = ' '))
          stop("please ensure df is in the correct format before proceeding")
        }
      }
      # missing colnames only
      if (length(missing_names)){
        warning("You are missing the following column names in df (depth is optional): ",
                paste(missing_names, collapse = ' '))
        stop("please ensure df is in the correct format before proceeding")
      }
      # extra names only
      if (length(extra_names)){
        warning("The following extra columns in df were detected: ", paste(extra_names, collapse = ' '))
        stop("please ensure df is in the correct format before proceeding")
      }
    

    
  }
  # check if names from X, Y are in standard names
  # Now df is in correct format
  # check Y
  # Y must be character vector of length 1 and must be any one of the variable names
  accepted_Y = unique(df$variable)
  if (!is.character(Y)){
    stop("The response, Y, must be a string equal to one of the following : ", paste(accepted_Y, collapse = ' '))
  }
  if(length(Y)>1){
    stop("The response, Y, must be a string equal to one of the following : ", paste(accepted_Y, collapse = ' '))
  }
  
  accepted_vars = unique(df$variable)
  #accepted_vars = c("temperature", "salt", "algae")
  
  
  if (!(Y %in% accepted_Y)){
    stop("The only supported names for the response variable, Y, are : ", paste(accepted_Y, collapse = ' '))
  }
  
  if (use_depth){  
    print("use depth")
  # check X's
  # must be character
  # be be Depth, DOY or c(Depth, DOY)
  accepted_Xs = c("depth", "DOY")
  
  if (!is.character(X)){
    stop("Inputs, X, must be strings. Accepted inputs are ", paste(accepted_Xs, collapse = ' '))
  }
  if (length(X) == length(accepted_Xs)){
    if(anyNA(match(X, accepted_Xs))){
      stop("Inputs, X, must be equal to : ", paste(accepted_Xs, collapse = ' '))
    }
  }else{
    # X is a different length than accepted Xs
    # check if there is an extra / bad X name
    if ( anyNA(match(X, accepted_Xs))){
      stop("Inputs, X, must be equal to : ", paste(accepted_Xs, collapse = ' '))
    }else{
      warning("both depth and DOY are present in df, but only one covariate was entered: ", X)
    }

  }# use_depth = FALSE
  }else{
    print("do not use depth")
    accepted_Xs = c("DOY")
    
    if (!is.character(X)){
      stop("Inputs, X, must be strings. Accepted inputs are ", paste(accepted_Xs, collapse = ' '))
    }
    if (length(X) == length(accepted_Xs)){
      if(anyNA(match(X, accepted_Xs))){
        stop("Inputs, X, must be equal to : ", paste(accepted_Xs, collapse = ' '))
      }
    }else{
      if ( is.na(match(X, accepted_Xs))){
        stop("Inputs, X, must be equal to : ", paste(accepted_Xs, collapse = ' '))
      }
    }
    
  }
  # also check dates before converting to julian day
  if (!is.Date(df$datetime)){
    stop("datetime column is not a date format")
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
  
  # convert df to data.frame if it is not already one
  df = as.data.frame(df)
  df = df[(df$site_id == site_id) & (df$variable == Y), ]
  
  # convert datetime to Julian Day--DOY,
  df$DOY = as.integer(format(df$datetime, "%j"))

  # extract X's
  X_idx = which(colnames(df) == X)
  
  Xmat = df[, X_idx]
  
  # get Y
  Y_resp = df$observation
  
  # fit model
  # warn that this could take time
  print("fitting model. For large datasets (>10,000 rows), this could take some time!")
  print("passes")
  #het_gp_fit <- hetGP::mleHetGP(Xmat, Y_resp, covtype =  "Gaussian")
  
  #het_gp_fit <- hetGP::rebuild(het_gp_fit, robust = TRUE)
  
  
  
  #return(list(het_gp_fit = het_gp_fit, df = df, Xmat = Xmat))
  
}

c("datetime", "site_id", "variable", "observation")
df2 = df %>% dplyr::select(datetime, site_id, variable, prediction)
df2$depth = 1
colnames(df2)[4] = "observation"
table(df$DOY)
head(df)
df$dummy=1
head(df2)
unique(df$site_id)
df=df2
fit_hetgp(X = "depth", Y = "temperature", site_id = c("BARC, TOMB"), df = df2)

  
  X = c("Depth", "DOY")
df = data.table::fread("aquatics-2023-03-20-xgboost_parallel.csv.gz")
df2 = filter(df, variable == "temperature")
plot(df2$datetime, df2$prediction)
stuff = readline()

stuff = filter(df, site_id == "BARC")
table(stuff$)

unique(df$parameter)
