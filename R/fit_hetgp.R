
#' Fit a heteroscedastic Gaussian process model
#'
#' @param X X name(s) of covariates for model fitting (string);
#' Accepted values are: one or both of "DOY" (day of year), "depth" (depth of measurement in m)
#' @param Y Y name of response (string); accepted values are one of unique(df$variable)
#' @param site_id name of site
#' @param df data.frame in standard format, see \href{here}{https://projects.ecoforecast.org/tern4cast/instructions.html#target-data-calculation}
#' @param covtype type of covariance function to use; can be one of
#' "Gaussian", "Matern3_2" or "Matern5_2"; for more information, see ?hetGP::mleHetGP()
#' @param silent if TRUE, more output will be printed to the console
#'
#' @return a list containing the hetGP fit object, the original df,
#' a boolean to denote whether not depth was used as an input, the vector of observations, and the name of the response variable
#' @export
#'
#' @examples data(sample_lake_data_1mdepth)
#' mod1 =  fit_hetgp(X = "DOY", Y = "temperature",site_id = "FCR", df = sample_lake_data_1mdepth)
#' @references Binois, Mickael, and Robert B. Gramacy. "hetgp: Heteroskedastic Gaussian process modeling and sequential design in R." (2021)
fit_hetgp <- function(X, Y, site_id, df, covtype = "Gaussian", silent = TRUE){
  use_depth = FALSE

  # function for safely converting datetime to DOY
  # if date cannot be converted to julian day then it will throw sensible error
  format_datetime_df <- function(df){
    tryCatch(
      expr = {
        df$DOY = as.integer(format(df$datetime, "%j"))
        message("Success")
        return(df)
        #message("Successfully executed the log(x) call.")
      },
      error = function(e){
        message('Cannot convert datetime variable into DOY. Please ensure datetime variable is
                formatted as yyyy-mm-dd')
        print(e)
      },
      warning = function(w){
        message('Cannot convert datetime variable into DOY. Please ensure datetime variable is
                formatted as yyyy-mm-dd')
        print(w)
      },
      finally = {

      }
    )
  }

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
    if (!silent){
      print("use depth")
    }
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
        use_depth = FALSE
      }

    }# use_depth = FALSE
  }else{
    if (!silent){
      print("depth is not a covariate")
    }
    accepted_Xs = "DOY"

    if (!is.character(X)){
      stop("Inputs, X, must be strings. Accepted inputs are ", paste(accepted_Xs, collapse = ' '))
    }
    if (length(X) == length(accepted_Xs)){
      if(anyNA(match(X, accepted_Xs))){
        stop("Inputs, X, must be equal to : ", paste(accepted_Xs, collapse = ' '))
      }
    }else{
      if ( anyNA(match(X, accepted_Xs))){
        stop("Inputs, X, must be equal to : ", paste(accepted_Xs, collapse = ' '))
      }
    }

  }
  # check if datetime is Date or Posix class
  if (!inherits(df$datetime, c("Date", "POSIXt"))){
    df$datetime = as.Date(df$datetime)
    #stop("datetime variable must be of class Date OR POSIxt")
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
    print("you selected multiple site_ids. This will pool data from multiple site_ids.")
    print("Do you wish to continue?")
    print("Press any key to continue or N to stop")
    user_input = readline()
    if (user_input == "N" || user_input == "n"){
      return("You have chosen to end the program and edit your input for site_id")
    }
  }

  # convert df to data.frame if it is not already one
  df = as.data.frame(df)
  df = df[(df$site_id == site_id) & (df$variable == Y), ]

  # convert datetime to Julian Day--DOY,
  # if datetime cannot be converted, will throw error
  df = format_datetime_df(df)
  #df$DOY = as.integer(format(df$datetime, "%j"))

  # extract X's
  Xmat = as.matrix(df[, X])

  # get Y
  Y_resp = df$observation


  accepted_covtypes = c("Gaussian", "Matern5_2", "Matern3_2")
  if (!(covtype %in% accepted_covtypes)){
    stop("Invalid covtype. Accepted names are: ", paste(accepted_covtypes, collapse = ' '))
  }
  # fit model
  # warn that this could take time
  yname = Y

  if(!silent){
    print(paste("site_id is", site_id))
    print("Y is, ", Yname)
    print("fitting model. For large datasets, this could take some time!")
  }

  het_gp_fit <- hetGP::mleHetGP(Xmat, Y_resp, covtype = covtype)

  het_gp_fit <- hetGP::rebuild(het_gp_fit, robust = TRUE)


  return(list(het_gp_fit = het_gp_fit,
              df = df,
              include_depth = use_depth,
              variable = Y_resp,
              Yname = yname))

}
