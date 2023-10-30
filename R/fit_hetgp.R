#' Fit a heteroscedastic Gaussian process model
#'
#' @param X X name(s) of covariates for model fitting (string);
#' Accepted values are: "DOY" and the name of a valid covariate found in df. Covariates other than "DOY"
#' must be numeric. Categorical variables are currently not supported.
#' @param Y Y name of response (string); accepted values are one of unique(df$variable)
#' @param site_id name of site; accepted values are one or more of unique(df$site_id)
#' @param df data.frame in standard format, see \href{here}{https://projects.ecoforecast.org/tern4cast/instructions.html#target-data-calculation}
#' @param covtype type of covariance function to use; can be one of
#' "Gaussian", "Matern3_2" or "Matern5_2"; for more information, see ?hetGP::mleHetGP()
#' @param silent if TRUE, more output will be printed to the console
#' @param covar_levels a numeric vector specifying the values of the covariate other than DOY (if applicable)
#'  at which forecasts should be made. If not specified, covar_levels will be set to unique values of the covariate if there
#'  are less than 10; otherwise 10 levels will be specified at the following quantiles: 0,11,22,33,44,55,66,77,88, 100
#' @return a list containing the hetGP fit object, the original df,
#' a boolean to denote whether not depth was used as an input; covar_name, the name of the covariate other than DOY;
#' the vector of observations; covar_levels, a vector containing values of the covariate on which predictions should be
#' based; and Yname, the name of the response variable
#'
#' @export
#'
#' @examples data(sample_lake_data_1mdepth)
#' mod1 =  fit_hetgp(X = "DOY", Y = "temperature",site_id = "FCR", df = sample_lake_data_1mdepth)
#'
#' \dontrun{data(lake_data_depth)
#' modeld = fit_hetgp(X = c("DOY","depth"), Y = "temperature", site_id = "BARC", df = lake_data_depth)
#' }
#' @references Binois, Mickael, and Robert B. Gramacy. "hetgp: Heteroskedastic Gaussian process modeling and sequential design in R." (2021)
fit_hetgp <- function(X, Y, site_id, df, covtype = "Gaussian", silent = TRUE, covar_levels = NULL){
  # function for safely converting datetime to DOY
  # if date cannot be converted to julian day then it will throw sensible error
  format_datetime_df <- function(df){
    tryCatch(
      expr = {
        df$DOY = as.integer(format(df$datetime, "%j"))
        return(df)
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

  standard_names = c("datetime", "site_id", "variable", "observation")


  names_in_df = df_names %in% standard_names

  if (!setequal(intersect(standard_names, df_names) , standard_names)){
    df_names1 =  (intersect(standard_names, df_names))
    missing_names = standard_names[!standard_names %in% df_names1]
    warning("You are missing the following column names in df : ",
            paste(missing_names, collapse = ' '))
    stop("Please ensure df is in correct format.")
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

  #accepted_vars = unique(df$variable)
  #accepted_vars = c("temperature", "salt", "algae")


  if (!(Y %in% accepted_Y)){
    stop("The only supported names for the response variable, Y, are : ", paste(accepted_Y, collapse = ' '))
  }

  num_vars = length(X)
  if (num_vars > 2){
    stop("this function currently only supports the use of maximum 2 covariates")
  }
  if (num_vars == 1){
    use_depth = FALSE
  }else{
    use_depth = TRUE
  }

  # using 2 covars
  if (use_depth){
    if (!silent){
      print("using 2 covariates for fitting")
    }
    # check X's
    # must be character
    Xvals = df_names[! df_names %in% standard_names]

    accepted_Xs = c(Xvals, "DOY")
    if (anyNA( match(X , accepted_Xs))){
     stop("you entered a name for an input X that does not exist in df")
    }

    # covariates must be of class numeric--no factors/characters
    NOTdoyx = X[ X %in% Xvals]
    if (!is.numeric(df[[NOTdoyx]])){
      stop("covariates must be of class numeric.")
    }

    # get levels of covariate at which predictions will be evaluated with predict_hetgp()
    if (!is.null(covar_levels)){
      myrange = range(NOTdoyx)
      if (!is.numeric(covar_levels)){
        stop("covar_levels must be numeric.")
      }

      if (length(which(covar_levels >= myrange[1] & covar_levels <= myrange[2])) != length(covar_levels)){
          warning(paste("some covariate levels are outside of the range of ", Xvals))
      }
      # check to make sure valid levels are entered
    }else{ # automatically get covariate levels

      uniq_vals  = unique(df[[NOTdoyx]])
      if (length(uniq_vals) <= 10){
        covar_levels = unique(df[[NOTdoyx]])
      }else{
      covar_levels = unname( quantile(df[[NOTdoyx]], probs=seq(0,1, length.out=10)) )
      }
    }

    covar_name = NOTdoyx

    # should be in order: DOY, covar
    if ("DOY" %in% X){
      idx = which(X == "DOY")
      if (idx == 2){
        X = rev(X)
      }
    }else{
      stop("X must contain 'DOY' " )
    }
  # use_depth = FALSE, using one covar--DOY
  }else{
    if (!silent){
      print("modeling with only one covariate")
    }
    covar_levels = NULL
    accepted_Xs = "DOY"

    if (!is.character(X)){
      stop("Inputs, X, must be strings. Accepted inputs are ", paste(accepted_Xs, collapse = ' '))
    }
    if (length(X) == length(accepted_Xs)){
      if(anyNA(match(X, accepted_Xs))){
        stop("Inputs, X, must be include : ", paste(accepted_Xs, collapse = ' '))
      }
    }else{
      if ( anyNA(match(X, accepted_Xs))){
        stop("Inputs, X, must be equal to : ", paste(accepted_Xs, collapse = ' '))
      }
    }

    covar_name = "DOY"
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
    print("Y is, ", yname)
    print("fitting model. For large datasets, this could take some time!")
  }

  het_gp_fit <- hetGP::mleHetGP(Xmat, Y_resp, covtype = covtype)

  het_gp_fit <- hetGP::rebuild(het_gp_fit, robust = TRUE)


  return(list(het_gp_fit = het_gp_fit,
              df = df,
              include_covar = use_depth,
              covar_levels = covar_levels,
              covar_name = covar_name,
              Y_resp = Y_resp,
              Yname = yname))

}

