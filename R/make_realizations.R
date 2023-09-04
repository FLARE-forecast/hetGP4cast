#' make_realizations
#'
#' @param predObject the object output from predict_hetgp()
#' @param nreals number of realizations to generate (default is 200)
#'
#' @return a list containing a data.frame of realizations and a matrix containing realizations
#' @export
#'
#' @examples
#' data(sample_lake_data_1mdepth)
#' mod1 =  fit_hetgp(X = "DOY", Y = "temperature",site_id = "FCR", df = sample_lake_data_1mdepth)
#' preds <- predict_hetgp(het_gp_object = mod1, reference_datetime = as.Date("2022-10-05"), save_covmat=TRUE)
#'
#' sims = make_realizations(predObject = preds)
#'
make_realizations = function(predObject, nreals = 200){
  preds4plotting = predObject$preds4plotting
  mu = predObject$preds4plotting$Mean
  sigma = predObject$covmat
  if (is.null(sigma)){
    stop("The covariance matrix cannot be NULL. Try running predict_hetgp() using save_covmat = TRUE")
  }

  reals = rmultnorm(n = nreals, mu = mu, sigma = sigma)
  temp = as.vector(t(reals))

  pred_df = predObject$pred_df
  pred_df = pred_df[pred_df$parameter == "mu", ]
  horizons = nrow(pred_df)
  bigdf = Reduce(function(.x, .y) rbind(.x, pred_df), seq_len(nreals -1), init = pred_df)
  bigdf$prediction = NULL
  tempdf = data.frame(prediction = temp, number = rep(1:nreals, each = horizons))
  bigdf = cbind(bigdf, tempdf)
  return(list(realizations = reals, pred_df_reals = bigdf))

}

#' rmultnorm
#'
#' @param n number of realizations
#' @param mu mean vector of length m
#' @param sigma covariance matrix of dimension mxm
#'
#' @return a matrix of dimension n x m
#' @export
#'
#' @examples
#' covpow <- function(locs,pow=2,scale=5){
#' d1 <- dist(locs)
#' n <- dim(locs)[1]
#' mat <- matrix(0,n,n)
#' mat[lower.tri(mat)] <- d1
#' mat <- mat+t(mat)
#' cc <- exp(-(mat/scale)^pow)
#' return(cc)
#' }
#' locs <- expand.grid(1:20,1:20)
#' locs <- cbind(locs[,1],locs[,2])
#' covmat<- covpow(locs)
#' z <- rmultnorm(1,rep(0,20*20), covmat)
rmultnorm <- function(n, mu, sigma){
  p <- length(mu)
  z <- matrix(rnorm(n * p),nrow = n)
  svdsig <- svd(sigma)
  ch <- sqrt(diag(svdsig$d)) %*% t(svdsig$u)
  zz <- z %*% ch
  zz + matrix(mu,nrow=n,ncol=p,byrow=T)
}
