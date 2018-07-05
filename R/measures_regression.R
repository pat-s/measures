###############################################################################
### regression ###
###############################################################################

#' Sum of squared errors
#' 
#' Defined as: sum((response - truth)^2)
#' 
#' @param truth [numeric] vector of true values
#' @param response [numeric] vector of predicted values
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' SSE(truth, response)
#' @export 
SSE = function(truth, response) {
  sum((response - truth)^2)
}

#' Mean of squared errors
#' 
#' Defined as: mean((response - truth)^2)
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values 
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' MSE(truth, response)
#' @export
MSE = function(truth, response) {
  mean((response - truth)^2)
}

#' Root mean squared error
#' 
#' The RMSE is aggregated as sqrt(mean(rmse.vals.on.test.sets^2))
#'
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values 
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' RMSE(truth, response)
#' @export
RMSE = function(truth, response) {
  sqrt(MSE(truth, response))
}

#' Median of squared errors
#' 
#' Defined as: median((response - truth)^2).
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values 
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' MEDSE(truth, response)
#' @export 
MEDSE = function(truth, response) {
  median((response - truth)^2)
}

#' Sum of absolute errors
#' 
#' Defined as: sum(abs(response - truth))"
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values 
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' SAE(truth, response)
#' @export
SAE = function(truth, response) {
  sum(abs(response - truth))
}

#' Mean of absolute errors
#' 
#' Defined as: mean(abs(response - truth))
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values 
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' MAE(truth, response)
#' @export 
MAE = function(truth, response) {
  mean(abs(response - truth))
}

#' Median of absolute errors
#' 
#' Defined as: median(abs(response - truth)).
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values 
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' MEDAE(truth, response)
#' @export 
MEDAE = function(truth, response) {
  median(abs(response - truth))
}

#' Coefficient of determination
#' 
#' Also called R-squared, which is 1 - residual_sum_of_squares / total_sum_of_squares.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values 
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' RSQ(truth, response)
#' @export 
RSQ = function(truth, response) {
  rss = SSE(truth, response)
  ess = sum((truth - mean(truth))^2L)
  if (ess == 0){
    warning(" is undefined if all truth values are equal.")
    return(NA_real_)
  }
  1 - rss / ess
}

#' Explained variance
#' 
#' Similar to RSQ (R-squared). Defined as explained_sum_of_squares / total_sum_of_squares.
#' 
#' @param truth [numeric] vector of true values
#' @param response [numeric] vector of predicted values 
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' EXPVAR(truth, response)
#' @export 
EXPVAR = function(truth, response) {
  regss = sum((response - mean(truth))^2L)
  ess = sum((truth - mean(truth))^2L)
  if (ess == 0){
    warning(" is undefined if all truth values are equal.")
    return(NA_real_)
  }
  regss / ess
}

#' Adjusted coefficient of determination
#' 
#' Defined as: 1 - (1 - rsq) * (p / (n - p - 1L)). Adjusted R-squared is only defined for normal linear regression.
#' 
#' @param truth [numeric] vector of true values
#' @param response [numeric] vector of predicted values 
#' @param n [numeric] number of observations 
#' @param p [numeric] number of predictors
#' @examples
#' n = 20
#' p = 5
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' ARSQ(truth, response, n, p)
#' @export 
ARSQ = function(truth, response, n, p) {
  n = length(truth)
  if (n == p + 1){
    warning("Adjusted R-squared is undefined if the number observations is equal to the number of independent variables plus one.")
    return(NA_real_)
  }
  1 - (1 - RSQ(truth, response)) * (p / (n - p - 1L))
}

#' Root relative squared error
#' 
#' Defined as sqrt (sum_of_squared_errors / total_sum_of_squares). Undefined for single instances and when every truth value is identical. In this case the output will be NA.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' RRSE(truth, response)
#' @export 
RRSE = function(truth, response){
  tss = sum((truth - mean(truth))^2L)
  if (tss == 0){
    warning(" is undefined if all truth values are equal.")
    return(NA_real_)
  }
  sqrt(SSE(truth, response) / tss)
}

#' Relative absolute error
#' 
#' Defined as sum_of_absolute_errors / mean_absolute_deviation. Undefined for single instances and when every truth value is identical. In this case the output will be NA.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' RAE(truth, response)
#' @export 
RAE = function(truth, response){
  meanad = sum(abs(truth - mean(truth)))
  if (meanad == 0){
    warning(" is undefined if all truth values are equal.")
    return(NA_real_)
  }
  return(SAE(truth, response) / meanad)
}

#' Mean absolute percentage error
#' 
#' Defined as the abs(truth_i - response_i) / truth_i. Won't work if any truth value is equal to zero. In this case the output will be NA.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' MAPE(truth, response)
#' @export
MAPE = function(truth, response){
  if (any(truth == 0)){
    warning(" is undefined if any truth value is equal to 0.")
    return(NA_real_)
  }
  return(mean(abs((truth - response) / truth)))
}

#' Mean squared logarithmic error
#' 
#' Defined as: mean((log(response + 1, exp(1)) - log(truth + 1, exp(1)))^2).
#' This is mostly used for count data, note that all predicted and actual target values must be greater or equal '-1'
#' to compute the mean squared logarithmic error.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values
#' @examples
#' n = 20
#' set.seed(123)
#' truth = abs(rnorm(n))
#' response = abs(rnorm(n))
#' MSLE(truth, response)
#' @export 
MSLE = function(truth, response) {
  if (any(truth < -1))
    stop("All truth values must be greater or equal -1")
  if (any(response < -1))
    stop("All predicted values must be greater or equal -1")
  
  mean((log(response + 1) - log(truth + 1))^2)
}

#' Root mean squared logarithmic error
#' 
#' Definition taken from: https: / /www.kaggle.com / wiki / RootMeanSquaredLogarithmicError.
#' This  is mostly used for count data, note that all predicted and actual target values
#' must be greater or equal '-1' to compute the root mean squared logarithmic error.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values
#' @examples
#' n = 20
#' set.seed(123)
#' truth = abs(rnorm(n))
#' response = abs(rnorm(n))
#' RMSLE(truth, response)
#' @export 
RMSLE = function(truth, response) {
  sqrt(MSLE(truth, response))
}

#' Kendall's tau
#' 
#' Defined as: Kendall's tau correlation between truth and response. Only looks at the order.
#' See Rosset et al.: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.1398&rep=rep1&type=pdf.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values
#' @importFrom stats as.formula cor median model.matrix
#' @importFrom utils combn tail
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' KendallTau(truth, response)
#' @export 
KendallTau = function(truth, response) {
  cor(truth, response, use = "na.or.complete", method = "kendall")
}

#' Spearman's rho
#' 
#' Defined as: Spearman's rho correlation between truth and response. Only looks at the order.
#' See Rosset et al.: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.1398&rep=rep1&type=pdf.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values
#' @examples
#' n = 20
#' set.seed(123)
#' truth = rnorm(n)
#' response = rnorm(n)
#' SpearmanRho(truth, response)
#' @export 
SpearmanRho = function(truth, response) {
  cor(truth, response, use = "na.or.complete", method = "spearman")
}
