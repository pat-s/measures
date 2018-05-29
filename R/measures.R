###############################################################################
### regression ###
###############################################################################

#' Sum of squared errors
#' 
#' Defined as: sum((response - truth)^2)
#' 
#' @param truth [numeric] vector of true values
#' @param response [numeric] vector of predicted values
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
#' @export 
ARSQ = function(truth, response, n, p) {
  n = length(pred$data$truth)
  p = length(model$features)
  if (n == p + 1){
    warning("Adjusted R-squared is undefined if the number observations is equal to the number of independent variables plus one.")
    return(NA_real_)
  }
  1 - (1 - RSQ(pred$data$truth, pred$data$response)) * (p / (n - p - 1L))
}

#' Root relative squared error
#' 
#' Defined as sqrt (sum_of_squared_errors / total_sum_of_squares). Undefined for single instances and when every truth value is identical. In this case the output will be NA.
#' 
#' @param truth [numeric] vector of true values 
#' @param response [numeric] vector of predicted values
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
#' @export 
SpearmanRho = function(truth, response) {
  cor(truth, response, use = "na.or.complete", method = "spearman")
}

###############################################################################
### classif multi ###
###############################################################################

#' Mean misclassification error
#' 
#' Defined as: mean(response != truth)
#' 
#' @param truth vector of true values 
#' @param response vector of predicted values
#' @export
MMCE = function(truth, response) {
  mean(response != truth)
}

#' Accuracy
#' 
#' Defined as: mean(response == truth)
#' 
#' @param truth vector of true values 
#' @param response vector of predicted values
#' @export 
ACC = function(truth, response) {
  mean(response == truth)
}

#' Balanced error rate
#' 
#' Mean of misclassification error rates on all individual classes.
#' 
#' @param truth vector of true values 
#' @param response vector of predicted values
#' @export
BER = function(truth, response) {
  # special case for predictions from FailureModel
  if (anyMissing(response))
    return(NA_real_)
  mean(diag(1 - (table(truth, response) / table(truth, truth))))
}

#' Average 1 vs. rest multiclass AUC
#' 
#' Computes the AUC treating a c-dimensional classifier as c two-dimensional classifiers, 
#' where classes are assumed to have uniform distribution, in order to have a measure which is 
#' independent of class distribution change. 
#' See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.
#' 
#' @param probabilities [numeric] vector of predicted probabilities 
#' @param truth vector of true values
#' @export
multiclass.AUNU = function(probabilities, truth) {
  if (length(unique(truth)) != nlevels(truth)){
    warning("Measure is undefined if there isn't at least one sample per class.")
    return(NA_real_)
  }
  mean(vnapply(1:nlevels(truth), function(i) colAUC(probabilities[, i], truth == levels(truth)[i])))
}

#' Weighted average 1 vs. rest multiclass AUC
#' 
#' Computes the AUC treating a c-dimensional classifier as c two-dimensional classifiers, 
#' taking into account the prior probability of each class. 
#' See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.
#' 
#' @param probabilities 
#' @param truth vector of true values 
#' @export
multiclass.AUNP = function(probabilities, truth) {
  if (length(unique(truth)) != nlevels(truth)){
    warning("Measure is undefined if there isn't at least one sample per class.")
    return(NA_real_)
  }
  sum(vnapply(1:nlevels(truth), function(i) mean(truth == levels(truth)[i]) * colAUC(probabilities[, i], truth == levels(truth)[i])))
}

#' Average 1 vs. 1 multiclass AUC
#' 
#' Computes AUC of c(c - 1) binary classifiers (all possible pairwise combinations) 
#' while considering uniform distribution of the classes. 
#' See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.
#' 
#' @param probabilities [numeric] vector of predicted probabilities 
#' @param truth vector of true values 
#' @export
multiclass.AU1U = function(probabilities, truth) {
  m = colAUC(probabilities, truth)
  c = c(combn(1:nlevels(truth), 2))
  mean(m[cbind(rep(seq_len(nrow(m)), each = 2), c)])
}

#' Weighted average 1 vs. 1 multiclass AUC
#' 
#' Computes AUC of c(c - 1) binary classifiers while considering the a priori distribution of the classes. 
#' See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.
#' 
#' @param probabilities [numeric] vector of predicted probabilities
#' @param truth vector of true values 
#' @export 
multiclass.AU1P = function(probabilities, truth) {
  m = colAUC(probabilities, truth)
  weights = table(truth) / length(truth)
  m = m * matrix(rep(weights, each = nrow(m)), ncol = length(weights))
  c = c(combn(1:nlevels(truth), 2))
  sum(m[cbind(rep(seq_len(nrow(m)), each = 2), c)]) / (nlevels(truth) - 1)
}

#' Multiclass Brier score
#' 
#' Defined as: (1/n) sum_i sum_j (y_ij - p_ij)^2, where y_ij = 1 if observation i has class j (else 0), 
#' and p_ij is the predicted probability of observation i for class j. 
#' From http://docs.lib.noaa.gov/rescue/mwr/078/mwr-078-01-0001.pdf.
#' 
#' @param probabilities [numeric] vector of predicted probabilities
#' @param truth vector of true values
#' @export 
multiclass.Brier = function(probabilities, truth) {
  truth = factor(truth, levels = colnames(probabilities))
  mat01 = createDummyFeatures(truth)
  mean(rowSums((probabilities - mat01)^2))
}

#' Logarithmic loss
#' 
#' Defined as: -mean(log(p_i)), where p_i is the predicted probability of the true 
#' class of observation i. Inspired by https://www.kaggle.com/wiki/MultiClassLogLoss.
#' 
#' @param probabilities [numeric] vector of predicted probabilities
#' @param truth vector of true values
#' @export
Logloss = function(probabilities, truth){
  eps = 1e-15
  #let's confine the predicted probabilities to [eps,1 - eps], so logLoss doesn't reach infinity under any circumstance
  probabilities[probabilities > 1 - eps] = 1 - eps
  probabilities[probabilities < eps] = eps
  truth = match(as.character(truth), colnames(probabilities))
  p = getRowEls(probabilities, truth)
  -1 * mean(log(p))
}

#' Spherical Scoring Rule
#' 
#' Defined as: mean(p_i(sum_j(p_ij))), where p_i is the predicted probability of the true 
#' class of observation i and p_ij is the predicted probablity of observation i for class j.
#' See: Bickel, J. E. (2007). Some comparisons among quadratic, spherical, and logarithmic 
#' scoring rules. Decision Analysis, 4(2), 49-65.
#' 
#' @param probabilities [numeric] vector of predicted probabilities 
#' @param truth vector of true values
#' @export
SSR = function(probabilities, truth){
  truth = match(as.character(truth), colnames(probabilities))
  p = getRowEls(probabilities, truth)
  mean(p / sqrt(rowSums(probabilities^2)))
}

#' Quadratic Scoring Rule
#' 
#' Defined as: 1 - (1/n) sum_i sum_j (y_ij - p_ij)^2, where y_ij = 1 if observation i has class j (else 0), 
#' and p_ij is the predicted probablity of observation i for class j.
#' This scoring rule is the same as 1 - multiclass.brier.
#' See: Bickel, J. E. (2007). Some comparisons among quadratic, spherical, and logarithmic scoring rules. Decision Analysis, 4(2), 49-65.
#' 
#' @param probabilities [numeric] vector of predicted probabilities
#' @param truth vector of true values 
#' @export
QSR = function(probabilities, truth){
  #We add this line because binary tasks only output one probability column
  if (is.null(dim(probabilities))) probabilities = cbind(probabilities, 1 - probabilities)
  truth = factor(truth, levels = colnames(probabilities))
  1 - mean(rowSums((probabilities - createDummyFeatures(truth))^2))
}

#' Logarithmic Scoring Rule
#' 
#' Defined as: mean(log(p_i)), where p_i is the predicted probability of the true class of observation i.
#' This scoring rule is the same as the negative logloss, self-information or surprisal.
#' See: Bickel, J. E. (2007). Some comparisons among quadratic, spherical, and logarithmic scoring rules. Decision Analysis, 4(2), 49-65.
#' 
#' @param probabilities [numeric] vector of predicted probabilities 
#' @param truth vector of true values 
#' @export
LSR = function(probabilities, truth){
  -1 * Logloss(probabilities, truth)
}

#' Cohen's kappa
#' 
#' Defined as: 1 - (1 - p0) / (1 - pe). With: p0 = 'observed frequency of
#' agreement' and pe = 'expected agremeent frequency under independence
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @export
KAPPA = function(truth, response) {
  # get confusion matrix
  conf.mat = table(truth, response)
  conf.mat = conf.mat / sum(conf.mat)
  
  # observed agreement frequency
  p0 = sum(diag(conf.mat))
  
  # get expected probs under independence
  rowsum = rowSums(conf.mat)
  colsum = colSums(conf.mat)
  pe = sum(rowsum * colsum) / sum(conf.mat)^2
  
  # calculate kappa
  1 - (1 - p0) / (1 - pe)
}

#' Mean quadratic weighted kappa
#' 
#' Defined as: 1 - sum(weights * conf.mat) / sum(weights * expected.mat),
#' the weight matrix measures seriousness of disagreement with the squared euclidean metric.
#' 
#' @param truth vector of true values 
#' @param response vector of predicted values
#' @export
WKAPPA = function(truth, response) {
  # get confusion matrix
  conf.mat = table(truth, response)
  conf.mat = conf.mat / sum(conf.mat)
  
  # get expected probs under independence
  rowsum = rowSums(conf.mat)
  colsum = colSums(conf.mat)
  expected.mat = rowsum %*% t(colsum)
  
  # get weights
  class.values = seq_along(levels(truth)) - 1L
  weights = outer(class.values, class.values, FUN = function(x, y) (x - y)^2)
  
  # calculate weighted kappa
  1 - sum(weights * conf.mat) / sum(weights * expected.mat)
}

###############################################################################
### classif binary ###
###############################################################################

#' Area under the curve
#' 
#' Integral over the graph that results from computing fpr and tpr for many different thresholds.
#' 
#' @param probabilities [numeric] vector of predicted probabilities 
#' @param truth vector of true values
#' @param negative negative class
#' @param positive positive class 
#' @export
AUC = function(probabilities, truth, negative, positive) {
  if (is.factor(truth)) {
    i = as.integer(truth) == which(levels(truth) == positive)
  } else {
    i = truth == positive
  }
  if (length(unique(i)) < 2L) {
    stop("truth vector must have at least two classes")
  }
  #Use fast ranking function from data.table for larger vectors
  #if (length(i) > 5000L) {
  #   r = frankv(probabilities)
  #} else {
    r = rank(probabilities)
  #}
  n.pos = as.numeric(sum(i))
  n.neg = length(i) - n.pos
  (sum(r[i]) - n.pos * (n.pos + 1) / 2) / (n.pos * n.neg)
}

#' Brier score
#' 
#' The Brier score is defined as the quadratic difference between the probability and the value (1,0) for the class.
#' That means we use the numeric representation 1 and 0 for our target classes. It is similiar to the mean squared error in regression.
#' multiclass.brier is the sum over all one vs. all comparisons and for a binary classifcation 2 * brier.
#' 
#' @param probabilities [numeric] vector of predicted probabilities 
#' @param truth vector of true values
#' @param negative negative class
#' @param positive positive class 
#' @export
Brier = function(probabilities, truth, negative, positive) {
  y = as.numeric(truth == positive)
  mean((y - probabilities)^2)
}

#' Brier scaled
#' 
#' Brier score scaled to [0,1], see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3575184/.
#' 
#' @param probabilities [numeric] vector of predicted probabilities 
#' @param truth vector of true values
#' @param negative negative class
#' @param positive positive class 
#' @export
BrierScaled = function(probabilities, truth, negative, positive) {
  y = as.numeric(truth == positive)
  brier = mean((y - probabilities)^2)
  inc = mean(probabilities)
  brier.max = inc * (1 - inc)^2 + (1 - inc) * inc^2
  1 - brier / brier.max
}

#' Balanced accuracy
#' 
#' Mean of true positive rate and true negative rate.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @param positive positive class 
#' @export
BAC = function(truth, response, negative, positive) {
  mean(c(
    TP(truth, response, positive) / sum(truth == positive),
    TN(truth, response, negative) / sum(truth == negative)
  ))
}

#' True positives
#' 
#' Sum of all correctly classified observations in the positive class.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param positive positive class 
#' @export TP 
TP = function(truth, response, positive) {
  sum(truth == response & response == positive)
}

#' True negatives
#' 
#' Sum of correctly classified observations in the negative class. Also called correct rejections.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @export
TN = function(truth, response, negative) {
  sum(truth == response & response == negative)
}

#' False positives
#' 
#' Sum of misclassified observations in the positive class. Also called false alarms.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param positive positive class 
#' @export 
FP = function(truth, response, positive) {
  sum(truth != response & response == positive)
}

#' False negatives
#' 
#' Sum of misclassified observations in the negative class. Also called misses.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @export 
FN = function(truth, response, negative) {
  sum(truth != response & response == negative)
}

#' True positive rate
#' 
#' Percentage of correctly classified observations in the positive class. Also called hit rate or recall or sensitivity.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param positive positive class 
#' @export 
TPR = function(truth, response, positive) {
  TP(truth, response, positive) / sum(truth == positive)
}

#' True negative rate
#' 
#' Percentage of correctly classified observations in the negative class. Also called specificity.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @export 
TNR = function(truth, response, negative) {
  TN(truth, response, negative) / sum(truth == negative)
}

#' False positive rate
#' 
#' Percentage of misclassified observations in the positive class. Also called false alarm rate or fall-out.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @param positive positive class 
#' @export 
FPR = function(truth, response, negative, positive) {
  FP(truth, response, positive) / sum(truth == negative)
}

#' False negative rate
#' 
#' Percentage of misclassified observations in the negative class.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @param positive positive class 
#' @export 
FNR = function(truth, response, negative, positive) {
  FN(truth, response, negative) / sum(truth == positive)
}

#' Positive predictive value
#' 
#' Defined as: tp / (tp + fp). Also called precision. If the denominator is 0, PPV is set to be either 1 or 0 
#' depending on whether the highest probability prediction is positive (1) or negative (0).
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param positive positive class 
#' @param probabilities [numeric] vector of predicted probabilities 
#' @export 
PPV = function(truth, response, positive, probabilities = NULL) {
  denominator = sum(response == positive)
  ifelse(denominator == 0, EdgeCase(truth, positive, probabilities), TP(truth, response, positive) / denominator)
}
EdgeCase = function(truth, positive, prob) {
  if (!is.null(prob)) {
    rs = sort(prob, index.return = TRUE)
    erst = ifelse(truth[getLast(rs$ix)] == positive, 1, 0)
  } else {
    erst = NA
  }
  erst
}

#' Negative predictive value
#' 
#' Defined as: tn / (tn + fn).
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @export  
NPV = function(truth, response, negative) {
  TN(truth, response, negative) / sum(response == negative)
}

#' False discovery rate
#' 
#' Defined as: fp / (tp + fp)
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param positive positive class 
#' @export 
FDR = function(truth, response, positive) {
  FP(truth, response, positive) / sum(response == positive)
}

#' Matthews correlation coefficient
#' 
#' Defined as (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)), denominator set to 1 if 0.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @param positive positive class 
#' @export 
MCC = function(truth, response, negative, positive) {
  tn = as.numeric(TN(truth, response, negative))
  tp = as.numeric(TP(truth, response, positive))
  fn = as.numeric(FN(truth, response, negative))
  fp = as.numeric(FP(truth, response, positive))
  denom = sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  # According to Wikipedia, the denominator can be set arbitrarily if it's 0. 1 seems to make as much sense as anything else.
  if (denom == 0) denom = 1
  (tp * tn - fp * fn) / denom
}

#' F1 measure
#' 
#' Defined as: 2 * tp/ (sum(truth == positive) + sum(response == positive))
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param positive positive class 
#' @export 
F1 = function(truth, response, positive) {
  2 * TP(truth, response, positive) /
    (sum(truth == positive) + sum(response == positive))
}

#' G-mean
#' 
#' Geometric mean of recall and specificity.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param negative negative class
#' @param positive positive class 
#' @export 
#' @references
#' He, H. & Garcia, E. A. (2009)
#' *Learning from Imbalanced Data.*
#' IEEE Transactions on Knowledge and Data Engineering, vol. 21, no. 9. pp. 1263-1284.
GMEAN = function(truth, response, negative, positive) {
  sqrt(TPR(truth, response, positive) * TNR(truth, response, negative))
}

#' Geometric mean of precision and recall.
#' 
#' Defined as: sqrt(ppv * tpr)
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @param positive positive class 
#' @export 
GPR = function(truth, response, positive) {
  sqrt(PPV(truth, response, positive) * TPR(truth, response, positive))
}

###############################################################################
### multilabel ###
###############################################################################

#' Hamming loss
#' 
#' Proportion of labels that are predicted incorrectly, following the definition
#' by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @export 
MultilabelHamloss = function(truth, response) {
  mean(truth != response)
}

#' Subset-0-1 loss
#' 
#' Proportion of observations where the complete multilabel set (all 0-1-labels) is predicted incorrectly,
#' following the definition by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @export MultilabelSubset01 
MultilabelSubset01 = function(truth, response) {
  mean(!apply(truth == response, 1, all))
}

#' F1 measure (multilabel)
#' 
#' Harmonic mean of precision and recall on a per instance basis (Micro-F1), following the
#' definition by Montanes et al.: http: / /www.sciencedirect.com / science / article / pii / S0031320313004019.
#' Fractions where the denominator becomes 0 are replaced with 1 before computing the average across all instances.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @export 
MultilabelF1 = function(truth, response) {
  numerator = 2 * rowSums(truth & response)
  denominator = rowSums(truth + response)
  mean(ifelse(denominator == 0, 1, numerator / denominator))
}

#' Accuracy (multilabel)
#' 
#' Averaged proportion of correctly predicted labels with respect to the total number of labels for each instance,
#' following the definition by Charte and Charte: https: / /journal.r-project.org / archive / 2015 - 2 / charte-charte.pdf.
#' Fractions where the denominator becomes 0 are replaced with 1 before computing the average across all instances.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @export 
MultilabelACC = function(truth, response) {
  numerator = rowSums(truth & response)
  denominator = rowSums(truth | response)
  mean(ifelse(denominator == 0, 1, numerator / denominator))
}

#' Positive predictive value (multilabel)
#' 
#' Also called precision. Averaged ratio of correctly predicted labels for each instance,
#' following the definition by Charte and Charte: https: / /journal.r-project.org / archive / 2015 - 2 / charte-charte.pdf.
#' Fractions where the denominator becomes 0 are ignored in the average calculation.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @export 
MultilabelPPV = function(truth, response) {
  numerator = rowSums(truth & response)
  denominator = rowSums(response)
  mean(numerator / denominator, na.rm = TRUE)
}

#' TPR (multilabel)
#' 
#' Also called recall. Averaged proportion of predicted labels which are relevant for each instance,
#' following the definition by Charte and Charte: https: / /journal.r-project.org / archive / 2015 - 2 / charte-charte.pdf.
#' Fractions where the denominator becomes 0 are ignored in the average calculation.
#' 
#' @param truth vector of true values
#' @param response vector of predicted values
#' @export 
MultilabelTPR = function(truth, response) {
  numerator = rowSums(truth & response)
  denominator = rowSums(truth)
  mean(numerator / denominator, na.rm = TRUE)
}
