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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' negative = 0
#' AUC(probabilities, truth, negative, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' negative = 0
#' Brier(probabilities, truth, negative, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' negative = 0
#' BrierScaled(probabilities, truth, negative, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' negative = 0
#' BAC(truth, response, negative, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' TP(truth, response, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' negative = 0
#' TN(truth, response, negative)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' FP(truth, response, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' negative = 0
#' FN(truth, response, negative)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' TPR(truth, response, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' negative = 0
#' TNR(truth, response, negative)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' negative = 0
#' FPR(truth, response, negative, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' negative = 0
#' FNR(truth, response, negative, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' PPV(truth, response, positive, probabilities = NULL)
#' @export 
PPV = function(truth, response, positive, probabilities = NULL) {
  denominator = sum(response == positive)
  ifelse(denominator == 0, EdgeCase(truth, positive, probabilities), TP(truth, response, positive) / denominator)
}
EdgeCase = function(truth, positive, prob) {
  if (!is.null(prob)) {
    rs = sort(prob, index.return = TRUE)
    erst = ifelse(truth[tail(rs$ix, 1)] == positive, 1, 0)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' negative = 0
#' NPV(truth, response, negative)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' FDR(truth, response, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' negative = 0
#' MCC(truth, response, negative, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' F1(truth, response, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' negative = 0
#' GMEAN(truth, response, negative, positive)
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
#' @examples
#' n = 20
#' set.seed(125)
#' truth = as.factor(sample(c(1,0), n, replace = TRUE))
#' probabilities = runif(n)
#' response = as.factor(as.numeric(probabilities > 0.5))
#' positive = 1
#' GPR(truth, response, positive)
#' @export 
GPR = function(truth, response, positive) {
  sqrt(PPV(truth, response, positive) * TPR(truth, response, positive))
}
