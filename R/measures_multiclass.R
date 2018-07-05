###############################################################################
### classif multi ###
###############################################################################

#' Mean misclassification error
#' 
#' Defined as: mean(response != truth)
#' 
#' @param truth vector of true values 
#' @param response vector of predicted values
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' response = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' MMCE(truth, response)
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
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' response = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' ACC(truth, response)
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
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' response = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' BER(truth, response)
#' @export
BER = function(truth, response) {
  # special case for predictions from FailureModel
  if (anyNA(response))
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
#' @param probabilities [numeric] matrix of predicted probabilities with columnnames of the classes
#' @param truth vector of true values
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' multiclass.AUNU(probabilities, truth)
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
#' @param probabilities [numeric] matrix of predicted probabilities with columnnames of the classes
#' @param truth vector of true values 
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' multiclass.AUNP(probabilities, truth)
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
#' @param probabilities [numeric] matrix of predicted probabilities with columnnames of the classes
#' @param truth vector of true values 
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' multiclass.AU1U(probabilities, truth)
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
#' @param probabilities [numeric] matrix of predicted probabilities with columnnames of the classes
#' @param truth vector of true values 
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' multiclass.AU1P(probabilities, truth)
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
#' @param probabilities [numeric] matrix of predicted probabilities with columnnames of the classes
#' @param truth vector of true values
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' multiclass.Brier(probabilities, truth)
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
#' @param probabilities [numeric] vector (or matrix with column names of the classes) of predicted probabilities
#' @param truth vector of true values
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' Logloss(probabilities, truth)
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
#' @param probabilities [numeric] vector (or matrix with column names of the classes) of predicted probabilities 
#' @param truth vector of true values
#' @examples
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' SSR(probabilities, truth)
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
#' @param probabilities [numeric] vector (or matrix with column names of the classes) of predicted probabilities
#' @param truth vector of true values 
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' QSR(probabilities, truth)
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
#' @param probabilities [numeric] vector (or matrix with column names of the classes) of predicted probabilities 
#' @param truth vector of true values 
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' probabilities = matrix(runif(60), 20, 3)
#' probabilities = probabilities/rowSums(probabilities)
#' colnames(probabilities) = c(1,2,3)
#' LSR(probabilities, truth)
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
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' response = as.factor(sample(c(1,2,3), n, repla
#' KAPPA(truth, response)
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
#' n = 20
#' set.seed(122)
#' truth = as.factor(sample(c(1,2,3), n, replace = TRUE))
#' response = as.factor(sample(c(1,2,3), n, repla
#' WKAPPA(truth, response)
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
