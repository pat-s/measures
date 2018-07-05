###############################################################################
### multilabel ###
###############################################################################

#' Hamming loss
#' 
#' Proportion of labels that are predicted incorrectly, following the definition
#' by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf.
#' 
#' @param truth matrix of true values
#' @param response matrix of predicted values
#' @examples
#' n = 20
#' set.seed(122)
#' truth = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' response = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' MultilabelHamloss(truth, response)
#' @export 
MultilabelHamloss = function(truth, response) {
  mean(truth != response)
}

#' Subset-0-1 loss
#' 
#' Proportion of observations where the complete multilabel set (all 0-1-labels) is predicted incorrectly,
#' following the definition by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf.
#' 
#' @param truth matrix of true values
#' @param response matrix of predicted values
#' @examples
#' n = 20
#' set.seed(122)
#' truth = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' response = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' MultilabelSubset01(truth, response)
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
#' @param truth matrix of true values
#' @param response matrix of predicted values
#' n = 20
#' set.seed(122)
#' truth = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' response = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' MultilabelF1(truth, response)
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
#' @param truth matrix of true values
#' @param response matrix of predicted values
#' n = 20
#' set.seed(122)
#' truth = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' response = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' MultilabelACC(truth, response)
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
#' @param truth matrix of true values
#' @param response matrix of predicted values
#' n = 20
#' set.seed(122)
#' truth = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' response = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' MultilabelPPV(truth, response)
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
#' @param truth matrix of true values
#' @param response matrix of predicted values
#' n = 20
#' set.seed(122)
#' truth = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' response = matrix(sample(c(0,1), 60, replace = TRUE), 20, 3)
#' MultilabelTPR(truth, response)
#' @export 
MultilabelTPR = function(truth, response) {
  numerator = rowSums(truth & response)
  denominator = rowSums(truth)
  mean(numerator / denominator, na.rm = TRUE)
}
