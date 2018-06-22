#' List all measures
#'
#' Lists all measures that are available in the package with their corresponding task. 
#'
#' @return Dataframe with all available measures and the correspoding task
#' @export
#'
#' @examples
#' listAllMeasures()
listAllMeasures = function() {
  reg = c("SSE", "MSE", "RMSE", "MEDSE", "SAE", "MAE", "MEDAE", "RSQ", "EXPVAR", "ARSQ", "RRSE", "RAE", "MAPE", "MSLE", "RMSLE", "KendallTau", "SpearmanRho")
  bin = c("AUC", "Brier", "BrierScaled", "BAC", "TP", "TN", "FP", "FN", "TPR", "TNR", "FPR", "FNR", "PPV", "NPV", "FDR", "MCC", "F1", "GMEAN", "GPR")
  class = c("MMCE", "ACC", "BER", "multiclass.AUNU", "multiclass.AUNP", "multiclass.AU1U", "multiclass.AU1P", "multiclass.Brier", "Logloss", "SSR", "QSR", "LSR", "KAPPA", "WKAPPA")
  multil = c("MultilabelHamloss", "MultilabelSubset01", "MultilabelF1", "MultilabelACC", "MultilabelPPV", "MultilabelTPR")
  tab = data.frame(c(reg, bin, class, multil), c(rep("regression", length(reg)), rep("binary classification", length(bin)), rep("multiclass classification", length(class)), rep("multilabel", length(multil))))
  colnames(tab) = c("function name", "task")
  return(tab)
}
