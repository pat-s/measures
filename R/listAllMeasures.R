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
  return(measureList)
}

# reg = c("SSE", "MSE", "RMSE", "MEDSE", "SAE", "MAE", "MEDAE", "RSQ", "EXPVAR", "ARSQ", "RRSE", "RAE", "MAPE", "MSLE", "RMSLE", "KendallTau", "SpearmanRho")
# bin = c("AUC", "Brier", "BrierScaled", "BAC", "TP", "TN", "FP", "FN", "TPR", "TNR", "FPR", "FNR", "PPV", "NPV", "FDR", "MCC", "F1", "GMEAN", "GPR")
# class = c("MMCE", "ACC", "BER", "multiclass.AUNU", "multiclass.AUNP", "multiclass.AU1U", "multiclass.AU1P", "multiclass.Brier", "Logloss", "SSR", "QSR", "LSR", "KAPPA", "WKAPPA")
# multil = c("MultilabelHamloss", "MultilabelSubset01", "MultilabelF1", "MultilabelACC", "MultilabelPPV", "MultilabelTPR")
# 
# library(tools)
# db <- Rd_db("measures")
# descr = cbind(unlist(unname(lapply(db, tools:::.Rd_get_metadata, "title"))), unlist(unname(lapply(db, tools:::.Rd_get_metadata, "name"))))
# funs = unlist(unname(lapply(db, tools:::.Rd_get_metadata, "usage")))[-seq(1, length(db)*2, 2)]
# probs = grepl("probabilities", funs)
# descr = data.frame(descr, probs)
# colnames(descr) = c("description", "function_name", "probabilities")
# 
# tab = data.frame(c(reg, bin, class, multil),
#   c(rep("regression", length(reg)), rep("binary classification", length(bin)), rep("multiclass classification", length(class)), rep("multilabel", length(multil))))
# colnames(tab) = c("function_name", "task")
# measureList = merge(tab, descr, by = "function_name", all.x = TRUE, sort = FALSE)
# measureList = measureList[, c(1, 3, 2, 4)]
# 
# measureList$minimize = TRUE
# measureList$minimize[measureList$function_name %in% c("RSQ", "EXPVAR", "ARSQ", "KendallTau", "SpearmanRho", 
#   "AUC", "BAC", "TP", "TN", "TPR", "TNR", "PPV", "MCC", "F1", "GMEAN", "GPR", 
#   "ACC", "multiclass.AUNU", "multiclass.AUNP", "multiclass.AU1U", "multiclass.AU1P", "SSR", "QSR", "LSR", "KAPPA", "WKAPPA",
#   "MultilabelF1", "MultilabelACC", "MultilabelPPV", "MultilabelTPR")] = FALSE
# 
# devtools::use_data(measureList, internal = TRUE, overwrite = TRUE)

# library(knitr)
# kable(listAllMeasures())
