# measures

Package that provides the biggest amount of statistical measures in the whole R world!

Includes measures of regression, (multiclass) classification, clustering, survival and multilabel classification.

It is based on measures of [mlr](https://github.com/mlr-org/mlr).

## Installation
The development version

    devtools::install_github("mlr-org/measures")
    
The available measures can be looked up by

    listAllMeasures()
    
|function_name      |task                      |description.x                              |
|:------------------|:-------------------------|:------------------------------------------|
|SSE                |regression                |Sum of squared errors                      |
|MSE                |regression                |Mean of squared errors                     |
|RMSE               |regression                |Root mean squared error                    |
|MEDSE              |regression                |Median of squared errors                   |
|SAE                |regression                |Sum of absolute errors                     |
|MAE                |regression                |Mean of absolute errors                    |
|MEDAE              |regression                |Median of absolute errors                  |
|RSQ                |regression                |Coefficient of determination               |
|EXPVAR             |regression                |Explained variance                         |
|ARSQ               |regression                |Adjusted coefficient of determination      |
|RRSE               |regression                |Root relative squared error                |
|RAE                |regression                |Relative absolute error                    |
|MAPE               |regression                |Mean absolute percentage error             |
|MSLE               |regression                |Mean squared logarithmic error             |
|RMSLE              |regression                |Root mean squared logarithmic error        |
|KendallTau         |regression                |Kendall's tau                              |
|SpearmanRho        |regression                |Spearman's rho                             |
|AUC                |binary classification     |Area under the curve                       |
|Brier              |binary classification     |Brier score                                |
|BrierScaled        |binary classification     |Brier scaled                               |
|BAC                |binary classification     |Balanced accuracy                          |
|TP                 |binary classification     |True positives                             |
|TN                 |binary classification     |True negatives                             |
|FP                 |binary classification     |False positives                            |
|FN                 |binary classification     |False negatives                            |
|TPR                |binary classification     |True positive rate                         |
|TNR                |binary classification     |True negative rate                         |
|FPR                |binary classification     |False positive rate                        |
|FNR                |binary classification     |False negative rate                        |
|PPV                |binary classification     |Positive predictive value                  |
|NPV                |binary classification     |Negative predictive value                  |
|FDR                |binary classification     |False discovery rate                       |
|MCC                |binary classification     |Matthews correlation coefficient           |
|F1                 |binary classification     |F1 measure                                 |
|GMEAN              |binary classification     |G-mean                                     |
|GPR                |binary classification     |Geometric mean of precision and recall.    |
|MMCE               |multiclass classification |Mean misclassification error               |
|ACC                |multiclass classification |Accuracy                                   |
|BER                |multiclass classification |Balanced error rate                        |
|multiclass.AUNU    |multiclass classification |Average 1 vs. rest multiclass AUC          |
|multiclass.AUNP    |multiclass classification |Weighted average 1 vs. rest multiclass AUC |
|multiclass.AU1U    |multiclass classification |Average 1 vs. 1 multiclass AUC             |
|multiclass.AU1P    |multiclass classification |Weighted average 1 vs. 1 multiclass AUC    |
|multiclass.Brier   |multiclass classification |Multiclass Brier score                     |
|Logloss            |multiclass classification |Logarithmic loss                           |
|SSR                |multiclass classification |Spherical Scoring Rule                     |
|QSR                |multiclass classification |Quadratic Scoring Rule                     |
|LSR                |multiclass classification |Logarithmic Scoring Rule                   |
|KAPPA              |multiclass classification |Cohen's kappa                              |
|WKAPPA             |multiclass classification |Mean quadratic weighted kappa              |
|MultilabelHamloss  |multilabel                |Hamming loss                               |
|MultilabelSubset01 |multilabel                |Subset-0-1 loss                            |
|MultilabelF1       |multilabel                |F1 measure (multilabel)                    |
|MultilabelACC      |multilabel                |Accuracy (multilabel)                      |
|MultilabelPPV      |multilabel                |Positive predictive value (multilabel)     |
|MultilabelTPR      |multilabel                |TPR (multilabel)                           |
