# measures

Package that provides the biggest amount of statistical measures in the whole R world!

Includes measures of regression, (multiclass) classification, clustering, survival and multilabel classification.

It is based on measures of [mlr](https://github.com/mlr-org/mlr).

## Installation
The development version

    devtools::install_github("mlr-org/measures")
    
The available measures can be looked up by

    listAllMeasures()
    
|function_name      |description                                |task                      |
|:------------------|:------------------------------------------|:-------------------------|
|SSE                |Sum of squared errors                      |regression                |
|MSE                |Mean of squared errors                     |regression                |
|RMSE               |Root mean squared error                    |regression                |
|MEDSE              |Median of squared errors                   |regression                |
|SAE                |Sum of absolute errors                     |regression                |
|MAE                |Mean of absolute errors                    |regression                |
|MEDAE              |Median of absolute errors                  |regression                |
|RSQ                |Coefficient of determination               |regression                |
|EXPVAR             |Explained variance                         |regression                |
|ARSQ               |Adjusted coefficient of determination      |regression                |
|RRSE               |Root relative squared error                |regression                |
|RAE                |Relative absolute error                    |regression                |
|MAPE               |Mean absolute percentage error             |regression                |
|MSLE               |Mean squared logarithmic error             |regression                |
|RMSLE              |Root mean squared logarithmic error        |regression                |
|KendallTau         |Kendall's tau                              |regression                |
|SpearmanRho        |Spearman's rho                             |regression                |
|AUC                |Area under the curve                       |binary classification     |
|Brier              |Brier score                                |binary classification     |
|BrierScaled        |Brier scaled                               |binary classification     |
|BAC                |Balanced accuracy                          |binary classification     |
|TP                 |True positives                             |binary classification     |
|TN                 |True negatives                             |binary classification     |
|FP                 |False positives                            |binary classification     |
|FN                 |False negatives                            |binary classification     |
|TPR                |True positive rate                         |binary classification     |
|TNR                |True negative rate                         |binary classification     |
|FPR                |False positive rate                        |binary classification     |
|FNR                |False negative rate                        |binary classification     |
|PPV                |Positive predictive value                  |binary classification     |
|NPV                |Negative predictive value                  |binary classification     |
|FDR                |False discovery rate                       |binary classification     |
|MCC                |Matthews correlation coefficient           |binary classification     |
|F1                 |F1 measure                                 |binary classification     |
|GMEAN              |G-mean                                     |binary classification     |
|GPR                |Geometric mean of precision and recall.    |binary classification     |
|MMCE               |Mean misclassification error               |multiclass classification |
|ACC                |Accuracy                                   |multiclass classification |
|BER                |Balanced error rate                        |multiclass classification |
|multiclass.AUNU    |Average 1 vs. rest multiclass AUC          |multiclass classification |
|multiclass.AUNP    |Weighted average 1 vs. rest multiclass AUC |multiclass classification |
|multiclass.AU1U    |Average 1 vs. 1 multiclass AUC             |multiclass classification |
|multiclass.AU1P    |Weighted average 1 vs. 1 multiclass AUC    |multiclass classification |
|multiclass.Brier   |Multiclass Brier score                     |multiclass classification |
|Logloss            |Logarithmic loss                           |multiclass classification |
|SSR                |Spherical Scoring Rule                     |multiclass classification |
|QSR                |Quadratic Scoring Rule                     |multiclass classification |
|LSR                |Logarithmic Scoring Rule                   |multiclass classification |
|KAPPA              |Cohen's kappa                              |multiclass classification |
|WKAPPA             |Mean quadratic weighted kappa              |multiclass classification |
|MultilabelHamloss  |Hamming loss                               |multilabel                |
|MultilabelSubset01 |Subset-0-1 loss                            |multilabel                |
|MultilabelF1       |F1 measure (multilabel)                    |multilabel                |
|MultilabelACC      |Accuracy (multilabel)                      |multilabel                |
|MultilabelPPV      |Positive predictive value (multilabel)     |multilabel                |
|MultilabelTPR      |TPR (multilabel)                           |multilabel                |
