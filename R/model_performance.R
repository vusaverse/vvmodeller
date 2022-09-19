#' @title Get model perfomance metrics (KS, AUC and ROC)
#' @description Get model performance for tree models (rpart library), or glm. It returns quality metrics: AUC (Area Under ROC Curve) and KS (Kolmogorov-Smirnov), and plots the ROC curve.
#' @param fit model, it could be any of the following: decision tree from rpart package, glm model or randomForest.
#' @param data data frame used to build the model. Also it supports data for testing, (it has to contain same columns as training data.
#' @param target_var It's the name of the column used as target/outcome. It's an string value, write it between apostrohpe.
#' @examples
#' \dontrun{fit_glm=glm(has_heart_disease ~ age + oldpeak, data=heart_disease, family = binomial)
#' model_performance(fit=fit_glm, data = heart_disease, target_var = "has_heart_disease")}
#' @return None.
#' @export
model_performance <- function (fit, data, target_var)
{
  if(grepl("glm", as.character(fit$call))[1]) {
    scr = predict(fit, data)
  } else if(grepl("rpart", as.character(fit$call))[1]) {
    scr = predict(fit, data)[,2] } else if(grepl("randomForest", as.character(fit$call))[1]) {
      scr = predict(fit, data, type = "prob")[, 2]
    } else {
      print("Model not supported")
    }


  ## Get prediction ROCR object
  pred = prediction(scr, data[,target_var])

  ## Plot ROC
  perfROC = performance(pred, "tpr", "fpr")
  plot(perfROC, col=rainbow(10))
  abline(a=0,b=1)
  grid()

  ## AUC
  auc=performance(pred,"auc")@y.values[[1]]
  sprintf("AUC: %0.3f", round(auc,3))

  ## KS
  ks=max(attr(perfROC, "y.values")[[1]] - (attr(perfROC, "x.values")[[1]]))
  sprintf("KS: %0.3f", round(ks,3))

  ## Accuracy
  perfAcc = performance(pred, "acc")
  max(perfAcc@y.values[[1]])

  # Table creation
  dfAcc=data.frame(acc=perfAcc@y.values[[1]], cutoff=perfAcc@x.values[[1]])

  ## Metrics table
  dataMetrics=data.frame(
    AUC=round(auc,3),
    KS=round(ks,3)
  )

  pandoc.table(dataMetrics)
}
