#' predict the classes using trained algorithms and give cnfusion matrix  in return

#'
#' @param x input is a dataframe for bag of word file in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return 3 confusion matrix for each trained classification algorithm
#' @author Atousa Zarindast
#' @export
#' @import tidyverse
#' @import tidyr
#' @import assertthat
#' @import testthat
#' @import caret
#' @import tidyr

BuildPrediction <- function(list) {
  library(assertthat)
  library(caret)
  library(tidyr)
  library(testthat)
  library(tidyverse)
  #library(RWeka)
  library(eply)
  library(purrr)

  #list <- BuildTraining(x)
  x <- list[1]
  x<-map_df(x, ~.x)

  target <- as.factor(as.matrix(x[, ncol(x)])) #target data to be compared with predicted model
  
  #gbm
  gbm_pred = predict(list[[2]], x) #prediction of gbm, to be compared to the data target
  gbm_con <- confusionMatrix(gbm_pred, target)  #confusion matrix gbm

  #knn
  #knn_pred = predict(list[[3]], x) #predict
  #knn_con <- confusionMatrix(knn_pred, target) #confusion matrix kNN
  
  #naive bayes
  NB_pred = predict(list[[4]], x) #predict
  NB_con <- confusionMatrix(NB_pred, target)  #confusion matrix naive bayes
  
  #random forest/decision tree
  DT_pred = predict(list[[5]], x ) #predict
  DT_con <- confusionMatrix(DT_pred, target)  #confusion matrix decision tree

  list2<-list(gbm_con[[2]], NB_con[[2]], DT_con[[2]])
  
  return(list2)

}
