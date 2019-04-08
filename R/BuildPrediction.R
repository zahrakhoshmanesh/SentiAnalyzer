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

BuildPrediction <- function(x) {
  library(assertthat)
  library(caret)
  library(tidyr)
  library(testthat)

  library(tidyverse)
  library(RWeka)
  library(eply)
  #x=sample
  list <- BuildTraining(x)

  ###############gbm####################################
  #prediction of gbm
  prediction_gbmFit2 = predict(list[[1]], x)
  #confusion matrix gbm
  gbm_con <- confusionMatrix(prediction_gbmFit2, x[, ncol(x)])

  #############random forest############
  #predict
  prediction_dec_parameterset = predict(list[[4]], x)
  #confusion matrix
  DT_con <- confusionMatrix(prediction_dec_parameterset, x[, ncol(x)])
  #DT_con



  ##########nb#############3
  #prediction in nb
  prediction_naive_parameterset = predict(list[[3]], x)
  #confusion matrix
  NB_con <-
    confusionMatrix(prediction_naive_parameterset, x[, ncol(x)])
  #NB_con

  #########knn##########3
  #prediction
  prediction_knn_parameterset = predict(list[[2]], x)
  #confusion matrix
  KKN_con <- confusionMatrix(prediction_knn_parameterset, x[, ncol(x)])

  #KKN_con

  list2<-list(KKN_con, NB_con, DT_con, gbm_con)

  return(list2)

}
