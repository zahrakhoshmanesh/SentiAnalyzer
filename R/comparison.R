#' visualize performance of the algorithm
#'
#' @param x input is a dataframe for bag of word file in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return visualization of trained algrithms (for classifiying textual data), comparision and select the best!
#' @author Atousa Zarindast
#' @export
#' @import tidyverse
#' @import tidyr
#' @import assertthat
#' @import testthat
#' @import caret
#' @import tidyr

comparison <- function(list) {
  library(assertthat)
  library(caret)
  library(tidyr)
  library(testthat)
  library(tidyverse)
  library(RWeka)
  library(eply)
  #list of 4 confusion matrix comes from prediction phase
  #list2<-list(KKN_con, NB_con, DT_con, gbm_con)
  list.conf.matrix <- list
  KKN_conf<-list.conf.matrix[1]
  NB_conf<-list.conf.matrix[2]
  DT_conf<-list.conf.matrix[3]
  gbm_conf<-list.conf.matrix[4]

  #formula for accuracy,Precision,recall, and f-score
  TP=KKN_conf[1,1]
  TP
  FP=KKN_conf[1,2]
  FP
  FN=KKN_conf[2,1]
  FN
  TN=KKN_conf[2,2]
  TN

  Accuracy = (TP + TN) / (TP + TN + FP + FN)
  Accuracy

  Precision = TP / (TP + FP)
  Precision

  Recall = TP / (TP + FN)
  Recall

  F1_Score = 2 * Precision * Recall / (Precision + Recall)
  F1_Score


}
