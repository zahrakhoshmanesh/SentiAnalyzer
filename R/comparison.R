#' visualize performance of the algorithm
#'
#' @param x input is a dataframe for bag of word file in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return visualization of trained algrithms (for classifiying textual data), comparision and select the best!
#' @author Atousa Zarindast
#' @import tidyverse
#' @import tidyr
#' @import assertthat
#' @import testthat
#' @import caret
#' @import tidyr



comparison <- function(x) {
  library(assertthat)
  library(caret)
  library(tidyr)
  library(testthat)
  library(tidyverse)
  library(RWeka)
  library(eply)
  #x=sample
  list <- BuildTraining(x)
  list2 <- BuildPrediction(x)
  KKN_con<-list2[1]
  NB_con<-list2[2]
  DT_con<-list2[3]
  gbm_con<-list2[4]
  list(gbmFit2, model_knn_10, model_naive_10, model_dectree_10)
  gbm<-list[1]
  knn<-list[2]
  nb<-list[3]
  tree<-list[4]


}
