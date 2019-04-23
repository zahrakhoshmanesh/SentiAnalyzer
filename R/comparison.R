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

comparison <- function(list){
  library(assertthat)
  library(caret)
  library(tidyr)
  library(testthat)
  library(tidyverse)
  library(RWeka)
  library(eply)
  list.conf.matrix<-BuildPrediction(x)
  #list of 4 confusion matrix comes from prediction phase
  #list2<-list(KKN_con, NB_con, DT_con, gbm_con)
  
  KKN_conf<-list.conf.matrix[1]%>%flatten()
  NB_conf<-list.conf.matrix[2]%>%flatten()
  DT_conf<-list.conf.matrix[3]%>%flatten()
  gbm_conf<-list.conf.matrix[4]%>%flatten()
  

  accuracy_f=function(x){(x$table[1]+x$table[2,2])/(x$table[1]+x$table[1,2]+x$table[2,1]+x$table[2,2])}
  precision_f=function(x){(i$table[1])/(i$table[1]+i$table[1,2])}
  recall_f=function(x){(i$table[1])/(i$table[1]+i$table[2,1])}
  f1score= function(x){2 * ((i$table[1])/(i$table[1]+i$table[1,2]))*((i$table[1])/(i$table[1]+i$table[2,1]))   / (((i$table[1])/(i$table[1]+i$table[1,2])) +  (i$table[1])/(i$table[1]+i$table[2,1]))}
  knn_acc<-accuracy_f(KKN_conf)
  NB_acc<-accuracy_f(NB_conf)
  DT_acc<-accuracy_f(DT_conf)
  gbm_acc<-accuracy_f(gbm_conf)
  
  knn_pre<-precision_f(KKN_conf)
  NB_pre<-precision_f(NB_conf)
  DT_pre<-precision_f(DT_conf)
  gbm_pre<-precision_f(gbm_conf)
  
  knn_recall<-recall_f(KKN_conf)
  NB_recall<-recall_f(NB_conf)
  DT_recall<-recall_f(DT_conf)
  gbm_recall<-recall_f(gbm_conf)
  
  knn_f1<-f1score(KKN_conf)
  NB_f1<-f1score(NB_conf)
  DT_f1<-f1score(DT_conf)
  gbm_f1<-f1score(gbm_conf)

}
