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
  list.conf.matrix<-BuildPrediction(x)
  #list2<-list(KKN_con, NB_con, DT_con, gbm_con)
  
  KKN_conf<-list.conf.matrix[1]%>%flatten()
  NB_conf<-list.conf.matrix[2]%>%flatten()
  DT_conf<-list.conf.matrix[3]%>%flatten()
  gbm_conf<-list.conf.matrix[4]%>%flatten()
  vector<-c(KKN_conf,NB_conf,DT_conf,gbm_conf)
  names(vector)
  #formula for accuracy,Precision,recall, and f-score_knn
  for (i in (KKN_conf,NB_conf,DT_conf,gbm_conf){
  
    name <- paste("Accuracy_", i, sep = "")
    name<-(i$table[1]+i$table[2,2])/(i$table[1]+i$table[1,2]+i$table[2,1]+i$table[2,2])
    name <- paste("Precision_", i, sep = "")
    name<-(i$table[1])/(i$table[1]+i$table[1,2])
    name <- paste("Recall_", i, sep = "")
    name<-(i$table[1])/(i$table[1]+i$table[2,1])
    name <- paste("F1_Score_", i, sep = "")
    name<-2 * ((i$table[1])/(i$table[1]+i$table[1,2]))*((i$table[1])/(i$table[1]+i$table[2,1]))   / (((i$table[1])/(i$table[1]+i$table[1,2])) +  (i$table[1])/(i$table[1]+i$table[2,1]))
  }
  
  TP
  
  TP=KKN_conf$table[1]
  
  FP=KKN_conf$table[1,2]
  
  FN=KKN_conf$table[2,1]
  
  TN=KKN_conf$table[2,2]
  

  Accuracy_knn = (TP + TN) / (TP + TN + FP + FN)
  

  Precision_knn = TP / (TP + FP)
  

  Recall_knn = TP / (TP + FN)
  

  F1_Score_knn = 2 * Precision_knn *  Recall_knn / (Precision_knn +  Recall_knn)
  

# gbm ---------------------------------------------------------------------

  TP=gbm_conf$table[1]
  
  FP=gbm_conf$table[1,2]
  
  FN=gbm_conf$table[2,1]
  
  TN=gbm_conf$table[2,2]
  
  
  Accuracy_gbm = (TP + TN) / (TP + TN + FP + FN)
  
  
  Precision_gbm = TP / (TP + FP)
  
  
  Recall_gbm = TP / (TP + FN)
  
  
  F1_Score_gbm = 2 * Precision_gbm *  Recall_gbm / (Precision_gbm +  Recall_gbm)

# nb ----------------------------------------------------------------------

  TP=NB_conf$table[1]
  
  FP=NB_conf$table[1,2]
  
  FN=NB_conf$table[2,1]
  
  TN=NB_conf$table[2,2]
  
  
  Accuracy_nb = (TP + TN) / (TP + TN + FP + FN)
  
  
  Precision_nb = TP / (TP + FP)
  
  
  Recall_nb = TP / (TP + FN)
  
  
  F1_Score_nb = 2 * Precision_nb *  Recall_nb / (Precision_nb +  Recall_nb)

# dt ----------------------------------------------------------------------
  TP=DT_conf$table[1]
  
  FP=DT_conf$table[1,2]
  
  FN=DT_conf$table[2,1]
  
  TN=DT_conf$table[2,2]
  
  
  Accuracy_DT = (TP + TN) / (TP + TN + FP + FN)
  
  
  Precision_DT = TP / (TP + FP)
  
  
  Recall_DT = TP / (TP + FN)
  
  
  F1_Score_DT = 2 * Precision_DT *  Recall_DT / (Precision_DT +  Recall_DT)
  
  

}
