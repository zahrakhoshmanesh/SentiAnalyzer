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

#function for formula for Confusion Matrix results: accuracy, precision, recall, F1_score
CmxParams <- function(confmat) {
  TP=confmat[1,1]
  FP=confmat[1,2]
  FN=confmat[2,1]
  TN=confmat[2,2]
  
  #TP=True Positive, TN=True Negative, FP=False Positive, FN=False Negative
  accuracy= (TP+TN)/(TP+TN+FP+TN)
  precision= TP / (TP + FP)
  recall= TP / (TP + FN)
  F1_Score = 2*precision*recall / (precision+recall)
  
  C(accuracy, precision, recall, F1_Score)
  return(c(accuracy, precision, recall, F1_Score))
  
}

comparisonjb <- function(listCmx) { #listCmx is list returned by BuildPrediction function
  #library(RWeka)
  library(purrr)
  library(plotly)
  Cmx <- listCmx %>% set_names(c("decision tree","naive bayes","gbm") ) %>% map_df(CmxParams)
  Cmx2 <- as.matrix(Cmx)
  rownames(Cmx2) <- c("accuracy","precision","recall","f1-score")
  plot_ly(z=Cmx2,x=colnames(Cmx2),y=rownames(Cmx2), type="heatmap")
}


# 
# #SAMPLE
# cmsample <- matrix(c(0.9,0.8,1,0.7), ncol=2) #ex of a conf matrix list
# 
# comp2<-matrix(rnorm(0:1),ncol=4)
# comp3<-matrix(rnorm(0:1),ncol=4)
# comp4<-matrix(rnorm(0:1),ncol=4)
# 
# 
# comp<-CMresults(cmsample)
# colnames(comp)<- c("accuracy","precision","recall","f1-score")
# comp
# 
# CMmatrix<-rbind(comp,comp2,comp3,comp4)
# CMmatrix
# heatmap(CMmatrix)
# library(plotly)  
# plot_ly(z=CMmatrix,x=rownames(comp),y=c("decision tree","naive bayes","gbm"), type="heatmap")
# 
