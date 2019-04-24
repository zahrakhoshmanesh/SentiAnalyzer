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
#' @example
#'  BuildPrediction('./data/testing.csv')

BuildPrediction <- function(x) {
  # library(assertthat)
  # library(caret)
  # library(tidyr)
  # library(testthat)
  # library(tidyverse)
  # library(RWeka)
  # library(eply)
  # library(purrr)

  list <- BuildTraining(x)
  
  xx <- list[1]%>%purrr::map_df(~.x)
  list<-list[-1]
  
  df <- data.frame(matrix(list, nrow=length(list), byrow=T)) 
  names(df)<-"method"
  
  t<-df%>%dplyr::mutate( prediction=purrr::map(.x=method,.f=function(d){predict(d,xx)}) )
  t<-t%>%dplyr::mutate(conf=purrr::map(.x=prediction,.f=function(d){confusionMatrix(d,xx[, ncol(xx)])}))
  t<-t[3]
  return(t)

}
