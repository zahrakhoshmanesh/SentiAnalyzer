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

comparison <- function(x){
  library(assertthat)
  library(caret)
  library(tidyr)
  library(testthat)
  library(tidyverse)
  library(RWeka)
  library(eply)
  
  df<-BuildPrediction(x)
  #list of 4 confusion matrix comes from prediction phase
  #list2<-list(KKN_con, NB_con, DT_con, gbm_con)
  
  accuracy_f=function(x){(x$table[1]+x$table[2,2])/(x$table[1]+x$table[1,2]+x$table[2,1]+x$table[2,2])}
  precision_f=function(x){(x$table[1])/(x$table[1]+x$table[1,2])}
  recall_f=function(x){(x$table[1])/(x$table[1]+x$table[2,1])}
  f1score= function(x){2 * ((x$table[1])/(x$table[1]+x$table[1,2]))*((x$table[1])/(x$table[1]+x$table[2,1]))   / (((x$table[1])/(x$table[1]+x$table[1,2])) +  (x$table[1])/(x$table[1]+x$table[2,1]))}


  
  df<-df%>%mutate(accuracy=purrr::map(.x=conf,.f=accuracy_f),
                                               precision=purrr::map(.x=cof,.f=precision_f),
                                               recall=purrr::map(.x=cof,.f=recall_f),
                                               f1score=purrr::map(.x=cof,.f=f1score))
  return(df)

  
  
}
