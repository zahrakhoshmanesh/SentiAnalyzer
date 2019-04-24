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
#' @example
#' comparison('./data/testing.csv')

comparison <- function(x){
  library(assertthat)
  library(caret)
  library(tidyr)
  library(testthat)
  library(tidyverse)
  library(RWeka)
  library(eply)

  
  df<-BuildPrediction(x)

  
  accuracy_f=function(x){(x$table[1]+x$table[2,2])/(x$table[1]+x$table[1,2]+x$table[2,1]+x$table[2,2])}
  precision_f=function(x){(x$table[1])/(x$table[1]+x$table[1,2])}
  recall_f=function(x){(x$table[1])/(x$table[1]+x$table[2,1])}
  f1score= function(x){2 * ((x$table[1])/(x$table[1]+x$table[1,2]))*((x$table[1])/(x$table[1]+x$table[2,1]))   / (((x$table[1])/(x$table[1]+x$table[1,2])) +  (x$table[1])/(x$table[1]+x$table[2,1]))}


  
  df<-df%>%mutate(accuracy=purrr::map(.x=conf,.f=accuracy_f),
                  precision=purrr::map(.x=conf,.f=precision_f),
                  recall=purrr::map(.x=conf,.f=recall_f),
                  f1score=purrr::map(.x=conf,.f=f1score))
  return(df)
}
