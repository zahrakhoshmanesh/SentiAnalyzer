#' predict the classes using trained algorithms and give cnfusion matrix  in return

#'
#' @param x input is a dataframe for bag of word file in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return 3 confusion matrix for each trained classification algorithm
#' @author Atousa Zarindast
#' @export
#' @examples 
#' \dontrun{
#' library(SentiAnalyzer)
#' csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing.csv"))

#my_training_data <- BuildPrediction(csv_data)}

BuildPrediction <- function(x) {
  # library(assertthat)
  # library(caret)
  # library(tidyr)
  # library(testthat)
  # library(tidyverse)
  # library(RWeka)
  # library(eply)
  # library(purrr)
  prediction=method=predict=NULL

  # list <- BuildTraining(x)
  if (checkmate::testList(x)) {
    # Add more checks here to make sure the list is properly formatted!!
    testthat::expect_equivalent(length(x), 5)
    list <- x
  }
  
  if (is.data.frame(x)) {
    list <-BuildTraining(x)
  } 
  

  xx <- list[1]%>%purrr::map_df(~.x)
  list<-list[-1]
  
  df <- data.frame(matrix(list, nrow=length(list), byrow=T)) 
  names(df)<-"method"
  
  t<-df%>%dplyr::mutate( prediction=purrr::map(.x=method,.f=function(d){predict(d,xx)}) )
  t<-t%>%dplyr::mutate(conf=purrr::map(.x=prediction,.f=function(d){confusionMatrix(d,xx[, ncol(xx)])}))
  pred_output<-t[3]
  out = as.data.frame(pred_output)
  return(out)

}
