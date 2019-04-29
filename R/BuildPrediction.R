#' predict the classes using trained algorithms and give confusion matrix  in return

#'
#' @param x input is a dataframe for document-term matrix in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return list of 4 confusion matrix for each trained classification algorithm
#' @author Atousa Zarindast
#' @export
#' @import tidyverse
#' @import tidyr
#' @import assertthat
#' @import testthat
#' @import caret
#' @import tidyr
#' @examples 
#' library(SentiAnalyzer)
#' csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing.csv"))
#' my_training_data <- BuildPrediction(csv_data)

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
  
  trained_df<- df %>%
    dplyr::mutate(
    prediction=purrr::map(.x=method,
                          .f=function(d){predict(d,xx)}) )
  
  trained_df<-trained_df %>% 
    dplyr::mutate(
      conf=purrr::map(.x=prediction,
                      .f=function(d){confusionMatrix(d,xx[, ncol(xx)])}))

  trained_df<-trained_df[3]
  return(trained_df)

}
