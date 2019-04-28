#' compare the performance of the 4 algorithms 
#'
#' @param x input is a dataframe for document-term matrix in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return visualization of trained algorithms (for classifiying textual data), comparision and select the best!
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
#' my_training_data <- BuildTraining(csv_data)
comparison <- function(x){

  df<-BuildPrediction(x)
  #gather the numbers for the confusion matrix: True Positive, True Negative, False positive, False Negative
  #embed in formula
  
  accuracy_f= function(x){
    (x$table[1]+x$table[2,2])/(x$table[1]+x$table[1,2]+x$table[2,1]+x$table[2,2])}
  precision_f= function(x){
    (x$table[1])/(x$table[1]+x$table[1,2])}
  recall_f= function(x){
    (x$table[1])/(x$table[1]+x$table[2,1])}
  f1score= function(x){
    2*((x$table[1])/(x$table[1]+x$table[1,2]))*((x$table[1])/(x$table[1]+x$table[2,1])) / 
      (((x$table[1])/(x$table[1]+x$table[1,2])) +  (x$table[1])/(x$table[1]+x$table[2,1]))}
  
  #pass the functions accuracy, precision, recall, f1score for each of the 4 algorithms
  df<-df%>%mutate(accuracy=purrr::map(.x=conf,.f=accuracy_f),
                  precision=purrr::map(.x=conf,.f=precision_f),
                  recall=purrr::map(.x=conf,.f=recall_f),
                  f1score=purrr::map(.x=conf,.f=f1score))
  return(df)
}
