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

heatmapCmx <- function(listCmx) { #listCmx is list returned by BuildPrediction function
  
  Cmx_4param <- Cmx[2:5]
  Cmx_4param <- as.data.frame(Cmx_4param) 
  colnames(Cmx_4param) <- c("accuracy","precision","recall","f1-score")
  rownames(Cmx_4param) <- c("Decision tree","Naive Bayes","k-Nearest Neighbors", "Gradient-Boosting")
  
  plotly::plot_ly(z=data.matrix(Cmx_4param),
                  x=colnames(Cmx_4param),
                  y=rownames(Cmx_4param), 
                  type="heatmap")
  
  plotly::plot_ly(
    type = 'table',
    header = list(
      values = c('<b>Confusion Matrix</b>', colnames(Cmx_4param)),
      line = list(color = '#506784'),
      fill = list(color = '#119DFF'),
      align = c('left','center'),
      font = list(color = 'white', size = 12)
    ),
    cells = list(
      values = rbind(rownames(Cmx_4param), 
                     data.matrix(Cmx_4param)),
      line = list(color = '#506784'),
      fill = list(color = c('#25FEFD', 'white')),
      align = c('left', 'left'),
      font = list(color = c('#black'), size = 12)
    ))
  
  

}
