#' visualize performance of the algorithm
#'
#' @param x input is a dataframe for bag of word file in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return visualization of trained algrithms (for classifiying textual data), comparision and select the best!
#' @author Atousa Zarindast
#' @export
#' @examples
#' \dontrun{
#' library(SentiAnalyzer)
#' csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing.csv"))

#comparison <- comparison(csv_data)}
comparison <- function(x){
  conf=NULL
  
  if (ncol(x) > 1) {
    df<-BuildPrediction(x)
  } else {
    # Assume x comes from BuildPrediction function
    # Add more checks here to make sure the list is properly formatted!!
    df <- x
  }
  
  accuracy_f=function(x){(x$table[1]+x$table[2,2])/(x$table[1]+x$table[1,2]+x$table[2,1]+x$table[2,2])}
  precision_f=function(x){(x$table[1])/(x$table[1]+x$table[1,2])}
  recall_f=function(x){(x$table[1])/(x$table[1]+x$table[2,1])}
  f1score= function(x){2 * ((x$table[1])/(x$table[1]+x$table[1,2]))*((x$table[1])/(x$table[1]+x$table[2,1]))   / (((x$table[1])/(x$table[1]+x$table[1,2])) +  (x$table[1])/(x$table[1]+x$table[2,1]))}


  
  df<-df%>%dplyr::mutate(accuracy=purrr::map(.x=conf,.f=accuracy_f),
                  precision=purrr::map(.x=conf,.f=precision_f),
                  recall=purrr::map(.x=conf,.f=recall_f),
                  f1score=purrr::map(.x=conf,.f=f1score))

  
  funm<-function(x){
    as.data.frame(unlist(x))
  }
  df<-df[-1]%>%purrr::map(.f=funm)%>%as.data.frame()%>%dplyr::mutate(method=c("gbm","KNN","NB","RandomForest","SVM"))
  names(df)<-(c("Accuracy","Precision","Recall","F1","Method"))

  return(df)
}
