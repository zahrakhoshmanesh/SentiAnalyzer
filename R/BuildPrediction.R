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
#' my_training_data <- BuildPrediction(csv_data)
#' }
#' 
#' # Predicted_obj <- BuildPrediction(csv_data)
BuildPrediction <- function(x) {
  
  prediction <- method <- predict <- NULL
  
  # list <- BuildTraining(x)
  if (is.list(x)) {
    # Add more checks here to make sure the list is properly formatted!!
    checkmate::checkList(x, len = 6)
    trainingList <- x
  }
  
  if (is.data.frame(x)) {
    trainingList <- BuildTraining(x)
  }
  
  
  xx <- trainingList[1] %>% purrr::map_df(~.x)
  trainingList <- trainingList[-1]
  
  df <- data.frame(matrix(trainingList, nrow = length(trainingList), byrow = T))
  names(df) <- "method"
  
  tmp <- df %>% dplyr::mutate(prediction = purrr::map(.x = method, .f = function(d) {
    predict(d, xx)
  }))
  tmp <- tmp %>% dplyr::mutate(conf = purrr::map(.x = prediction, .f = function(d) {
    caret::confusionMatrix(d, xx[, ncol(xx)])
  }))
  pred_output <- tmp[3]
  out <- as.data.frame(pred_output)
  return(out)
}