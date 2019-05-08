#' Train Machine learning algorithms and give trained models
#'
#' @param x input is a dataframe for bag of word file in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return trained models
#' @author Atousa Zarindast
#' @export
#' @importFrom caret train trainControl
#' @examples
#' \dontrun{
#' library(SentiAnalyzer)
#' csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing1.csv"))
#' my_training_data <- BuildTraining(csv_data)
#' }

BuildTraining <- function(x) {
  
  if (is.factor(x[, ncol(x)]) == FALSE) {
    x[, ncol(x)] <- ifelse(x[, ncol(x)] == 1, "Yes", "No")
    x[, ncol(x)] <- factor(x[, ncol(x)], levels = c("Yes", "No"))
  } else {
    x[, ncol(x)] <- ifelse(x[, ncol(x)] == 1, "Yes", "No")
  }
  
  res_name <- colnames(x)[ncol(x)]
  formula <- as.formula(paste(res_name, " ~ ."))
  # cross valication
  
  ############ Decision tree with cross validation training#######################
  
  # train control
  ctrl_cv10 <- caret::trainControl(
    method = "cv",
    number = 5,
    savePred = T,
    classProb = T
  )
  # expanding the tree
  treeGrid_dectree <- expand.grid(C = (1:2) * 0.02, M = (1:2))
  message("Finished building control")
  
  # decision tree with cross validation
  model_dectree_10 <- try(caret::train(
    formula,
    data = x,
    method = "rf",
    trControl = ctrl_cv10,
    ntree = 100
  ))
  message("Finished building decision tree")
  
  ################## Naive Bayes with Cross Validation training ################
  treeGrid_naive <- expand.grid(
    fL = c(1:2),
    usekernel = TRUE,
    adjust = c(0.5:1)
  )
  # NB model training
  
  model_naive_10 <- try(caret::train(
    formula,
    data = x,
    method = "nb",
    trControl = ctrl_cv10,
    tuneGrid = treeGrid_naive
  ))
  
  treeGrid_knn <- expand.grid(k = c(1:100))
  message("Finished building Naive Bayes classifier")
  
  ###### kNN with Cross Validation ############
  model_knn_10 <- try(caret::train(
    formula,
    data = x,
    method = "knn",
    trControl = ctrl_cv10,
    tuneGrid = treeGrid_knn
  ))
  message("Finished building kNN")
  
  ##### GBM
  gbmGrid <- expand.grid(
    interaction.depth = c(1:3),
    n.trees = (1:5)*10 ,
    shrinkage = 0.1,
    n.minobsinnode = 20
  )
  
  gbmFit2 <- try(caret::train(
    formula,
    data = x,
    method = "gbm",
    verbose = FALSE,
    tuneGrid = gbmGrid,
    trControl = ctrl_cv10
  ))
  message("Finished building GBM")
  # SVM
  svmGrid <- expand.grid(degree = (1:10), scale = 0.01, C = (1:2))
  svm_Poly <- try(caret::train(formula, data = x, method = "svmPoly", tuneGrid = svmGrid))
  message("Finished building SVM")
  train_output <- list(x = x, gbmFit2, model_knn_10, model_naive_10, model_dectree_10, svm_Poly)
  return(train_output)
}