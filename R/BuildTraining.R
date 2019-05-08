
Skip to content
Pull requests
Issues
Marketplace
Explore
@atousaz

0
0

4

srvanderplas/SentiAnalyzer forked from zahrakhoshmanesh/SentiAnalyzer
Code
Pull requests 0
Projects 0
Wiki
Insights
SentiAnalyzer/R/BuildTraining.R
@srvanderplas srvanderplas Merge https://github.com/zahrakhoshmanesh/SentiAnalyzer fc2c84f 16 hours ago
@zahrakhoshmanesh
@atousaz
@srvanderplas
@joeybudi
106 lines (92 sloc) 2.8 KB
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
#' csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing.csv"))
#' my_training_data <- BuildTraining(csv_data)
#' }
#' 
#' # csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing1.csv"))
#' # trained_models <- BuildTraining(csv_data)
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
    number = 10,
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
    interaction.depth = c(1:4),
    n.trees = (1:5) * 3,
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
  
  # SVM
  svmGrid <- expand.grid(degree = (1:10), scale = 0.01, C = (1:2))
  svm_Poly <- try(caret::train(formula, data = x, method = "svmPoly", tuneGrid = svmGrid))
  
  train_output <- list(x = x, gbmFit2, model_knn_10, model_naive_10, model_dectree_10, svm_Poly)
  return(train_output)
}


