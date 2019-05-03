#' Train Machine learning algorithms and give trained models
#'
#' @param x input is a dataframe for bag of word file in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @return trained models
#' @author Atousa Zarindast
#' @export
#' @import caret
#' @examples
#' library(SentiAnalyzer)
#' csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing.csv"))
#' my_training_data <- BuildTraining(csv_data)

BuildTraining<- function(x) {

 # assertthat::assert_that(not_empty(x), noNA(x), is.data.frame(x))
  #assertthat::assert_that(unique(sapply(x[, -ncol(x)], is.numeric)))


  if (is.factor(x[, ncol(x)]) == FALSE) {
    x[, ncol(x)] <- ifelse(x[, ncol(x)] == 1, "Yes", "No")

    x[, ncol(x)] <- factor(x[, ncol(x)], level = c("Yes", "No"))
    } else {
    x[, ncol(x)] <- ifelse(x[, ncol(x)] == 1, "Yes", "No")
  }

  res_name <- colnames(x)[ncol(x)]
  formula <- as.formula(paste(res_name, ' ~ .'))
  #cross valication

  ############Decision tree with cross validation training#######################

  # train control
  ctrl_cv10 = caret::trainControl(
    method = "cv",
    number = 10,
    savePred = T,
    classProb = T
  )
  #expanding the tree
  treeGrid_dectree = expand.grid(C = (1:2) * 0.02, M = (1:2))
  message("Finished building control")

  #decision tree with cross validation
  model_dectree_10 = caret::train(
    formula,
    data = x,
    method = "rf",
    trControl = ctrl_cv10,
    ntree = 100
  )
  message("Finished building decision tree")

  #predict
  # prediction_dec_parameterset = predict(model_dectree_10, x)
  #confusion matrix
  # DT_con<-confusionMatrix(prediction_dec_parameterset, x[,ncol(x)])
  # DT_con


  ################## Naive Bayes with Cross Validation training ################
  treeGrid_naive = expand.grid(
    fL = c(1:2),
    usekernel = TRUE,
    adjust = c(0.5:1)
  )
  #NB model training

  model_naive_10 = caret::train(
    formula,
    data = x,
    method = "nb",
    trControl = ctrl_cv10,
    tuneGrid = treeGrid_naive
  )


  treeGrid_knn = expand.grid(k = c(1:100))


  ###### kNN with Cross Validation ############
  model_knn_10 =caret::train(
    formula,
    data = x,
    method = "knn",
    trControl = ctrl_cv10,
    tuneGrid = treeGrid_knn
  )

  # plot(model_knn_10)
  #prediction
  # prediction_knn_parameterset = predict(model_knn_10, x)
  #confusion matrix
  #KKN_con<-confusionMatrix(prediction_knn_parameterset, x[,ncol(x)])
  #KKN_con


  #####GBM
  gbmGrid <-  expand.grid(
    interaction.depth = c(1:9),
    n.trees = (1:5) * 50,
    shrinkage = 0.1,
    n.minobsinnode = 20
  )
  
  gbmFit2 <- caret::train(
    formula,
    data = x,
    method = "gbm",

    verbose = FALSE,

    tuneGrid = gbmGrid,
    trControl = ctrl_cv10
  )
  #prediction
  # prediction_gbmFit2 = predict(gbmFit2, x)
  #confusion matrix
  #gbm_con<-confusionMatrix(prediction_gbmFit2, x[,ncol(x)])
  #gbm_con

  #plot(gbmFit2)
  
  #SVM
  svmGrid<-expand.grid(degree=(1:10),scale=0.01,C=(1:2))
  svm_Poly <- caret::train(formula, data = x, method = "svmPoly",tuneGrid=svmGrid)
  
  
  

  list <-
    list(x=x, gbmFit2, model_knn_10, model_naive_10, model_dectree_10,svm_Poly)
  return(list)

}
