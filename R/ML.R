#' Train Machine learning algorithms and give performance metrics in return
#'
#' @param x input is a dataframe for bag of word file in which columns are the terms and row are binary variable 1 if that term exist in that data instance
#' @export
#' @return 3 performance metric for each algorithm
#' @author Atousa Zarindast
#' @import tidyverse
#' @import tidyr
#' @import assertthat
#' @import testthat
#' @import Caret
#' @import tidyr

library(assertthat)
library(caret)
library(tidyr)
library(testthat)
library(tidyverse)
library(RWeka)
library(eply)
Training_Alg <- function(x){
  x=dataset
  assert_that(not_empty(x),noNA(x), is.data.frame(x))
  expect_equal(unique(sapply(dataset[,-ncol(dataset)], is.numeric)), TRUE)

  last_co<-x[,ncol(x)]
  if (expect_equal(is.factor(x[,ncol(x)]), FALSE)){
    x[,ncol(x)] <-ifelse(x[,ncol(x)]==1,"Yes","No")
    x[,ncol(x)] <-factor(x[,ncol(x)],level=c("Yes","No"))
}

replace()
#cross valication

  ############Decision tree with cross validation training#######################

  # train control
  ctrl_cv10 = trainControl(method = "cv", number = 10, savePred = T, classProb = T)
  #expanding the tree
  treeGrid_dectree = expand.grid(C=(1:4)*0.02, M=(1:5))
  #decision tree with cross validation
  model_dectree_10 = caret::train(formula, data=x, method="rf", trControl=ctrl_cv10,  ntree = 5)
  #predict
  prediction_dec_parameterset = predict(model_dectree_10, x)
  #confusion matrix
  DT_con<-confusionMatrix(prediction_dec_parameterset, x[,ncol(x)])
  DT_con


  ################## Naive Bayes with Cross Validation training ################
  treeGrid_naive = expand.grid(fL=c(1:2), usekernel=c(TRUE, FALSE), adjust=c(0.5:1))
  #NB model training
  res_name<-colnames(x)[ncol(x)]
  formula <- as.formula(paste(res_name, ' ~ .' ))
  model_naive_10 = train(formula, data=x, method="nb", trControl=ctrl_cv10, tuneGrid=treeGrid_naive)
  #prediction
  prediction_naive_parameterset = predict(model_naive_10, x)
  #confusion matrix
  NB_con<-confusionMatrix(prediction_naive_parameterset, x[,ncol(x)])

  treeGrid_knn = expand.grid(k=c(1:50))
  ###### kNN with Cross Validation ############
  model_knn_10 = train(formula, data=x, method="knn", trControl=ctrl_cv10, tuneGrid=treeGrid_knn)
  #prediction
  prediction_knn_parameterset = predict(model_knn_parameterset, x)
  #confusion matrix
  KKN_con<-confusionMatrix(prediction_knn_parameterset, x[,ncol(x)])



  #prediction
  prediction_SVM = predict(svm_Radial, x[,ncol(x)])
  #confusion matrix
  KKN_con<-confusionMatrix(svm_Radial, x$class)

  #####GBM
  gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                          n.trees = (1:5)*50,
                          shrinkage = 0.1,
                          n.minobsinnode = 20)



  #trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  #set.seed(3233)

  #trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  #set.seed(3233)
  #svm_Linear <- train(formula, data = x, method = "svmLinear",
                      #trControl=trctrl,
                      #preProcess = c("center", "scale"),
                      #tuneLength = 10)
  #prediction_SVM = predict(svm_Linear, x[,ncol(x)])

  #set.seed(825)
  gbmFit2 <- train(formula, data = x,
                   method = "gbm",

                   verbose = FALSE,
                   ## Now specify the exact models
                   ## to evaluate:
                   tuneGrid = gbmGrid,trControl=ctrl_cv10 )

plot(gbmFit2)


