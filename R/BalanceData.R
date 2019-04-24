
#' Clean text and buil term matrix for bag of words model or TF DFI.
#'
#' @param x unbalanced dataset, a tsv format : tab delimeter, two column: first text and second binary class label.
#' @return balanced dataset, save as out.tsv name in inst folder
#' @author Zahra Khoshmanesh
#' @export
#' @import ROSE
#' @examples
#' BalanceData('./data/Imbalance_Restaurant_Reviews.tsv')


BalanceData<-function(dataset){

  library(ROSE)

  #read  datasets :put two dataset, 1 balance and 1 imbalanced for testing purpose
  source_datasets=read.delim(dataset,quote='',stringsAsFactors = FALSE)
  #source_datasets=read.delim('./inst/Imbalance_Restaurant_Reviews.tsv',quote='',stringsAsFactors = FALSE)
  
  assert_that(not_empty(dataset), noNA(dataset), is.data.frame(dataset))
  expect_equal(unique(sapply(dataset[, -ncol(x)], is.numeric)), TRUE)

  #check the number of two class label
  check_class <-table(source_datasets[[-1]])

  res_name <- colnames(source_datasets)[ncol(source_datasets)]
  formula <- as.formula(paste(res_name, ' ~ .'))

    if (check_class[1]!=check_class[2]){

    print("dataset is imbalance, balancing it in few seconds")
    #data.rose <- ROSE(class_label ~ ., data = source_datasets, seed = 1)$data
    data.rose <- ROSE(formula, data = source_datasets, seed = 1)$data
    table(data.rose[[2]])

  }  else {
    print("dataset is balanced dataset and no need to balance it")
  }


  #save output in file
  write.table(data.rose, file='./data/out.tsv', quote=FALSE, sep='\t', col.names = NA)

  if (file.access('./data/out.tsv', mode = 0)==0){
    print("balancing dataset is done! and new balanced dataset saved in data folder under name out.tsv ")

  }
}

#BalanceData('./data/Imbalance_Restaurant_Reviews.tsv')



