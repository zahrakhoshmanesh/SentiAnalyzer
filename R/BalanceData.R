
#' Clean text and buil term matrix for bag of words model or TF DFI.
#'
#' @param x unbalanced dataset, a tsv format : tab delimeter, two column: first text and second binary class label.
#' @return balanced dataset, save as out.tsv name in inst folder
#' @author Zahra Khoshmanesh
#' @export
#' @import ROSE
#' @examples
#' BalanceData('./inst/Imbalance_Restaurant_Reviews.tsv')


BalanceData<-function(dataset){

  library(ROSE)

  #read  datasets :put two dataset, 1 balance and 1 imbalanced for testing purpose
  source_datasets=read.delim(dataset,quote='',stringsAsFactors = FALSE)
  #source_datasets=read.delim('./inst/Restaurant_Reviews.tsv',quote='',stringsAsFactors = FALSE)

  #check the number of two class label
  check_class <-table(source_datasets[[2]])
  class_label<-names(source_datasets)[2]
  class_text<-names(source_datasets)[1]
    if (check_class[1]!=check_class[2]){

    print("dataset is imbalance, balancing it in few seconds")
    #data.rose <- ROSE(class_label ~ ., data = source_datasets, seed = 1)$data
    data.rose <- ROSE(Liked ~ ., data = source_datasets, seed = 1)$data
    table(data.rose[[2]])

  }  else {
    print("dataset is balanced dataset and no need to balance it")
  }


  #save output in file
  write.table(data.rose, file='./inst/out.tsv', quote=FALSE, sep='\t', col.names = NA)

  if (file.access('./inst/out.tsv', mode = 0)==0){
    print("balancing dataset is done! and new balanced dataset saved in inst folder under name out.tsv ")

  }
}

#BalanceData('./inst/Imbalance_Restaurant_Reviews.tsv')



