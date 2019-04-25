
#' Clean text and buil term matrix for bag of words model or TF DFI.
#'
#' @param x unbalanced dataset, a dataframe : two column: first reviews and second binary class label: negative =0 and positive=1.
#' @return balanced dataset, save as out.tsv name in inst folder
#' @author Zahra Khoshmanesh
#' @export
#' @import ROSE
#' @import usethis
#' @import assertthat
#' @import testthat
#' @examples
#' library(SentiAnalyzer)
#' imbalance_data<- read.delim(system.file(package = "SentiAnalyzer", "extdata/Imbalance_Restaurant_Reviews.tsv"),quote='',stringsAsFactors = FALSE)
#' BalanceData(imbalance_data)


BalanceData<-function(dataset){

  #library(ROSE)

  #read  datasets :put two dataset, 1 balance and 1 imbalanced for testing purpose
 
  source_datasets=dataset
  #source_datasets=read.delim(source_datasets,quote=' ', stringsAsFactors = FALSE)
  
  #check the number of two class label
  check_class <-table(source_datasets[[-1]])

  res_name <- colnames(source_datasets)[ncol(source_datasets)]
  formula <- as.formula(paste(res_name, ' ~ .'))

    if (check_class[1]!=check_class[2])
     {

    print("dataset is imbalance, balancing it in few seconds")
    data.rose <- ROSE:: ROSE(formula, data = source_datasets, seed = 1)$data
    table(data.rose[[2]])

     }  
   else
    {
    print("dataset is balanced  and no need to balance it")
    }


  #save output in file
  #write.table(data.rose, file='./inst/extdata/out.tsv', quote=FALSE, sep='\t', col.names = NA)
  usethis::use_data(data.rose,overwrite = TRUE)
  data(package = "SentiAnalyzer", data.rose)
  
  if (file.access(system.file(package = "SentiAnalyzer", "extdata/out.tsv"), mode = 0)==0){
    print("balancing dataset is done! and new balanced dataset saved in extdata folder under name out.tsv ")
  }
  return(data.rose)
}





