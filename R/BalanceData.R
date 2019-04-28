
#' Clean text and buil term matrix for bag of words model or TF DFI.
#'
#' @param dataset unbalanced dataset, a dataframe : two column: first text reviews and second binary class, label: negative =0 and positive=1.
#' @return balanced_dataframe balanced dataframe containing two columns: review texts and binary class , label: negative =0 and positive=1.
#' @author Zahra Khoshmanesh
#' @export
#' @import ROSE
#' @import usethis
#' @examples
#' library("SentiAnalyzer")
#' direction <- system.file(package = "SentiAnalyzer", "extdata/Imbalance_Restaurant_Reviews.tsv")
#' imbalance_data<- read.delim(direction,quote='',stringsAsFactors = FALSE)
#' BalanceData(imbalance_data)


BalanceData<-function(dataset){

  source_datasets=dataset #read dataset and assign it to local variable 
  
  check_class <-table(source_datasets[[-1]]) #check the number of two class label

  res_name <- colnames(source_datasets)[ncol(source_datasets)] # take class label
  formula <- as.formula(paste(res_name, ' ~ .')) #build formula for balanceing data

  if (check_class[1]!=check_class[2]) #check if the data is unbalance, balnce it otherwise inform user that no need to balance
     {
       print("dataset is imbalance, Starting balancing it")
       balanced_dataframe <- ROSE:: ROSE(formula, data = source_datasets, seed = 1)$data
       table(data.rose[[2]])
     }  
  else
    {
    print("dataset is balanced  and no need to balance it")
    }

  
  #write.table(data.rose, file='./inst/extdata/out.tsv', quote=FALSE, sep='\t', col.names = NA)
  
  #usethis::use_data(balanced_dataframe,overwrite = TRUE) #save output file in Data folder as balanced_dataframe.rda file
  
  #check if file exist, message user that balancing was successful
  #if (file.access(system.file(package = "SentiAnalyzer", "balanced_dataframe"), mode = 0)==0){
  #  print("balancing dataset is done! and new balanced dataset saved in Data folder as balanced_dataframe.rda file ")
  #}
  return(balanced_dataframe)
}





