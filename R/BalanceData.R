
#' Clean text and build term matrix for bag of words model or TF DFI.
#'
#' @param dataset unbalanced dataset, a dataframe : two column: first text reviews and second binary class, label: negative =0 and positive=1.
#' @return balanced_dataframe balanced dataframe containing two columns: review texts and binary class , label: negative =0 and positive=1.
#' @author Zahra Khoshmanesh
#' @import ROSE
#' @import usethis
#' @importFrom stats as.formula
#' @importFrom methods hasArg
#' @export 
#' @return A balanced dataframe 
#' @examples
#' \dontrun{
#' library("SentiAnalyzer")
#' direction <- system.file(package = "SentiAnalyzer", "extdata/Imbalance_Restaurant_Reviews.tsv")
#' imbalance_data<- read.delim(direction,quote='',stringsAsFactors = FALSE)
#' BalanceData(imbalance_data)}

BalanceData<-function(dataset){
  
  
  if(!hasArg(dataset)){
    dataset=system.file(package = "SentiAnalyzer", "extdata/Imbalance_Restaurant_Reviews.tsv")
    warning('file path does not provided by user, set to default file path')
  }

  source_datasets=dataset #read dataset and assign it to local variable 
  
  check_class <-table(source_datasets[[-1]]) #check the number of two class label

  res_name <- colnames(source_datasets)[ncol(source_datasets)] # take class label
  formula <- as.formula(paste(res_name, ' ~ .')) #build formula for balanceing data

  if (check_class[1]!=check_class[2]) #check if the data is unbalance, balnce it otherwise inform user that no need to balance
     {
       print("dataset is imbalance, Starting balancing it")
       balanced_dataframe <- ROSE:: ROSE(formula, data = source_datasets, seed = 1)$data
       table(balanced_dataframe[[2]])
       output=balanced_dataframe
       
     }  
  else
    {
    print("dataset is balanced  and no need to balance it")
      output=source_datasets
      
    }

  checkmate::testDataFrame(output)
  return(output)
}

# reg for deleting non-ascii character
#source_datasets[[1]] <- gsub("[^\x20-\x7E]", "", source_datasets[[1]])



