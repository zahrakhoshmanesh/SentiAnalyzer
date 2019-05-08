
#' Clean text and build term matrix for bag of words,TF DFI and bi-gram.
#'
#' @param source_dataset A dataframe  having two columns, review as text, label as binary.
#' @param dtm_method 1 for bag of word, 2 for TF DFI, 3 for bigram.
#' @param reductionrate how many percent of term matrix you want to keep,usually 0.999 and not less than 0.99.
#' @return dataframe "dataset" : The term matrix converted to dataframe plus target label.
#' @author Zahra Khoshmanesh
#' @import tm
#' @import matlib
#' @import SnowballC
#' @import checkmate
#' @importFrom NLP ngrams words
#' @importFrom methods hasArg
#' @export 
#' @return A clean dataframe,a term-matrix
#' @examples
#' \dontrun{
#' library("SentiAnalyzer")
#' direction <- system.file(package = "SentiAnalyzer", "extdata/Restaurant_Reviews.tsv")
#' orignal_dataset <- read.delim(direction,quote='',stringsAsFactors = FALSE)
#' CleanText(original_dataset,dtm_method=1,reductionrate=0.99)
#' CleanText(original_dataset,dtm_method=2,reductionrate=0.99)
#' CleanText(original_dataset,dtm_method=3,reductionrate=0.999)}
 
CleanText <- function(source_dataset,dtm_method,reductionrate){


    if(!hasArg(source_dataset)){
      source_dataset=system.file(package = "SentiAnalyzer", "extdata/Restaurant_Reviews.tsv")
      warning('file path does not provided by user, set to default file path')
    }
    else if(!hasArg(dtm_method)){
      dtm_method=1
      warning('dtm_method does not exist, set to default method, bag of word with simple count')
    }
    # check whether the reductionrate is a number 
    if(!is.numeric(reductionrate)){
      reductionrate=0.99
      warning('reductionrate is not numeric,set to default 0.99 value')
      
    }
    # check whether the reductionrate is a number between 0.99 and 1.
    checkmate::assertNumber(reductionrate,lower = 0.99, upper =1) 
  
  

  origin_data=source_dataset

  corpus <- tm::VCorpus(VectorSource(origin_data[[1]])) %>%
      tm::tm_map(content_transformer(tolower)) %>% #convert all review to lower case
      tm::tm_map(removeNumbers) %>% # remove numbers from reviews
      tm::tm_map(removePunctuation) %>% # remove punctuations from reviews
      tm::tm_map(removeWords,stopwords()) %>% # remove Stop words from reviews
      tm::tm_map(stemDocument) %>% # Stemming
      tm::tm_map(stripWhitespace)  # remove extra space that created in cleaning stage when for example number remove
  

  #creating document term matrix of words in reviews

  # bigram
  BigramTokenizer <-  function(x)  unlist(lapply(NLP::ngrams(NLP::words(x), 2), paste, collapse = " "), use.names = FALSE)
  dtm <-switch(dtm_method,
               '1' = tm::DocumentTermMatrix(corpus),
               '2' = tm::DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))),
               '3' = t(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)))
                )

  # reduce dimention of sparse matrix
  dtm = tm:: removeSparseTerms(dtm,reductionrate)

  # convert matrix of independent variables to data frame
  clean_dataset = as.data.frame(as.matrix(dtm))
  # encode the target feature as factor
  clean_dataset$target = factor(origin_data[[-1]],levels=c(0,1))
  #check outputs
  checkmate::checkFactor(clean_dataset$target)
  checkmate::testDataFrame(clean_dataset)
  
  return(clean_dataset)
}



