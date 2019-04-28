
#' Clean text and build term matrix for bag of words,TF DFI and bi-gram.
#'
#' @param source_dataset A dataframe  having two columns, review as text, label as binary.
#' @param dtm-method 1 for bag of word, 2 for TF DFI, 3 for bigram.
#' @param reductionrate how many percent of term matrix you want to keep,usually 0.999 and not less than 0.99.
#' @return dataframe "dataset" : The term matrix converted to dataframe plus target label.
#' @author Zahra Khoshmanesh
#' @export
#' @import tm
#' @import matlib
#' @import SnowballC
#' @import NLP
#' @examples
#' library("SentiAnalyzer")
#' direction <- system.file(package = "SentiAnalyzer", "extdata/Restaurant_Reviews.tsv")
#' orignal_dataset <- read.delim(direction,quote='',stringsAsFactors = FALSE)
#' CleanText(original_dataset,dtm_method=1,reductionrate=0.99)
#' CleanText(original_dataset,dtm_method=2,reductionrate=0.99)
#' CleanText(original_dataset,dtm_method=3,reductionrate=0.999)
 
CleanText <- function(source_dataset,dtm_method,reductionrate){

  #assertthat(not_empty(source_dataset), noNA(source_dataset),not_empty(dtm_method),not_empty(reductionrate))

  # library(tm)
  #library(rJava)
  #library(RWeka)
  #library(matlib)
  #source_datasets=read.delim(source_dataset,quote='',stringsAsFactors = FALSE)
  origin_data=source_dataset
  dim(origin_data)
  dim(origin_data[[1]])
  corpus <- tm::VCorpus(VectorSource(origin_data[[1]])) %>%
      tm::tm_map(content_transformer(tolower)) %>% #convert all review to lower case
      tm::tm_map(removeNumbers) %>% # remove numbers from reviews
      tm::tm_map(removePunctuation) %>% # remove punctuations from reviews
      tm::tm_map(removeWords,stopwords()) %>% # remove Stop words from reviews
      tm::tm_map(stemDocument) %>% # Stemming
      tm::tm_map(stripWhitespace)  # remove extra space that created in cleaning stage when for example number remove
  
  dim(corpus)

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
  clean_dataset$target = factor(origin_data[[-1]],level=c(0,1))
  #assertthat(not_empty(dataset), noNA(dataset),is.data.frame(dataset))
  #usethis::use_data(clean_dataset,overwrite = TRUE)
  return(clean_dataset)
}

#df1 <- CleanText(orignal_dataset,dtm_method=1,reductionrate=0.99)
#df2 <- CleanText(orignal_dataset,dtm_method=2,reductionrate=0.99)
#df3 <- CleanText(orignal_dataset,dtm_method=3,reductionrate=0.999)

