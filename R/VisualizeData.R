
#' Visualize dataset and get some insight of data.
#'
#' @param x dataset.
#' @return some diagram and insight of data
#' @author Zahra Khoshmanesh
#' @export
#' @import tidyverse
#' @examples
#' VisualizeData('./inst/Restaurant_Reviews.tsv')


VisualizeData<-function(dataset){

  library(tidyverse)
  library(tm)
  library(qdap)

  #source_datasets=read.delim(dataset,quote='',stringsAsFactors = FALSE)
  source_datasets=read.delim('./inst/Restaurant_Reviews.tsv',quote='',stringsAsFactors = FALSE)

  #check the number of two class label
  check_class <-table(source_datasets[[2]])
  class_label<-names(source_datasets)[2]
  class_text<-names(source_datasets)[1]
  if (check_class[1]!=check_class[2]){

    print("dataset is imbalance, balance it with calling BalanceData ")

  }  else {
    print("dataset is balanced dataset and no need to balance it")
  }

  corpus=VCorpus(VectorSource(source_datasets[[1]]))
  #convert all review to lower case
  corpus= tm_map(corpus,content_transformer(tolower))
  # remove numbers from reviews
  corpus=tm_map(corpus,removeNumbers)
  # remove punctuations from reviews
  corpus=tm_map(corpus,removePunctuation)
  # remove Stop words from reviews
  corpus=tm_map(corpus,removeWords,stopwords())
  # Stemming
  corpus=tm_map(corpus,stemDocument)
  # remove extra space that created in cleaning stage when for example number remove
  corpus=tm_map(corpus,stripWhitespace)
  myTDM <- TermDocumentMatrix(corpus)
  findFreqTerms(myTDM,lowfreq = 20, highfreq = Inf)



  a <- tolower(source_datasets[[1]])
  a <- removePunctuation(a)
  a <- removeNumbers(a)
  a <-rm_stopwords(a, tm::stopwords("english"))


  freq_term<-freq_terms(a)
  ## S3 method for class 'freq_terms'
  plot(freq_term, plot = TRUE)


}





