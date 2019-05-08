
#' Visualize dataset and get some insight of data.
#'
#' @param dataset dataset.
#' @param termcount termcount:used for filtering the highest terms repeated in reviews usually > 10
#' @return some diagram and insight of data
#' @author Zahra Khoshmanesh
#' @import tidytext
#' @import dplyr
#' @import ggplot2 
#' @importFrom  wordcloud comparison.cloud wordcloud
#' @importFrom  reshape2 acast
#' @importFrom  RColorBrewer brewer.pal
#' @importFrom tibble as.tibble
#' @importFrom stats reorder
#' @importFrom methods hasArg
#' @export 
#' @return A ggplot
#' @examples
#' \dontrun{
#' library(SentiAnalyzer)
#' direction <- system.file(package = "SentiAnalyzer", "extdata/Restaurant_Reviews.tsv")
#' original_dataset <- read.delim(direction,quote='',stringsAsFactors = FALSE)
#' VisualizeData(dataset=original_dataset,termcount=15)}

VisualizeData<-function(dataset,termcount){
  word=sentiment=value=NULL
  
  if(!hasArg(dataset)){
    dataset=system.file(package = "SentiAnalyzer", "extdata/Restaurant_Reviews.tsv")
    warning('file path does not provided by user, set to default file')
  }
  else if(!hasArg(termcount)){
    termcount=15
    warning('termcount did not provided, set to default 15')
  }
  # check whether the reductionrate is a number 
  if(!is.integer(termcount)){
    termcount=15
    warning('termcount is not a number,set to default 15 value')
    
  }


  source_datasets <- dataset
  termcount <- termcount
  tidy_text <- source_datasets[[1]] %>%
    as.tibble %>% 
    tidytext:: unnest_tokens(word, value) %>%
    dplyr:: anti_join(tidytext:: stop_words)

 wordfreqplot = tidy_text %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::filter(n > termcount) %>%
    dplyr::mutate(word = stats::reorder(word, n)) %>%
    ggplot2::ggplot(aes(x=word,y=n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()


 wordcloadplot = tidy_text %>%
   dplyr::count(word) %>%
    with(wordcloud::wordcloud(word, n, max.words = 100,rot.per=0.35,colors=RColorBrewer::brewer.pal(8, "Dark2")))

 
 
  reshapplot = tidy_text %>%
    dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
    dplyr::count(word, sentiment, sort = TRUE) %>%
    reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    wordcloud::comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 100)
 

 return(wordfreqplot)

}





