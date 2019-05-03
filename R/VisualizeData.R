
#' Visualize dataset and get some insight of data.
#'
#' @param x dataset.
#' @param y termcount:used for filtering the highest terms repeated in reviews usually > 10
#' @return some diagram and insight of data
#' @author Zahra Khoshmanesh
#' @export
#' @import tidyverse
#' @import tidytext
#' @import dplyr
#' @import ggplot2
#' @import wordcloud
#' @import reshape2
#' @import tibble
#' @import RColorBrewer
#' @examples
#' library(SentiAnalyzer)
#' direction <- system.file(package = "SentiAnalyzer", "extdata/Restaurant_Reviews.tsv")
#' orignal_dataset <- read.delim(direction,quote='',stringsAsFactors = FALSE)
#' VisualizeData(dataset=orignal_dataset,termcount=15)

VisualizeData<-function(dataset,termcount){

  #library(tidytext)
  #library(dplyr)
  #library(ggplot2)
  #library(wordcloud)
  #library(reshape2)

  source_datasets <- dataset
 
  #text_df <- tibble(text = source_datasets[[1]])

 tidy_text <- source_datasets[[1]] %>%
    as.tibble %>% 
   tidytext:: unnest_tokens(word, value) %>%
     dplyr:: anti_join(tidytext:: stop_words)

 wordfreqplot = tidy_text %>%
    count(word, sort = TRUE) %>%
    filter(n > termcount) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

  wordcloadplot = tidy_text %>%
     count(word) %>%
     with(wordcloud::wordcloud(word, n, max.words = 100,rot.per=0.35,colors=brewer.pal(8, "Dark2")))
 
 
  reshapplot = tidy_text %>%
     inner_join(get_sentiments("bing")) %>%
     count(word, sentiment, sort = TRUE) %>%
     acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 100)
 
 #visualizeplots <- c(wordfreqplot,wordcloadplot,reshapplot)
 
 #return(visualizeplots)

}





