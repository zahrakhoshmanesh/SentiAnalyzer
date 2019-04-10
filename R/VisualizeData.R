
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

  library(tidytext)
  library(dplyr)
  library(ggplot2)
  library(wordcloud)
  library(reshape2)

  source_datasets=read.delim(dataset,quote='',stringsAsFactors = FALSE)
  #source_datasets=read.delim('./inst/Restaurant_Reviews.tsv',quote='',stringsAsFactors = FALSE)


  text_df <- tibble(text = source_datasets[[1]])

  tidy_text <- text_df %>%
    unnest_tokens(word, text)

  data(stop_words)
  tidy_text <- tidy_text %>%
    anti_join(stop_words)

  tidy_text  %>%
    count(word, sort = TRUE)


 wordfreqplot= tidy_text %>%
    count(word, sort = TRUE) %>%
    filter(n > 10) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
 wordfreqplot



 wordcloadplot= tidy_text %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))

 wordcloadplot



 reshapplot= tidy_text %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 100)
 reshapplot

}





