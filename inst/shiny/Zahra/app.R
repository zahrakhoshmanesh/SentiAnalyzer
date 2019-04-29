library(shiny)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(RColorBrewer)

#preparing data

data(stop_words)
source_datasets=read.delim('Restaurant_Reviews.tsv',quote='\t',stringsAsFactors = FALSE)
text_df <- tibble(text = source_datasets[[1]])
tidy_text <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)



ui <- fluidPage(
  
  titlePanel("Restaurant Reviews Data"),
  
  sidebarPanel(
    # Input: Slider for the user frequency of term ----
    sliderInput(inputId = "freq",
                label = "I want filter frequency of words to:",
                min = 1,
                max = 50,
                value = 10)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Barchart of frequent terms", plotOutput("HighTerm")),
      tabPanel("wordcloud chart", plotOutput("cloud")),
      tabPanel("negative/positive words chart", plotOutput("negpos"))
      
      
    )
  )
)


server <- function(input, output) {
  
  source_subset <- reactive({
    tidy_text %>%
      filter(n > input$freq) %>%
      mutate(word = reorder(word, n))
  })
  
  output$HighTerm <- renderPlot({
    
    
    ggplot(data=source_subset(),aes(word, n,fill=word)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()+
      ggtitle(paste("highest ranked term filtered by frequency of term >", input$freq, "In restaurant Reviews"))
    
    
  })
  
  
  output$cloud <- renderPlot({
    
    wordcloadplot = tidy_text %>%
      with(wordcloud(word, n, max.words = input$freq,rot.per=0.35,colors=brewer.pal(8, "Dark2")))
    
    
  })
  
  output$negpos <- renderPlot({
    
    
    reshapplot = text_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("gray20", "gray80"),
                       max.words = input$freq)
    
    
    
  })
  
}

shinyApp(ui, server)