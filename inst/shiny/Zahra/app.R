library(shiny)

library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

#preparing data

data(stop_words)
source_datasets=read.delim('Restaurant_Reviews.tsv',quote='',stringsAsFactors = FALSE)
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


nyc <- read.csv("nyc_emergency.csv", stringsAsFactors = FALSE)

ui <- fluidPage(

    titlePanel("NYC Crime Data"),

	sidebarPanel(
	    selectInput("incident_type", "Incident Type", choices = sort(unique(nyc$Incident.Type)), selected = "Fire-3rd Alarm")
	),

	mainPanel(
	  tabsetPanel(
	    tabPanel("Barchart", plotOutput("crime")),
	    tabPanel("Scatterplot of Locations", plotOutput("location"))
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
      with(wordcloud(word, n, max.words = input$freq))
    
    
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
  


  nyc_subset <- reactive({
    nyc %>%
      filter(Incident.Type == input$incident_type)
  })

  output$crime <- renderPlot({
    ggplot(data = nyc_subset(), aes(x = Borough, fill = Borough)) +
      geom_bar(stat = "count") +
      theme_bw() +
      ggtitle(paste("Number of", input$incident_type, "Reports by Borough"))
  })

  output$location <- renderPlot({
    ggplot(data = nyc_subset(), aes(x = Longitude, y = Latitude)) +
      geom_point(aes(colour = Borough)) +
      theme_bw() +
      ggtitle("Locations of incidents")
  })

}

shinyApp(ui, server)
