library(shiny)
library(SentiAnalyzer)
require(ggplot2)
require(dplyr)

csv_data <- read.csv(system.file(package = "SentiAnalyzer", "extdata/testing.csv"))

# Set up data - only run once
if (!file.exists("testRes.Rdata")) {
  x<-read.csv("x.csv")
  training <- BuildTraining(x)
  prediction <- BuildPrediction(training)
  comp<-comparison(prediction)
  if(exists("comp")) { # if the object exists with the sample results, save it
    save(x,training, prediction, comp, file = "testRes.Rdata")
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Comparing different machine learning algorithms"),
  
 
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      fileInput("userfile", label = "Upload text data", accept = "csv"),
      checkboxInput("sampledata", label = "Demonstrate app with sample data", value = T),
      helpText("Warning: Running these computations can take 15-20 minutes; please be patient or use test data."),
      
      selectizeInput("Method", "Method:", 
                  choices=c("gbm","KNN","NB","RandomForest","SVM"), 
                  selected=c("gbm","KNN","NB","RandomForest","SVM"),
                  multiple = T),
      hr(),
      helpText("Select the method that you want the measurment for.")
    ),
    
 
  
  
  mainPanel(# Output: Header + table of distribution ----
            h4("view"),
            tableOutput("view"), 
            plotOutput("confPlot")
  
  )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  modeltraining <- reactive({
    # depend on userfile (if exists) and sampledata
    if (!is.null(input$userfile)) {
      # if userfile exists, use it!
      validate(need(exists(input$userfile$datapath), "Please upload a file"),
               need(file.exists(input$userfile$datapath), "File not found"))
      
      res <- SentiAnalyzer::BuildTraining(read_csv(input$userfile$datapath[1]))
    } else if (input$sampledata) {
      load("testRes.Rdata")
      res <- training
    } else {
      validate(need(!is.null(input$userfile) | input$sampledata, "Please either use sample data or provide your own."))
    }
    res
  })
  
  modelpred <- reactive({
    BuildPrediction(modeltraining())
  })
  
  modelcompare <- reactive({
    comparison(modelpred())
  })
  
  output$view <- renderTable({
    modelcompare() %>%
      filter(Method %in% input$Method)
    
  })
  
  output$confPlot <- renderPlot({
    
    make_conf_df <- function(x, y) {
      # Confusion matrix table = x
      x$table %>%
        as_data_frame() %>%
        mutate(Method = y)
    }
    
    purrr::map2_df(modelpred()$conf, 
                   c("gbm","KNN","NB","RandomForest","SVM"), 
                   make_conf_df
                   ) %>%
      filter(Method %in% input$Method) %>%
      ggplot(data = .) + 
      geom_tile(aes(x = Reference, y = Prediction, fill = n)) + 
      geom_text(aes(x = Reference, y = Prediction, label = n)) + 
      facet_wrap(~Method)
    
    
  })
}

# Bind ui and server together
shinyApp(ui, server)
