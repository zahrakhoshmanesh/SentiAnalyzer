library(shiny)
library(caret)

library(tidyr)
library(tidyverse)
library(RWeka)
library(eply)
library(ggplot2)
#install.packages("shinythemes") to run code
x<-read.csv("./Data/x.csv")
ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To set and use a specific theme, uncomment this
      "ML training algorithms training",
      #sidebarLayout(
               sidebarPanel(

                 #textInput("txt", "Text input:", "general"),
                 #sliderInput("slider", "Slider input:", 1, 100, 30),
                 # tags$h5("Deafult actionButton:"),
                 # actionButton("action", "Search"),

                 # tags$h5("actionButton with CSS class:"),
                 # actionButton("action2", "Action button", class = "btn-primary")
                 radioButtons(
                   "whichM",
                   "Training Alg",
                   choices = c("knn", "gbm"),
                   selected = "knn"
                 ),

                 conditionalPanel(
                   condition = "input.whichM=='knn'",

                   numericInput("knn_min", "min number of neigbors: ", value = 1, min=1, max=50),
                   numericInput("knn_max", "max number of neigbors: ", value = 1, min=1, max=50)
                 ),

                 conditionalPanel(
                   condition = "input.whichM=='gbm'",

                   numericInput("n.trees_l", " min number of trees:",
                                value = 1, min=5, max=500),
                   numericInput("n.trees_m", "max number of trees:",
                                value = 1, min=5, max=500)
                 )

               ),

                 tabsetPanel(
                   tabPanel("Visual",
                            mainPanel(plotOutput("train")

                            )
                            )


                  # tabPanel("Summary",
                           # mainPanel(h4("Summary")),
                             # verbatimTextOutput("summary")



                            # Output: Header + table of distribution ----
                            #h4("Observations"),
                            #tableOutput("view")










                   )#for tabpanel

                 )#for tabset panel
              #for main panel

      #tabPanel("Visual",

                 #textInput("txt", "Text input:", "general"),
                 #sliderInput("slider", "Slider input:", 1, 100, 30),
                 # tags$h5("Deafult actionButton:"),
                 # actionButton("action", "Search"),

                 # tags$h5("actionButton with CSS class:"),
                 # actionButton("action2", "Action button", class = "btn-primary")
              # ),


)
#)






         #  tabPanel("Summary",
                    #verbatimTextOutput("summary") ),

# Define server logic required to draw a histogram
server <- function(input, output) {
  #datasetInput <- reactive({
  #input$whichM
  output$train <- renderPlot({

    if (input$whichM=="knn")

    res_name <- colnames(x)[ncol(x)]
    formula <- as.formula(paste(res_name, ' ~ .'))


    treeGrid_knn = expand.grid(k = c(input$knn_min:input$knn_max))

    # train control
    ctrl_cv10 = trainControl(
      method = "cv",
      number = 5,
      savePred = T,
      classProb = T
    )

    ###### kNN with Cross Validation ############
    model_knn_10 = train(
      formula,
      data = x,
      method = "knn",
      trControl = ctrl_cv10,
      tuneGrid = treeGrid_knn
    )

    plot(model_knn_10)


    if (input$whichM=="gbm")
      gbmGrid <-  expand.grid(
        interaction.depth = c(1, 5, 9),
        n.trees = c(input$n.trees_l:input$n.trees_M) * 50,
        shrinkage = 0.1,
        n.minobsinnode = 20
      )

    gbmFit2 <- train(
      formula,
      data = x,
      method = "gbm",

      verbose = FALSE,

      tuneGrid = gbmGrid,
      trControl = ctrl_cv10
    )

    plot(gbmFit2)
  })
 # if (input$whichM=="knn")
  # Generate a summary of the dataset ----
  #output$summary <- renderPrint({
  #  print(model_knn_10)})
 # }#for reactive
}

# Bind ui and server together
shinyApp(ui, server)


