library(shiny)
library(caret)

library(tidyr)
library(tidyverse)
library(RWeka)
library(eply)
library(ggplot2)
#install.packages("shinythemes") to run code
x<-read.csv("x.csv")
ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To set and use a specific theme, uncomment this
      "ML training algorithms",
      #sidebarLayout(
             sidebarPanel(
                 radioButtons(
                   "whichM",
                   h2("Training Alg"),
                   choices = c("knn", "gbm"),
                   selected = "knn"),


                 conditionalPanel(
                   condition = "input.whichM=='knn'",

                   numericInput("knn_min", h5("min number of neigbors: "), value = 1, min=1, max=50),
                   numericInput("knn_max", h5("max number of neigbors: "), value = 3, min=1, max=50)),


                 conditionalPanel(
                   condition = "input.whichM=='gbm'",

                   numericInput("n_l", h5("min number of trees:"),
                                value = 1, min=5, max=500),
                   numericInput("n_m", h5("max number of trees:"),
                                value = 3, min=5, max=500)),



                mainPanel(
                 tabsetPanel(
                   tabPanel(h4("Visual"),plotOutput("train")),
                   tabPanel(h4("summary"),verbatimTextOutput("summary"))
                            )
                          )
               )))


server <- function(input, output) {
  #datasetInput <- reactive({
  #input$whichM
  ctrl_cv10 = trainControl(
    method = "cv",
    number = 5,
    savePred = T,
    classProb = T
  )
  output$train <- renderPlot({
    # train control
    ctrl_cv10 = trainControl(
      method = "cv",
      number = 5,
      savePred = T,
      classProb = T
    )
    if (input$whichM=="knn"){

    res_name <- colnames(x)[ncol(x)]
    formula <- as.formula(paste(res_name, ' ~ .'))


    treeGrid_knn = expand.grid(k = c(input$knn_min:input$knn_max))



    ###### kNN with Cross Validation ############
    model_knn_10 = train(
      formula,
      data = x,
      method = "knn",
      trControl = ctrl_cv10,
      tuneGrid = treeGrid_knn
    )

    plot(model_knn_10)}



    else if (input$whichM=="gbm"){

      gbmGrid <-  expand.grid(
        interaction.depth = c(1, 5, 9),
        n.trees = c(input$n_l:input$n_m) * 50,
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

    plot(gbmFit2)}
  })

  # Generate a summary of the training ----
  output$summary <- renderPrint({


    if (input$whichM=="knn"){

      res_name <- colnames(x)[ncol(x)]
      formula <- as.formula(paste(res_name, ' ~ .'))


      treeGrid_knn = expand.grid(k = c(input$knn_min:input$knn_max))



      ###### kNN with Cross Validation ############
      model_knn_10 = train(
        formula,
        data = x,
        method = "knn",
        trControl = ctrl_cv10,
        tuneGrid = treeGrid_knn)

      print(model_knn_10)


    }
    else if (input$whichM=="gbm"){
      gbmGrid <-  expand.grid(
        interaction.depth = c(1, 5, 9),
        n.trees = c(input$n_l:input$n_m) * 50,
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
      print(gbmFit2)

      }


 # }#for reactive
  })
}
# Bind ui and server together
shinyApp(ui, server)


