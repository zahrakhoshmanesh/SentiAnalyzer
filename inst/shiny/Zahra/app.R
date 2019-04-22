library(shiny)

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
