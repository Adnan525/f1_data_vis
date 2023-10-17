library(shiny)
library(sf)
library(mapview)
library(leaflet)

# dataset
circuits <- read.csv("data/circuits.csv")
names(circuits)[3] <- "circuit_name"

# variables
circuit_options_vector <- unique(circuits$circuit_name)

# shiny
ui <- fluidPage(
  titlePanel("F1 Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("topics", label = "Topics", choices = c("Please select a topic", "Circuits", "Drivers")),
      conditionalPanel(
        condition = "input.topics == 'Circuits'",
        selectizeInput("circuit_search", label = "Select a circuit", choices = circuit_options_vector, multiple = FALSE)
      ),
    ),
    mainPanel(
      img(src = "dataset-cover.jpg", height = 140, width = 800),
      leafletOutput("map", width = "50%", height = "300px")
    )
  )
)

server <- function(input, output) {
  # selected circuit or driver
  selected_item <- reactiveVal(NULL)
  observeEvent(input$circuit_search, {
    selected_item(input$circuit_search)
  })

  output$selected_text <- renderText({
    selected <- selected_item()
    if (input$topics == "Circuits") {
      if (!is.null(selected)) {
        paste("You selected:", selected)
      } else {
        "Please select a circuit."
      }
    }
  })
  output$map <- renderLeaflet(
    leaflet() %>%
      setView(lng = 144.968000, lat = -37.84970, zoom = 13) %>%
      addTiles() %>%
      addMarkers(lng = 144.968000, lat = -37.84970, label = "Point") 
  )
}

shinyApp(ui, server)