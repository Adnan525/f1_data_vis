library(shiny)
library(sf)
library(mapview)
library(leaflet)

# dataset
circuits <- read.csv("data/circuits_updated.csv")

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
      textOutput("selected_text"),
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

  # output$selected_text <- renderText({
  #   selected <- selected_item()
  #   if (input$topics == "Circuits") {
  #     if (!is.null(selected)) {
  #       paste("You selected:", selected)
  #     } else {
  #       "Please select a circuit."
  #     }
  #   }
  # })
  output$selected_text <- renderText({
    selected <- selected_item()
    if (input$topics == "Circuits") {
      if (!is.null(selected)) {
        temp <- circuits %>% filter(circuit_name == selected)
        temp <- as.list(temp)
        paste(temp, collapse = " ")
      } else {
        "Please select a circuit."
      }
    }
  })
  output$map <- renderLeaflet({
    selected <- selected_item()
    temp <- circuits %>% filter(circuit_name == selected)
    if (!is.null(selected)) {
      leaflet() %>%
        setView(lng = temp$lng, lat = temp$lat, zoom = 14) %>%
        addTiles() %>%
        addMarkers(lng = temp$lng, lat = temp$lat, label = "Point") 
    }
  })
}

shinyApp(ui, server)