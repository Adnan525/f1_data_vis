library(shiny)
library(sf)
library(leaflet)

# function
source("get_driver_details.R")

# dataset
circuits <- read.csv("data/circuits_updated.csv")
driver_standings <- read.csv("data/driver_standings_updated.csv")

# variables
circuit_options_vector <- unique(circuits$circuit_name)
drivers_vector <- unique(driver_standings$driver_name)

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
      conditionalPanel(
        condition = "input.topics == 'Drivers'",
        selectizeInput("driver_search", label = "Select a driver", choices = drivers_vector, multiple = FALSE)
      )
    ),
    mainPanel(
      img(src = "dataset-cover.jpg", height = 140, width = 600),
      uiOutput("space"),
      conditionalPanel(
        # condition = "!is.na(input.circuit_search) && input.circuit_search !== null",
        condition = "input.topics == 'Circuits'",
        leafletOutput("map", width = "50%", height = "300px"),
        uiOutput("selected_text")
      )
      # driver
      # conditionalPanel(
      #   condition = "input.topics == 'Drivers'",
      #   uiOutput("driver_details_text"),
      #   DTOutput("win_by_team_table"),     # Display win_by_team as a table
      #   uiOutput("career_podium_text"),
      #   DTOutput("podium_by_team_table"),
      #   uiOutput("career_point_text"),
      #   DTOutput("points_by_team_table")
      # )
    )
  )
)

server <- function(input, output) {
  # selected circuit or driver
  selected_item <- reactiveVal(NULL)
  observeEvent(input$circuit_search, {
    selected_item(input$circuit_search)
  })

  output$space <- renderText({paste("<br>")})

  output$map <- renderLeaflet({
    selected <- selected_item()
    if (!is.na(selected)) {
      test <- circuits %>% filter(circuit_name == selected)
      leaflet() %>%
        setView(lng = test$lng, lat = test$lat, zoom = 14) %>%
        addTiles() %>%
        addMarkers(lng = test$lng, lat = test$lat, label = test$circuitRef)
    }
  })

  output$selected_text <- renderText({
    selected <- selected_item()
    if (input$topics == "Circuits") {
      if (!is.null(selected)) {
        temp <- circuits %>% filter(circuit_name == selected)
        temp <- as.list(temp)
        # paste(temp, collapse = " ")
        paste("<br>",
              "Circuit Name :", temp$circuit_name, "<br>",
              "Location :", temp$location, "<br>",
              "Fastest Lap :", temp$fastestLapTime_inSeconds, "<br>",
              "Driver :", temp$driverRef, "<br>",
              "Constructor :", temp$constructorRef, "<br>",
              "Year :", temp$year)
      }
    }
  })
}


shinyApp(ui, server)
