library(shiny)
library(sf)
library(DT)
library(leaflet)
library(tidyverse)

# Load the get_driver_details function
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
      width = 3,
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
      width = 8,
      img(src = "dataset-cover.jpg", height = 140, width = 800),
      uiOutput("space"),
      conditionalPanel(
        # condition = "!is.na(input.circuit_search) && input.circuit_search !== null",
        condition = "input.topics == 'Circuits'",
        conditionalPanel(
          condition = "!input.circuit_search == ''",
          leafletOutput("map", width = "50%", height = "300px")
        ),
        uiOutput("selected_text")
      ),
      # driver
      conditionalPanel(
        condition = "input.topics == 'Drivers'",
        uiOutput("driver_details_text"),
        DTOutput("win_by_team_table"),
        uiOutput("career_podium_text"),
        DTOutput("podium_by_team_table"),
        uiOutput("career_point_text"),
        DTOutput("points_by_team_table")
      )
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
    if (!is.na(selected) && !is.null(selected)) {
      if (input$topics == "Circuits") {
        test <- circuits %>% filter(circuit_name == selected)
        leaflet() %>%
          setView(lng = test$lng, lat = test$lat, zoom = 14) %>%
          addTiles() %>%
          addMarkers(lng = test$lng, lat = test$lat, label = test$circuitRef)
      }
    }
  })
  
  output$selected_text <- renderText({
    selected <- selected_item()
    if (!is.null(selected)) {
      temp <- circuits %>% filter(circuit_name == selected)
      temp <- as.list(temp)
      paste("<br>",
            "Circuit Name :", temp$circuit_name, "<br>",
            "Location :", temp$location, "<br>",
            "Fastest Lap :", temp$fastestLapTime_inSeconds, "<br>",
            "Driver :", temp$driverRef, "<br>",
            "Constructor :", temp$constructorRef, "<br>",
            "Year :", temp$year)
    }
  })
  
  
  
  
  
  driver_details <- reactive({
    selected_driver <- input$driver_search
    if (!is.null(selected_driver) && input$topics == "Drivers") {
      get_driver_details(selected_driver, 2023)
    } else {
      NULL
    }
  })
  
  # Render the string and integer variables vertically
  output$driver_details_text <- renderText({
    driver_info <- driver_details()
    if (!is.null(driver_info)) {
      paste("<br>",
            "Name :", driver_info$name, "<br>",
            "Appearance :", driver_info$appearance, "<br>",
            "Championship :", driver_info$championship, "<br>",
            "Still on the grid :", driver_info$currently_driving, "<br>",
            "Current team :", driver_info$current_team, "<br>",
            "All Teams :", paste(driver_info$all_teams, collapse = ", "), "<br>",
            "Total wins :", driver_info$wins,"<br>",
            "<br>",
            "Career win with each team :", "<br>"
      )
    }
  })
  output$win_by_team_table <- renderDataTable({
    driver_info <- driver_details()
    if (!is.null(driver_info)) {
      win_by_team_df <- driver_info$win_by_team
      datatable(win_by_team_df, options = list(dom = 't', searching = FALSE))
    }
  })
  
  output$career_podium_text <- renderText({
    driver_info <- driver_details()
    if (!is.null(driver_info)) {
      paste("<br>",
            "Total podiums :", driver_info$podiums, "<br>",
            "<br>",
            "Career podium with each team :"
      )
    }
  })
  output$podium_by_team_table <- renderDataTable({
    driver_info <- driver_details()
    if (!is.null(driver_info)) {
      podium_by_team_df <- driver_info$podium_by_team
      datatable(podium_by_team_df, options = list(dom = 't', searching = FALSE))
    }
  })
  
  output$career_point_text <- renderText({
    driver_info <- driver_details()
    if (!is.null(driver_info)) {
      paste("<br>",
            "Total points :", driver_info$career_points, "<br>",
            "<br>",
            "Career points with each team :"
      )
    }
  })
  output$points_by_team_table <- renderDataTable({
    driver_info <- driver_details()
    if (!is.null(driver_info)) {
      points_by_team_df <- driver_info$points_by_team
      datatable(points_by_team_df, options = list(dom = 't', searching = FALSE))
    }
  })
}

shinyApp(ui, server)
