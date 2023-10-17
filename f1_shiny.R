library(shiny)
library(sf)
library(mapview)

# get_driver_details function
source("get_driver_details.R")

# dataset
circuits <- read.csv("data/circuits.csv")
names(circuits)[3] <- "circuit_name"
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
      img(src = "dataset-cover.jpg", height = 140, width = 800),
      textOutput("selected_text")
    )
  )
)

server <- function(input, output) {
  # selected circuit or driver
  selected_item <- reactiveVal(NULL)
  observeEvent(input$circuit_search, {
    selected_item(input$circuit_search)
  })
  observeEvent(input$driver_search, {
    selected_item(input$driver_search)
  })

  output$selected_text <- renderTable({
    selected <- selected_item()
    if (input$topics == "Circuits") {
      if (!is.null(selected)) {
        paste("You selected:", selected)
      } else {
        "Please select a circuit."
      }
    } else if (input$topics == "Drivers") {
      if (!is.null(selected)) {
        driver_details <- get_driver_details(selected, 2023)
        str(driver_details)
      } else {
        "Please select a driver."
      }
    }
  })
}

shinyApp(ui, server)