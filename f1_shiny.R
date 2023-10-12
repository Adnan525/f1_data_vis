library(shiny)
library(sf)
library(mapview)

# dataset
circuits <- read.csv("data/circuits.csv")
names(circuits)[3] <- "circuit_name"

# variables
circuit_options_vector  = unique(circuits$circuit_name)


ui <- fluidPage(
  "F1 Analysis"
)
server <- function(input, output, session){
  
}
library(shiny)

ui <- fluidPage(
  titlePanel("F1 Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Topics", label = "Topics", choices = c("Please select a topic", "Circuits", "Drives")),
      conditionalPanel(
        condition = "input.Topics == 'Circuits'",
        selectizeInput("search for circuits", label = "Search", choices = circuit_options_vector, multiple = FALSE)
      )
    ),
    mainPanel(
      img(src = "dataset-cover.jpg", height = 140, width = 800),
    )
  )
)


server <- function(input, output) {
}



shinyApp(ui, server)