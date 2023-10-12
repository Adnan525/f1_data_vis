library(shiny)
library(sf)
library(mapview)

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
      selectInput("topics", label = "Topics", choices = c("Please select a topic", "Circuits", "Drives")),
      conditionalPanel(
        condition = "input.topics == 'Circuits'",
        selectizeInput("circuit_search", label = "Select a circuit", choices = circuit_options_vector, multiple = FALSE)
      )
    ),
    mainPanel(
      img(src = "dataset-cover.jpg", height = 140, width = 800),
      textOutput("selected_circuit_text")
    )
  )
)

server <- function(input, output) {
  # selected circuit
  selected_circuit <- reactiveVal(NULL)
  observeEvent(input$circuit_search, {
    selected_circuit(input$circuit_search)
  })
  
  output$selected_circuit_text <- renderText({
    selected <- selected_circuit()
    if(input$topics == "Circuits")
    {
      if (!is.null(selected)) {
        paste("You selected:", selected)
      } else {
        "Please select a circuit."
      }
    }
  })
}

shinyApp(ui, server)
