library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(viridis)
library(shinyFiles)


# Shiny App
ui <- fluidPage(
  titlePanel("Power Input and Output Analysis of medaco data"),

  sidebarLayout(
    sidebarPanel(width = 0),
    mainPanel(
      shinyDirButton("directory", "Select Folder", "Please select a folder"),
      selectInput("plot_type",
                  label = "Select Plot Type",
                  choices = c("Line Chart", "Heatmap", "Ridgeline Plot", "Stacked Area Chart", "By hour and month", "By month", "By hour"),
                  selected = "Line Chart"),
      plotOutput("plot",fill = TRUE, height = "700px"),
      width = "90%"
    )
  )
)

server <- function(input, output, session) {
  source("Read.R", local = TRUE)
  source("Plot.R", local = TRUE)
  roots = c(
    win = "C:/",     # Root directory for Windows
    lin = "/",       # Root directory for Linux
    home ="~"        # Root directory for Home
  )
  # Enable shinyFiles to interact with the file system
  shinyDirChoose(input, "directory", roots = roots, defaultRoot = 'home', allowDirCreate = FALSE)
  # Reactive expression to read CSV files from the selected folder
  data <- reactive({
    req(input$directory)

    # Get the selected directory path
    folder_path <- parseDirPath(roots = roots, input$directory)

    # Read and combine CSV files from the folder
    if (!is.na(folder_path) && folder_path != ''){
      df <- read_power_data(folder_path)
    }
  })

  # Reactive plot output based on user selection
  output$plot <- renderPlot({
    req(data())

    if (input$plot_type == "Line Chart") {
      plot_line_chart(data())
    } else if (input$plot_type == "Heatmap") {
      plot_heatmap(data())
    } else if (input$plot_type == "Ridgeline Plot") {
      plot_ridgeline(data())
    } else if (input$plot_type == "Stacked Area Chart") {
      plot_stacked_area(data())
    } else if (input$plot_type == "By month") {
      plot_aggregated_by_month(data())
    } else if (input$plot_type == "By hour") {
      plot_aggregated_by_hour(data())
    } else if (input$plot_type == "By hour and month") {
      plot_by_hour_and_month(data())
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
