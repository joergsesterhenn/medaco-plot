library(shiny)
library(shinyFiles)

source("./Plot.R", local = TRUE)
# Shiny App
ui <- fluidPage(
  titlePanel("Power Input and Output Analysis of medaco data"),
  sidebarLayout(
    sidebarPanel(
      shinyDirButton(
        "directory",
        "Select Folder",
        "Please select folder containing data"
      ),
      selectInput("plot_type",
        label = "Select Plot Type",
        choices = row.names(plot_map),
        selected = row.names(plot_map)[1]
      ),
      width = 10
    ),
    mainPanel(
      plotOutput("plot", fill = TRUE, height = "700px"),
      width = 90
    )
  )
)
