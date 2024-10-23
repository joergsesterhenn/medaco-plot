library(shiny)
library(shinyFiles)

# Shiny App
ui <- shiny::fluidPage(
  shiny::titlePanel("Power Input and Output Analysis of medaco data"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shinyFiles::shinyDirButton(
        "directory",
        "Select Folder",
        "Please select folder containing data"
      ),
      shiny::selectInput("plot_type",
        label = "Select Plot Type",
        choices = ""
      ),
      width = 10
    ),
    shiny::mainPanel(
      plotOutput("plot", fill = TRUE, height = "700px"),
      width = 90
    )
  )
)
