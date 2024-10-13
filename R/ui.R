library(shiny)
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
