library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(viridis)
library(shiny)
library(shinyFiles)

ui <- fluidPage(
  titlePanel("Power Input and Output Analysis of medaco data"),

  sidebarLayout(
    sidebarPanel(width = 0),
    mainPanel(
      shinyDirButton("directory", "Select Folder", "Please select a folder"),
      selectInput("plot_type",
                  label = "Select Plot Type",
                  choices = c("Line Chart", "Heatmap", "Ridgeline Plot", "Stacked Area Chart", "By hour and month", "By month", "By hour","Box Plot by month" ,"Box Plot by hour"),
                  selected = "Line Chart"),
      plotOutput("plot",fill = TRUE, height = "700px"),
      width = "90%"
    )
  )
)
