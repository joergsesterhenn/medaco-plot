# Shiny App
ui <- shiny::fluidPage(
  shiny::titlePanel("Power input and output analysis of medaco data"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      if (Sys.getenv("MEDACO_DATA") == "") {
        shinyFiles::shinyDirButton(
          "directory",
          "Select Folder",
          "Please select folder containing data"
        )
      },
      shiny::selectInput("plot_type",
        label = "Select Plot Type",
        choices = ""
      ),
      width = 10
    ),
    shiny::mainPanel(
      shiny::plotOutput("plot", fill = TRUE, height = "700px"),
      width = 90
    )
  )
)
