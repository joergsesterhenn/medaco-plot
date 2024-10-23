library(shiny)
library(shinyFiles)

server <- function(input, output, session) {
  source("read.R", local = TRUE)
  source("plot.R", local = TRUE)
  source("data-manipulation.R", local = TRUE)
  roots <- c(
    win = "C:/", # Root directory for Windows
    lin = "/", # Root directory for Linux
    home = "~" # Root directory for Home
  )
  # Enable shinyFiles to interact with the file system
  shinyFiles::shinyDirChoose(
    input,
    "directory",
    roots = roots,
    defaultRoot = "home",
    allowDirCreate = FALSE
  )
  # Reactive expression to read CSV files from the selected folder
  data <- shiny::reactive({
    shiny::req(input$directory)

    # Get the selected directory path
    folder_path <- shinyFiles::parseDirPath(roots = roots, input$directory)

    # Read and combine CSV files from the folder
    shiny::req(folder_path)
    return(read_power_data(folder_path))
  })

  # Reactive plot output based on user selection
  output$plot <- shiny::renderPlot({
    shiny::req(data())
    plot(input$plot_type, data())
  })

  shiny::updateSelectInput(
    session = session,
    inputId = "plot_type",
    choices = row.names(plot_map)
  )
}
