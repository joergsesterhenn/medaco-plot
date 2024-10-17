library(shiny)
library(shinyFiles)

server <- function(input, output, session) {
  source("./Read.R", local = TRUE)
  source("./Plot.R", local = TRUE)
  roots <- c(
    win = "C:/", # Root directory for Windows
    lin = "/", # Root directory for Linux
    home = "~" # Root directory for Home
  )
  # Enable shinyFiles to interact with the file system
  shinyDirChoose(
    input,
    "directory",
    roots = roots,
    defaultRoot = "home",
    allowDirCreate = FALSE
  )
  # Reactive expression to read CSV files from the selected folder
  data <- reactive({
    req(input$directory)

    # Get the selected directory path
    folder_path <- parseDirPath(roots = roots, input$directory)

    # Read and combine CSV files from the folder
    req(folder_path)
    return(read_power_data(folder_path))
  })

  # Reactive plot output based on user selection
  output$plot <- renderPlot({
    req(data())
    plot(input$plot_type, data())
  })
}
