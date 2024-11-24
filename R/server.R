server <- function(input, output, session) {
  source("read.R", local = TRUE)
  source("plot.R", local = TRUE)
  source("data-manipulation.R", local = TRUE)


  if (Sys.getenv("MEDACO_DATA") == "") {
    roots <- c(
      windows = "C:/", # Root directory for Windows
      linux = "/", # Root directory for Linux
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
  }
  # Reactive expression to read CSV files from the selected directory
  inputdata <- shiny::reactive({
    if (Sys.getenv("MEDACO_DATA") == "") {
      # Get the selected directory path
      shiny::req(input$directory)
      selected_directory <- shinyFiles::parseDirPath(
        roots = roots,
        input$directory
      )
    } else {
      selected_directory <- Sys.getenv("MEDACO_DATA")
    }


    # Read and combine CSV files from the selected directory
    shiny::req(selected_directory)
    return(read_power_data(selected_directory))
  })


  # Reactive plot output based on user selection
  output$plot <- shiny::renderPlot({
    shiny::req(inputdata())
    plot(input$plot_type, inputdata())
  })

  shiny::updateSelectInput(
    session = session,
    inputId = "plot_type",
    choices = row.names(plot_map)
  )
}
