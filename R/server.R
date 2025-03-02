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
    read_power_data(selected_directory)
  })
  shiny::observe({
    shiny::req(plot_map)
    shiny::updateSelectInput(
      session = session,
      inputId = "plot_type",
      choices = row.names(plot_map)
    )
  })
  shiny::observe({
    shiny::req(inputdata())
    shiny::updateSelectInput(
      session = session,
      inputId = "year_to_plot",
      choices = get_years_in_data(inputdata())
    )
  })

  shiny::observe({
    shiny::req(filtered_inputdata())
    shiny::req(input$year_to_plot)
    number_of_rows <- nrow(filtered_inputdata())
    shiny::showNotification(
      paste(
        "Showing ", number_of_rows, " datapoints for ", input$year_to_plot, "."
      ),
      type = "message",
      duration = 5
    )
  })

  filtered_inputdata <- shiny::reactive({
    shiny::req(inputdata())
    shiny::req(input$year_to_plot)
    get_data_for_year(inputdata(), input$year_to_plot)
  })

  output$output_data <- DT::renderDT({
    shiny::req(filtered_inputdata())
    shiny::req(input$plot_type)
    function_name <- plot_map[input$plot_type, "map"]
    if (startsWith(function_name, "plot_by_year")) {
      get_yearly_data_long(filtered_inputdata())
    } else if (startsWith(function_name, "plot_by_month")) {
      get_monthly_data_long(filtered_inputdata())
    } else if (startsWith(function_name, "plot_by_day")) {
      get_daily_data_long(filtered_inputdata())
    } else if (startsWith(function_name, "plot_by_hour_per_year")) {
      get_hourly_data_long(filtered_inputdata())
    } else if (startsWith(function_name, "plot_by_hour_per_month")) {
      get_hourly_monthly_data_long(filtered_inputdata())
    } else {
      filtered_inputdata()
    }
  })


  # Reactive plot output based on user selection
  output$plot <- shiny::renderPlot({
    shiny::req(filtered_inputdata())
    shiny::req(input$plot_type)
    shiny::req(input$year_to_plot)
    shiny::req(input$display_mode)
    plot(
      input$plot_type,
      filtered_inputdata(),
      input$display_mode,
      input$year_to_plot
    )
  })
}
