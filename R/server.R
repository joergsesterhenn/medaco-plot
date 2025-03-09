server <- function(input, output, session) {
  source("read.R", local = TRUE)
  source("plot.R", local = TRUE)
  source("data-manipulation.R", local = TRUE)

  shiny::observeEvent(input$selected_language, {
    shiny.i18n::update_lang(
      language = input$selected_language
    )
  })

  if (Sys.getenv("MEDACO_DATA") == "") {
    roots <- c(
      windows = "C:/", # Root directory for Windows
      linux = "/", # Root directory for Linux
      home = "~" # Root directory for Home
    )
    shinyFiles::shinyDirChoose(
      input,
      "directory",
      roots = roots,
      defaultRoot = "home",
      allowDirCreate = FALSE
    )
  }
  # read CSV files
  inputdata <- shiny::reactive({
    if (Sys.getenv("MEDACO_DATA") == "") {
      shiny::req(input$directory)
      selected_directory <- shinyFiles::parseDirPath(
        roots = roots,
        input$directory
      )
    } else {
      selected_directory <- Sys.getenv("MEDACO_DATA")
    }
    # read and combine CSV files from the selected directory
    shiny::req(selected_directory)
    read_power_data(selected_directory)
  })

  # react to change of year
  shiny::observe({
    shiny::req(inputdata())
    shiny::updateSelectInput(
      session = session,
      inputId = "year_to_plot",
      choices = get_years_in_data(inputdata())
    )
  })

  # filter inputdata for selected year
  filtered_inputdata <- shiny::reactive({
    shiny::req(inputdata())
    shiny::req(input$year_to_plot)
    get_data_for_year(inputdata(), input$year_to_plot)
  })

  # react to change in data
  shiny::observe({
    shiny::req(filtered_inputdata())
    shiny::req(input$year_to_plot)
    number_of_rows <- nrow(filtered_inputdata())
    shiny::showNotification(
      paste(
        i18n$t("Showing "),
        number_of_rows,
        i18n$t(" datapoints for "),
        input$year_to_plot, "."
      ),
      type = "message",
      duration = 5
    )
  })

  # react to change of plot type
  shiny::observe({
    shiny::req(plot_map)
    shiny::updateSelectInput(
      session = session,
      inputId = "plot_type",
      choices = stats::setNames(
        row.names(plot_map),
        i18n$t(row.names(plot_map))
      )
    )
  })

  # reactive data output based on user selection
  output$output_data <- DT::renderDT({
    shiny::req(filtered_inputdata())
    shiny::req(input$plot_type)
    get_data_for_plot_type(input$plot_type, filtered_inputdata())
  })

  # reactive plot output based on user selection
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
