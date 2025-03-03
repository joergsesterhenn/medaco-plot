ui <- bslib::page_sidebar(
  title = "Power input and output analysis of medaco data",
  sidebar = bslib::sidebar(
    width = 300,
    bslib::input_dark_mode(id = "display_mode"),
    if (Sys.getenv("MEDACO_DATA") == "") {
      shinyFiles::shinyDirButton(
        "directory",
        "Select Folder",
        "Please select folder containing data"
      )
    },
    shiny::selectInput("year_to_plot",
      label = "Select year",
      choices = ""
    ),
    shiny::selectInput("plot_type",
      label = "Select plot type",
      choices = ""
    )
  ),
  bslib::navset_card_underline(
    title = "Visualization",
    bslib::nav_panel("Plot", shiny::plotOutput(
      "plot",
      fill = TRUE, height = "700px"
    )),
    bslib::nav_panel("Data", DT::dataTableOutput(
      outputId = "output_data", width = "100%", height = "auto", fill = TRUE
    ))
  )
)
