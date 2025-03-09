ui <- bslib::page_sidebar(
  outputId = "main",
  title = i18n$t("Power input and output analysis of medaco data"),
  shiny.i18n::usei18n(i18n),
  sidebar = bslib::sidebar(
    outputId = "sidebar",
    width = 300,
    bslib::input_dark_mode(id = "display_mode"),
    shiny::selectInput(
      inputId = "selected_language",
      label = i18n$t("Select language"),
      choices = stats::setNames(
        i18n$get_languages(),
        c("\U0001F1EC\U0001F1E7 English", "\U0001F1E9\U0001F1EA Deutsch")
      ),
      selected = "de"
    ),
    if (Sys.getenv("MEDACO_DATA") == "") {
      shinyFiles::shinyDirButton(
        "directory",
        i18n$t("Select Folder"),
        i18n$t("Please select folder containing data")
      )
    },
    shiny::selectInput("year_to_plot",
      label = i18n$t("Select year"),
      choices = ""
    ),
    shiny::selectInput("plot_type",
      label = i18n$t("Select plot type"),
      choices = ""
    )
  ),
  bslib::navset_card_underline(
    title = i18n$t("Visualization"),
    bslib::nav_panel(i18n$t("Plot"), shiny::plotOutput(
      "plot",
      fill = TRUE, height = "700px"
    )),
    bslib::nav_panel(i18n$t("Data"), DT::dataTableOutput(
      outputId = "output_data", width = "100%", height = "auto", fill = TRUE
    ))
  )
)
