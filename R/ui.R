library(bslib)
library(DT)
# Shiny App
# ui <- shiny::fluidPage(
#   theme = bslib::bs_theme(preset = "shiny"),
#   shiny::titlePanel("Power input and output analysis of medaco data"),
#   shiny::sidebarLayout(
#     shiny::sidebarPanel(
#       bslib::input_dark_mode(id = "display_mode"),
#       ,
#
#   )
# )
ui <- bslib::page_sidebar(
  title = "Power input and output analysis of medaco data",
  sidebar = sidebar(
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
      label = "Select Plot Type",
      choices = ""
    )
  ),

  # Main panel for displaying outputs ----
  # Output: A tabset that combines three panels ----
  navset_card_underline(
    title = "Visualizations",
    nav_panel("Plot", shiny::plotOutput("plot", fill = TRUE, height = "700px")),
    nav_panel("Data", dataTableOutput(
      outputId = "output_data", width = "100%", height = "auto", fill = TRUE
    ))
  )
)
