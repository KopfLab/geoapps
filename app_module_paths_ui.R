source("module_zoom_plot.R")

# paths module user interface functions
paths_ui_upload_data_button <- function(id) {
  ns <- NS(id)
  fileInput(ns("upload"), "Upload data", multiple = TRUE, accept = ".ab1") |>
    add_tooltip("Upload your own data files")
}

paths_ui_load_example_button <- function(id) {
  ns <- NS(id)
  actionButton(ns("load_example"), "Load example", icon = icon("eye")) |>
    add_tooltip("Load example files")
}

paths_ui_file_selector <- function(id) {
  ns <- NS(id)
  selectInput(ns("file"), "Files", c()) |>
    shinyjs::hidden()
}

paths_ui_seq_start <- function(id) {
  ns <- NS(id)
  numericInput(ns("seq_start"), "Start", c()) |>
    add_tooltip("Where does the high quality part of the sequence start?") |>
    shinyjs::hidden()
}

paths_ui_seq_end <- function(id) {
  ns <- NS(id)
  numericInput(ns("seq_end"), "End", c()) |>
    add_tooltip("Where does the high quality part of the sequence end?") |>
    shinyjs::hidden()
}

paths_ui_file_plot <- function(id, plot_height = 650) {
  ns <- NS(id)
  tagList(
    # plot div
    div(id = ns("file_plot_div"),
        # plot box ------
        shinydashboard::box(
          title = textOutput(ns("file_plot_title")), width = 12,
          status = "info", solidHeader = TRUE,
          div(style = paste0("min-height: ", plot_height, "px;"),
              div(id = ns("file_plot_actions"),
                  fluidRow(
                    # column(width = 4,
                    # ),
                    column(width = 12, align = "center",
                      zoom_plot_ui_nav_buttons(ns("plot"))
                    ),
                    # column(width = 4, align = "right",
                    #        tooltipInput(actionButton, ns("plot_refresh"), NULL, icon = icon("sync"),
                    #                     tooltip = "Refresh the plot with the selected filters and plot options.") |> shinyjs::disabled(),
                    #        spaces(1),
                    #        plotDownloadLink(ns("plot_download"), label = NULL) |> shinyjs::disabled(),
                    #        spaces(1),
                    #        dataDownloadLink(ns("data_download"), label = NULL) |> shinyjs::disabled()
                    # )
                  )
              ),
              zoom_plot_ui_plot_output(ns("plot"))
          )
        )
    ) |> shinyjs::hidden(),
    # output div
    div(id = ns("file_seq_div"),
        # plot box ------
        shinydashboard::box(
          title = textOutput(ns("file_seq_title")), width = 12,
          status = "info", solidHeader = TRUE,
          tags$pre(textOutput(ns("seq")))
        )
    ) |> shinyjs::hidden()
  )
}

