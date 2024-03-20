# paths module server
module_paths_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    last_term = NULL
  )

  # data functions ===========

  # available terms
  get_terms <- function() {
    get_current_term() |>
      get_available_terms(n_years_past_current = 2)
  }

  # get upper division classes
  get_ud_classes <- reactive({
    validate(need(data$classes$get_data(), "something went wrong retrieving the data"))
    data$classes$get_data() |>
      dplyr::select(-".rowid", -".add", -".update", -".delete") |>
      get_unique_classes() |>
      get_classes_above_level(level = 3000) |>
      complete_semester_availabilities()
  })

  # get path recommendations
  get_paths <- reactive({
    validate(need(data$paths$get_data(), "something went wrong retrieving the data"))
    data$paths$get_data() |>
      dplyr::select(-".rowid", -".add", -".update", -".delete") |>
      prep_path_recommendations()
  })

  # get path names
  get_paths_list <- reactive({
    req(get_paths())
    get_paths() |> dplyr::pull(path) |> unique()
  })

  # selected path
  get_selected_path <- reactive({
    req(!is.null(input$path) && nchar(input$path) > 0)
    input$path
  })

  # get classes
  get_classes <- reactive({
    req(get_selected_path())
    req(get_paths())
    req(get_ud_classes())
    log_info(ns = ns, "loading path", user_msg = sprintf("Loading %s path", get_selected_path()))
    get_path_classes(input$path, get_paths(), get_ud_classes()) |>
      dplyr::mutate(id = .data$class)
  })

  # generate UI =====================

  # sidebar GUI
  output$sidebar <- renderUI({
    req(get_terms())
    log_info("generating sidebar")
    terms <- get_terms() |> drop_summers()
    tagList(
      selectInput(
        ns("path"), "Select path:",
        choices = c("Select path" = "", get_paths_list()),
        selected = if (is_dev_mode()) get_paths_list()[1] else NULL
      ),
      selectizeInput(
        ns("last_term"), "Show terms to:",
        multiple = FALSE,
        choices = get_sorted_terms(terms),
        selected =
          isolate({
            if (!is.null(values$last_term) && values$last_term %in% terms) values$last_term
            else get_past_or_future_term(years_shift = +2)
          })
      )
    )
  })

  # main GUI
  output$main <- renderUI({
    req(get_paths())
    log_info("loading list")
    tagList(
      div(id = ns("path_box"),
          shinydashboard::box(
            title =
              span(
                "Classes recommended for the ",
                tags$strong(textOutput(ns("path_label"), inline = TRUE)),
                " path",
                div(
                  style = "position: absolute; right: 10px; top: 5px;",
                  module_selector_table_deselect_all_button(ns("classes"), border = FALSE),
                  actionButton(ns("check"), "Check", icon = icon("check"), style = "border: 0;") |>
                    add_tooltip("Check selected classes for fulfillment of degree requirementes (degree audit)."),
                )
              ), width = 12,
            status = "info", solidHeader = TRUE,
            module_selector_table_ui(ns("classes"))
          )
      ) |> shinyjs::hidden()
    )
  })

  # path label
  output$path_label <- renderText({
    req(get_selected_path())
    get_selected_path()
  })

  # box visibility
  observeEvent(input$path, {
    show <- !is.null(input$path) && nchar(input$path) > 0
    if (show) log_debug(ns = ns, "showing path box") else log_debug(ns = ns, "hiding path box")
    shinyjs::toggle("path_box", condition = show)
  }, ignoreNULL = FALSE)

  # classes table ======
  # FIXME: this needs to be dynamic re. columns, coloring etc.
  classes <- callModule(
    module_selector_table_server,
    "classes",
    get_data = get_classes,
    id_column = "id",
    available_columns = list(
      Category =
        ifelse(
          !is.na(category_description),
          sprintf("%s<br/><i>%s</i>",
                  htmltools::htmlEscape(category_info),
                  htmltools::htmlEscape(category_description)),
          htmltools::htmlEscape(category_info)
        ),
      Class = sprintf("%s(%s)", class, credits),
      Title = title,
      `Relevance for this path` = reason,
      `Spring 2023`, `Fall 2023`, `Spring 2024`, `Fall 2024`
    ),
    allow_view_all = TRUE,
    initial_page_length = -1,
    dom = "ft",
    selection = "multiple",
    # row grouping
    render_html = "Category",
    extensions = c("RowGroup", "FixedHeader"),
    rowGroup = list(dataSrc = 0),
    columnDefs = list(list(visible = FALSE, targets = 0)),
    # scrolling
    scrollX = TRUE,
    fixedHeader = TRUE,
    # formatting
    formatting_calls = list(
      list(
        func = DT::formatStyle, columns = c("Spring 2023", "Fall 2023", "Spring 2024", "Fall 2024"),
        backgroundColor = DT::styleEqual(
          get_classes_teaching_info_placeholder() |> names(),
          get_classes_teaching_info_placeholder() |> as.character()
        )
      )
    )
  )


}

# load UI dynamically
module_path_sidebar <- function(id) {
  ns <- NS(id)
  uiOutput(ns("sidebar"))
}

# load UI dynamically
module_paths_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
