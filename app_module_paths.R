# paths module server
module_paths_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # constants
  data_err_prefix <- "Encountered database issue, the app may not function properly: "

  # reactive values
  values <- reactiveValues(
    last_term = NULL
  )

  # data functions ===========

  # available terms
  get_terms <- function() {
    get_current_term() |>
      get_available_terms(n_years_past_current = 5)
  }

  # monitor last term
  observeEvent(input$last_term, {
    if (is.null(values$last_term) || !identical(values$last_term, input$last_term))
      values$last_term <- input$last_term
  })

  # selected terms
  get_selected_terms <- reactive({
    req(get_terms())
    req(values$last_term)
    terms <- get_terms() |> filter_terms(end_term = values$last_term)
    # check if summers should be shown (checkbox?)
    terms <- terms |> drop_summers()
    return(terms)
  })

  # schedule
  get_schedule <- reactive({
    req(data$schedule$get_data())
    schedule <-
      prepare_schedule(data$schedule$get_data())
    return(schedule)
  })

  # classes
  get_classes <- reactive({
    req(data$classes$get_data())
    prepare_classes(data$classes$get_data())
  })

  # get path recommendations
  get_paths <- reactive({
    validate(need(data$paths$get_data(), "something went wrong retrieving the data"))
    data$paths$get_data() |>
      prepare_path_recommendations()
  })

  # get path names
  get_paths_list <- reactive({
    req(get_paths())
    c("ALL", get_paths() |> dplyr::pull(path_w_id) |> unique())
  })

  # selected path
  get_selected_path <- reactive({
    req(!is.null(input$path) && nchar(input$path) > 0)
    input$path
  })

  # is all selected
  show_all_paths <- reactive({
    req(get_selected_path())
    identical(get_selected_path(), "ALL")
  })

  # selected path url
  get_selected_path_url <- reactive({
    req(get_paths())
    req(get_selected_path())
    dplyr::filter(get_paths(), .data$path_w_id == get_selected_path())$url[1]
  })

  # path for table
  get_path_for_table <- reactive({
    req(get_paths())
    req(get_selected_path())
    req(get_classes())
    req(get_schedule())
    req(get_selected_terms())
    # always reset visible columns to load new selection
    classes$reset_visible_columns()
    if (show_all_paths()) {
      log_info(ns = ns, "loading all paths with terms: ", paste(get_selected_terms(), collapse = ", "), user_msg = "Loading all paths")
      get_paths() |>
        prepare_all_path_classes(classes = get_classes()) |>
        combine_path_classes_with_schedule(get_schedule(), selected_terms = get_selected_terms()) |>
        prepare_all_paths_table_columns()
    } else {
      log_info(ns = ns, "loading path with terms: ", paste(get_selected_terms(), collapse = ", "), user_msg = sprintf("Loading %s path", get_selected_path()))
      get_paths() |>
        prepare_path_classes(selected_path = get_selected_path(), classes = get_classes()) |>
        combine_path_classes_with_schedule(get_schedule(), selected_terms = get_selected_terms()) |>
        prepare_path_classes_table_columns()
    }
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
        selected = if (shiny::in_devmode()) get_paths_list()[1] else NULL
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
                htmlOutput(ns("path_label"), inline = TRUE),
                div(
                  style = "position: absolute; right: 10px; top: 5px;",
                  module_selector_table_deselect_all_button(ns("classes"), border = FALSE),
                  actionButton(ns("check"), "Check", icon = icon("check"), style = "border: 0;") |>
                    add_tooltip("Check selected classes for fulfillment of degree requirementes (degree audit)."),
                )
              ), width = 12,
            status = "info", solidHeader = TRUE,
            module_selector_table_ui(ns("classes")),
            footer = tagList(
              "Use the search bar in the upper right to filter the recommended classes (e.g. by course number or course name)."
            )
          )
      ) |> shinyjs::hidden()
    )
  })

  # path label
  output$path_label <- renderUI({
    req(get_selected_path())
    if (show_all_paths()) {
      "Overview of recommended classes by path in the Department of Earth Science"
    } else {
      tags$a("Classes recommended for the ", tags$strong(get_selected_path(), icon("link")), " path", href = get_selected_path_url(), target = "_blank")
    }
  })

  # box visibility
  observeEvent(input$path, {
    show <- !is.null(input$path) && nchar(input$path) > 0
    if (show) log_debug(ns = ns, "showing path box") else log_debug(ns = ns, "hiding path box")
    shinyjs::toggle("path_box", condition = show)
  }, ignoreNULL = FALSE)

  # classes table ======
  classes <- callModule(
    module_selector_table_server,
    "classes",
    get_data = get_path_for_table,
    id_column = "row",
    # row grouping
    render_html = "Category",
    extensions = "RowGroup",
    rowGroup = list(dataSrc = 1),
    columnDefs = list(
      list(visible = FALSE, targets = 0:1)
    ),
    # view all & scrolling
    allow_view_all = TRUE,
    initial_page_length = -1,
    dom = "ft",
    ordering = FALSE,
    scrollX = TRUE,
    scrollY = "calc(100vh - 300px)", # account for size of header with the -x px
    selection = "multiple",
    # formatting
    formatting_calls = list(
      list(
        func = DT::formatStyle,
        columns_expr = expr(dplyr::matches(get_term_regexp())),
        backgroundColor = DT::styleEqual(
          levels = c("?", "no", "canceled"),
          values = c("lightgray", "lightpink", "lightpink"),
          default = "lightgreen"
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
