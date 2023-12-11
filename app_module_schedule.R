# paths module server
module_schedule_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # data functions ===========

  # available terms
  get_terms <- reactive({
    validate(need(data$schedule$get_data(), "something went wrong retrieving the data"))
    get_available_terms(first_term = data$schedule$get_data()$term[1])
  })

  # selected terms
  get_selected_terms <- reactive({
    req(get_terms())
    req(input$first_term)
    req(input$last_term)
    get_terms() |> filter_terms(input$first_term, input$last_term)
  })

  # classes
  get_classes <- reactive({
    req(data$classes$get_data())
    data$classes$get_data() |>
      # not teaching placeholder
      dplyr::bind_rows(dplyr::tibble(class = "XXXX0000", title = "not teaching placeholder")) |>
      # future classes placeholder
      dplyr::bind_rows(dplyr::tibble(class = "XXXX9999", title = "future classes placeholder")) |>
      dplyr::mutate(
        inactive = !is.na(.data$inactive) & .data$inactive,
        class = forcats::as_factor(.data$class)
      )
  })

  # instructors
  get_instructors <- reactive({
    req(data$instructors$get_data())
    data$instructors$get_data() |>
      dplyr::mutate(inactive = !is.na(.data$inactive) & .data$inactive)
  })

  # not teaching
  get_not_teaching <- reactive({
    req(data$not_teaching$get_data())
    req(get_instructors())
    not_teaching <- data$not_teaching$get_data()
    if (nrow(missing <- not_teaching |> dplyr::anti_join(get_instructors(), by = "instructor_id")) > 0)
      sprintf("missing instructor_id in 'not_teaching': %s", paste(unique(missing$instructor_id), collapse = ", ")) |>
      log_error(ns = ns)
    if (nrow(wrong <- not_teaching |> dplyr::filter(!stringr::str_detect(.data$term, "(Spring|Summer|Fall) \\d{4}"))) > 0)
      sprintf("incorrect term formatting in 'not_teaching': %s", paste(unique(wrong$term), collapse = ", ")) |>
      log_error(ns = ns)
    not_teaching
  })

  # schedule
  get_schedule <- reactive({
    req(data$schedule$get_data())
    req(get_terms())
    req(get_instructors())
    req(get_classes())
    req(get_not_teaching())

    # safety checks
    schedule <- data$schedule$get_data() |>
      dplyr::mutate(
        canceled = !is.na(.data$canceled) & .data$canceled,
        deleted = !is.na(.data$deleted) & .data$deleted
      )
    if (nrow(missing <- schedule |> dplyr::anti_join(get_instructors(), by = "instructor_id")) > 0)
      sprintf("missing instructor_id in 'schedule': %s", paste(unique(missing$instructor_id), collapse = ", ")) |>
      log_error(ns = ns)

    if (nrow(missing <- schedule |> dplyr::anti_join(get_classes(), by = "class")) > 0)
      sprintf("missing class in 'classes': %s", paste(unique(missing$class), collapse = ", ")) |>
      log_error(ns = ns)

    if (nrow(wrong <- schedule |> dplyr::filter(!stringr::str_detect(.data$term, "(Spring|Summer|Fall) \\d{4}"))) > 0)
      sprintf("incorrect term formatting in 'schedule': %s", paste(unique(wrong$term), collapse = ", ")) |>
      log_error(ns = ns)

    # schedule with unique ID (for editing purposes)
    schedule |>
      dplyr::mutate(
        id = dplyr::row_number(),
        .before = 1L
      )
  })

  get_schedule_for_table <- reactive({
    req(get_schedule())
    req(get_not_teaching())
    req(get_instructors())
    req(get_classes())
    req(get_selected_terms())

    print(input$show_options)

    combine_schedule(
      schedule = get_schedule(),
      not_teaching = get_not_teaching(),
      instructors = get_instructors(),
      classes = get_classes(),
      selected_terms = get_selected_terms()
    ) |>
      dplyr::select(-"class", -"subtitle", -"instructor_id", -"title", -"credits", -"inactive")
  })

  # generate UI =====================

  # main GUI
  output$main <- renderUI({
    req(get_terms())
    log_info("loading terms")
    tagList(
      h2("Terms"),
      selectInput(
        ns("first_term"), "Select first term to display:",
        choices = c("Select first term" = "", as.character(get_terms())),
        selected = find_term(get_terms(), years_shift = -2)
      ),
      selectInput(
        ns("last_term"), "Select last term to display:",
        choices = c("Select first term" = "", as.character(get_terms())),
        selected = find_term(get_terms(), years_shift = +2)
      ),
      radioButtons(
        ns("show_options"), "Select information to display:",
        choices = c("Section #", "Day/Time", "Location", "Enrollment"),
        selected = "Day/Time"
      ),
      div(id = ns("schedule_box"),
          shinydashboard::box(
            title =
              span(
                "Schedule",
                # div(
                #   style = "position: absolute; right: 10px; top: 5px;",
                #   module_selector_table_deselect_all_button(ns("classes"), border = FALSE),
                #   actionButton(ns("check"), "Check", icon = icon("check"), style = "border: 0;") |>
                #     add_tooltip("Check selected classes for fulfillment of degree requirementes (degree audit)."),
                # )
              ), width = 12,
            status = "info", solidHeader = TRUE,
            module_selector_table_ui(ns("schedule"))
          )
      ) |> shinyjs::hidden()
    )
  })

  # check for selected terms
  observeEvent(
    get_selected_terms(),
    {
      shinyjs::hide("schedule_box")
      if (length(get_selected_terms()) == 0) {
        log_warning("invalid range", user_msg = "No terms fall into the selected terms range.")
      } else {
        log_info(
          "generating schedule table",
          user_msg = sprintf(
            "Loading schedule from %s to %s (%d terms )",
            get_selected_terms()[1], tail(get_selected_terms(), 1), length(get_selected_terms())
          )
        )

        shinyjs::show("schedule_box")
      }
    },
    ignoreNULL = FALSE
  )

  # schedule table ======

  schedule <- callModule(
    module_selector_table_server,
    "schedule",
    get_data = get_schedule_for_table,
    id_column = "id",
    # row grouping
    render_html = "full_title",
    extensions = c("RowGroup", "FixedHeader"),
    rowGroup = list(dataSrc = 0),
    columnDefs = list(
      # don't show row group column
      list(visible = FALSE, targets = 0),
      # render newlines correctly
      list(
        render = JS("function(data){return data.replace(/\\n/g, '<br>');}"),
        targets = c(1:10)
      )
    ),
    # view all & scrolling
    allow_view_all = TRUE,
    initial_page_length = -1,
    dom = "ft",
    scrollX = TRUE,
    fixedHeader = TRUE
  )


  # classes <- callModule(
  #   module_selector_table_server,
  #   "classes",
  #   get_data = get_classes,
  #   id_column = "id",
  #   available_columns = list(
  #     Category =
  #       ifelse(
  #         !is.na(category_description),
  #         sprintf("%s<br/><i>%s</i>",
  #                 htmltools::htmlEscape(category_info),
  #                 htmltools::htmlEscape(category_description)),
  #         htmltools::htmlEscape(category_info)
  #       ),
  #     Class = sprintf("%s(%s)", class, credits),
  #     Title = title,
  #     `Relevance for this path` = reason,
  #     `Spring 2023`, `Fall 2023`, `Spring 2024`, `Fall 2024`
  #   ),
  #   allow_view_all = TRUE,
  #   initial_page_length = -1,
  #   dom = "ft",
  #   selection = "multiple",
  #   # row grouping
  #   render_html = "Category",
  #   extensions = c("RowGroup", "FixedHeader"),
  #   rowGroup = list(dataSrc = 0),
  #   columnDefs = list(list(visible = FALSE, targets = 0)),
  #   # scrolling
  #   scrollX = TRUE,
  #   fixedHeader = TRUE,
  #   # formatting
  #   formatting_calls = list(
  #     list(
  #       func = DT::formatStyle, columns = c("Spring 2023", "Fall 2023", "Spring 2024", "Fall 2024"),
  #       backgroundColor = DT::styleEqual(
  #         get_classes_teaching_info_placeholder() |> names(),
  #         get_classes_teaching_info_placeholder() |> as.character()
  #       )
  #     )
  #   )
  # )
  #

}


# load UI dynamically
module_schedule_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
