# paths module server
module_schedule_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # constants
  data_err_prefix <- "Encountered database issue, the app may not function properly: "

  # reactive vaalues
  values <- reactiveValues(
    first_term = NULL,
    last_term = NULL,
    instructor_id = NULL,
    instructor = NULL
  )

  # data functions ===========

  # available terms
  get_terms <- reactive({
    validate(need(data$schedule$get_data(), "something went wrong retrieving the data"))
    get_available_terms(first_term = data$schedule$get_data()$term[1])
  })

  # monitor terms
  observeEvent(input$first_term, {
    if (is.null(values$first_term) || !identical(values$first_term, input$first_term))
      values$first_term <- input$first_term
  })
  observeEvent(input$last_term, {
    if (is.null(values$last_term) || !identical(values$last_term, input$last_term))
      values$last_term <- input$last_term
  })

  # monitor instructor
  observeEvent(input$instructor_id, {
    if (is.null(values$instructor_id) || !identical(values$instructor_id, input$instructor_id)) {
      log_debug(ns = ns, "new instructor_id selected: ", input$instructor_id)
      if (!is.na(input$instructor_id) && input$instructor_id != "NA") {
        values$instructor_id <- input$instructor_id
        values$instructor <- dplyr::filter(get_instructors(), .data$instructor_id == values$instructor_id)[1,]
      } else {
        values$instructor_id <- NULL
        values$instructor <- NULL
      }
    }
    shinyjs::toggle("add_leave", condition = is_dev_mode() || !is.null(values$instructor_id))
  })

  # selected terms
  get_selected_terms <- reactive({
    req(get_terms())
    req(values$first_term)
    req(values$last_term)
    terms <- get_terms() |> filter_terms(values$first_term, values$last_term)

    # include summers?
    if (!"Summers" %in% input$show_options) {
      terms <- terms |> drop_summers()
    }
    return(terms)
  })

  # classes
  get_classes <- reactive({
    req(data$classes$get_data())
    prepare_classes(data$classes$get_data())
  })

  # instructors
  get_instructors <- reactive({
    req(data$instructors$get_data())
    prepare_instructors(data$instructors$get_data())
  })

  # active geol instructors for dropdown
  get_active_geol_instructors <- reactive({
    req(get_instructors())
    get_instructors() |>
      dplyr::filter(!.data$inactive, .data$department == "GEOL") |>
      dplyr::arrange(.data$full_name) |>
      dplyr::select("full_name", "instructor_id") |>
      tibble::deframe()
  })

  # not teaching
  get_not_teaching <- reactive({
    req(data$not_teaching$get_data())
    req(get_instructors())
    not_teaching <- prepare_not_teaching(data$not_teaching$get_data())
    if (nrow(missing <- not_teaching |> dplyr::anti_join(get_instructors(), by = "instructor_id")) > 0) {
      msg <- sprintf("missing instructor_id in 'not_teaching': %s", paste(unique(missing$instructor_id), collapse = ", "))
      log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
    }
    if (nrow(wrong <- not_teaching |> dplyr::filter(!stringr::str_detect(.data$term, get_term_regexp()))) > 0) {
      msg <- sprintf("incorrect term formatting in 'not_teaching': %s", paste(unique(wrong$term), collapse = ", "))
      log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
    }
    not_teaching
  })

  # reasons
  get_reasons <- reactive({
    req(get_not_teaching())
    get_not_teaching()$reason |> unique() |> na.omit()
  })

  # schedule
  get_schedule <- reactive({
    req(data$schedule$get_data())
    req(get_terms())
    req(get_instructors())
    req(get_classes())
    req(get_not_teaching())

    # safety checks
    schedule <- prepare_schedule(data$schedule$get_data())

    # filter out canceled classes
    if (!"Canceled" %in% input$show_options) {
      schedule <- schedule |> dplyr::filter(!.data$canceled)
    }

    if (nrow(missing <- schedule |> dplyr::anti_join(get_instructors(), by = "instructor_id")) > 0) {
      msg <- sprintf("unrecognized `instructor_id` in `schedule`: '%s'", paste(unique(missing$instructor_id), collapse = "', '"))
      log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
    }

    if (nrow(missing <- schedule |> dplyr::anti_join(get_classes(), by = "class")) > 0) {
      msg <- sprintf("unrecognized `class` in `classes`: '%s'", paste(unique(missing$class), collapse = "', '"))
      log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
    }

    if (nrow(wrong <- schedule |> dplyr::filter(!stringr::str_detect(.data$term, get_term_regexp()))) > 0) {
      msg <- sprintf("incorrect term formatting in `schedule`: %s", paste(unique(wrong$term), collapse = ", "))
      log_error(ns = ns, msg, user_msg = paste0(data_err_prefix, msg))
    }

    # schedule with unique ID (for editing purposes)
    schedule |>
      dplyr::mutate(
        id = dplyr::row_number(),
        .before = 1L
      )
  })

  # schedule for data table
  get_schedule_for_table <- reactive({
    req(get_schedule())
    req(get_not_teaching())
    req(get_instructors())
    req(get_classes())
    req(get_selected_terms())

    # always reset visible columns to load new selection
    schedule$reset_visible_columns()

    # combine schedule information
    combine_schedule(
      schedule = get_schedule(),
      not_teaching = get_not_teaching(),
      instructors = get_instructors(),
      classes = get_classes(),
      available_terms = get_terms(),
      selected_terms = get_selected_terms(),
      recognized_reasons = get_reasons(),
      include_section_nr = "Section #" %in% input$show_options,
      include_day_time = "Day/Time" %in% input$show_options,
      include_location = "Location" %in% input$show_options,
      include_enrollment = "Enrollment" %in% input$show_options,
      instructor_schedule = values$instructor_id
    ) |>
      # select columns here to get proper order (instead of later, since the cols are dynamic depending on terms)
      dplyr::select(full_title, Instructor = instructor, dplyr::matches(get_term_regexp())) |>
      # escape html characters for safety and then create \n as <br>
      dplyr::mutate(dplyr::across(dplyr::where(is.character), function(x) {
        x |> htmltools::htmlEscape() |> stringr::str_replace_all("\\n", "<br>") |>
          # enable italics again
          stringr::str_replace_all(stringr::fixed("&lt;i&gt;"), "<i>") |>
          stringr::str_replace_all(stringr::fixed("&lt;/i&gt;"), "</i>")
      }))
  })

  # generate UI =====================

  # sidebar GUI
  output$sidebar <- renderUI({
    req(get_terms())
    req(get_active_geol_instructors())
    log_info("generating sidebar")
    terms <- get_terms() |> drop_summers()
    tagList(
      selectizeInput(
        ns("first_term"), "Select first term to display:",
        multiple = FALSE,
        choices = get_sorted_terms(terms),
        selected =
          isolate({
            if (!is.null(values$first_term) && values$first_term %in% as.character(terms)) values$first_term
            else get_current_term()
          })
      ),
      selectizeInput(
        ns("last_term"), "Select last term to display:",
        multiple = FALSE,
        choices = get_sorted_terms(terms),
        selected =
          isolate({
            if (!is.null(values$last_term) && values$last_term %in% as.character(terms)) values$last_term
            else find_term(get_terms(), years_shift = +2)
          })
      ),
      selectizeInput(
        ns("instructor_id"), "Select instructor to schedule:",
        multiple = FALSE,
        choices =
          c(
            list("Show all" = NA_character_),
            get_active_geol_instructors()
          ),
        selected =
          isolate({
            if (!is.null(values$instructor_id) && values$instructor_id %in% as.character(get_active_geol_instructors())) values$instructor_id
            else NA_character_
          })
      ),
      checkboxGroupInput(
        ns("show_options"), "Select information to display:",
        choices = c("Summers", "Canceled", "Section #", "Day/Time", "Location", "Enrollment"),
        selected = c("Day/Time", "Location", "Enrollment")
        #, inline = TRUE
      )
    )
  })

  # main GUI
  output$main <- renderUI({
    tagList(
      shinydashboard::box(
        title =
          span(
            "Schedule", textOutput(ns("instructor_name"), inline = TRUE),
            div(
              style = "position: absolute; right: 10px; top: 5px;",
              #module_selector_table_deselect_all_button(ns("classes"), border = FALSE),
              actionButton(ns("add_leave"), "Add Absence", icon = icon("rainbow"), style = "border: 0;") |>
                add_tooltip("Add information about a teaching absence (sabbatical, family leave, chair, directorship, etc.).") |>
                shinyjs::hidden(),
            )
          ), width = 12,
        status = "info", solidHeader = TRUE,
        module_selector_table_ui(ns("schedule")),
        footer = tagList(
          "Use the search bar in the upper right to filter the schedule (e.g. by instructor name, course number, etc.). Use the scrollbar to scroll through all results.", tags$br(),
          tags$strong("Note that classes that have not yet been confirmed by the UPA are shown in", tags$em("italics."))
        )
      )
    )
  })

  output$instructor_name <- renderText({
    if(!is.null(values$instructor_id))
      paste("for", values$instructor$full_name)
    else ""
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
    ignoreNULL = FALSE, priority = 100
  )

  # schedule table ======

  schedule <- callModule(
    module_selector_table_server,
    "schedule",
    get_data = get_schedule_for_table,
    id_column = "id",
    # row grouping
    render_html = dplyr::everything(),
    extensions = "RowGroup",
    rowGroup = list(dataSrc = 0),
    columnDefs = list(
      list(visible = FALSE, targets = 0)
    ),
    # view all & scrolling
    allow_view_all = TRUE,
    initial_page_length = -1,
    dom = "ft",
    ordering = FALSE,
    scrollX = TRUE,
    scrollY = "calc(100vh - 280px)", # account for size of header with the -x px
    # don't escape (since we made the columns safe and replaced \n with <br>)
    escape = FALSE,
    selection = list(mode = "single", target = "cell")
  )

  # formatting the schedule for easy visibility
  observeEvent(get_reasons(), {
    log_debug(ns = ns, "update formatting with reasons")
    schedule$change_formatting_calls(
      list(
        list(
          func = DT::formatStyle,
          columns_expr = expr(dplyr::matches(get_term_regexp())),
          backgroundColor = DT::styleEqual(
            levels = c("?", "no", "canceled", get_reasons()),
            values = c("lightgray", "lightpink", "lightpink", rep("lightyellow", length(get_reasons()))),
            default = "lightgreen"
          )
        )
      )
    )
  }, priority = 99)

  # add leave =========
  add_leave_dialog_inputs <- reactive({
    #req(values$instructor_id)
    log_debug(ns = ns, "generating leave dialog inputs")
    tagList(
      if (!is.null(values$instructor_id)) {
        h4(values$instructor$full_name)
      } else {
        # super user only?
        selectizeInput(
          ns("leave_instructor_id"), "Instructor",
          multiple = FALSE,
          choices = c("Select instructor" = "", get_active_geol_instructors())
        )
      },
      selectizeInput(
        ns("leave_term"), "Term",
        multiple = FALSE,
        choices = c("Select term" = "", get_sorted_terms(get_selected_terms()))
      ),
      selectizeInput(
        ns("leave_reason"), "Type",
        multiple = FALSE,
        choices = c("Enter/select type of absence" = "", get_reasons()),
        options = list(create = TRUE)
      )
    )
  })

  observe({
    shinyjs::toggleState(
      "save_leave",
      condition = (!is.null(values$instructor_id) || nchar(input$leave_instructor_id) > 0) &&
        nchar(input$leave_term) > 0 && nchar(input$leave_reason) > 0
    )
  })

  observeEvent(input$add_leave, {
    data$not_teaching$start_add()
    # modal dialog
    dlg <- modalDialog(
      size = "s",
      title = "Adding Teaching Absence",
      add_leave_dialog_inputs(),
      footer = tagList(
        actionButton(ns("save_leave"), "Add") |> shinyjs::disabled(),
        modalButton("Cancel")
      )
    )
    showModal(dlg)
  })

  # leave save =====
  observeEvent(input$save_leave, {
    # disable inputs while saving
    c("leave_instructor_id", "leave_term_id", "leave_reason", "save") |>
      purrr::walk(shinyjs::disable)

    # values
    values <- list(
      term = input$leave_term,
      instructor_id =
        if(!is.null(values$instructor_id)) values$instructor_id
        else input$leave_instructor_id,
      reason = input$leave_reason
    )

    # update data
    data$not_teaching$update(.list = values)

    # commit
    if (data$not_teaching$commit()) removeModal()
  })


  # add/edit dialog ========
  # add_edit_dialog_inputs <- reactive({
  #   req(get_inventory())
  #   log_debug(ns = ns, "generating dialog inputs")
  #   tagList(
  #     textInput(ns("name"), "Name"),
  #     selectInput(ns("status"), "Status", choices = get_inventory()$status |> levels()) |>
  #       add_tooltip("Indicate whether this item needs confirmation, is current, or is outdated"),
  #     selectizeInput(ns("vendor"), "Vendor", choices = get_inventory()$vendor |> levels(), options = list(create = TRUE)),
  #     textInput(ns("catalog_nr"), "Catalog #"),
  #     numericInput(ns("unit_price"), "Unit price", value = 0, min = 0),
  #     textInput(ns("unit_size"), "Unit size"),
  #     textInput(ns("url"), "URL"),
  #     textAreaInput(ns("details"), "Details", resize = "vertical")
  #   )
  # })

}


# load UI dynamically
module_schedule_sidebar <- function(id) {
  ns <- NS(id)
  uiOutput(ns("sidebar"))
}

module_schedule_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
