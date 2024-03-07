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
    instructor = NULL,
    edit = list()
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
    shinyjs::toggle("delete_leave", condition = is_dev_mode() || !is.null(values$instructor_id))
    shinyjs::toggle("add_class", condition = is_dev_mode() || !is.null(values$instructor_id))
    shinyjs::toggle("edit_class", condition = is_dev_mode() || !is.null(values$instructor_id))
    shinyjs::toggle("delete_class", condition = is_dev_mode() || !is.null(values$instructor_id))
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

  # future terms
  get_future_terms <- reactive({
    req(get_selected_terms())
    terms <- get_selected_terms()
    print(
      get_selected_terms() |>
        filter_terms(start_term = get_current_term(), inclusive = FALSE)
    )
    # start in term after current
    get_selected_terms() |>
      filter_terms(start_term = get_current_term(), inclusive = FALSE)
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

    # return schedule
    return(schedule)
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
    schedule_table <-
      combine_schedule(
        schedule = get_schedule(),
        not_teaching = get_not_teaching(),
        instructors = get_instructors(),
        classes = get_classes(),
        selected_terms = get_selected_terms(),
        recognized_reasons = get_reasons(),
        include_section_nr = "Section #" %in% input$show_options,
        include_day_time = "Day/Time" %in% input$show_options,
        include_location = "Location" %in% input$show_options,
        include_enrollment = "Enrollment" %in% input$show_options,
        instructor_schedule = values$instructor_id
      ) |>
      # select columns here to get proper order (instead of later, since the cols are dynamic depending on terms)
      dplyr::select("row", "instructor_id", "class", "full_title", Instructor = "instructor", dplyr::matches(get_term_regexp())) |>
      # escape html characters for safety and then create \n as <br>
      dplyr::mutate(dplyr::across(dplyr::where(is.character), function(x) {
        x |> htmltools::htmlEscape() |> stringr::str_replace_all("\\n", "<br>") |>
          # enable italics again
          stringr::str_replace_all(stringr::fixed("&lt;i&gt;"), "<i>") |>
          stringr::str_replace_all(stringr::fixed("&lt;/i&gt;"), "</i>") |>
          stringr::str_replace_all(stringr::fixed("&lt;u&gt;"), "<u>") |>
          stringr::str_replace_all(stringr::fixed("&lt;/u&gt;"), "</u>")
      }))

    # return schedule table
    return(schedule_table)
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
            if (!is.null(values$first_term) && values$first_term %in% terms) values$first_term
            else get_current_term()
          })
      ),
      selectizeInput(
        ns("last_term"), "Select last term to display:",
        multiple = FALSE,
        choices = get_sorted_terms(terms),
        selected =
          isolate({
            if (!is.null(values$last_term) && values$last_term %in% terms) values$last_term
            else get_past_or_future_term(years_shift = +2)
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
              # add class
              actionButton(ns("add_class"), "Schedule Class", icon = icon("person-chalkboard"), style = "border: 0;") |>
                shinyjs::hidden(),
              # edit class
              actionButton(ns("edit_class"), "Edit Schedule", icon = icon("pen-to-square"), style = "border: 0;") |>
                shinyjs::disabled() |> shinyjs::hidden(),
              # delete class
              actionButton(ns("delete_class"), "Unschedule", icon = icon("xmark"), style = "border: 0;") |>
                shinyjs::disabled() |> shinyjs::hidden(),
              # add absence
              actionButton(ns("add_leave"), "Add Absence", icon = icon("plane"), style = "border: 0;") |>
                add_tooltip("Add information about a teaching absence (sabbatical, family leave, chair, directorship, etc.).") |>
                shinyjs::hidden(),
              # delete absence
              actionButton(ns("delete_leave"), "Delete Absence", icon = icon("plane-slash"), style = "border: 0;") |>
                add_tooltip("Delete a teaching absence.") |>
                shinyjs::disabled() |> shinyjs::hidden()
            )
          ), width = 12,
        status = "info", solidHeader = TRUE,
        module_selector_table_ui(ns("schedule")),
        footer = tagList(
          "Use the search bar in the upper right to filter the schedule (e.g. by course number or course name). ",
          "Use the scrollbar to scroll through all results.", "Modifications are only possible once a specific instructor is selected in the left menu bar, and only for future semesters (highlighted in ",
          tags$span(style = "background-color: yellow;", "yellow"),
          ") and classes that have not yet been confirmed by the UPA (shown in ",
          HTML("<i><u>underlined italics</u></i>"), "). ",
          "To modify anything else, please contact our UPA at ", tags$a(href = "mailto:geoupa@colorado.edu", target = "_new", "geoupa@colorado.edu"), "."
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

  # generate table ======

  schedule <- callModule(
    module_selector_table_server,
    "schedule",
    get_data = get_schedule_for_table,
    id_column = "row",
    # row grouping
    render_html = dplyr::everything(),
    extensions = "RowGroup",
    rowGroup = list(dataSrc = 3),
    columnDefs = list(
      list(visible = FALSE, targets = 0:3)
    ),
    # view all & scrolling
    allow_view_all = TRUE,
    initial_page_length = -1,
    dom = "ft",
    ordering = FALSE,
    scrollX = TRUE,
    scrollY = "calc(100vh - 300px)", # account for size of header with the -x px
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

  # formatting the headers based on which terms are selected
  observeEvent(get_selected_terms(), {
    log_debug(ns = ns, "updating header backgrounds")
    future_idx <- is_term_after(get_selected_terms(), after = get_current_term()) |> which()
    header_calls <- sprintf("$(thead).closest('thead').find('th').eq(%s).css('background-color', 'yellow');", future_idx)
    schedule$update_options(
      headerCallback = DT::JS(
        sprintf("function( thead, data, start, end, display ) { %s }", paste(header_calls, collapse = " "))
      )
    )
  }, priority = 98)

  # formatting the headers depending on what terms are

  # process record selection ================

  observeEvent(schedule$get_selected_cells(), {

    # disable edit buttons
    shinyjs::disable("delete_leave")
    shinyjs::disable("add_class")
    shinyjs::disable("edit_class")
    shinyjs::disable("delete_class")

    # make sure something is selected
    req(schedule$get_selected_ids())
    log_debug(ns = ns, "new cell selected")

    # build the edit information
    values$edit <- list(
      instructor_id = schedule$get_selected_items()$instructor_id,
      instructor = schedule$get_selected_items()$Instructor,
      class = as.character(schedule$get_selected_items()$class),
      term = as.character(schedule$get_selected_cells()),
      info = schedule$get_selected_items()[[as.character(schedule$get_selected_cells())]]
    )

    # check if it is a future record and if it is the instructor who is selected
    # (unless in dev mode which could become an admin mode feature)
    if (
      is_term_after(values$edit$term) &&
      ((is.null(values$instructor_id) && is_dev_mode()) || identical(values$instructor_id, values$edit$instructor_id))
    ) {

      # check which kind of record it is
      if (values$edit$info %in% c("no", "?")) {
        # unscheduled class
        log_debug(ns = ns, "selected an unscheduled class")
        shinyjs::enable("add_class")
      } else if (values$edit$info %in% get_reasons()) {
        # absence record
        log_debug(ns = ns, "selected a teaching absence cell")
        shinyjs::enable("delete_leave")
      } else if (stringr::str_detect(values$edit$info, "^<i>")) {
        # selected an unconfirmed scheduled class
        log_debug(ns = ns, "selected an editable class")
        shinyjs::enable("edit_class")
        shinyjs::enable("delete_class")
      }

    }
  })

  # add leave dialog =========
  add_leave_dialog_inputs <- reactive({
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
        choices = c("Select term" = "", get_sorted_terms(get_future_terms())),
        selected = if(!is.null(values$edit$term)) values$edit$term else 1
      ),
      selectizeInput(
        ns("leave_reason"), "Type",
        multiple = FALSE,
        choices = c("Enter/select type of absence" = "", get_reasons()),
        options = list(create = TRUE)
      )
    )
  })

  # add leave ==============
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

  observe({
    shinyjs::toggleState(
      "save_leave",
      condition = (!is.null(values$instructor_id) || nchar(input$leave_instructor_id) > 0) &&
        nchar(input$leave_term) > 0 && nchar(input$leave_reason) > 0
    )
  })

  # save leave =====
  observeEvent(input$save_leave, {
    # disable inputs while saving
    c("leave_instructor_id", "leave_term", "leave_reason", "save_leave") |>
      purrr::walk(shinyjs::disable)

    # try to save
    tryCatch({
      # info
      log_info("adding teaching absence", user_msg = "Adding teaching absence...")

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
    },
    error = function(e) {
      log_error(ns = ns, "failed", user_msg = "Data saving error", error = e)
    })
  })

  # delete leave ============
  observeEvent(input$delete_leave, {
    showModal(
      modalDialog(
        title="Delete teaching absence",
        h4(
          sprintf("Are you sure you want to delete the %s related teaching absence for %s in %s?",
                  values$edit$info, values$edit$instructor, values$edit$term)
        ),
        footer = tagList(actionButton(ns("delete_leave_confirm"), "Delete"), modalButton("Cancel"))
      )
    )

  })

  observeEvent(input$delete_leave_confirm, {
    # pull out record
    record <- get_not_teaching() |>
      dplyr::filter(.data$instructor_id == !!values$edit$instructor_id, .data$term == !!values$edit$term)

    # try to delete
    tryCatch({
      # is there a record?
      if (nrow(record) == 0L)
        stop("could not find teaching absence to delete")

      # info
      log_info("deleting teaching absence", user_msg = "Removing teaching absence...")

      # flag for delete
      data$not_teaching$start_edit(idx = record$idx)
      data$not_teaching$update(deleted = TRUE)

      # commit
      if (data$not_teaching$commit()) removeModal()
    },
    error = function(e) {
      log_error(ns = ns, "failed", user_msg = "Data saving error", error = e)
    })
  })

  # add class dialog =========
  add_class_dialog_inputs <- reactive({
    req(schedule$get_selected_ids())
    req(values$edit)
    log_debug(ns = ns, "generating class dialog inputs")
    tagList(
      if (!is.null(values$instructor_id)) {
        h4(values$instructor$full_name)
      } else {
        # super user only?
        selectizeInput(
          ns("class_instructor_id"), "Instructor",
          multiple = FALSE,
          choices = c("Select instructor" = "", get_active_geol_instructors()),
          selected = values$edit$instructor_id
        )
      },
      selectizeInput(
        ns("class_term"), "Term",
        multiple = FALSE,
        choices = c("Select term" = "", get_sorted_terms(get_future_terms())),
        selected = values$edit$term
      ),
      selectizeInput(
        ns("class_id"), "Class",
        multiple = FALSE,
        choices = c("Select class" = "", levels(get_classes()$class)),
        selected = values$edit$class
      )
    )
  })

  # add class =========
  observeEvent(input$add_class, {
    data$schedule$start_add()
    # modal dialog
    dlg <- modalDialog(
      size = "s",
      title = "Schedule Class",
      add_class_dialog_inputs(),
      footer = tagList(
        actionButton(ns("save_class"), "Add") |> shinyjs::disabled(),
        modalButton("Cancel")
      )
    )
    showModal(dlg)
  })

  observe({
    shinyjs::toggleState(
      "save_class",
      condition = (!is.null(values$instructor_id) || nchar(input$class_instructor_id) > 0) &&
        nchar(input$class_term) > 0 && nchar(input$class_id) > 0
    )
  })

  # save class =====
  observeEvent(input$save_class, {
    # disable inputs while saving
    c("class_instructor_id", "class_term", "class_id", "save_class") |>
      purrr::walk(shinyjs::disable)

    # try to save
    tryCatch({
      # info
      log_info("adding class to schedule", user_msg = "Adding class to schedule...")

      # values
      values <- list(
        term = input$class_term,
        instructor_id =
          if(!is.null(values$instructor_id)) values$instructor_id
        else input$class_instructor_id,
        class = input$class_id,
        confirmed = FALSE
      )

      # update data
      data$schedule$update(.list = values)

      # commit
      if (data$schedule$commit()) removeModal()
    },
    error = function(e) {
      log_error(ns = ns, "failed", user_msg = "Data saving error", error = e)
    })
  })

  # edit class ====
  observeEvent(input$edit_class, {
    showModal(
      modalDialog(
        title="Edit scheduled class",
        h4("Sorry, this functionality is not yet implemented. If you really need to change this class, please delete the existing record and add it anew."),
        footer = tagList(modalButton("Cancel"))
      )
    )

  })

  # delete class ====
  observeEvent(input$delete_class, {
    showModal(
      modalDialog(
        title="Unschedule class",
        h4(
          sprintf("Are you sure you want to remove all sections of %s from the teaching schedule of %s for %s?",
                  values$edit$class, values$edit$instructor, values$edit$term)
        ),
        footer = tagList(actionButton(ns("delete_class_confirm"), "Delete"), modalButton("Cancel"))
      )
    )

  })

  observeEvent(input$delete_class_confirm, {
    # pull out record
    record <- get_schedule() |>
      dplyr::filter(
        .data$class == !!values$edit$class,
        .data$instructor_id == !!values$edit$instructor_id,
        .data$term == !!values$edit$term
      )

    # try to delete
    tryCatch({
      # is there a record?
      if (nrow(record) == 0L)
        stop("could not find schedule record to delete")

      # info
      log_info(
        "deleting schedule record(s)",
        user_msg = sprintf("Unscheduling %d section%s...", nrow(record), if(nrow(record) > 0) "s")
      )

      # update
      data$schedule$start_edit(idx = record$idx)
      data$schedule$update(deleted = TRUE)

      # commit
      if (data$schedule$commit()) removeModal()
    },
    error = function(e) {
      log_error(ns = ns, "failed", user_msg = "Data saving error", error = e)
    })
  })



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
