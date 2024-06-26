# data server ----
# @param data_sheet_id google sheets id with the app data
# @param data_folder_id google drive folder id for file storage
module_data_schedule_server <- function(input, output, session, data_sheet_id, gs_key_file) {

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    load_data = 1L,
    file_path = NULL,
    error = FALSE
  )

  # data tables =========
  get_local_file <- reactive({ values$file_path })
  report_error <- function() {
    values$error <- TRUE
  }

  # classes
  classes <- callModule(
    module_data_table_server, id = "classes",
    data_sheet_id = data_sheet_id, gs_key_file = gs_key_file, local_file = get_local_file,
    report_error = report_error,  reload_data = reload_data,
    sheet = "classes",
    cols = c("class", "title", "credits" = "integer", "inactive" = "logical")
  )

  # instructors
  instructors <- callModule(
    module_data_table_server, id = "instructors",
    data_sheet_id = data_sheet_id, gs_key_file = gs_key_file, local_file = get_local_file,
    report_error = report_error,  reload_data = reload_data,
    sheet = "instructors",
    cols = c("instructor_id", "last_name", "first_name", "department", "position", "inactive" = "logical")
  )

  # not teaching
  not_teaching <- callModule(
    module_data_table_server, id = "not_teaching",
    data_sheet_id = data_sheet_id, gs_key_file = gs_key_file, local_file = get_local_file,
    report_error = report_error,  reload_data = reload_data,
    sheet = "not_teaching",
    cols = c("term", "instructor_id", "reason", "created" = "datetime", "deleted" = "datetime")
  )

  # schedule
  schedule <- callModule(
    module_data_table_server, id = "schedule",
    data_sheet_id = data_sheet_id, gs_key_file = gs_key_file, local_file = get_local_file,
    report_error = report_error,  reload_data = reload_data,
    sheet = "schedule",
    cols = c("term", "class", "section", "subtitle", "instructor_id", "enrollment" = "integer", "enrollment_cap" = "integer", "building", "room", "days", "start_time", "end_time", "created" = "datetime", "updated" = "datetime", "deleted" = "datetime", "canceled" = "logical", "confirmed" = "logical", "notes")
  )

  # (re-) load data event =====
  reload_data <- function() {
    # enforce reload even for dev mode
    if (is_dev_mode() && file.exists(get_local_path()))
      file.remove(get_local_path())
    values$load_data <- values$load_data + 1L
  }
  observeEvent(input$reload, reload_data())

  # download data event =========
  observeEvent(values$load_data, {

    # lock when this cascade starts
    lock_app()
    log_info(ns = ns, "requesting google spreadsheet data", user_msg = "Fetching data")
    values$file_path <-
      tryCatch({
        # don't download from scratch every time if in development mode
        if (is_dev_mode() && file.exists(get_local_path())) {
          file_path <- get_local_path()
          log_debug(ns = ns, "in DEV mode, using local data file")
        } else
          file_path <- download_gs(data_sheet_id, gs_key_file = gs_key_file)

        # save locally if in dev mode
        if (is_dev_mode() && !file.exists(get_local_path())) {
          file.copy(file_path, get_local_path())
          log_debug(ns = ns, "in DEV mode, saving downloaded data to local file")
        }
        file_path
      },
      error = function(e) {
        log_error(ns = ns, "download failed", user_msg = "Data loading error", error = e)
        values$error <- TRUE
        NULL
      })
  }, priority = 10L)

  # read data event =========
  observeEvent(values$load_data, {
    req(!values$error)
    log_debug(ns = ns, "file path: ", values$file_path)
    log_info(ns = ns, "loading data from xlsx file", user_msg = "Loading data")

    # reading data sheets
    tryCatch({
      classes$read_data(ignore_other_cols = TRUE)
      instructors$read_data(ignore_other_cols = TRUE)
      not_teaching$read_data(ignore_other_cols = TRUE)
      schedule$read_data(ignore_other_cols = TRUE)
    },
    error = function(e) {
      log_error(ns = ns, "data read failed", user_msg = "Data reading error", error = e)
      values$error <- TRUE
    })
  }, priority = 9L)

  # always authenticate
  is_authenticated <- reactive({ TRUE })

    # lock/unlock events ====
  observe({
    # triggers
    values$load_data
    values$locked
    values$error

    # unlock if authenticated
    isolate({
      if (is_authenticated() && values$locked && !values$error) {
        values$locked <- FALSE
      } else if (!values$locked) {
        unlock_app()
      } else {
        # reset
        classes$reset()
        instructors$reset()
        not_teaching$reset()
        schedule$reset()
        #shinyjs::hide("menu", asis = TRUE)
        log_info(ns = ns, "app stays locked")
      }
    })
  }, priority = 1L)

  lock_app <- function() {
    log_info(ns = ns, "locking app")
    values$locked <- TRUE
    values$error <- FALSE
  }

  unlock_app <- function() {
    log_info(ns = ns, "unlocking app")
    shinyjs::show("menu", asis = TRUE)
    log_success(ns = ns, "loading all done", user_msg = "Complete")
  }

  #  return function ====
  list(
    reload_data = reload_data,
    is_authenticated = is_authenticated,
    classes = classes,
    instructors = instructors,
    not_teaching = not_teaching,
    schedule = schedule
  )
}

# data ui components - reload button ------
module_data_schedule_reload_button <- function(id) {
  ns <- NS(id)
  actionButton(ns("reload"), "Reload", icon = icon("rotate")) |>
    add_tooltip("Reload all data")
}
