# data server ----
# @param data_sheet_id google sheets id with the app data
# @param data_folder_id google drive folder id for file storage
module_data_paths_server <- function(input, output, session, data_sheet_id, gs_key_file) {

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    load_data = 1L,
    file_path = NULL,
    error = FALSE
  )
  local_path <- "local_data_paths.xlsx"

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
    cols = c("class", "title", "credits" = "integer", "Spring 2023", "Fall 2023", "Spring 2024", "Fall 2024")
  )

  # path recommendations
  paths <- callModule(
    module_data_table_server, id = "paths",
    data_sheet_id = data_sheet_id, gs_key_file = gs_key_file, local_file = get_local_file,
    report_error = report_error,  reload_data = reload_data,
    sheet = "paths",
    cols = c("path", "category", "category_description", "rec_min", "class", "reason")
  )

  # (re-) load data event =====
  reload_data <- function() {
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
        if (is_dev_mode() && file.exists(local_path)) {
          file_path <- local_path
          log_debug(ns = ns, "in DEV mode, using local data file")
        } else
          file_path <- download_gs(data_sheet_id, gs_key_file = gs_key_file)

        # save locally if in dev mode
        if (is_dev_mode() && !file.exists(local_path)) {
          file.copy(file_path, local_path)
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
      paths$read_data(ignore_other_cols = TRUE)
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
        paths$reset()
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
    paths = paths
  )
}

# data ui components - reload button ------
module_data_paths_reload_button <- function(id) {
  ns <- NS(id)
  actionButton(ns("reload"), "Reload", icon = icon("rotate")) |>
    add_tooltip("Reload all data")
}
