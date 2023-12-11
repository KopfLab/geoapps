# data table server ----
# @param local_file (usually reactive) function retrieving local file
# @param report_error (usually reactive) function reporting an error
module_data_table_server <- function(
    input, output, session, data_sheet_id, gs_key_file,
    local_file, report_error, reload_data,
    sheet, cols = dplyr::everything()) {

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    load_data = 1L,
    data = NULL,
    data_changes = NULL,
    hash = NULL,
    edit_id = NULL,
    edit_idx = NULL
  )

  # reset ========

  reset <- function() {
    values$data <- NULL
    values$data_changes <- NULL
    values$hash <- NULL
    values$edit_id <- NULL
    values$edit_idx <- NULL
  }

  # read data ==========

  read_data <- function(timezone = Sys.timezone(), ignore_other_cols = FALSE) {

    # info
    log_debug(ns = ns, "reading data from xlsx file for sheet '", sheet, "'")
    validate(need(file.exists(local_file()), "something went wrong retrieving the data"))
    sheets <- readxl::excel_sheets(local_file())

    # check if sheet exists
    if (!sheet %in% sheets) {
      log_error(ns = ns, user_msg = "Cannot read data", error = sprintf("'%s' tab doesn't exist", sheet))
      report_error()
      return(NULL)
    }

    # try to read data
    data <- tryCatch({
      data <- read_excel_sheet(local_file(), sheet = sheet, cols = {{ cols }}, timezone = timezone, ignore_other_cols = ignore_other_cols)
      data
    },
    warning = function(w) {
      log_error(ns = ns, "data reading failed", user_msg = sprintf("Data warning in table '%s'", sheet), error = w)
      report_error()
      NULL
    },
    error = function(e) {
      log_error(ns = ns, "data reading failed", user_msg = sprintf("Missing data in table '%s'", sheet), error = e)
      report_error()
      NULL
    })

    # hash
    hash <- data |> hash_data()
    if (!identical(hash, isolate(values$hash))) {
      log_info(ns = ns, "found new '", sheet, "' data")
      values$data <- data
      values$hash <- hash
      values$data_changed <- values$data
    }
  }

  # hash data
  hash_data <- function(data) {
    if (is.null(data))
      return(digest::digest(data))

    # remove the other columns
    data |>
      dplyr::select(-".add", -".update", -".delete") |>
      digest::digest()
  }

  # get data ==========

  get_data <- reactive({
    validate(need(values$data, "something went wrong retrieving the data"))
    return(values$data)
  })

  get_data_changed <- reactive({
    validate(need(values$data_changed, "something went wrong retrieving the data"))
    return(values$data_changed)
  })

  # data modifications =========

  # returns whether add was started (or is resuming)
  start_add <- function() {
    log_debug(ns = ns, "starting add")
    values$edit_id <- NULL
    values$edit_idx <- NULL
  }

  start_edit <- function(id, idx = get_index_by_id(values$data, id)) {
    log_debug(ns = ns, "starting edit for ",
              sprintf("id '%s' / idx %d", id, idx) |>
                paste(collapse = ", "))
    values$edit_id <- id
    values$edit_idx <- idx
  }

  is_add <- function() {
    return(is_empty(values$edit_idx))
  }

  update <- function(...) {
    if (!is_add()) {
      # edit
      values$data_changed <- values$data_changed |>
        update_data(.idx = values$edit_idx, ...)
    } else {
      # add
      values$data_changed <- values$data_changed |>
        add_data(...)
    }
  }

  # data checks =========

  # check whether specific value has changed
  has_value_changed <- function(idx = values$edit_idx, column) {
    if (!is_empty(idx)) {
      # check
      old_value <- values$data[idx, column]
      new_value <- values$data_changed[idx, column]
      return(!is_value_identical(old_value, new_value))
    }
    # when in add mode always has new value
    return(TRUE)
  }

  # check whether there are any changes overall
  has_changes <- function() {
    n_changes <- values$data_changed |>
      dplyr::filter(.add | .update | .delete) |>
      nrow()
    return(n_changes > 0L)
  }

  commit <- function() {

    if (!has_changes()) {
      # no changes
      log_info(ns = ns, "no changes, nothing to update")
      return(TRUE)
    }

    # has changes
    log_info(ns = ns, "writing data to spreadsheet", user_msg = "Saving data")

    # try to save data
    success <-
      tryCatch({
        commit_changes_to_gs(
          df = values$data_changed,
          gs_id = data_sheet_id,
          gs_sheet = sheet,
          gs_key_file = gs_key_file
        )
        TRUE
      },
      warning = function(w) {
        log_error(ns = ns, "data writing failed", user_msg = "Encountered warning during data saving", error = w)
        # unuscessful commit
        FALSE
      },
      error = function(e) {
        log_error(ns = ns, "data writing failed", user_msg = "Encountered error during data saving", error = e)
        # unuscessful commit
        FALSE
      })

    # success
    if (success) {

      # enfore reload even for dev mode
      if (is_dev_mode() && file.exists("local_data.xlsx"))
        file.remove("local_data.xlsx")

      if (!any(values$data_changed$.add)) {
        # nothing new added, just update values$data
        values$data_changed <-
          values$data_changed |>
          dplyr::filter(!.delete) |>
          dplyr::mutate(.add = FALSE, .update = FALSE)
        values$data <- values$data_changed
        values$hash <- values$data |> hash_data()
      } else {
        # something was added - reload data from server
        # to get new IDs --> reloads everything
        reload_data()
      }
      # succcesful commit
      log_success(ns = ns, "data writing complete", user_msg = "Complete")
    } else{
      # reset data changed
      values$data_changed <- value$data
    }
    return(success)
  }

  # module functions ======
  list(
    reset = reset,
    read_data = read_data,
    get_data = get_data,
    get_data_changed = get_data_changed,
    start_add = start_add,
    start_edit = start_edit,
    is_add = is_add,
    update = update,
    has_value_changed = has_value_changed,
    has_changes = has_changes,
    commit = commit
  )

}
