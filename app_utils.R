# general utilities ====

`%then%` <- function(a, b) {
  if (is.null(a)) return(b)
  out <- a
  out[sapply(a, is.null) | sapply(a, is.na)] <- b
  return(out)
}

omit_duplicates <- function(x) {
  x[!duplicated(x)]
}

# helper function to create factors with levels in the order of data appearnace
# this is a simpler implementation of forcats::fct_inorder (no other need for forcats dependency)
factor_in_order <- function(x) {
  if (!is.factor(x)) x <- as.factor(x)
  idx <- as.integer(x)[!duplicated(x)]
  idx <- idx[!is.na(idx)]
  return(factor(x, levels = levels(x)[idx]))
}

is_dev_mode <- function() {
  return(identical(Sys.getenv("GEOAPPS_DEV"), "ON"))
}

# logging utilities ====
# note: fatal and trace are overkill for this app

# call the other log functions instead for clarity in the code
# @param ... toaster parameters
log_any <- function(msg, log_fun, toaster_fun, ns = NULL, toaster = NULL, ...) {
  ns <- if(!is.null(ns)) paste0("[", ns(NULL), "] ") else ""
  if(!is.null(toaster)) {
    log_fun(paste0(ns, msg, " [GUI msg: '", toaster, "']", collapse = ""))
    toaster_fun(toaster, position = "bottom-right", newestOnTop = TRUE, ...)
  } else {
    log_fun(paste0(ns, msg, collapse = ""))
  }
}

log_error <- function(..., ns = NULL, user_msg = NULL, error = NULL) {

  error_msg <-
    if (!is.null(error))
      gsub("\\n", "<br>", cli::ansi_html(error)) |> paste(collapse = "<br>")
    else ""

  issue_title <- sprintf(
    "Error in %s: %s",
    if (getPackageName() != ".GlobalEnv") packageVersion(getPackageName()) else "app", user_msg
  )
  issue_url <- sprintf(
    "https://github.com/KopfLab/geoapps/issues/new?title=%s&body=%s",
    URLencode(issue_title), URLencode(HTML(error_msg))
  )

  error_screen <- modalDialog(
    title =
      span(style = "color: red;",
        h2(user_msg, style = "color: red;"),
        h4("Please try again. If the issue persists, please",
           tags$a("report this error", href = issue_url, target = "_blank"),
        )
      ),
    if (nchar(error_msg) > 0) p(style = "color: red;", HTML(error_msg))
  )

  log_any(
    msg = paste0(..., if(!is.null(error)) paste0(": ", error$message), collapse = ""), ns = ns,
    log_fun = rlog::log_error,
    toaster_fun = shinytoastr::toastr_error,
    toaster = user_msg,
    title = "Error",
    timeOut = 10000,
    closeButton = TRUE
  )

  showModal(error_screen)
}

log_warning <- function(..., ns = NULL, user_msg = NULL, warning = NULL) {
  log_any(
    msg = paste0(..., collapse = ""), ns = ns,
    log_fun = rlog::log_warn,
    toaster_fun = shinytoastr::toastr_warning,
    toaster = if (!is.null(warning)) cli::ansi_strip(warning) else user_msg,
    title = if (!is.null(warning)) user_msg else NULL,
    progressBar = TRUE,
    extendedTimeOut = 3000
  )
}

log_info <- function(..., ns = NULL, user_msg = NULL) {
  log_any(
    msg = paste0(..., collapse = ""), ns = ns,
    log_fun = rlog::log_info,
    toaster_fun = shinytoastr::toastr_info,
    toaster = user_msg
  )
}

log_success <- function(..., ns = NULL, user_msg = NULL) {
  log_any(
    msg = paste0(..., collapse = ""), ns = ns,
    log_fun = rlog::log_info,
    toaster_fun = shinytoastr::toastr_success,
    toaster = user_msg
  )
}

log_debug <- function(..., ns = NULL) {
  log_any(msg = paste0(..., collapse = ""), ns = ns, log_fun = rlog::log_debug)
}

# ui elements ======

# convenience function for adding spaces (not the most elegant way but works)
spaces <- function(n) {
  HTML(rep("&nbsp;", n))
}

# adding an inline UI item
inline <- function(...) {
  div(style = "display: inline-block;", ...)
}

# convenience function to add tooltip
add_tooltip <- function(widget, message, size = "medium") {
  prompter::add_prompt(widget, message = message, size = size)
}
