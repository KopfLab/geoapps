# user interface
paths_ui <- function() {

  # constants
  app_title <- "ERTH Paths"
  app_title_width <- 160
  app_color <- "green"
  spinner_color <- "#2c3b41"
  app_box_default <- "#2c3b41"

  # options
  options(spinner.color = spinner_color)

  # header
  header <- shinydashboard::dashboardHeader(
    title = app_title, titleWidth = app_title_width,
    tags$li(class = "dropdown", tags$a(href = "https://apps.kopflab.org/app_direct/geoschedule", icon("link"), "Switch to ERTH Schedule App"))
  )

  # sidebar
  sidebar <-
    shinydashboard::dashboardSidebar(
      collapsed = FALSE, disable = FALSE, width = app_title_width,
      shinyjs::useShinyjs(), # enable shinyjs
      shinytoastr::useToastr(), # enable toaster
      prompter::use_prompt(), # enable prompter
      tags$head( # css headers
        # custom
        tags$style(
          type="text/css",
          HTML(paste(
            # body top padding
            ".box-body {padding-top: 5px; padding-bottom: 0px}",
            # custom background box
            sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}", app_box_default, app_box_default),
            sprintf(".box.box-solid.box-info{border:1px solid %s;}", app_box_default),
            sep="\n"))
        )
      ),
      module_data_paths_reload_button("paths_data"),
      module_path_sidebar("paths"),
      if (shiny::in_devmode()) actionButton("dev_mode_toggle", "Toggle Dev Mode")
    )

  # body
  body <- shinydashboard::dashboardBody(
    module_paths_ui("paths")
  )

  # dashboard page
  shinydashboard::dashboardPage(
    title = app_title, # tab title
    skin = app_color, # styling
    header = header,
    sidebar = sidebar,
    body = body
  )

}
