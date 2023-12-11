# user interface
paths_ui <- function() {

  # constants
  app_title <- "Geopaths"
  app_title_width <- 150
  app_color <- "green"
  spinner_color <- "#2c3b41"
  app_box_default <- "#2c3b41"

  # options
  options(spinner.color = spinner_color)

  # header
  header <- shinydashboard::dashboardHeader(title = app_title, titleWidth = app_title_width)

  # sidebar
  sidebar <-
    shinydashboard::dashboardSidebar(
      collapsed = TRUE, disable = TRUE,
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
      )
    )

  # body
  body <- shinydashboard::dashboardBody(
    module_data_paths_reload_button("paths_data"),
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

# user interface
schedule_ui <- function() {

  # constants
  app_title <- "Geoschedule"
  app_title_width <- 150
  app_color <- "yellow"
  spinner_color <- "#2c3b41"
  app_box_default <- "#2c3b41"

  # options
  options(spinner.color = spinner_color)

  # header
  header <- shinydashboard::dashboardHeader(title = app_title, titleWidth = app_title_width)

  # sidebar
  sidebar <-
    shinydashboard::dashboardSidebar(
      collapsed = TRUE, disable = TRUE,
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
      )
    )

  # body
  body <- shinydashboard::dashboardBody(
    module_data_schedule_reload_button("schedule_data"),
    module_schedule_ui("schedule")
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

