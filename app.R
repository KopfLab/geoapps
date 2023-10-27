library(rlang)
library(shiny)
library(DT)

source("app_ui.R")
source("app_server.R")
source("app_utils.R")
source("app_data.R")
source("app_module_data_table.R")
source("app_module_paths.R")
source("app_module_selector_table.R")

source("logic_gdrive.R")
source("logic_paths.R")

source("credentials.R")


# server <- shinyServer(function(input, output, session) {
#
#   data <- readxl::read_excel("classes.xlsx", sheet = "classes")
#   semesters <- data |> names() |> stringr::str_extract("^[SF]\\d+")
#
#   get_classes <- reactive({
#     data |>
#       dplyr::mutate(
#         dplyr::across(dplyr::matches("^[SF]\\d+"), ~dplyr::case_when(.x == "y" ~ "yes", .x == "n" ~ "no", TRUE ~ "?"))
#       )
#   })
#
#   classes <- callModule(
#     module_selector_table_server,
#     "classes_table",
#     get_data = get_classes,
#     id_column = "class",
#     available_columns = list(
#       Class = class,
#       Name = title,
#       Credits = credits,
#       F2024 = F2024,
#       S2024 = S2024,
#       F2023 = F2023
#       # Status = status,
#       # Vendor = vendor,
#       # `Catalog #` =
#       #   ifelse(
#       #     !is.na(url) & nchar(url) > 0,
#       #     sprintf(
#       #       "<a href = '%s' target = '_blank'>%s</a>",
#       #       gsub("^(http(s?)://)?", "https://", url), htmltools::htmlEscape(catalog_nr)
#       #     ),
#       #     htmltools::htmlEscape(catalog_nr)
#       #   ),
#       # `Unit price` = unit_price,
#       # `Unit size` = unit_size,
#       # `Last price update` = as.character(last_price_update),
#       # `Added by` = paste(first_name %then% "", last_name %then% ""),
#       # `Add timestamp` = as.character(added_on),
#       # `Details` = details
#     ),
#     #visible_columns = 1:7, # through price update
#     allow_view_all = TRUE,
#     initial_page_length = -1,
#     dom = "ft",
#     selection = list(target = "cell"),
#     editable = list(target = "cell", disable = list(columns = 4:6)),
#     # scrolling - messes with the header row somehow
#     #extensions = c("FixedColumns"),
#     #scrollX = TRUE,
#     #fixedColumns = list(leftColumns = 2),
#     formatting_calls = list(
#       list(
#         func = DT::formatStyle, columns = semesters,
#         backgroundColor = DT::styleEqual(
#           get_class_teaching_info() |> names(),
#           get_class_teaching_info() |> as.character()
#         )
#       )
#     )
#   )
#
#
#   #
#   # output$dt = DT::renderDataTable(
#   #   data,
#   #   selection = list(target = 'cell'),
#   #   editable = list(target = 'column', disable = list(columns = c(5:7))),
#   #   extensions = c('FixedColumns'),
#   #   options = list(
#   #     # not entirely sure
#   #     scrollX = TRUE, scrollY = TRUE,
#   #     # fixed column may only be necessary if scrollling
#   #     dom = 't', fixedColumns = list(leftColumns = 2),
#   #     #
#   #     list(
#   #       func = DT::formatStyle, columns = "Status",
#   #       backgroundColor = DT::styleEqual(
#   #         get_item_status_levels() |> names(),
#   #         get_item_status_levels() |> as.character()
#   #       )
#   #     )
#   #   )
#   # )
#
# })

# ui <- function() {
#
#   app_box_default <- "#2c3b41"
#
#   fluidPage(
#     shinytoastr::useToastr(), # enable toaster
#     prompter::use_prompt(), # enable prompter
#     tags$head( # css headers
#       # custom
#       tags$style(
#         type="text/css",
#         HTML(paste(
#           # body top padding
#           ".box-body {padding-top: 5px; padding-bottom: 0px}",
#           # custom background box
#           sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}", app_box_default, app_box_default),
#           sprintf(".box.box-solid.box-info{border:1px solid %s;}", app_box_default),
#           sep="\n"))
#       )
#     ),
#     module_data_reload_button("data"),
#     module_paths_ui("paths")
#   )
#
# }
#

# library(rlang)
# library(shiny)
# library(ggplot2)
# source("utilities.R")
# source("module_paths_ui.R")
# source("module_paths_server.R")
#
# # dependencies
# # rlang
# # shiny
# # tibble
# # dplyr
# # purrr
# # stringr
# # readr
# # ggplot2
# # rlog
# # shinydashboard
# # shinyjs
# # shinytoastr
# # shinycssloader
# # prompter
#
# # app user interface
# ui <- function() {
#
#   # constants
#   app_title <- ""
#   app_title_width <- 200
#   app_sidebar_width <- app_title_width
#   app_color <- "yellow"
#   spinner_color <- "#2c3b41"
#   app_box_default <- "#2c3b41"
#
#   # options
#   options(spinner.color = spinner_color)
#
#   # header
#   header <- shinydashboard::dashboardHeader(title = "paths App", titleWidth = app_title_width)
#
#   # sidebar
#   sidebar <- shinydashboard::dashboardSidebar(
#     width = app_sidebar_width,
#     paths_ui_upload_data_button("paths"),
#     paths_ui_load_example_button("paths"),
#     paths_ui_file_selector("paths"),
#     paths_ui_seq_start("paths"),
#     paths_ui_seq_end("paths"),
#     shinyjs::useShinyjs(), # enable shinyjs
#     shinytoastr::useToastr(), # enable toaster
#     prompter::use_prompt(), # enable prompter
#     tags$head( # css headers
#       # custom
#       tags$style(
#         type="text/css",
#         HTML(paste(
#           # custom background box
#           sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}", app_box_default, app_box_default),
#           sprintf(".box.box-solid.box-info{border:1px solid %s;}", app_box_default),
#           # code break
#           "pre { white-space: pre-wrap; word-break: break-all; }",
#           sep="\n"))
#       )
#     )
#   )
#
#   # body
#   body <- shinydashboard::dashboardBody(
#     paths_ui_file_plot("paths")
#   )
#
#   # dashboard page
#   shinydashboard::dashboardPage(
#     title = app_title,
#     skin = app_color,
#     header = header,
#     sidebar = sidebar,
#     body = body
#   )
# }
#
# # app server
# server <- shinyServer(
#   function(input, output, session) {
#     log_info("Starting App...")
#     # modules
#     paths <- callModule(paths_server, "paths")
#   }
# )
#

# app start
#shinyApp(ui(), server)

shinyApp(ui(), server(gs_id, key_file))

