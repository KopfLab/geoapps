library(rlang)
library(shiny)
library(DT)

get_local_path <- function() return("local_data_schedule.xlsx")

source("logic_gdrive.R")
source("app_utils.R")
source("app_module_data_table.R")
source("app_module_selector_table.R")

source("logic_schedule.R")
source("app_schedule_ui.R")
source("app_schedule_server.R")
source("app_data_schedule.R")
source("app_module_schedule.R")

source("credentials.R")

app <- shinyApp(schedule_ui(), schedule_server(schedule_gs_id, key_file))
app
