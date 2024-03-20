library(rlang)
library(shiny)
library(DT)

get_local_path <- function() return("local_data_paths.xlsx")

source("logic_gdrive.R")
source("app_utils.R")
source("app_module_data_table.R")
source("app_module_selector_table.R")

source("logic_paths.R")
source("app_paths_ui.R")
source("app_paths_server.R")
source("app_data_paths.R")
source("app_module_paths.R")

source("credentials.R")

app <- shinyApp(paths_ui(), paths_server(paths_gs_id, key_file))
app
