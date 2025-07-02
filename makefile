# tools for gui development
gui_dev:
	R -e "rm(list = ls()); Sys.setenv('LOG_LEVEL' = 'DEBUG'); options(shiny.autoreload = TRUE); Sys.setenv('GEOAPPS_DEV'= 'ON'); shiny::runApp('app.R', port = 4446)"
