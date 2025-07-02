# tools for gui development
gui_dev:
	R -e "rm(list = ls()); Sys.setenv('LOG_LEVEL' = 'DEBUG'); shiny::devmode(TRUE); shiny::runApp('app.R', port = 4446)"
