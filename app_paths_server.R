# server
paths_server <- function(data_sheet_id, gs_key_file) {

  shinyServer(function(input, output, session) {

    log_info("\n\n========================================================")
    log_info("starting geopaths GUI", if(is_dev_mode()) " in DEV mode")

    # data module
    data <- callModule(
      module_data_paths_server, id = "paths_data",
      data_sheet_id = data_sheet_id,
      gs_key_file = gs_key_file
    )

    # paths module
    paths <- callModule(module_paths_server, id = "paths", data = data)

    # dev mode
    observeEvent(input$dev_mode_toggle, {
      if (is_dev_mode())
        Sys.setenv('GEOAPPS_DEV'= 'OFF')
      else
        Sys.setenv('GEOAPPS_DEV'= 'ON')
    })

  })

}
