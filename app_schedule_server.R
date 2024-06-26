# server
schedule_server <- function(data_sheet_id, gs_key_file) {

  shinyServer(function(input, output, session) {

    log_info("\n\n========================================================")
    log_info("starting geoschedule GUI", if(is_dev_mode()) " in DEV mode")

    # data module
    data <- callModule(
      module_data_schedule_server, id = "schedule_data",
      data_sheet_id = data_sheet_id,
      gs_key_file = gs_key_file
    )

    # paths module
    schedule <- callModule(module_schedule_server, id = "schedule", data = data)

    # dev mode
    observeEvent(input$dev_mode_toggle, {
      if (is_dev_mode())
        Sys.setenv('GEOAPPS_DEV'= 'OFF')
      else
        Sys.setenv('GEOAPPS_DEV'= 'ON')
    })
  })

}
