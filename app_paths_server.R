# server
paths_server <- function(data_sheet_id, gs_key_file) {
  function(input, output, session) {

    log_info("\n\n========================================================")
    log_info("starting geopaths GUI", if(shiny::in_devmode()) " in DEV mode")

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
      if (shiny::in_devmode())
        shiny::devmode(FALSE)
      else
        shiny::devmode(TRUE)
    })

  }
}
