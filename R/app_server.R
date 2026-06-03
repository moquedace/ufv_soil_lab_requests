app_server <- function(input, output, session) {
  app_config <- load_app_config()
  store <- shiny::reactiveVal(sample_store())

  mod_solicitante_server(
    id = "solicitante",
    app_config = app_config,
    store = store
  )

  mod_recepcao_server(
    id = "recepcao",
    app_config = app_config,
    store = store
  )
}
