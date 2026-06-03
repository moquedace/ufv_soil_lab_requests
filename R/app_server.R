app_server <- function(input, output, session) {
  app_config <- load_app_config()
  store <- shiny::reactiveVal(load_initial_store())

  mod_solicitante_server(
    id = "solicitante",
    app_config = app_config,
    store = store,
    persist_store = persist_new_store
  )

  mod_recepcao_server(
    id = "recepcao",
    app_config = app_config,
    store = store,
    persist_requests = persist_requests_store
  )

  exportTestValues(
    solicitacoes_count = nrow(store()$solicitacoes),
    amostras_count = nrow(store()$amostras),
    analises_count = nrow(store()$analises)
  )
}

load_initial_store <- function() {
  if (is_test_mode()) {
    return(sample_store())
  }

  if (!use_google_sheets()) {
    return(sample_store())
  }

  tryCatch(
    read_google_store(),
    error = function(e) {
      warning("Nao foi possivel ler o Google Sheets. Usando dados simulados. Erro: ", conditionMessage(e))
      sample_store()
    }
  )
}

persist_new_store <- function(new_store) {
  if (is_test_mode()) {
    return(invisible(FALSE))
  }

  if (!use_google_sheets()) {
    return(invisible(FALSE))
  }

  tryCatch(
    append_google_store(new_store),
    error = function(e) {
      warning("Nao foi possivel gravar no Google Sheets: ", conditionMessage(e))
      FALSE
    }
  )
}

persist_requests_store <- function(solicitacoes) {
  if (is_test_mode()) {
    return(invisible(FALSE))
  }

  if (!use_google_sheets()) {
    return(invisible(FALSE))
  }

  tryCatch(
    write_google_sheet(google_sheet_id(), "solicitacoes", solicitacoes),
    error = function(e) {
      warning("Nao foi possivel atualizar solicitacoes no Google Sheets: ", conditionMessage(e))
      FALSE
    }
  )
}
