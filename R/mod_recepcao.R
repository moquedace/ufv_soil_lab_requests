mod_recepcao_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2(class = "section-title", "Recepcao de amostras"),
    p(class = "muted-help", "Consulte solicitacoes, filtre por grupo de analise e exporte os dados recebidos."),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header("Filtros"),
        textInput(ns("busca"), "Buscar", placeholder = "Solicitante, municipio, amostra, analise..."),
        bslib::layout_columns(
          selectInput(ns("status"), "Status", choices = c("Todos" = "todos")),
          selectInput(ns("laboratorio"), "Grupo de analise", choices = c("Todos" = "todos"))
        )
      ),
      bslib::card(
        bslib::card_header("Resumo"),
        verbatimTextOutput(ns("resumo_store")),
        tags$hr(),
        downloadButton(ns("baixar_csv"), "CSV filtrado"),
        downloadButton(ns("baixar_xlsx"), "XLSX filtrado")
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Solicitacoes"),
        DT::DTOutput(ns("solicitacoes"))
      ),
      bslib::card(
        bslib::card_header("Detalhe selecionado"),
        uiOutput(ns("detalhe_solicitacao")),
        tags$hr(),
        h4("Campos internos"),
        bslib::layout_columns(
          textInput(ns("data_entrada_lab"), "Data de entrada"),
          textInput(ns("numero_laboratorio"), "Numero de laboratorio")
        ),
        bslib::layout_columns(
          textInput(ns("custo_total_lab"), "Custo total"),
          textInput(ns("pedido_numero_lab"), "Pedido numero")
        ),
        selectInput(
          ns("status_interno_edit"),
          "Status interno",
          choices = c("Recebida", "Aguardando amostra", "Em analise", "Finalizada", "Cancelada", "Teste")
        ),
        selectInput(
          ns("forma_pagamento_lab"),
          "Forma de pagamento",
          choices = c("", "PIX", "SIF", "FACEV", "FUNARBE", "Boleto", "Nota Fiscal", "Boleto c/ nota", "Transferencia entre convenios")
        ),
        textAreaInput(ns("observacoes_internas"), "Observacoes internas", rows = 3),
        actionButton(ns("salvar_interno"), "Salvar campos internos", class = "btn btn-primary"),
        uiOutput(ns("salvar_feedback"))
      )
    ),
    bslib::card(
      bslib::card_header("Amostras por analise"),
      DT::DTOutput(ns("analises"))
    )
  )
}

mod_recepcao_server <- function(id, app_config, store, persist_requests = function(solicitacoes) invisible(FALSE)) {
  moduleServer(id, function(input, output, session) {
    last_saved_request <- reactiveVal("")

    observe({
      current <- store()
      statuses <- sort(unique(current$solicitacoes$status_interno))
      statuses <- statuses[!is.na(statuses) & nzchar(statuses)]
      updateSelectInput(session, "status", choices = c("Todos" = "todos", statuses))

      labs <- sort(unique(current$analises$laboratorio))
      labs <- labs[!is.na(labs) & nzchar(labs)]
      lab_labels <- stats::setNames(labs, vapply(labs, function(lab) {
        app_config$analises[[lab]]$nome %||% lab
      }, character(1)))
      updateSelectInput(session, "laboratorio", choices = c("Todos" = "todos", lab_labels))
    })

    analysis_data <- reactive({
      filter_reception_data(
        data = flatten_store_by_analysis(store()),
        search = trimws(input$busca %||% ""),
        status = input$status %||% "todos",
        laboratorio = input$laboratorio %||% "todos"
      )
    })

    request_data <- reactive({
      data <- store()$solicitacoes
      if (!nrow(data)) {
        return(data)
      }

      search_filter <- trimws(input$busca %||% "")
      status_filter <- input$status %||% "todos"
      lab_filter <- input$laboratorio %||% "todos"
      matching_ids <- unique(analysis_data()$solicitacao_id)
      if (length(matching_ids)) {
        data <- data[data$solicitacao_id %in% matching_ids, , drop = FALSE]
      } else if (nzchar(search_filter) || !identical(status_filter, "todos") || !identical(lab_filter, "todos")) {
        data <- data[0, , drop = FALSE]
      }

      data
    })

    output$solicitacoes <- DT::renderDT({
      request_data()
    }, selection = "single", options = list(pageLength = 8, order = list(list(1, "desc"))))

    output$analises <- DT::renderDT({
      analysis_data()
    }, options = list(pageLength = 8, scrollX = TRUE))

    output$resumo_store <- renderPrint({
      data <- analysis_data()
      cat("Solicitacoes filtradas:", length(unique(data$solicitacao_id)), "\n")
      cat("Amostras filtradas:", length(unique(data$amostra_id)), "\n")
      cat("Analises filtradas:", nrow(data), "\n")
    })

    output$detalhe_solicitacao <- renderUI({
      selected <- input$solicitacoes_rows_selected
      requests <- request_data()
      if (!length(selected) || !nrow(requests)) {
        return(p(class = "muted-help", "Selecione uma solicitacao na tabela."))
      }

      request <- requests[selected[1], , drop = FALSE]
      request_id <- request$solicitacao_id[[1]]
      samples <- store()$amostras[store()$amostras$solicitacao_id == request_id, , drop = FALSE]
      analyses <- store()$analises[store()$analises$amostra_id %in% samples$amostra_id, , drop = FALSE]

      tagList(
        tags$dl(
          tags$dt("Solicitante"), tags$dd(request$nome_solicitante[[1]]),
          tags$dt("Contato"), tags$dd(paste(request$email[[1]], request$telefone[[1]])),
          tags$dt("Status"), tags$dd(request$status_interno[[1]]),
          tags$dt("Solicitacao"), tags$dd(request_id),
          tags$dt("Amostras"), tags$dd(nrow(samples)),
          tags$dt("Analises"), tags$dd(nrow(analyses))
        )
      )
    })

    selected_request <- reactive({
      selected <- input$solicitacoes_rows_selected
      requests <- request_data()
      if (!length(selected) || !nrow(requests)) {
        return(NULL)
      }

      requests[selected[1], , drop = FALSE]
    })

    observeEvent(selected_request(), {
      request <- selected_request()
      if (is.null(request)) {
        return()
      }

      updateTextInput(session, "data_entrada_lab", value = field_value(request, "data_entrada_lab"))
      updateTextInput(session, "numero_laboratorio", value = field_value(request, "numero_laboratorio"))
      updateTextInput(session, "custo_total_lab", value = field_value(request, "custo_total_lab"))
      updateTextInput(session, "pedido_numero_lab", value = field_value(request, "pedido_numero_lab"))
      updateSelectInput(session, "status_interno_edit", selected = field_value(request, "status_interno", "Recebida"))
      updateSelectInput(session, "forma_pagamento_lab", selected = field_value(request, "forma_pagamento_lab"))
      updateTextAreaInput(session, "observacoes_internas", value = field_value(request, "observacoes_internas"))
    }, ignoreInit = TRUE)

    observeEvent(input$salvar_interno, {
      request <- selected_request()
      if (is.null(request)) {
        showNotification("Selecione uma solicitacao antes de salvar.", type = "warning")
        return()
      }

      request_id <- request$solicitacao_id[[1]]
      current <- store()
      row <- current$solicitacoes$solicitacao_id == request_id

      if (!any(row)) {
        showNotification("Solicitacao nao encontrada no armazenamento atual.", type = "error")
        return()
      }

      current$solicitacoes <- update_request_internal_fields(
        solicitacoes = current$solicitacoes,
        request_id = request_id,
        values = list(
          data_entrada_lab = input$data_entrada_lab,
          numero_laboratorio = input$numero_laboratorio,
          custo_total_lab = input$custo_total_lab,
          forma_pagamento_lab = input$forma_pagamento_lab,
          pedido_numero_lab = input$pedido_numero_lab,
          observacoes_internas = input$observacoes_internas,
          status_interno = input$status_interno_edit
        )
      )

      store(current)
      persist_requests(current$solicitacoes)
      last_saved_request(request_id)
      showNotification("Campos internos salvos.", type = "message")
    })

    output$salvar_feedback <- renderUI({
      if (!nzchar(last_saved_request())) {
        return(NULL)
      }

      div(class = "alert alert-success", paste("Ultima solicitacao salva:", last_saved_request()))
    })

    exportTestValues(
      ultimo_salvamento_id = last_saved_request()
    )

    output$baixar_csv <- downloadHandler(
      filename = function() paste0("solicitacoes_filtradas_", Sys.Date(), ".csv"),
      content = function(file) write_export_csv(analysis_data(), file)
    )

    output$baixar_xlsx <- downloadHandler(
      filename = function() paste0("solicitacoes_filtradas_", Sys.Date(), ".xlsx"),
      content = function(file) write_export_xlsx(analysis_data(), file)
    )
  })
}

field_value <- function(data, field, fallback = "") {
  if (!field %in% names(data)) {
    return(fallback)
  }

  value <- data[[field]][[1]]
  if (is.null(value) || !length(value) || is.na(value)) {
    fallback
  } else {
    as.character(value)
  }
}

ensure_request_internal_columns <- function(data) {
  columns <- c(
    "data_entrada_lab",
    "numero_laboratorio",
    "custo_total_lab",
    "forma_pagamento_lab",
    "pedido_numero_lab",
    "observacoes_internas"
  )

  for (column in columns) {
    if (!column %in% names(data)) {
      data[[column]] <- ""
    }
  }

  data
}

update_request_internal_fields <- function(solicitacoes, request_id, values) {
  solicitacoes <- ensure_request_internal_columns(solicitacoes)
  row <- solicitacoes$solicitacao_id == request_id

  if (!any(row)) {
    return(solicitacoes)
  }

  editable <- c(
    "data_entrada_lab",
    "numero_laboratorio",
    "custo_total_lab",
    "forma_pagamento_lab",
    "pedido_numero_lab",
    "observacoes_internas",
    "status_interno"
  )

  for (field in intersect(names(values), editable)) {
    solicitacoes[row, field] <- values[[field]]
  }

  solicitacoes
}
