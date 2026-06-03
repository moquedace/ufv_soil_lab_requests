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
        uiOutput(ns("detalhe_solicitacao"))
      )
    ),
    bslib::card(
      bslib::card_header("Amostras por analise"),
      DT::DTOutput(ns("analises"))
    )
  )
}

mod_recepcao_server <- function(id, app_config, store) {
  moduleServer(id, function(input, output, session) {
    observe({
      current <- store()
      statuses <- sort(unique(current$solicitacoes$status_interno))
      statuses <- statuses[nzchar(statuses)]
      updateSelectInput(session, "status", choices = c("Todos" = "todos", statuses))

      labs <- sort(unique(current$analises$laboratorio))
      labs <- labs[nzchar(labs)]
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
