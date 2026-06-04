mod_recepcao_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("recepcao_conteudo"))
  )
}

mod_recepcao_conteudo_ui <- function(ns) {
  tagList(
    h2(class = "section-title", "Recepção de amostras"),
    p(class = "muted-help", "Consulte solicitações, filtre por grupo de análise e exporte os dados recebidos."),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("funnel"), "Filtros"),
        textInput(ns("busca"), "Buscar", placeholder = "Solicitante, município, amostra, análise..."),
        bslib::layout_columns(
          selectInput(ns("status"), "Status", choices = c("Todos" = "todos")),
          selectInput(ns("laboratorio"), "Grupo de análise", choices = c("Todos" = "todos"))
        )
      ),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("clipboard-data"), "Resumo"),
        verbatimTextOutput(ns("resumo_store")),
        tags$hr(),
        downloadButton(ns("baixar_csv"), "Exportar CSV"),
        downloadButton(ns("baixar_xlsx"), "Exportar XLSX")
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("inbox"), "Solicitações"),
        DT::DTOutput(ns("solicitacoes"))
      ),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("file-text"), "Detalhe selecionado"),
        uiOutput(ns("detalhe_solicitacao")),
        tags$hr(),
        h4("Campos internos"),
        bslib::layout_columns(
          textInput(ns("data_entrada_lab"), "Data de entrada"),
          textInput(ns("numero_laboratorio"), "N° de laboratório")
        ),
        bslib::layout_columns(
          textInput(ns("custo_total_lab"), "Custo total (R$)"),
          textInput(ns("pedido_numero_lab"), "N° do pedido")
        ),
        selectInput(
          ns("status_interno_edit"),
          "Status",
          choices = c("Recebida", "Aguardando amostra", "Em análise", "Finalizada", "Cancelada", "Teste")
        ),
        selectInput(
          ns("forma_pagamento_lab"),
          "Forma de pagamento",
          choices = c("", "PIX", "SIF", "FACEV", "FUNARBE", "Boleto", "Nota Fiscal", "Boleto c/ nota", "Transferência entre convênios")
        ),
        textAreaInput(ns("observacoes_internas"), "Observações internas", rows = 3),
        actionButton(ns("salvar_interno"), "Salvar campos internos", class = "btn btn-primary"),
        uiOutput(ns("salvar_feedback"))
      )
    ),
    bslib::card(
      bslib::card_header(bsicons::bs_icon("eyedropper"), "Amostras por análise"),
      DT::DTOutput(ns("analises"))
    )
  )
}

recepcao_senha <- function() {
  Sys.getenv("LAB_RECEPTION_PASSWORD", unset = "dps2024")
}

blank_value <- function(v) {
  if (is.null(v) || !length(v)) return(TRUE)
  s <- as.character(v[[1]])
  is.na(s) || !nzchar(trimws(s)) || identical(trimws(s), "NA")
}

detail_dd <- function(label, value) {
  if (blank_value(value)) return(NULL)
  htmltools::tagList(
    htmltools::tags$dt(label),
    htmltools::tags$dd(as.character(value[[1]]))
  )
}

recepcao_code_label <- function(value, map) {
  v <- as.character(value %||% "")
  if (v %in% names(map)) unname(map[[v]]) else v
}

vinculo_recepcao_label <- function(vinculo, vinculo_outro = "") {
  base <- recepcao_code_label(vinculo, list(
    agricultor = "Agricultor/produtor",
    ic = "Iniciação científica",
    mestrado = "Mestrado",
    doutorado = "Doutorado",
    outro = "Outro"
  ))
  if (identical(as.character(vinculo %||% ""), "outro") && !blank_value(vinculo_outro)) {
    paste0("Outro: ", vinculo_outro)
  } else {
    base
  }
}

clean_text <- function(v) {
  s <- trimws(as.character(v[[1]] %||% ""))
  if (is.na(s) || identical(s, "NA")) "" else s
}

format_request_address <- function(request) {
  cidade <- clean_text(request$cidade_solicitante)
  uf <- clean_text(request$uf_solicitante)
  cidade_uf <- if (nzchar(cidade) && nzchar(uf)) {
    paste0(cidade, "/", uf)
  } else {
    paste0(cidade, uf)
  }
  parts <- c(
    clean_text(request$endereco),
    clean_text(request$bairro),
    cidade_uf,
    clean_text(request$cep)
  )
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = " · ")
}

format_sample_location <- function(sample) {
  lat <- suppressWarnings(as.numeric(sample$latitude_wgs84[[1]]))
  lng <- suppressWarnings(as.numeric(sample$longitude_wgs84[[1]]))
  if (is.na(lat) || is.na(lng)) return("")
  tipo <- recepcao_code_label(sample$tipo_localizacao[[1]], list(
    exata = "exato", aproximada = "aproximado", municipio_regiao = "município/região"
  ))
  sprintf("%.5f, %.5f (%s)", lat, lng, tipo)
}

format_sample_detail <- function(sample, sample_analyses) {
  grupos <- strsplit(as.character(sample$grupos_analise[[1]] %||% ""), ";")[[1]]
  is_v <- "vegetal" %in% grupos
  is_chn <- "chn" %in% grupos
  is_aa <- any(c("absorcao_atomica", "icp_oes") %in% grupos)

  municipio <- trimws(paste(
    c(sample$municipio_amostra[[1]] %||% "", sample$uf_amostra[[1]] %||% ""),
    collapse = "/"
  ))
  if (municipio == "/") municipio <- ""

  analises_txt <- if (nrow(sample_analyses)) {
    paste(sample_analyses$analise_nome, collapse = ", ")
  } else {
    ""
  }

  div(
    class = "review-box", style = "margin-bottom:.8rem;",
    tags$dl(
      detail_dd("Referência", sample$referencia_amostra),
      detail_dd("Material", sample$tipo_material),
      detail_dd("Município", municipio),
      detail_dd("Localidade", sample$localidade_descricao),
      detail_dd("Coordenadas", format_sample_location(sample)),
      if (is_v) detail_dd("Tipo vegetal", sample$tipo_amostra_vegetal),
      if (is_v) detail_dd("Cultura/planta", sample$cultura_planta),
      if (is_chn) detail_dd("Carbonato", recepcao_code_label(sample$carbonato_presente[[1]], list(sim = "Sim", nao = "Não", nao_sei = "Não sei"))),
      if (is_chn) detail_dd("%C estimado", sample$percentual_c_estimado),
      if (is_chn) detail_dd("%N estimado", sample$percentual_n_estimado),
      if (is_chn) detail_dd("Nº do projeto", sample$numero_registro_projeto),
      if (is_aa) detail_dd("Elementos", sample$elementos_aa_icp),
      if (is_aa) detail_dd("Digestão", sample$tipo_digestao),
      if (is_aa) detail_dd("Vol. após digestão", sample$volume_apos_digestao),
      if (is_aa) detail_dd("Alíquota", sample$aliquota),
      if (is_aa) detail_dd("Volume final", sample$volume_final),
      if (is_aa) detail_dd("Diluição", sample$diluicao),
      if (is_aa) detail_dd("Departamento", recepcao_code_label(sample$departamento_origem[[1]], list(dps = "DPS - Solos UFV", outro = "Outro"))),
      if (is_aa) detail_dd("Projeto registrado", recepcao_code_label(sample$projeto_registrado[[1]], list(sim = "Sim", nao = "Não"))),
      if (is_aa) detail_dd("Nº do projeto", sample$numero_registro_projeto_aa),
      detail_dd("Análises", analises_txt)
    )
  )
}

status_badge_html <- function(status) {
  status <- as.character(status %||% "")
  if (!nzchar(status)) status <- "Recebida"
  cls <- switch(
    status,
    "Recebida" = "status-recebida",
    "Aguardando amostra" = "status-aguardando",
    "Em análise" = "status-em-analise",
    "Em analise" = "status-em-analise",
    "Finalizada" = "status-finalizada",
    "Cancelada" = "status-cancelada",
    "Teste" = "status-teste",
    "status-teste"
  )
  sprintf('<span class="status-badge %s">%s</span>', cls, htmltools::htmlEscape(status))
}

mod_recepcao_server <- function(id, app_config, store, persist_requests = function(solicitacoes) invisible(FALSE)) {
  moduleServer(id, function(input, output, session) {
    autenticado <- reactiveVal(FALSE)

    output$recepcao_conteudo <- renderUI({
      if (!autenticado()) {
        div(
          style = "max-width:360px; margin:3rem auto;",
          bslib::card(
            bslib::card_header(bsicons::bs_icon("lock"), "Acesso restrito"),
            p(class = "muted-help", "Esta área é exclusiva para a equipe de recepção do laboratório."),
            passwordInput(session$ns("senha_recepcao"), "Senha"),
            actionButton(session$ns("entrar_recepcao"), "Entrar", class = "btn btn-primary"),
            uiOutput(session$ns("erro_senha"))
          )
        )
      } else {
        mod_recepcao_conteudo_ui(session$ns)
      }
    })

    observeEvent(input$entrar_recepcao, {
      if (identical(input$senha_recepcao, recepcao_senha())) {
        autenticado(TRUE)
      } else {
        output$erro_senha <- renderUI({
          p(style = "color:#c0392b; font-size:.88rem; margin-top:.4rem;", "Senha incorreta.")
        })
      }
    })

    last_saved_request <- reactiveVal("")

    observe({
      req(autenticado())
      current <- store()
      statuses <- sort(unique(current$solicitacoes$status_interno))
      statuses <- statuses[!is.na(statuses) & nzchar(statuses)]
      updateSelectInput(session, "status", choices = c("Todos" = "todos", statuses))

      labs <- sort(unique(current$analises$laboratorio))
      labs <- labs[!is.na(labs) & nzchar(labs)]
      lab_labels <- stats::setNames(labs, vapply(labs, \(lab) {
        app_config$analises[[lab]]$nome %||% lab
      }, character(1)))
      updateSelectInput(session, "laboratorio", choices = c("Todos" = "todos", lab_labels))
    })

    analysis_data <- reactive({
      req(autenticado())
      filter_reception_data(
        data = flatten_store_by_analysis(store()),
        search = trimws(input$busca %||% ""),
        status = input$status %||% "todos",
        laboratorio = input$laboratorio %||% "todos"
      )
    })

    request_data <- reactive({
      req(autenticado())
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
      data <- request_data()
      label_map <- c(
        solicitacao_id = "ID",
        data_hora_envio = "Data/Hora",
        nome_solicitante = "Solicitante",
        cidade_solicitante = "Município",
        status_interno = "Status"
      )
      cols <- intersect(names(label_map), names(data))
      escape_cols <- TRUE
      if (length(cols)) {
        data <- data[, cols, drop = FALSE]
        names(data) <- label_map[names(data)]
        if ("Status" %in% names(data)) {
          data$Status <- vapply(data$Status, status_badge_html, character(1))
          # escapa todas as colunas exceto Status (evita XSS nos dados de usuario)
          escape_cols <- which(names(data) != "Status")
        }
      }
      DT::datatable(
        data,
        selection = "single",
        escape = escape_cols,
        options = list(pageLength = 8, order = list(list(1, "desc")))
      )
    })

    output$analises <- DT::renderDT({
      data <- analysis_data()
      label_map <- c(
        referencia_amostra = "Amostra",
        laboratorio = "Laboratório",
        analise_nome = "Análise",
        nome_solicitante = "Solicitante",
        municipio_amostra = "Município",
        uf_amostra = "UF",
        status_interno = "Status"
      )
      cols <- intersect(names(label_map), names(data))
      if (length(cols)) {
        data <- data[, cols, drop = FALSE]
        names(data) <- label_map[names(data)]
      }
      data
    }, options = list(pageLength = 8, scrollX = TRUE))

    output$resumo_store <- renderPrint({
      data <- analysis_data()
      cat("Solicitações:", length(unique(data$solicitacao_id)), "\n")
      cat("Amostras:", length(unique(data$amostra_id)), "\n")
      cat("Análises:", nrow(data), "\n")
    })

    output$detalhe_solicitacao <- renderUI({
      selected <- input$solicitacoes_rows_selected
      requests <- request_data()
      if (!length(selected) || !nrow(requests)) {
        return(p(class = "muted-help", "Selecione uma linha na tabela para ver o detalhe."))
      }

      request <- requests[selected[1], , drop = FALSE]
      request_id <- request$solicitacao_id[[1]]
      samples <- store()$amostras[store()$amostras$solicitacao_id == request_id, , drop = FALSE]
      all_analyses <- store()$analises

      tagList(
        div(
          class = "review-box",
          tags$dl(
            detail_dd("Solicitante", request$nome_solicitante),
            detail_dd("E-mail", request$email),
            detail_dd("Telefone", request$telefone),
            detail_dd("CPF/CNPJ", request$cpf_cnpj),
            detail_dd("Endereço", format_request_address(request)),
            detail_dd("Vínculo", vinculo_recepcao_label(request$vinculo[[1]], request$vinculo_outro[[1]] %||% "")),
            detail_dd("Matrícula", request$matricula),
            detail_dd("Instituição", request$instituicao),
            detail_dd("Orientador", request$orientador),
            detail_dd("Observações", request$observacoes_solicitante),
            tags$dt("Status"), tags$dd(HTML(status_badge_html(request$status_interno[[1]]))),
            detail_dd("Enviada em", request$data_hora_envio),
            detail_dd("Protocolo", request_id)
          )
        ),
        h4(sprintf("Amostras (%d)", nrow(samples))),
        if (nrow(samples)) {
          lapply(seq_len(nrow(samples)), function(i) {
            s <- samples[i, , drop = FALSE]
            sa <- all_analyses[all_analyses$amostra_id == s$amostra_id[[1]], , drop = FALSE]
            format_sample_detail(s, sa)
          })
        } else {
          p(class = "muted-help", "Nenhuma amostra vinculada.")
        }
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
        showNotification("Selecione uma solicitação antes de salvar.", type = "warning")
        return()
      }

      request_id <- request$solicitacao_id[[1]]
      current <- store()
      row <- current$solicitacoes$solicitacao_id == request_id

      if (!any(row)) {
        showNotification("Solicitação não encontrada.", type = "error")
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
      showNotification("Campos internos salvos com sucesso.", type = "message")
    })

    output$salvar_feedback <- renderUI({
      if (!nzchar(last_saved_request())) {
        return(NULL)
      }

      div(class = "alert alert-success", paste("Última solicitação salva:", last_saved_request()))
    })

    exportTestValues(
      ultimo_salvamento_id = last_saved_request(),
      autenticado = autenticado()
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
