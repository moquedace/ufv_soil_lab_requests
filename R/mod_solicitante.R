mod_solicitante_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2(class = "section-title", "Nova solicitação"),
    p(class = "muted-help", "Preencha os dados do solicitante uma vez e adicione uma ou mais amostras."),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("person-vcard"), "Dados do solicitante"),
        textInput(ns("nome_solicitante"), "Nome completo"),
        textInput(ns("email"), "E-mail"),
        textInput(ns("telefone"), "Telefone"),
        textInput(ns("cpf_cnpj"), "CPF/CNPJ"),
        textInput(ns("endereco"), "Endereço"),
        bslib::layout_columns(
          col_widths = c(8, 4),
          textInput(ns("bairro"), "Bairro"),
          textInput(ns("cep"), "CEP")
        ),
        bslib::layout_columns(
          textInput(ns("cidade_solicitante"), "Cidade"),
          textInput(ns("uf_solicitante"), "UF", value = "MG")
        )
      ),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("mortarboard"), "Vínculo e observações"),
        selectInput(
          ns("vinculo"),
          "Vínculo",
          choices = c(
            "Agricultor/produtor" = "agricultor",
            "Iniciação científica" = "ic",
            "Mestrado" = "mestrado",
            "Doutorado" = "doutorado",
            "Outro" = "outro"
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'outro'", ns("vinculo")),
          textInput(ns("vinculo_outro"), "Especifique o vínculo")
        ),
        conditionalPanel(
          condition = sprintf("['ic','mestrado','doutorado'].includes(input['%s'])", ns("vinculo")),
          textInput(ns("matricula"), "Matrícula")
        ),
        textInput(ns("instituicao"), "Instituição/departamento/laboratório (opcional)"),
        conditionalPanel(
          condition = sprintf("['ic','mestrado','doutorado'].includes(input['%s'])", ns("vinculo")),
          textInput(ns("orientador"), "Professor/orientador")
        ),
        textAreaInput(ns("observacoes"), "Observações", rows = 4)
      )
    ),
    tags$hr(),
    mod_amostras_ui(ns("amostras")),
    tags$hr(),
    bslib::layout_columns(
      col_widths = c(8, 4),
      div(
        h3("Revisão"),
        div(class = "review-box", uiOutput(ns("resumo"))),
        br(),
        DT::DTOutput(ns("resumo_amostras"))
      ),
      div(
        h3("Privacidade e envio"),
        div(
          class = "review-box",
          style = "font-size:.82rem; line-height:1.5;",
          tags$p(
            style = "margin-bottom:.6rem;",
            tags$strong("Tratamento de dados pessoais. "),
            "Os dados informados (nome, contato, documento e endereço) são coletados pelo Departamento de Solos da UFV exclusivamente para identificação do solicitante, processamento da análise e comunicação dos resultados. São armazenados de forma restrita e não compartilhados com terceiros. Você pode solicitar acesso, correção ou exclusão dos seus dados pelo contato do laboratório, conforme a Lei nº 13.709/2018 (LGPD)."
          ),
          checkboxInput(
            ns("consentimento_lgpd"),
            "Li e concordo com o tratamento dos meus dados pessoais para os fins descritos acima.",
            value = FALSE
          )
        ),
        br(),
        actionButton(
          ns("enviar"), "Enviar solicitação", class = "btn btn-primary",
          onclick = "var b=this; setTimeout(function(){ if(!b.classList.contains('disabled')){ b.dataset.lbl=b.innerHTML; b.innerHTML='Enviando…'; } }, 0);"
        ),
        br(), br(),
        uiOutput(ns("confirmacao"))
      )
    )
  )
}

mod_solicitante_server <- function(id, app_config, store, persist_store = function(new_store) invisible(FALSE)) {
  moduleServer(id, function(input, output, session) {
    reset_amostras <- reactiveVal(0)
    samples <- mod_amostras_server("amostras", app_config, reset_trigger = reactive(reset_amostras()))
    last_submission_id <- reactiveVal("")
    ultimo_envio_ts <- reactiveVal(0)

    request_payload <- reactive({
      list(
        solicitante = list(
          nome_solicitante = input$nome_solicitante,
          email = input$email,
          telefone = input$telefone,
          cpf_cnpj = input$cpf_cnpj,
          endereco = input$endereco,
          bairro = input$bairro,
          cep = input$cep,
          cidade_solicitante = input$cidade_solicitante,
          uf_solicitante = input$uf_solicitante,
          vinculo = input$vinculo,
          vinculo_outro = input$vinculo_outro,
          matricula = input$matricula,
          instituicao = input$instituicao,
          orientador = input$orientador,
          observacoes = input$observacoes
        ),
        amostras = samples()
      )
    })

    review_table <- reactive({
      build_review_table(request_payload()$amostras)
    })

    output$resumo <- renderUI({
      payload <- request_payload()
      sample_count <- length(payload$amostras)
      location_count <- sum(vapply(payload$amostras, has_sample_coordinates, logical(1)))

      s <- payload$solicitante
      vinculo_display <- if (identical(s$vinculo %||% "", "outro") && nzchar(s$vinculo_outro %||% "")) {
        paste0("Outro: ", s$vinculo_outro)
      } else {
        s$vinculo %||% ""
      }

      tagList(
        tags$dl(
          tags$dt("Solicitante"),
          tags$dd(s$nome_solicitante %||% "Não informado"),
          tags$dt("Contato"),
          tags$dd(paste(s$email %||% "", s$telefone %||% "")),
          tags$dt("Cidade/UF"),
          tags$dd(paste(s$cidade_solicitante %||% "", s$uf_solicitante %||% "")),
          tags$dt("Vínculo"),
          tags$dd(vinculo_display),
          if (nzchar(s$matricula %||% "")) tagList(tags$dt("Matrícula"), tags$dd(s$matricula)),
          if (nzchar(s$instituicao %||% "")) tagList(tags$dt("Instituição"), tags$dd(s$instituicao)),
          if (nzchar(s$orientador %||% "")) tagList(tags$dt("Orientador"), tags$dd(s$orientador)),
          tags$dt("Amostras"),
          tags$dd(sample_count),
          tags$dt("Com localização no mapa"),
          tags$dd(paste0(location_count, " de ", sample_count)),
          if (nzchar(s$observacoes %||% "")) tagList(tags$dt("Observações"), tags$dd(s$observacoes))
        )
      )
    })

    output$resumo_amostras <- DT::renderDT({
      table <- review_table()
      if (!nrow(table)) {
        return(data.frame(Mensagem = "Nenhuma amostra adicionada."))
      }

      table
    }, options = list(pageLength = 5, searching = FALSE, scrollX = TRUE))

    exportTestValues(
      review_sample_count = nrow(review_table()),
      review_location_count = count_review_locations(review_table())
    )

    observeEvent(input$enviar, {
      shinyjs::disable("enviar")
      on.exit({
        shinyjs::enable("enviar")
        updateActionButton(session, "enviar", label = "Enviar solicitação")
      })

      # guarda contra duplo envio: ignora cliques em sequencia rapida
      agora <- as.numeric(Sys.time())
      if (agora - ultimo_envio_ts() < 5) {
        return()
      }

      payload <- request_payload()

      if (!nzchar(payload$solicitante$nome_solicitante %||% "")) {
        showNotification("Informe o nome do solicitante.", type = "error")
        return()
      }

      if (!isTRUE(input$consentimento_lgpd)) {
        showNotification("É necessário concordar com o tratamento dos dados pessoais para enviar.", type = "error")
        return()
      }

      email_informado <- trimws(payload$solicitante$email %||% "")
      if (nzchar(email_informado) && !is_valid_email(email_informado)) {
        showNotification("E-mail em formato inválido. Verifique antes de enviar.", type = "error")
        return()
      }

      if (!length(payload$amostras)) {
        showNotification("Adicione pelo menos uma amostra.", type = "error")
        return()
      }

      telefone_informado <- trimws(payload$solicitante$telefone %||% "")
      if (nzchar(telefone_informado) && !is_valid_phone(telefone_informado)) {
        showNotification("Telefone parece incompleto. A solicitação será enviada, confira o contato.", type = "warning")
      }

      ultimo_envio_ts(agora)
      request_id <- next_request_id()
      current <- store()

      new_request <- data.frame(
        solicitacao_id = request_id,
        data_hora_envio = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        nome_solicitante = payload$solicitante$nome_solicitante,
        email = payload$solicitante$email,
        telefone = payload$solicitante$telefone,
        cpf_cnpj = payload$solicitante$cpf_cnpj %||% "",
        endereco = payload$solicitante$endereco %||% "",
        bairro = payload$solicitante$bairro %||% "",
        cep = payload$solicitante$cep %||% "",
        cidade_solicitante = payload$solicitante$cidade_solicitante,
        uf_solicitante = payload$solicitante$uf_solicitante %||% "",
        vinculo = payload$solicitante$vinculo %||% "",
        vinculo_outro = payload$solicitante$vinculo_outro %||% "",
        matricula = payload$solicitante$matricula %||% "",
        instituicao = payload$solicitante$instituicao %||% "",
        orientador = payload$solicitante$orientador %||% "",
        observacoes_solicitante = payload$solicitante$observacoes %||% "",
        consentimento_aceito = "sim",
        status_interno = "Recebida",
        data_entrada_lab = "",
        numero_laboratorio = "",
        custo_total_lab = "",
        forma_pagamento_lab = "",
        pedido_numero_lab = "",
        observacoes_internas = "",
        stringsAsFactors = FALSE
      )

      new_samples <- do.call(rbind, lapply(seq_along(payload$amostras), function(index) {
        sample <- payload$amostras[[index]]
        data.frame(
          amostra_id = next_sample_id(request_id, index),
          solicitacao_id = request_id,
          ordem_amostra = index,
          referencia_amostra = sample$referencia_amostra,
          municipio_amostra = sample$municipio_amostra,
          uf_amostra = sample$uf_amostra,
          localidade_descricao = sample$localidade_descricao,
          latitude_wgs84 = sample$latitude_wgs84,
          longitude_wgs84 = sample$longitude_wgs84,
          tipo_localizacao = sample$tipo_localizacao,
          tipo_material = sample$tipo_material,
          tipo_amostra_vegetal = sample$tipo_amostra_vegetal %||% NA_character_,
          cultura_planta = sample$cultura_planta %||% NA_character_,
          grupos_analise = paste(sample$grupos_analise, collapse = ";"),
          carbonato_presente = sample$carbonato_presente,
          percentual_c_estimado = sample$percentual_c_estimado %||% NA_real_,
          percentual_n_estimado = sample$percentual_n_estimado %||% NA_real_,
          numero_registro_projeto = sample$numero_registro_projeto %||% NA_character_,
          elementos_aa_icp = sample$elementos_aa_icp %||% NA_character_,
          tipo_digestao = sample$tipo_digestao %||% NA_character_,
          volume_apos_digestao = sample$volume_apos_digestao %||% NA_real_,
          aliquota = sample$aliquota %||% NA_real_,
          diluicao = sample$diluicao %||% NA_character_,
          volume_final = sample$volume_final %||% NA_real_,
          departamento_origem = sample$departamento_origem %||% NA_character_,
          projeto_registrado = sample$projeto_registrado %||% NA_character_,
          numero_registro_projeto_aa = sample$numero_registro_projeto_aa %||% NA_character_,
          pre_tratamento_necessario = sample$pre_tratamento_necessario,
          stringsAsFactors = FALSE
        )
      }))

      analysis_rows <- lapply(seq_along(payload$amostras), function(index) {
        sample <- payload$amostras[[index]]
        if (!nrow(sample$analises)) {
          return(NULL)
        }

        amostra_id <- new_samples$amostra_id[index]
        data.frame(
          amostra_id = amostra_id,
          laboratorio = sample$analises$laboratorio,
          analise_id = sample$analises$analise_id,
          analise_nome = sample$analises$analise_nome,
          stringsAsFactors = FALSE
        )
      })

      new_analyses <- do.call(rbind, analysis_rows)

      new_store <- list(
        solicitacoes = new_request,
        amostras = new_samples,
        analises = if (is.null(new_analyses)) data.frame() else new_analyses
      )

      current$solicitacoes <- safe_rbind(current$solicitacoes, new_request)
      current$amostras <- safe_rbind(current$amostras, new_samples)
      if (!is.null(new_analyses)) {
        current$analises <- safe_rbind(current$analises, new_analyses)
      }
      store(current)
      persist_store(new_store)
      last_submission_id(request_id)

      # limpa o formulario para uma nova solicitacao
      campos_texto <- c("nome_solicitante", "email", "telefone", "cpf_cnpj", "endereco",
                        "bairro", "cep", "cidade_solicitante", "vinculo_outro", "matricula",
                        "instituicao", "orientador")
      lapply(campos_texto, \(campo) { updateTextInput(session, campo, value = "") })
      updateTextAreaInput(session, "observacoes", value = "")
      updateSelectInput(session, "vinculo", selected = "agricultor")
      updateCheckboxInput(session, "consentimento_lgpd", value = FALSE)
      reset_amostras(reset_amostras() + 1)

      showNotification(paste0("Solicitação enviada com sucesso. Protocolo: ", request_id), type = "message", duration = 8)
      output$confirmacao <- renderUI({
        div(
          class = "alert alert-success",
          tags$strong("Solicitação enviada. "),
          "Protocolo: ", tags$strong(request_id), tags$br(),
          "Guarde este número. Você já pode registrar uma nova solicitação."
        )
      })
    })

    exportTestValues(ultimo_envio_id = last_submission_id())
  })
}

is_valid_email <- function(x) {
  grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", x)
}

is_valid_phone <- function(x) {
  digits <- gsub("[^0-9]", "", x)
  nchar(digits) >= 8
}

build_review_table <- function(samples) {
  if (!length(samples)) {
    return(data.frame())
  }

  data.frame(
    referencia = vapply(samples, `[[`, character(1), "referencia_amostra"),
    material = vapply(samples, `[[`, character(1), "tipo_material"),
    municipio = vapply(samples, \(s) { paste(s$municipio_amostra, s$uf_amostra, sep = "/") }, character(1)),
    localizacao_mapa = vapply(samples, \(s) { if (has_sample_coordinates(s)) "sim" else "nao" }, character(1)),
    precisao = vapply(samples, `[[`, character(1), "tipo_localizacao"),
    grupos = vapply(samples, \(s) { paste(s$grupos_analise, collapse = "; ") }, character(1)),
    analises = vapply(samples, \(s) { paste(s$analises_nomes, collapse = "; ") }, character(1)),
    stringsAsFactors = FALSE
  )
}

has_sample_coordinates <- function(sample) {
  !is.null(sample$latitude_wgs84) &&
    !is.null(sample$longitude_wgs84) &&
    !is.na(sample$latitude_wgs84) &&
    !is.na(sample$longitude_wgs84)
}

count_review_locations <- function(review_table) {
  if (!nrow(review_table) || !"localizacao_mapa" %in% names(review_table)) {
    return(0L)
  }

  sum(review_table$localizacao_mapa == "sim")
}
