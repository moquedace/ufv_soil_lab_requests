mod_solicitante_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2(class = "section-title", "Nova solicitacao"),
    p(class = "muted-help", "Preencha os dados do solicitante uma vez e adicione uma ou mais amostras."),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Dados do solicitante"),
        textInput(ns("nome_solicitante"), "Nome completo"),
        textInput(ns("email"), "E-mail"),
        textInput(ns("telefone"), "Telefone"),
        textInput(ns("cpf_cnpj"), "CPF/CNPJ"),
        textInput(ns("endereco"), "Endereco"),
        bslib::layout_columns(
          textInput(ns("cidade_solicitante"), "Cidade"),
          textInput(ns("uf_solicitante"), "UF", value = "MG")
        )
      ),
      bslib::card(
        bslib::card_header("Vinculo e observacoes"),
        selectInput(
          ns("vinculo"),
          "Vinculo",
          choices = c(
            "Agricultor/produtor" = "agricultor",
            "Iniciacao cientifica" = "ic",
            "Mestrado" = "mestrado",
            "Doutorado" = "doutorado",
            "Outro" = "outro"
          )
        ),
        textInput(ns("instituicao"), "Instituicao/departamento/laboratorio"),
        textInput(ns("orientador"), "Professor/orientador"),
        textAreaInput(ns("observacoes"), "Observacoes", rows = 4)
      )
    ),
    tags$hr(),
    mod_amostras_ui(ns("amostras")),
    tags$hr(),
    bslib::layout_columns(
      col_widths = c(8, 4),
      div(
        h3("Revisao"),
        div(class = "review-box", verbatimTextOutput(ns("resumo")))
      ),
      div(
        br(),
        actionButton(ns("enviar"), "Enviar solicitacao", class = "btn btn-primary"),
        br(), br(),
        uiOutput(ns("confirmacao"))
      )
    )
  )
}

mod_solicitante_server <- function(id, app_config, store) {
  moduleServer(id, function(input, output, session) {
    samples <- mod_amostras_server("amostras", app_config)
    last_submission_id <- reactiveVal("")

    request_payload <- reactive({
      list(
        solicitante = list(
          nome_solicitante = input$nome_solicitante,
          email = input$email,
          telefone = input$telefone,
          cpf_cnpj = input$cpf_cnpj,
          endereco = input$endereco,
          cidade_solicitante = input$cidade_solicitante,
          uf_solicitante = input$uf_solicitante,
          vinculo = input$vinculo,
          instituicao = input$instituicao,
          orientador = input$orientador,
          observacoes = input$observacoes
        ),
        amostras = samples()
      )
    })

    output$resumo <- renderPrint({
      payload <- request_payload()
      cat("Solicitante:", payload$solicitante$nome_solicitante %||% "", "\n")
      cat("Contato:", payload$solicitante$email %||% "", payload$solicitante$telefone %||% "", "\n")
      cat("Amostras cadastradas:", length(payload$amostras), "\n\n")

      for (sample in payload$amostras) {
        cat(sample$referencia_amostra, "-", sample$tipo_material, "\n")
        cat("Municipio:", sample$municipio_amostra, "/", sample$uf_amostra, "\n")
        cat("Analises:", paste(sample$analises_nomes, collapse = "; "), "\n\n")
      }
    })

    observeEvent(input$enviar, {
      payload <- request_payload()

      if (!nzchar(payload$solicitante$nome_solicitante %||% "")) {
        showNotification("Informe o nome do solicitante.", type = "error")
        return()
      }

      if (!length(payload$amostras)) {
        showNotification("Adicione pelo menos uma amostra.", type = "error")
        return()
      }

      request_id <- next_request_id()
      current <- store()

      new_request <- data.frame(
        solicitacao_id = request_id,
        data_hora_envio = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        nome_solicitante = payload$solicitante$nome_solicitante,
        email = payload$solicitante$email,
        telefone = payload$solicitante$telefone,
        cidade_solicitante = payload$solicitante$cidade_solicitante,
        status_interno = "Recebida",
        stringsAsFactors = FALSE
      )

      new_samples <- do.call(rbind, lapply(seq_along(payload$amostras), function(index) {
        sample <- payload$amostras[[index]]
        data.frame(
          amostra_id = next_sample_id(index),
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
          grupos_analise = paste(sample$grupos_analise, collapse = ";"),
          carbonato_presente = sample$carbonato_presente,
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

      current$solicitacoes <- rbind(current$solicitacoes, new_request)
      current$amostras <- rbind(current$amostras, new_samples)
      if (!is.null(new_analyses)) {
        current$analises <- rbind(current$analises, new_analyses)
      }
      store(current)
      last_submission_id(request_id)

      showNotification("Solicitacao registrada no prototipo.", type = "message")
      output$confirmacao <- renderUI({
        div(class = "alert alert-success", paste("Solicitacao enviada:", request_id))
      })
    })

    exportTestValues(
      ultimo_envio_id = last_submission_id()
    )
  })
}
