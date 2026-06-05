mod_amostras_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3(bsicons::bs_icon("eyedropper"), " Amostras"),
    div(
      class = "sample-card",
      bslib::layout_columns(
        col_widths = c(7, 5),
        textInput(ns("referencia_amostra"), "Referência da amostra"),
        selectInput(
          ns("tipo_material"),
          "Material",
          choices = c("Solo", "Vegetal", "Extrato/digestão", "Outro")
        )
      ),
      checkboxGroupInput(
        ns("grupos_analise"),
        "Grupos de análise para esta amostra",
        choices = c(
          "Solo rotina" = "solo_rotina",
          "Vegetal" = "vegetal",
          "CHN" = "chn",
          "Absorção atômica" = "absorcao_atomica",
          "ICP-OES" = "icp_oes"
        ),
        selected = "solo_rotina",
        inline = TRUE
      ),
      uiOutput(ns("analises_ui")),
      uiOutput(ns("vegetal_ui")),
      uiOutput(ns("chn_ui")),
      uiOutput(ns("aa_icp_ui")),
      bslib::layout_columns(
        col_widths = c(8, 4),
        div(
          textInput(ns("busca_lugar"), "Buscar município, localidade ou digitar coordenada"),
          p(
            class = "muted-help",
            "Exemplo: -22.7253, -47.6492 (latitude, longitude em decimal)."
          )
        ),
        div(
          br(),
          actionButton(ns("buscar_lugar"), "Buscar no mapa", class = "btn btn-outline-primary")
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 2, 6),
        textInput(ns("municipio_amostra"), "Município da amostra"),
        textInput(ns("uf_amostra"), "UF", value = "MG"),
        textInput(ns("localidade_descricao"), "Propriedade, comunidade, talhão ou referência")
      ),
      div(
        class = "muted-help",
        style = "margin-bottom:.6rem; font-size:.82rem;",
        tags$strong("Localização opcional."),
        " As coordenadas são salvas em graus decimais WGS84 e utilizadas exclusivamente para identificação do ponto de coleta das amostras."
      ),
      bslib::layout_columns(
        col_widths = c(8, 4),
        div(class = "map-box", leaflet::leafletOutput(ns("mapa"), height = 420)),
        div(
          radioButtons(
            ns("tipo_localizacao"),
            "O ponto marcado representa",
            choices = c(
              "Local exato da coleta" = "exata",
              "Local aproximado da coleta" = "aproximada",
              "Apenas município/região" = "municipio_regiao"
            ),
            selected = "aproximada"
          ),
          verbatimTextOutput(ns("coords")),
          actionButton(ns("usar_anterior"), "Usar local da última amostra", class = "btn btn-secondary"),
          uiOutput(ns("acao_amostra_ui"))
        )
      )
    ),
    h4("Amostras adicionadas"),
    tags$div(
      style = "display: none;",
      numericInput(ns("test_selected_sample_index"), "Indice de teste", value = NA_integer_),
      textInput(ns("test_selected_sample_indices"), "Indices de teste", value = ""),
      numericInput(ns("test_map_lat"), "Latitude de teste", value = NA_real_),
      numericInput(ns("test_map_lng"), "Longitude de teste", value = NA_real_)
    ),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      actionButton(ns("editar_amostra"), "Editar selecionada", class = "btn btn-secondary"),
      actionButton(ns("duplicar_amostra"), "Duplicar selecionada(s)", class = "btn btn-secondary"),
      actionButton(ns("aplicar_analises"), "Aplicar análises às selecionadas", class = "btn btn-secondary"),
      actionButton(ns("remover_amostra"), "Remover selecionada(s)", class = "btn btn-outline-primary")
    ),
    p(class = "muted-help", "Selecione uma ou mais linhas. Editar usa a primeira; duplicar, remover e aplicar análises agem em todas as selecionadas."),
    DT::DTOutput(ns("tabela_amostras"))
  )
}

mod_amostras_server <- function(id, app_config, reset_trigger = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    marker <- reactiveVal(NULL)
    samples <- reactiveVal(list())
    editing_index <- reactiveVal(NULL)

    observeEvent(reset_trigger(), {
      samples(list())
      editing_index(NULL)
      marker(NULL)
      updateTextInput(session, "referencia_amostra", value = "")
      updateTextInput(session, "municipio_amostra", value = "")
      updateTextInput(session, "localidade_descricao", value = "")
      updateCheckboxGroupInput(session, "grupos_analise", selected = "solo_rotina")
      leaflet::leafletProxy("mapa", session = session) |> leaflet::clearMarkers()
    }, ignoreInit = TRUE)

    output$acao_amostra_ui <- renderUI({
      if (is.null(editing_index())) {
        tagList(
          actionButton(session$ns("adicionar"), "Adicionar amostra", class = "btn btn-primary"),
          actionButton(session$ns("abrir_gerador"), "Gerar várias amostras", class = "btn btn-outline-primary")
        )
      } else {
        tagList(
          actionButton(session$ns("atualizar_amostra"), "Atualizar amostra", class = "btn btn-primary"),
          actionButton(session$ns("cancelar_edicao"), "Cancelar edicao", class = "btn btn-secondary")
        )
      }
    })

    output$analises_ui <- renderUI({
      groups <- input$grupos_analise %||% character()
      if (!length(groups)) {
        return(p(class = "muted-help", "Selecione pelo menos um grupo de analise."))
      }

      tagList(lapply(groups, \(group_id) {
        choices <- analysis_choices(app_config, group_id)
        group_name <- app_config$analises[[group_id]]$nome %||% group_id

        checkboxGroupInput(
          session$ns(paste0("analises_", group_id)),
          paste("Analises -", group_name),
          choices = choices,
          selected = character()
        )
      }))
    })

    output$vegetal_ui <- renderUI({
      if (!("vegetal" %in% (input$grupos_analise %||% character()))) {
        return(NULL)
      }

      tagList(
        selectInput(
          session$ns("tipo_amostra_vegetal"),
          "Tipo de amostra vegetal",
          choices = c(
            "Selecione..." = "",
            "Folha" = "folha",
            "Galho" = "galho",
            "Casca" = "casca",
            "Raiz" = "raiz",
            "Serrapilheira" = "serrapilheira",
            "Outro" = "outro"
          )
        ),
        textInput(session$ns("cultura_planta"), "Cultura/planta")
      )
    })

    output$chn_ui <- renderUI({
      if (!("chn" %in% (input$grupos_analise %||% character()))) {
        return(NULL)
      }

      tagList(
        radioButtons(
          session$ns("carbonato_presente"),
          "Presença ou suspeita de carbono proveniente de carbonato",
          choices = c("Sim" = "sim", "Não" = "nao", "Não sei" = "nao_sei"),
          inline = TRUE
        ),
        conditionalPanel(
          condition = sprintf("(input['%s'] || []).includes('carbono_organico_total')", session$ns("analises_chn")),
          div(
            class = "alert alert-warning",
            "Carbono orgânico total requer pré-tratamento da amostra antes da determinação."
          )
        ),
        bslib::layout_columns(
          numericInput(session$ns("percentual_c_estimado"), "%C estimado (opcional)", value = NA, min = 0, max = 100, step = 0.1),
          numericInput(session$ns("percentual_n_estimado"), "%N estimado (opcional)", value = NA, min = 0, max = 100, step = 0.1)
        ),
        textInput(session$ns("numero_registro_projeto"), "N° de registro do projeto (opcional)")
      )
    })

    output$aa_icp_ui <- renderUI({
      groups <- input$grupos_analise %||% character()
      is_aa  <- "absorcao_atomica" %in% groups
      is_icp <- "icp_oes" %in% groups

      if (!is_aa && !is_icp) {
        return(NULL)
      }

      tagList(
        if (is_icp) {
          div(
            class = "info-soil",
            tags$strong("Equipamento: ICP-OES OPTIMA 8300."),
            " Amostras devem ser entregues em solução após digestão. Consulte o laboratório para dúvidas sobre preparo."
          )
        },
        textInput(session$ns("elementos_aa_icp"), "Elementos a determinar",
          placeholder = "Ex: Ca, Mg, Fe, Mn, Cu, Zn"),
        textInput(session$ns("tipo_digestao"), "Tipo de digestão realizada",
          placeholder = "Ex: nítrico-perclórica, água régia, DTPA"),
        bslib::layout_columns(
          numericInput(session$ns("volume_apos_digestao"), "Volume após digestão (mL)", value = NA, min = 0),
          numericInput(session$ns("aliquota"), "Alíquota (mL)", value = NA, min = 0)
        ),
        bslib::layout_columns(
          textInput(session$ns("diluicao"), "Diluição", placeholder = "Ex: 1:10"),
          numericInput(session$ns("volume_final"), "Volume final (mL)", value = NA, min = 0)
        ),
        selectInput(
          session$ns("departamento_origem"),
          "Departamento de origem",
          choices = c("DPS - Solos UFV" = "dps", "Outro departamento/instituição" = "outro")
        ),
        radioButtons(
          session$ns("projeto_registrado"),
          "Projeto de pesquisa registrado?",
          choices = c("Sim" = "sim", "Não" = "nao"),
          selected = "nao",
          inline = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'sim'", session$ns("projeto_registrado")),
          textInput(session$ns("numero_registro_projeto_aa"), "N° de registro do projeto")
        )
      )
    })

    output$mapa <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satélite") |>
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "Mapa") |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.PositronOnlyLabels,
          group = "Rótulos",
          options = leaflet::tileOptions(opacity = 0.85)
        ) |>
        leaflet::setView(lng = -42.8825, lat = -20.7546, zoom = 10) |>
        leaflet::addLayersControl(
          baseGroups = c("Satélite", "Mapa"),
          overlayGroups = "Rótulos",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::showGroup("Rótulos")
    })

    observeEvent(input$buscar_lugar, {
      query <- trimws(input$busca_lugar %||% "")
      if (!nzchar(query)) {
        showNotification("Digite um municipio, localidade ou referencia para buscar.", type = "warning")
        return()
      }

      result <- parse_decimal_coordinate(query)
      if (is.null(result)) {
        result <- geocode_osm(query)
      }

      if (is.null(result)) {
        showNotification("Nao encontrei esse local. Tente incluir municipio, UF, Brasil ou coordenada decimal.", type = "warning")
        return()
      }

      leaflet::leafletProxy("mapa") |>
        leaflet::setView(lng = result$lon, lat = result$lat, zoom = 13)

      showNotification(paste("Mapa centralizado em:", result$label), type = "message")
    })

    observeEvent(input$mapa_click, {
      marker(input$mapa_click)
      leaflet::leafletProxy("mapa") |>
        leaflet::clearMarkers() |>
        leaflet::addMarkers(
          lng = input$mapa_click$lng,
          lat = input$mapa_click$lat,
          popup = "Local da amostra"
        )
    })

    output$coords <- renderPrint({
      point <- marker()
      if (is.null(point)) {
        cat("Nenhum ponto marcado no mapa.")
      } else {
        cat("Latitude WGS84:", round(point$lat, 6), "\n")
        cat("Longitude WGS84:", round(point$lng, 6), "\n")
      }
    })

    observeEvent(input$usar_anterior, {
      current <- samples()
      if (!length(current)) {
        showNotification("Ainda nao ha amostra anterior.", type = "warning")
        return()
      }

      last <- current[[length(current)]]
      updateTextInput(session, "municipio_amostra", value = last$municipio_amostra)
      updateTextInput(session, "uf_amostra", value = last$uf_amostra)
      updateTextInput(session, "localidade_descricao", value = last$localidade_descricao)
      updateRadioButtons(session, "tipo_localizacao", selected = last$tipo_localizacao)

      if (!is.na(last$latitude_wgs84) && !is.na(last$longitude_wgs84)) {
        point <- list(lat = last$latitude_wgs84, lng = last$longitude_wgs84)
        marker(point)
        leaflet::leafletProxy("mapa") |>
          leaflet::clearMarkers() |>
          leaflet::setView(lng = point$lng, lat = point$lat, zoom = 14) |>
          leaflet::addMarkers(lng = point$lng, lat = point$lat, popup = "Local da amostra")
      }
    })

    observeEvent(input$adicionar, {
      sample <- build_sample_from_inputs(input, app_config, current_marker(marker(), input))
      if (!validate_sample_for_ui(sample)) {
        return()
      }

      samples(append(samples(), list(sample)))
      updateTextInput(session, "referencia_amostra", value = "")
      showNotification("Amostra adicionada.", type = "message")
    })

    gerador_refs <- reactive({
      build_sample_references(
        mode = input$gerar_modo %||% "numerico",
        prefixo = input$gerar_prefixo %||% "",
        inicio = input$gerar_inicio,
        fim = input$gerar_fim,
        digitos = input$gerar_digitos %||% 3,
        sufixo = input$gerar_sufixo %||% "",
        profundidades = split_lines(input$gerar_profundidades %||% ""),
        lista = split_lines(input$gerar_lista %||% "")
      )
    })

    output$gerar_preview <- renderText({
      preview_references(gerador_refs())
    })

    observeEvent(input$abrir_gerador, {
      ns <- session$ns
      showModal(modalDialog(
        title = "Gerar várias amostras",
        size = "l",
        easyClose = TRUE,
        p(
          class = "muted-help",
          "As amostras serão criadas com o material, grupos de análise, análises e ",
          "localização preenchidos no formulário (o molde). Apenas a referência muda."
        ),
        radioButtons(
          ns("gerar_modo"),
          "Como gerar as referências?",
          choices = c(
            "Numérico (prefixo + intervalo)" = "numerico",
            "Por camadas (pontos × profundidades)" = "camadas",
            "Colar lista (uma por linha)" = "lista"
          ),
          selected = "numerico"
        ),
        conditionalPanel(
          condition = sprintf("['numerico','camadas'].includes(input['%s'])", ns("gerar_modo")),
          bslib::layout_columns(
            col_widths = c(4, 3, 3, 2),
            textInput(ns("gerar_prefixo"), "Prefixo", value = "P"),
            numericInput(ns("gerar_inicio"), "Início", value = 1, min = 0),
            numericInput(ns("gerar_fim"), "Fim", value = 10, min = 0),
            numericInput(ns("gerar_digitos"), "Dígitos", value = 3, min = 1, max = 6)
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'numerico'", ns("gerar_modo")),
          textInput(ns("gerar_sufixo"), "Sufixo (opcional)", value = "")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'camadas'", ns("gerar_modo")),
          textAreaInput(ns("gerar_profundidades"), "Profundidades/camadas (uma por linha)", value = "0-20\n20-40", rows = 3)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'lista'", ns("gerar_modo")),
          textAreaInput(ns("gerar_lista"), "Referências (uma por linha)", rows = 6, placeholder = "Amostra A\nAmostra B")
        ),
        tags$hr(),
        div(class = "review-box", textOutput(ns("gerar_preview"))),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("gerar_confirmar"), "Gerar amostras", class = "btn btn-primary")
        )
      ))
    })

    observeEvent(input$gerar_confirmar, {
      refs <- gerador_refs()
      if (!length(refs)) {
        showNotification("Defina ao menos uma referência para gerar.", type = "error")
        return()
      }

      molde <- build_sample_from_inputs(input, app_config, current_marker(marker(), input))
      if (!nzchar(molde$municipio_amostra %||% "")) {
        showNotification("Informe o município no formulário antes de gerar.", type = "error")
        return()
      }
      if (!nrow(molde$analises)) {
        showNotification("Marque ao menos uma análise no formulário antes de gerar.", type = "error")
        return()
      }

      novas <- lapply(refs, function(ref) {
        m <- molde
        m$referencia_amostra <- ref
        m
      })
      samples(c(samples(), novas))
      removeModal()
      showNotification(sprintf("%d amostras geradas.", length(refs)), type = "message")
    })

    observeEvent(input$editar_amostra, {
      selected <- selected_sample_index(input)
      current <- samples()

      if (!length(selected) || !length(current)) {
        showNotification("Selecione uma amostra para editar.", type = "warning")
        return()
      }

      editing_index(selected[1])
      load_sample_into_form(session, current[[selected[1]]], marker)
      showNotification("Edicao iniciada. Ajuste os campos e clique em atualizar.", type = "message")
    })

    observeEvent(input$atualizar_amostra, {
      index <- editing_index()
      if (is.null(index)) {
        return()
      }

      sample <- build_sample_from_inputs(input, app_config, current_marker(marker(), input))
      if (!validate_sample_for_ui(sample)) {
        return()
      }

      samples(replace_sample_at(samples(), index, sample))
      editing_index(NULL)
      updateTextInput(session, "referencia_amostra", value = "")
      showNotification("Amostra atualizada.", type = "message")
    })

    observeEvent(input$cancelar_edicao, {
      editing_index(NULL)
      showNotification("Edicao cancelada.", type = "message")
    })

    observeEvent(input$duplicar_amostra, {
      selected <- selected_sample_indices(input)
      current <- samples()

      if (!length(selected) || !length(current)) {
        showNotification("Selecione ao menos uma amostra para duplicar.", type = "warning")
        return()
      }

      selected <- selected[selected >= 1 & selected <= length(current)]
      novas <- lapply(selected, function(i) {
        d <- current[[i]]
        d$referencia_amostra <- paste(d$referencia_amostra, "copia")
        d
      })
      samples(c(current, novas))
      showNotification(sprintf("%d amostra(s) duplicada(s).", length(novas)), type = "message")
    })

    observeEvent(input$remover_amostra, {
      selected <- selected_sample_indices(input)
      current <- samples()

      if (!length(selected) || !length(current)) {
        showNotification("Selecione ao menos uma amostra para remover.", type = "warning")
        return()
      }

      restante <- remove_samples_at(current, selected)
      removidas <- length(current) - length(restante)
      samples(restante)
      showNotification(sprintf("%d amostra(s) removida(s).", removidas), type = "message")
    })

    output$aplicar_analises_ui <- renderUI({
      ns <- session$ns
      groups <- input$aplicar_grupos %||% character()
      if (!length(groups)) {
        return(p(class = "muted-help", "Selecione ao menos um grupo de análise."))
      }
      tagList(lapply(groups, function(group_id) {
        choices <- analysis_choices(app_config, group_id)
        group_name <- app_config$analises[[group_id]]$nome %||% group_id
        checkboxGroupInput(
          ns(paste0("aplicar_analises_", group_id)),
          paste("Análises -", group_name),
          choices = choices,
          selected = character()
        )
      }))
    })

    observeEvent(input$aplicar_analises, {
      ns <- session$ns
      selected <- selected_sample_indices(input)
      if (!length(selected)) {
        showNotification("Selecione ao menos uma amostra na tabela.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Aplicar análises às amostras selecionadas",
        size = "l",
        easyClose = TRUE,
        p(class = "muted-help", sprintf("%d amostra(s) selecionada(s) receberão as análises marcadas abaixo.", length(selected))),
        checkboxGroupInput(
          ns("aplicar_grupos"),
          "Grupos de análise",
          choices = c(
            "Solo rotina" = "solo_rotina",
            "Vegetal" = "vegetal",
            "CHN" = "chn",
            "Absorção atômica" = "absorcao_atomica",
            "ICP-OES" = "icp_oes"
          ),
          inline = TRUE
        ),
        uiOutput(ns("aplicar_analises_ui")),
        radioButtons(
          ns("aplicar_modo"),
          "Como aplicar?",
          choices = c(
            "Acrescentar às análises existentes" = "acrescentar",
            "Substituir as análises existentes" = "substituir"
          ),
          selected = "acrescentar"
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("aplicar_confirmar"), "Aplicar", class = "btn btn-primary")
        )
      ))
    })

    observeEvent(input$aplicar_confirmar, {
      selected <- selected_sample_indices(input)
      groups <- input$aplicar_grupos %||% character()
      new_analyses <- collect_analyses_from(input, app_config, groups, "aplicar_analises_")

      if (!length(selected)) {
        showNotification("Nenhuma amostra selecionada.", type = "warning")
        return()
      }
      if (!nrow(new_analyses)) {
        showNotification("Marque ao menos uma análise para aplicar.", type = "error")
        return()
      }

      samples(apply_analyses_to_samples(samples(), selected, new_analyses, input$aplicar_modo %||% "acrescentar"))
      removeModal()
      showNotification(sprintf("Análises aplicadas a %d amostra(s).", length(unique(selected))), type = "message")
    })

    output$tabela_amostras <- DT::renderDT({
      current <- samples()
      if (!length(current)) {
        return(data.frame(Mensagem = "Nenhuma amostra adicionada."))
      }

      grupos_str <- vapply(current, \(s) { paste(s$grupos_analise, collapse = "; ") }, character(1))
      analises_str <- vapply(current, \(s) { paste(s$analises_nomes, collapse = "; ") }, character(1))
      data.frame(
        Referencia = vapply(current, `[[`, character(1), "referencia_amostra"),
        Material = vapply(current, `[[`, character(1), "tipo_material"),
        Grupos = grupos_str,
        Municipio = vapply(current, `[[`, character(1), "municipio_amostra"),
        UF = vapply(current, `[[`, character(1), "uf_amostra"),
        Localizacao = vapply(current, `[[`, character(1), "tipo_localizacao"),
        Analises = analises_str,
        stringsAsFactors = FALSE
      )
    }, selection = "multiple", options = list(pageLength = 5, searching = FALSE))

    exportTestValues(
      editing_index = editing_index(),
      samples_count = length(samples())
    )

    samples
  })
}

split_lines <- function(text) {
  if (is.null(text) || !length(text) || !nzchar(text)) {
    return(character())
  }
  parts <- strsplit(text, "\r?\n")[[1]]
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

build_sample_references <- function(mode = "numerico", prefixo = "", inicio = NA,
                                    fim = NA, digitos = 3, sufixo = "",
                                    profundidades = character(), lista = character(),
                                    limite = 1000L) {
  digitos <- suppressWarnings(as.integer(digitos))
  if (is.na(digitos) || digitos < 1) digitos <- 1L

  seq_pontos <- function() {
    i <- suppressWarnings(as.integer(inicio))
    f <- suppressWarnings(as.integer(fim))
    if (is.na(i) || is.na(f) || f < i) {
      return(character())
    }
    paste0(prefixo, formatC(seq.int(i, f), width = digitos, flag = "0"))
  }

  refs <- switch(
    mode,
    numerico = paste0(seq_pontos(), sufixo),
    camadas = {
      pontos <- seq_pontos()
      profs <- trimws(profundidades)
      profs <- profs[nzchar(profs)]
      if (!length(pontos) || !length(profs)) {
        character()
      } else {
        grid <- expand.grid(d = profs, p = pontos, stringsAsFactors = FALSE)
        paste(grid$p, grid$d)
      }
    },
    lista = {
      l <- trimws(lista)
      l[nzchar(l)]
    },
    character()
  )

  refs <- refs[nzchar(refs)]
  if (length(refs) > limite) {
    refs <- refs[seq_len(limite)]
  }
  refs
}

preview_references <- function(refs) {
  n <- length(refs)
  if (!n) {
    return("Nenhuma referência gerada — preencha os campos acima.")
  }
  amostra <- if (n <= 6) {
    paste(refs, collapse = ", ")
  } else {
    paste0(paste(utils::head(refs, 3), collapse = ", "), " … ", paste(utils::tail(refs, 2), collapse = ", "))
  }
  sprintf("Serão geradas %d amostras: %s", n, amostra)
}

remove_sample_at <- function(samples, index) {
  if (!length(samples) || index < 1 || index > length(samples)) {
    return(samples)
  }

  if (length(samples) == 1) {
    return(list())
  }

  samples[-index]
}

duplicate_sample_at <- function(samples, index) {
  if (!length(samples) || index < 1 || index > length(samples)) {
    return(samples)
  }

  duplicated <- samples[[index]]
  duplicated$referencia_amostra <- paste(duplicated$referencia_amostra, "copia")
  append(samples, list(duplicated))
}

selected_sample_index <- function(input) {
  selected <- input$tabela_amostras_rows_selected
  if (length(selected)) {
    return(selected)
  }

  test_selected <- input$test_selected_sample_index
  if (!is.null(test_selected) && length(test_selected) && !is.na(test_selected)) {
    return(as.integer(test_selected))
  }

  integer()
}

selected_sample_indices <- function(input) {
  selected <- input$tabela_amostras_rows_selected
  if (length(selected)) {
    return(as.integer(selected))
  }

  multi <- input$test_selected_sample_indices
  if (!is.null(multi) && length(multi) && nzchar(multi)) {
    nums <- suppressWarnings(as.integer(trimws(strsplit(multi, ",")[[1]])))
    nums <- nums[!is.na(nums)]
    if (length(nums)) {
      return(nums)
    }
  }

  single <- input$test_selected_sample_index
  if (!is.null(single) && length(single) && !is.na(single)) {
    return(as.integer(single))
  }

  integer()
}

remove_samples_at <- function(samples, indices) {
  indices <- indices[indices >= 1 & indices <= length(samples)]
  if (!length(indices)) {
    return(samples)
  }
  keep <- setdiff(seq_along(samples), unique(indices))
  samples[keep]
}

apply_analyses_to_samples <- function(samples, indices, new_analyses, mode = "acrescentar") {
  indices <- unique(indices[indices >= 1 & indices <= length(samples)])
  if (!length(indices) || !nrow(new_analyses)) {
    return(samples)
  }

  for (i in indices) {
    s <- samples[[i]]
    if (identical(mode, "substituir")) {
      combined <- new_analyses
    } else {
      combined <- rbind(s$analises, new_analyses)
      combined <- combined[!duplicated(combined[c("laboratorio", "analise_id")]), , drop = FALSE]
    }
    s$analises <- combined
    s$analises_ids <- combined$analise_id
    s$analises_nomes <- combined$analise_nome
    s$grupos_analise <- unique(combined$laboratorio)
    samples[[i]] <- s
  }

  samples
}

collect_analyses_from <- function(input, app_config, groups, prefix) {
  rows <- lapply(groups, function(group_id) {
    selected <- input[[paste0(prefix, group_id)]] %||% character()
    if (!length(selected)) {
      return(NULL)
    }
    choices <- analysis_choices(app_config, group_id)
    data.frame(
      laboratorio = group_id,
      analise_id = selected,
      analise_nome = unname(names(choices)[match(selected, choices)]),
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      laboratorio = character(),
      analise_id = character(),
      analise_nome = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

replace_sample_at <- function(samples, index, sample) {
  if (!length(samples) || index < 1 || index > length(samples)) {
    return(samples)
  }

  samples[[index]] <- sample
  samples
}

current_marker <- function(point, input) {
  if (!is.null(point)) {
    return(point)
  }

  test_point_from_inputs(input)
}

test_point_from_inputs <- function(input) {
  lat <- input$test_map_lat
  lng <- input$test_map_lng

  if (is.null(lat) || is.null(lng) || is.na(lat) || is.na(lng)) {
    return(NULL)
  }

  list(lat = lat, lng = lng)
}

build_sample_from_inputs <- function(input, app_config, point) {
  groups <- input$grupos_analise %||% character()
  analyses <- collect_selected_analyses(input, app_config, groups)

  is_vegetal <- "vegetal" %in% groups
  is_chn     <- "chn" %in% groups
  is_aa_icp  <- "absorcao_atomica" %in% groups || "icp_oes" %in% groups

  tipo_amostra_vegetal <- if (is_vegetal) (input$tipo_amostra_vegetal %||% "") else NA_character_
  cultura_planta <- if (is_vegetal) (input$cultura_planta %||% "") else NA_character_
  carbonato_presente      <- if (is_chn) input$carbonato_presente else NA_character_
  percentual_c_estimado   <- if (is_chn) input$percentual_c_estimado else NA_real_
  percentual_n_estimado   <- if (is_chn) input$percentual_n_estimado else NA_real_
  numero_registro_projeto    <- if (is_chn) (input$numero_registro_projeto %||% "") else NA_character_
  elementos_aa_icp           <- if (is_aa_icp) (input$elementos_aa_icp %||% "") else NA_character_
  tipo_digestao              <- if (is_aa_icp) (input$tipo_digestao %||% "") else NA_character_
  volume_apos_digestao       <- if (is_aa_icp) input$volume_apos_digestao else NA_real_
  aliquota                   <- if (is_aa_icp) input$aliquota else NA_real_
  diluicao                   <- if (is_aa_icp) (input$diluicao %||% "") else NA_character_
  volume_final               <- if (is_aa_icp) input$volume_final else NA_real_
  departamento_origem        <- if (is_aa_icp) (input$departamento_origem %||% "") else NA_character_
  projeto_registrado         <- if (is_aa_icp) (input$projeto_registrado %||% "nao") else NA_character_
  numero_registro_projeto_aa <- if (is_aa_icp && identical(input$projeto_registrado, "sim")) (input$numero_registro_projeto_aa %||% "") else NA_character_

  lat_wgs84 <- if (is.null(point)) NA_real_ else point$lat
  lng_wgs84 <- if (is.null(point)) NA_real_ else point$lng

  list(
    referencia_amostra = input$referencia_amostra,
    tipo_material = input$tipo_material,
    grupos_analise = groups,
    analises = analyses,
    analises_ids = analyses$analise_id,
    analises_nomes = analyses$analise_nome,
    municipio_amostra = input$municipio_amostra,
    uf_amostra = input$uf_amostra,
    localidade_descricao = input$localidade_descricao,
    latitude_wgs84 = lat_wgs84,
    longitude_wgs84 = lng_wgs84,
    tipo_localizacao = input$tipo_localizacao,
    tipo_amostra_vegetal = tipo_amostra_vegetal,
    cultura_planta = cultura_planta,
    carbonato_presente = carbonato_presente,
    percentual_c_estimado = percentual_c_estimado,
    percentual_n_estimado = percentual_n_estimado,
    numero_registro_projeto = numero_registro_projeto,
    elementos_aa_icp = elementos_aa_icp,
    tipo_digestao = tipo_digestao,
    volume_apos_digestao = volume_apos_digestao,
    aliquota = aliquota,
    diluicao = diluicao,
    volume_final = volume_final,
    departamento_origem = departamento_origem,
    projeto_registrado = projeto_registrado,
    numero_registro_projeto_aa = numero_registro_projeto_aa,
    pre_tratamento_necessario = "carbono_organico_total" %in% analyses$analise_id
  )
}

validate_sample_for_ui <- function(sample) {
  if (!nzchar(sample$referencia_amostra %||% "")) {
    showNotification("Informe uma referencia para a amostra.", type = "error")
    return(FALSE)
  }

  if (!nzchar(sample$municipio_amostra %||% "")) {
    showNotification("Informe o municipio da amostra.", type = "error")
    return(FALSE)
  }

  if (!length(sample$grupos_analise)) {
    showNotification("Selecione pelo menos um grupo de analise.", type = "error")
    return(FALSE)
  }

  if (!nrow(sample$analises)) {
    showNotification("Marque pelo menos uma analise para a amostra.", type = "error")
    return(FALSE)
  }

  TRUE
}

load_sample_into_form <- function(session, sample, marker) {
  updateTextInput(session, "referencia_amostra", value = sample$referencia_amostra)
  updateSelectInput(session, "tipo_material", selected = sample$tipo_material)
  updateCheckboxGroupInput(session, "grupos_analise", selected = sample$grupos_analise)
  updateTextInput(session, "municipio_amostra", value = sample$municipio_amostra)
  updateTextInput(session, "uf_amostra", value = sample$uf_amostra)
  updateTextInput(session, "localidade_descricao", value = sample$localidade_descricao)
  updateRadioButtons(session, "tipo_localizacao", selected = sample$tipo_localizacao)

  if (!is.na(sample$latitude_wgs84) && !is.na(sample$longitude_wgs84)) {
    point <- list(lat = sample$latitude_wgs84, lng = sample$longitude_wgs84)
    marker(point)
    leaflet::leafletProxy("mapa", session = session) |>
      leaflet::clearMarkers() |>
      leaflet::setView(lng = point$lng, lat = point$lat, zoom = 14) |>
      leaflet::addMarkers(lng = point$lng, lat = point$lat, popup = "Local da amostra")
  }

  later::later(function() {
    lapply(sample$grupos_analise, \(group_id) {
      updateCheckboxGroupInput(
        session,
        paste0("analises_", group_id),
        selected = sample$analises$analise_id[sample$analises$laboratorio == group_id]
      )
    })

    if ("vegetal" %in% sample$grupos_analise) {
      updateSelectInput(session, "tipo_amostra_vegetal", selected = sample$tipo_amostra_vegetal %||% "")
      updateTextInput(session, "cultura_planta", value = sample$cultura_planta %||% "")
    }

    if ("absorcao_atomica" %in% sample$grupos_analise || "icp_oes" %in% sample$grupos_analise) {
      updateTextInput(session, "elementos_aa_icp",    value = sample$elementos_aa_icp %||% "")
      updateTextInput(session, "tipo_digestao",        value = sample$tipo_digestao %||% "")
      updateNumericInput(session, "volume_apos_digestao", value = sample$volume_apos_digestao %||% NA)
      updateNumericInput(session, "aliquota",          value = sample$aliquota %||% NA)
      updateTextInput(session, "diluicao",             value = sample$diluicao %||% "")
      updateNumericInput(session, "volume_final",      value = sample$volume_final %||% NA)
      updateSelectInput(session, "departamento_origem", selected = sample$departamento_origem %||% "dps")
      updateRadioButtons(session, "projeto_registrado", selected = sample$projeto_registrado %||% "nao")
      updateTextInput(session, "numero_registro_projeto_aa", value = sample$numero_registro_projeto_aa %||% "")
    }

    if ("chn" %in% sample$grupos_analise) {
      updateRadioButtons(session, "carbonato_presente", selected = sample$carbonato_presente)
      updateNumericInput(session, "percentual_c_estimado", value = sample$percentual_c_estimado %||% NA)
      updateNumericInput(session, "percentual_n_estimado", value = sample$percentual_n_estimado %||% NA)
      updateTextInput(session, "numero_registro_projeto", value = sample$numero_registro_projeto %||% "")
    }
  }, delay = 0.1)
}

collect_selected_analyses <- function(input, app_config, groups) {
  rows <- lapply(groups, \(group_id) {
    selected <- input[[paste0("analises_", group_id)]] %||% character()
    if (!length(selected)) {
      return(NULL)
    }

    choices <- analysis_choices(app_config, group_id)
    data.frame(
      laboratorio = group_id,
      analise_id = selected,
      analise_nome = unname(names(choices)[match(selected, choices)]),
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      laboratorio = character(),
      analise_id = character(),
      analise_nome = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

geocode_osm <- function(query) {
  if (!requireNamespace("httr2", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    showNotification("Instale os pacotes 'httr2' e 'jsonlite' para usar a busca.", type = "error")
    return(NULL)
  }

  response <- tryCatch(
    httr2::request("https://nominatim.openstreetmap.org/search") |>
      httr2::req_url_query(
        q = query,
        format = "json",
        limit = 1,
        countrycodes = "br"
      ) |>
      httr2::req_headers(
        "User-Agent" = "ufv-soil-lab-requests/0.1 (Shiny prototype)"
      ) |>
      httr2::req_perform(),
    error = function(e) { NULL }
  )

  if (is.null(response) || httr2::resp_status(response) >= 300) {
    return(NULL)
  }

  data <- httr2::resp_body_json(response, simplifyVector = TRUE)
  if (!is.data.frame(data) || nrow(data) < 1) {
    return(NULL)
  }

  list(
    lat = as.numeric(data$lat[[1]]),
    lon = as.numeric(data$lon[[1]]),
    label = data$display_name[[1]]
  )
}

parse_decimal_coordinate <- function(query) {
  pattern <- "-?\\d+(?:[\\.,]\\d+)?"
  numbers <- regmatches(query, gregexpr(pattern, query))[[1]]

  if (length(numbers) < 2) {
    return(NULL)
  }

  lat <- as.numeric(gsub(",", ".", numbers[[1]], fixed = TRUE))
  lon <- as.numeric(gsub(",", ".", numbers[[2]], fixed = TRUE))

  if (is.na(lat) || is.na(lon) || abs(lat) > 90 || abs(lon) > 180) {
    return(NULL)
  }

  list(
    lat = lat,
    lon = lon,
    label = paste0("coordenada ", round(lat, 6), ", ", round(lon, 6))
  )
}
