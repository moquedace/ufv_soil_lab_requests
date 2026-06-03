mod_amostras_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Amostras"),
    div(
      class = "sample-card",
      bslib::layout_columns(
        col_widths = c(7, 5),
        textInput(ns("referencia_amostra"), "Referencia da amostra"),
        selectInput(
          ns("tipo_material"),
          "Material",
          choices = c("Solo", "Vegetal", "Extrato/digestao", "Outro")
        )
      ),
      checkboxGroupInput(
        ns("grupos_analise"),
        "Grupos de analise para esta amostra",
        choices = c(
          "Solo rotina" = "solo_rotina",
          "Vegetal" = "vegetal",
          "CHN" = "chn",
          "Absorcao atomica" = "absorcao_atomica",
          "ICP-OES" = "icp_oes"
        ),
        selected = "solo_rotina",
        inline = TRUE
      ),
      uiOutput(ns("analises_ui")),
      uiOutput(ns("chn_ui")),
      bslib::layout_columns(
        col_widths = c(8, 4),
        div(
          textInput(ns("busca_lugar"), "Buscar municipio, localidade, referencia ou digitar a coordenada"),
          p(
            class = "muted-help",
            "Exemplo de coordenada: -22.7253, -47.6492. Use latitude primeiro e longitude depois."
          )
        ),
        div(
          br(),
          actionButton(ns("buscar_lugar"), "Buscar no mapa", class = "btn btn-outline-primary")
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 2, 6),
        textInput(ns("municipio_amostra"), "Municipio da amostra"),
        textInput(ns("uf_amostra"), "UF", value = "MG"),
        textInput(ns("localidade_descricao"), "Propriedade, comunidade, talhao ou referencia")
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
              "Apenas municipio/regiao" = "municipio_regiao"
            ),
            selected = "aproximada"
          ),
          verbatimTextOutput(ns("coords")),
          actionButton(ns("usar_anterior"), "Usar local da ultima amostra", class = "btn btn-secondary"),
          uiOutput(ns("acao_amostra_ui"))
        )
      )
    ),
    h4("Amostras adicionadas"),
    tags$div(
      style = "display: none;",
      numericInput(ns("test_selected_sample_index"), "Indice de teste", value = NA_integer_)
    ),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      actionButton(ns("editar_amostra"), "Editar selecionada", class = "btn btn-secondary"),
      actionButton(ns("duplicar_amostra"), "Duplicar selecionada", class = "btn btn-secondary"),
      actionButton(ns("remover_amostra"), "Remover selecionada", class = "btn btn-outline-primary"),
      p(class = "muted-help", "Selecione uma linha para editar, duplicar ou remover.")
    ),
    DT::DTOutput(ns("tabela_amostras"))
  )
}

mod_amostras_server <- function(id, app_config) {
  moduleServer(id, function(input, output, session) {
    marker <- reactiveVal(NULL)
    samples <- reactiveVal(list())
    editing_index <- reactiveVal(NULL)

    output$acao_amostra_ui <- renderUI({
      if (is.null(editing_index())) {
        actionButton(session$ns("adicionar"), "Adicionar amostra", class = "btn btn-primary")
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

      tagList(lapply(groups, function(group_id) {
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

    output$chn_ui <- renderUI({
      if (!("chn" %in% (input$grupos_analise %||% character()))) {
        return(NULL)
      }

      tagList(
        radioButtons(
          session$ns("carbonato_presente"),
          "Presenca ou suspeita de carbono proveniente de carbonato",
          choices = c("Sim" = "sim", "Nao" = "nao", "Nao sei" = "nao_sei"),
          inline = TRUE
        ),
        conditionalPanel(
          condition = sprintf("(input['%s'] || []).includes('carbono_organico_total')", session$ns("analises_chn")),
          div(
            class = "alert alert-warning",
            "Carbono organico total requer pre-tratamento da amostra antes da determinacao."
          )
        )
      )
    })

    output$mapa <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
        leaflet::setView(lng = -42.8825, lat = -20.7546, zoom = 10)
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
      sample <- build_sample_from_inputs(input, app_config, marker())
      if (!validate_sample_for_ui(sample)) {
        return()
      }

      samples(append(samples(), list(sample)))
      updateTextInput(session, "referencia_amostra", value = "")
      showNotification("Amostra adicionada.", type = "message")
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

      sample <- build_sample_from_inputs(input, app_config, marker())
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
      selected <- selected_sample_index(input)
      current <- samples()

      if (!length(selected) || !length(current)) {
        showNotification("Selecione uma amostra para duplicar.", type = "warning")
        return()
      }

      samples(duplicate_sample_at(current, selected[1]))
      showNotification("Amostra duplicada.", type = "message")
    })

    observeEvent(input$remover_amostra, {
      selected <- selected_sample_index(input)
      current <- samples()

      if (!length(selected) || !length(current)) {
        showNotification("Selecione uma amostra para remover.", type = "warning")
        return()
      }

      samples(remove_sample_at(current, selected[1]))
      showNotification("Amostra removida.", type = "message")
    })

    output$tabela_amostras <- DT::renderDT({
      current <- samples()
      if (!length(current)) {
        return(data.frame(Mensagem = "Nenhuma amostra adicionada."))
      }

      data.frame(
        Referencia = vapply(current, `[[`, character(1), "referencia_amostra"),
        Material = vapply(current, `[[`, character(1), "tipo_material"),
        Grupos = vapply(current, function(sample) paste(sample$grupos_analise, collapse = "; "), character(1)),
        Municipio = vapply(current, `[[`, character(1), "municipio_amostra"),
        UF = vapply(current, `[[`, character(1), "uf_amostra"),
        Localizacao = vapply(current, `[[`, character(1), "tipo_localizacao"),
        Analises = vapply(current, function(sample) paste(sample$analises_nomes, collapse = "; "), character(1)),
        stringsAsFactors = FALSE
      )
    }, selection = "single", options = list(pageLength = 5, searching = FALSE))

    exportTestValues(
      editing_index = editing_index(),
      samples_count = length(samples())
    )

    samples
  })
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

replace_sample_at <- function(samples, index, sample) {
  if (!length(samples) || index < 1 || index > length(samples)) {
    return(samples)
  }

  samples[[index]] <- sample
  samples
}

build_sample_from_inputs <- function(input, app_config, point) {
  groups <- input$grupos_analise %||% character()
  analyses <- collect_selected_analyses(input, app_config, groups)

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
    latitude_wgs84 = if (is.null(point)) NA_real_ else point$lat,
    longitude_wgs84 = if (is.null(point)) NA_real_ else point$lng,
    tipo_localizacao = input$tipo_localizacao,
    carbonato_presente = if ("chn" %in% groups) input$carbonato_presente else NA_character_,
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
    for (group_id in sample$grupos_analise) {
      updateCheckboxGroupInput(
        session,
        paste0("analises_", group_id),
        selected = sample$analises$analise_id[sample$analises$laboratorio == group_id]
      )
    }

    if ("chn" %in% sample$grupos_analise) {
      updateRadioButtons(session, "carbonato_presente", selected = sample$carbonato_presente)
    }
  }, delay = 0.1)
}

collect_selected_analyses <- function(input, app_config, groups) {
  rows <- lapply(groups, function(group_id) {
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
    error = function(error) NULL
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
