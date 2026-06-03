mod_amostras_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Amostras"),
    div(
      class = "sample-card",
      bslib::layout_columns(
        col_widths = c(5, 3, 4),
        textInput(ns("referencia_amostra"), "Referencia da amostra"),
        selectInput(
          ns("tipo_material"),
          "Material",
          choices = c("Solo", "Vegetal", "Extrato/digestao", "Outro")
        ),
        selectInput(
          ns("laboratorio"),
          "Grupo de analise",
          choices = c(
            "Solo rotina" = "solo_rotina",
            "Vegetal" = "vegetal",
            "CHN" = "chn",
            "Absorcao atomica" = "absorcao_atomica",
            "ICP-OES" = "icp_oes"
          )
        )
      ),
      uiOutput(ns("analises_ui")),
      uiOutput(ns("chn_ui")),
      bslib::layout_columns(
        col_widths = c(8, 4),
        textInput(ns("busca_lugar"), "Buscar municipio, localidade, referencia ou digitar a coordenada"),
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
          actionButton(ns("usar_anterior"), "Usar local da ultima amostra"),
          actionButton(ns("adicionar"), "Adicionar amostra", class = "btn btn-primary")
        )
      )
    ),
    h4("Amostras adicionadas"),
    DT::DTOutput(ns("tabela_amostras"))
  )
}

mod_amostras_server <- function(id, app_config) {
  moduleServer(id, function(input, output, session) {
    marker <- reactiveVal(NULL)
    samples <- reactiveVal(list())

    output$analises_ui <- renderUI({
      choices <- analysis_choices(app_config, input$laboratorio)
      checkboxGroupInput(
        session$ns("analises"),
        "Analises solicitadas",
        choices = choices,
        selected = character()
      )
    })

    output$chn_ui <- renderUI({
      if (!identical(input$laboratorio, "chn")) {
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
          condition = sprintf("(input['%s'] || []).includes('carbono_organico_total')", session$ns("analises")),
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
      if (!nzchar(input$referencia_amostra %||% "")) {
        showNotification("Informe uma referencia para a amostra.", type = "error")
        return()
      }

      if (!nzchar(input$municipio_amostra %||% "")) {
        showNotification("Informe o municipio da amostra.", type = "error")
        return()
      }

      point <- marker()
      selected <- input$analises %||% character()
      choices <- analysis_choices(app_config, input$laboratorio)

      sample <- list(
        referencia_amostra = input$referencia_amostra,
        tipo_material = input$tipo_material,
        laboratorio = input$laboratorio,
        analises_ids = selected,
        analises_nomes = unname(names(choices)[match(selected, choices)]),
        municipio_amostra = input$municipio_amostra,
        uf_amostra = input$uf_amostra,
        localidade_descricao = input$localidade_descricao,
        latitude_wgs84 = if (is.null(point)) NA_real_ else point$lat,
        longitude_wgs84 = if (is.null(point)) NA_real_ else point$lng,
        tipo_localizacao = input$tipo_localizacao,
        carbonato_presente = if (identical(input$laboratorio, "chn")) input$carbonato_presente else NA_character_,
        pre_tratamento_necessario = "carbono_organico_total" %in% selected
      )

      samples(append(samples(), list(sample)))
      updateTextInput(session, "referencia_amostra", value = "")
      showNotification("Amostra adicionada.", type = "message")
    })

    output$tabela_amostras <- DT::renderDT({
      current <- samples()
      if (!length(current)) {
        return(data.frame(Mensagem = "Nenhuma amostra adicionada."))
      }

      data.frame(
        Referencia = vapply(current, `[[`, character(1), "referencia_amostra"),
        Material = vapply(current, `[[`, character(1), "tipo_material"),
        Municipio = vapply(current, `[[`, character(1), "municipio_amostra"),
        UF = vapply(current, `[[`, character(1), "uf_amostra"),
        Localizacao = vapply(current, `[[`, character(1), "tipo_localizacao"),
        Analises = vapply(current, function(sample) paste(sample$analises_nomes, collapse = "; "), character(1)),
        stringsAsFactors = FALSE
      )
    }, options = list(pageLength = 5, searching = FALSE))

    samples
  })
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
