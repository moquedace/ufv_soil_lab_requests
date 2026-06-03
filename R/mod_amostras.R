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
        leaflet::setView(lng = -42.8825, lat = -20.7546, zoom = 10) |>
        leaflet.extras::addSearchOSM(
          options = leaflet.extras::searchOptions(
            position = "topleft",
            textPlaceholder = "Buscar municipio, localidade ou referencia",
            zoom = 13,
            hideMarkerOnCollapse = TRUE,
            autoCollapse = TRUE
          )
        )
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
