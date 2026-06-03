mod_recepcao_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2(class = "section-title", "Recepcao de amostras"),
    p(class = "muted-help", "Painel inicial para consultar solicitacoes e exportar dados do prototipo."),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header("Solicitacoes"),
        DT::DTOutput(ns("solicitacoes"))
      ),
      bslib::card(
        bslib::card_header("Exportacao"),
        downloadButton(ns("baixar_csv"), "Baixar CSV"),
        downloadButton(ns("baixar_xlsx"), "Baixar XLSX"),
        tags$hr(),
        verbatimTextOutput(ns("resumo_store"))
      )
    ),
    bslib::card(
      bslib::card_header("Amostras e analises"),
      DT::DTOutput(ns("amostras"))
    )
  )
}

mod_recepcao_server <- function(id, app_config, store) {
  moduleServer(id, function(input, output, session) {
    output$solicitacoes <- DT::renderDT({
      store()$solicitacoes
    }, options = list(pageLength = 8))

    output$amostras <- DT::renderDT({
      flatten_store(store())
    }, options = list(pageLength = 8, scrollX = TRUE))

    output$resumo_store <- renderPrint({
      current <- store()
      cat("Solicitacoes:", nrow(current$solicitacoes), "\n")
      cat("Amostras:", nrow(current$amostras), "\n")
      cat("Analises por amostra:", nrow(current$analises), "\n")
    })

    output$baixar_csv <- downloadHandler(
      filename = function() paste0("solicitacoes_", Sys.Date(), ".csv"),
      content = function(file) write_export_csv(flatten_store(store()), file)
    )

    output$baixar_xlsx <- downloadHandler(
      filename = function() paste0("solicitacoes_", Sys.Date(), ".xlsx"),
      content = function(file) write_export_xlsx(flatten_store(store()), file)
    )
  })
}
