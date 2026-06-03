app_ui <- function() {
  bslib::page_navbar(
    title = "Solicitacao de analises - DPS/UFV",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#176b3a",
      base_font = bslib::font_google("Inter")
    ),
    header = tags$head(
      tags$style(
        "
        .navbar-brand { font-weight: 700; }
        .page-wrap { max-width: 1180px; margin: 0 auto; padding: 1.25rem; }
        .section-title { margin-bottom: .25rem; }
        .muted-help { color: #5c6670; font-size: .95rem; }
        .sample-card { border: 1px solid #d8dee4; border-radius: 8px; padding: 1rem; margin-bottom: 1rem; background: #fff; }
        .map-box { min-height: 420px; }
        .review-box { background: #f6f8fa; border-radius: 8px; padding: 1rem; }
        "
      )
    ),
    bslib::nav_panel(
      "Solicitar analise",
      div(class = "page-wrap", mod_solicitante_ui("solicitante"))
    ),
    bslib::nav_panel(
      "Recepcao",
      div(class = "page-wrap", mod_recepcao_ui("recepcao"))
    )
  )
}
