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
        .brand-strip {
          max-width: 1180px;
          margin: 0 auto;
          padding: 1rem 1.25rem .5rem;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 1.5rem;
        }
        .brand-title {
          font-size: 1.1rem;
          font-weight: 700;
          color: #23332b;
          margin: 0;
          text-align: center;
        }
        .brand-subtitle {
          color: #5c6670;
          margin: .15rem 0 0;
          text-align: center;
        }
        .brand-logo {
          display: block;
          max-width: 220px;
          max-height: 58px;
          object-fit: contain;
        }
        .brand-logo-dps { max-width: 190px; }
        @media (max-width: 760px) {
          .brand-strip {
            flex-direction: column;
            align-items: center;
            gap: .75rem;
          }
          .brand-logo {
            max-width: 210px;
            max-height: 54px;
          }
        }
        .page-wrap { max-width: 1180px; margin: 0 auto; padding: 1.25rem; }
        .section-title { margin-bottom: .25rem; }
        .muted-help { color: #5c6670; font-size: .95rem; }
        .sample-card { border: 1px solid #d8dee4; border-radius: 8px; padding: 1rem; margin-bottom: 1rem; background: #fff; }
        .map-box { min-height: 420px; }
        .review-box { background: #f6f8fa; border-radius: 8px; padding: 1rem; }
        "
      )
    ),
    div(
      class = "brand-strip",
      tags$img(class = "brand-logo", src = "img/logo_ufv.png", alt = "Universidade Federal de Vicosa"),
      div(
        tags$p(class = "brand-title", "Sistema de solicitacao de analises"),
        tags$p(class = "brand-subtitle", "Departamento de Solos - UFV")
      ),
      tags$img(class = "brand-logo brand-logo-dps", src = "img/logo_dps_ufv.png", alt = "Departamento de Solos UFV")
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
