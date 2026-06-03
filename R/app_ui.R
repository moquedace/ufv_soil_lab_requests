app_ui <- function() {
  bslib::page_navbar(
    title = "Solicitacao de analises - DPS/UFV",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#7a4d2b",
      base_font = bslib::font_google("Inter")
    ),
    header = tags$head(
      tags$style(
        "
        .navbar {
          background: linear-gradient(90deg, #5a3822 0%, #7a4d2b 48%, #946432 100%) !important;
          box-shadow: 0 2px 12px rgba(45, 28, 17, .18);
        }
        .navbar-brand {
          font-weight: 800;
          color: #fff !important;
          letter-spacing: 0;
        }
        .navbar .nav-link {
          color: rgba(255, 255, 255, .86) !important;
          font-weight: 650;
        }
        .navbar .nav-link.active,
        .navbar .nav-link:focus,
        .navbar .nav-link:hover {
          color: #f2d58a !important;
        }
        .navbar-logos {
          display: flex;
          align-items: center;
          gap: .7rem;
          margin-left: 1rem;
          padding: .32rem .55rem;
          border-radius: 8px;
          background: rgba(255, 255, 255, .92);
          box-shadow: inset 0 0 0 1px rgba(255, 255, 255, .4);
        }
        .navbar-logo {
          display: block;
          max-height: 34px;
          max-width: 140px;
          object-fit: contain;
        }
        .navbar-logo-dps {
          max-height: 38px;
          max-width: 118px;
        }
        @media (max-width: 760px) {
          .navbar-logos {
            width: 100%;
            justify-content: center;
            margin: .55rem 0 0;
          }
          .navbar-logo {
            max-width: 132px;
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
    bslib::nav_panel(
      "Solicitar analise",
      div(class = "page-wrap", mod_solicitante_ui("solicitante"))
    ),
    bslib::nav_panel(
      "Recepcao",
      div(class = "page-wrap", mod_recepcao_ui("recepcao"))
    ),
    bslib::nav_spacer(),
    bslib::nav_item(
      div(
        class = "navbar-logos",
        tags$img(class = "navbar-logo", src = "img/logo_ufv.png", alt = "Universidade Federal de Vicosa"),
        tags$img(class = "navbar-logo navbar-logo-dps", src = "img/logo_dps_ufv.png", alt = "Departamento de Solos UFV")
      )
    )
  )
}
