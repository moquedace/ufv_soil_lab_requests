app_ui <- function() {
  bslib::page_navbar(
    title = "Solicitacao de analises - DPS/UFV",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#6f4a2f",
      base_font = bslib::font_google("Inter")
    ),
    header = tags$head(
      tags$style(
        "
        :root {
          --soil-900: #2f241c;
          --soil-800: #443124;
          --soil-700: #5b3f2b;
          --soil-600: #6f4a2f;
          --soil-100: #efe6da;
          --leaf-700: #1f5b43;
          --leaf-600: #287253;
          --gold-500: #c9a227;
          --paper-50: #f8f6f2;
          --paper-100: #f1ede6;
          --line-soft: #ded6ca;
          --ink-900: #1f2523;
          --ink-600: #5d6761;
          --shadow-soft: 0 12px 30px rgba(47, 36, 28, .08);
        }
        body {
          background:
            linear-gradient(180deg, rgba(241, 237, 230, .72) 0%, rgba(248, 246, 242, .96) 260px),
            var(--paper-50);
          color: var(--ink-900);
        }
        .navbar {
          background: linear-gradient(90deg, var(--soil-900) 0%, var(--soil-700) 56%, var(--leaf-700) 100%) !important;
          box-shadow: 0 8px 24px rgba(47, 36, 28, .18);
          min-height: 72px;
          border-bottom: 1px solid rgba(255, 255, 255, .13);
        }
        .navbar .container-fluid {
          gap: .75rem;
        }
        .navbar-brand {
          font-weight: 800;
          color: #fff !important;
          letter-spacing: 0;
          font-size: 1.05rem;
        }
        .navbar .nav-link {
          color: rgba(255, 255, 255, .82) !important;
          font-weight: 700;
          border-radius: 8px;
          padding: .55rem .8rem;
          margin: 0 .05rem;
          transition: background-color .18s ease, color .18s ease;
        }
        .navbar .nav-link.active,
        .navbar .nav-link:focus,
        .navbar .nav-link:hover {
          background: rgba(255, 255, 255, .12);
          color: #fff !important;
        }
        .navbar .nav-link.active {
          box-shadow: inset 0 -3px 0 var(--gold-500);
        }
        a {
          color: var(--leaf-700);
        }
        a:hover {
          color: var(--soil-700);
        }
        .btn {
          border-radius: 8px;
          font-weight: 750;
          border-width: 1px;
          box-shadow: none !important;
        }
        .btn-primary {
          background: var(--leaf-700);
          border-color: var(--leaf-700);
          color: #fff;
        }
        .btn-primary:hover,
        .btn-primary:focus {
          background: #184a37;
          border-color: #184a37;
          color: #fff;
        }
        .btn-outline-primary {
          color: var(--soil-700);
          border-color: #b89f7b;
          background: #fffaf1;
        }
        .btn-outline-primary:hover,
        .btn-outline-primary:focus {
          background: var(--soil-700);
          border-color: var(--soil-700);
          color: #fff;
        }
        .btn-secondary,
        .btn-default {
          background: #ece3d6;
          border-color: #d8c6ae;
          color: var(--soil-800);
        }
        .btn-secondary:hover,
        .btn-secondary:focus,
        .btn-default:hover,
        .btn-default:focus {
          background: #dfd0bc;
          border-color: #cbb393;
          color: var(--soil-900);
        }
        .page-link {
          color: var(--soil-700);
          border-color: #e1d8ca;
        }
        .page-item.active .page-link {
          background: var(--leaf-700);
          border-color: var(--leaf-700);
          color: #fff;
        }
        .page-link:hover {
          color: var(--soil-900);
          background: var(--paper-100);
        }
        .navbar-logos {
          display: flex;
          align-items: center;
          gap: .8rem;
          margin-left: 1rem;
          padding: .34rem .65rem;
          border-radius: 8px;
          background: rgba(255, 255, 255, .94);
          box-shadow: 0 6px 18px rgba(47, 36, 28, .13), inset 0 0 0 1px rgba(255, 255, 255, .7);
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
        .page-wrap {
          max-width: 1220px;
          margin: 0 auto;
          padding: 1.55rem 1.25rem 2.5rem;
        }
        .section-title {
          margin-bottom: .2rem;
          color: var(--soil-900);
          font-weight: 850;
        }
        .section-title + .muted-help {
          margin-bottom: 1.25rem;
        }
        .muted-help {
          color: var(--ink-600);
          font-size: .94rem;
        }
        .card {
          border: 1px solid rgba(122, 94, 63, .16);
          border-radius: 8px;
          box-shadow: var(--shadow-soft);
          background: rgba(255, 255, 255, .94);
        }
        .card-header {
          background: linear-gradient(180deg, #fff 0%, #fbf8f2 100%);
          border-bottom: 1px solid rgba(122, 94, 63, .14);
          color: var(--soil-900);
          font-weight: 800;
        }
        .sample-card {
          border: 1px solid rgba(122, 94, 63, .18);
          border-radius: 8px;
          padding: 1.1rem;
          margin-bottom: 1.1rem;
          background: rgba(255, 255, 255, .94);
          box-shadow: var(--shadow-soft);
        }
        .sample-card > .bslib-grid,
        .card-body > .bslib-grid {
          row-gap: .8rem;
        }
        label,
        .control-label {
          color: var(--soil-900);
          font-size: .88rem;
          font-weight: 750;
          margin-bottom: .3rem;
        }
        .form-control,
        .form-select,
        .selectize-input {
          border-color: #d9cec0;
          border-radius: 8px;
          color: var(--ink-900);
          background-color: #fff;
        }
        .form-control:focus,
        .form-select:focus,
        .selectize-input.focus {
          border-color: var(--leaf-600);
          box-shadow: 0 0 0 .2rem rgba(40, 114, 83, .15);
        }
        .checkbox-inline,
        .radio-inline {
          margin-right: .95rem;
        }
        input[type='checkbox'],
        input[type='radio'] {
          accent-color: var(--leaf-700);
        }
        .map-box {
          min-height: 420px;
          overflow: hidden;
          border: 1px solid #d8cdbc;
          border-radius: 8px;
          background: #e9e2d8;
        }
        .leaflet-container {
          border-radius: 8px;
          font-family: Inter, system-ui, sans-serif;
        }
        pre {
          color: var(--soil-900);
          background: #faf7f1;
          border: 1px solid #e2d6c8;
          border-radius: 8px;
          padding: .75rem;
        }
        .review-box {
          background: #fbf8f2;
          border: 1px solid #e2d6c8;
          border-radius: 8px;
          padding: 1rem;
        }
        .review-box dl {
          display: grid;
          grid-template-columns: minmax(130px, .45fr) 1fr;
          gap: .45rem .9rem;
          margin: 0;
        }
        .review-box dt {
          color: var(--ink-600);
          font-size: .84rem;
          font-weight: 750;
        }
        .review-box dd {
          margin: 0;
          color: var(--soil-900);
          font-weight: 700;
        }
        table.dataTable {
          border-collapse: separate !important;
          border-spacing: 0;
        }
        .dataTables_wrapper {
          color: var(--ink-900);
        }
        .dataTables_wrapper .dataTables_filter input,
        .dataTables_wrapper .dataTables_length select {
          border: 1px solid #d9cec0;
          border-radius: 8px;
          padding: .28rem .5rem;
        }
        table.dataTable thead th {
          background: #f2ece2;
          color: var(--soil-900);
          border-bottom: 1px solid #d6c8b6 !important;
          font-size: .86rem;
        }
        table.dataTable tbody td {
          border-top: 1px solid #eee6dc;
          vertical-align: middle;
        }
        table.dataTable tbody tr:hover {
          background: #fbf6ec;
        }
        .alert {
          border-radius: 8px;
          border: 1px solid rgba(201, 162, 39, .35);
          background: #fff7df;
          color: #5e4810;
        }
        hr {
          border-color: rgba(122, 94, 63, .18);
          opacity: 1;
          margin: 1.35rem 0;
        }
        @media (max-width: 760px) {
          .page-wrap {
            padding: 1rem .85rem 2rem;
          }
          .navbar {
            min-height: auto;
          }
          .review-box dl {
            grid-template-columns: 1fr;
          }
        }
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
