app_ui <- function() {
  bslib::page_navbar(
    title = "Solicitação de análises - DPS/UFV",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#1f5f4a",
      base_font = bslib::font_google("Inter")
    ),
    header = tags$head(
      tags$style(
        "
        :root {
          --brand-900: #17372f;
          --brand-800: #1c493d;
          --brand-700: #1f5f4a;
          --brand-600: #28745a;
          --soil-700: #6b573f;
          --accent-500: #b08a2e;
          --surface-0: #ffffff;
          --surface-50: #f7f8f7;
          --surface-100: #eef2f0;
          --line-soft: #d9e0dc;
          --ink-900: #202624;
          --ink-700: #3f4b47;
          --ink-600: #64716c;
          --shadow-soft: 0 8px 22px rgba(28, 73, 61, .07);
        }
        body {
          background: var(--surface-50);
          color: var(--ink-900);
        }
        .navbar {
          background: var(--brand-900) !important;
          box-shadow: 0 2px 14px rgba(23, 55, 47, .14);
          min-height: 68px;
          border-bottom: 1px solid rgba(255, 255, 255, .1);
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
          background: rgba(255, 255, 255, .1);
          color: #fff !important;
        }
        .navbar .nav-link.active {
          box-shadow: inset 0 -3px 0 var(--accent-500);
        }
        a {
          color: var(--brand-700);
        }
        a:hover {
          color: var(--brand-900);
        }
        .btn {
          border-radius: 6px;
          font-weight: 600;
          border-width: 1px;
          box-shadow: none !important;
          font-size: .88rem;
          letter-spacing: .01em;
        }
        .btn-primary {
          background: var(--brand-700);
          border-color: var(--brand-700);
          color: #fff;
        }
        .btn-primary:hover,
        .btn-primary:focus {
          background: var(--brand-800);
          border-color: var(--brand-800);
          color: #fff;
        }
        .btn-outline-primary {
          color: var(--brand-700);
          border-color: #9cb6ac;
          background: #fff;
        }
        .btn-outline-primary:hover,
        .btn-outline-primary:focus {
          background: var(--brand-700);
          border-color: var(--brand-700);
          color: #fff;
        }
        .btn-secondary:not(.btn-primary):not(.btn-success):not(.btn-danger),
        .btn-default:not(.btn-primary):not(.btn-success):not(.btn-danger),
        .shiny-download-link {
          background: #f0f4f2 !important;
          border-color: #ccd7d1 !important;
          color: var(--ink-700) !important;
        }
        .btn-secondary:not(.btn-primary):not(.btn-success):not(.btn-danger):hover,
        .btn-secondary:not(.btn-primary):not(.btn-success):not(.btn-danger):focus,
        .btn-default:not(.btn-primary):not(.btn-success):not(.btn-danger):hover,
        .btn-default:not(.btn-primary):not(.btn-success):not(.btn-danger):focus,
        .shiny-download-link:hover,
        .shiny-download-link:focus {
          background: #e0ebe5 !important;
          border-color: #b8cec6 !important;
          color: var(--ink-900) !important;
        }
        .shiny-download-link {
          display: inline-flex;
          align-items: center;
          gap: .35rem;
          width: 100%;
          justify-content: center;
          margin-bottom: .4rem;
        }
        .page-link,
        .dataTables_wrapper .paginate_button a.page-link {
          color: var(--brand-700) !important;
          border-color: var(--line-soft) !important;
          background-color: #f5f7f6 !important;
          background-image: none !important;
        }
        .page-item.active .page-link,
        .dataTables_wrapper .paginate_button.active a.page-link {
          background-color: var(--brand-700) !important;
          background-image: none !important;
          border-color: var(--brand-700) !important;
          color: #fff !important;
        }
        .page-link:hover,
        .dataTables_wrapper .paginate_button a.page-link:hover {
          color: var(--brand-900) !important;
          background-color: var(--surface-100) !important;
          background-image: none !important;
        }
        .page-item.disabled .page-link,
        .dataTables_wrapper .paginate_button.disabled a.page-link {
          color: var(--ink-600) !important;
          background-color: #fafbfa !important;
          background-image: none !important;
          opacity: .6;
        }
        .navbar-logos {
          display: flex;
          align-items: center;
          gap: .8rem;
          margin-left: 1rem;
          padding: .34rem .65rem;
          border-radius: 8px;
          background: rgba(255, 255, 255, .96);
          box-shadow: 0 4px 12px rgba(23, 55, 47, .1), inset 0 0 0 1px rgba(255, 255, 255, .7);
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
          margin-bottom: .15rem;
          color: var(--ink-900);
          font-weight: 800;
          font-size: 1.55rem;
          letter-spacing: -.01em;
        }
        .section-title + .muted-help {
          margin-bottom: 1.4rem;
        }
        .muted-help {
          color: var(--ink-600);
          font-size: .91rem;
        }
        h3 {
          font-size: 1.1rem;
          font-weight: 700;
          color: var(--ink-900);
          margin-bottom: .75rem;
        }
        h4 {
          font-size: .95rem;
          font-weight: 700;
          color: var(--ink-700);
          text-transform: uppercase;
          letter-spacing: .05em;
          margin-bottom: .65rem;
        }
        .card {
          border: 1px solid var(--line-soft);
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(28, 73, 61, .09), 0 1px 2px rgba(0,0,0,.04);
          background: var(--surface-0);
        }
        .card-header {
          background: var(--surface-50);
          border-bottom: 1px solid var(--line-soft);
          color: var(--ink-700);
          font-weight: 700;
          font-size: .82rem;
          text-transform: uppercase;
          letter-spacing: .06em;
          padding: .65rem 1rem;
        }
        .sample-card {
          border: 1px solid var(--line-soft);
          border-radius: 8px;
          padding: 1.1rem;
          margin-bottom: 1.1rem;
          background: var(--surface-0);
          box-shadow: var(--shadow-soft);
        }
        .sample-card > .bslib-grid,
        .card-body > .bslib-grid {
          row-gap: .8rem;
        }
        label,
        .control-label {
          color: var(--ink-700);
          font-size: .83rem;
          font-weight: 600;
          margin-bottom: .25rem;
          letter-spacing: .01em;
        }
        .form-control,
        .form-select,
        .selectize-input {
          border-color: #cfd8d3;
          border-radius: 8px;
          color: var(--ink-900);
          background-color: #fff;
        }
        .form-control:focus,
        .form-select:focus,
        .selectize-input.focus {
          border-color: var(--brand-600);
          box-shadow: 0 0 0 .2rem rgba(31, 95, 74, .14);
        }
        .checkbox-inline,
        .radio-inline {
          margin-right: .95rem;
        }
        input[type='checkbox'],
        input[type='radio'] {
          accent-color: var(--brand-700);
        }
        .map-box {
          min-height: 420px;
          overflow: hidden;
          border: 1px solid var(--line-soft);
          border-radius: 8px;
          background: #e7ece9;
        }
        .leaflet-container {
          border-radius: 8px;
          font-family: Inter, system-ui, sans-serif;
        }
        pre {
          color: var(--ink-900);
          background: var(--surface-50);
          border: 1px solid var(--line-soft);
          border-radius: 8px;
          padding: .75rem;
        }
        .review-box {
          background: var(--surface-50);
          border: 1px solid var(--line-soft);
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
          color: var(--ink-900);
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
          border: 1px solid #cfd8d3;
          border-radius: 8px;
          padding: .28rem .5rem;
        }
        table.dataTable thead th {
          background: var(--surface-100);
          color: var(--ink-900);
          border-bottom: 1px solid var(--line-soft) !important;
          font-size: .86rem;
        }
        table.dataTable tbody td {
          border-top: 1px solid #edf1ef;
          vertical-align: middle;
        }
        table.dataTable tbody tr:hover {
          background: #f5f8f6;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button,
        .dataTables_wrapper .dataTables_paginate .paginate_button.previous,
        .dataTables_wrapper .dataTables_paginate .paginate_button.next {
          border-radius: 6px !important;
          border: 1px solid var(--line-soft) !important;
          background: #f5f7f6 !important;
          background-image: none !important;
          color: var(--brand-700) !important;
          padding: .28rem .6rem !important;
          margin: 0 2px;
          font-size: .84rem;
          box-shadow: none !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover,
        .dataTables_wrapper .dataTables_paginate .paginate_button.previous:hover,
        .dataTables_wrapper .dataTables_paginate .paginate_button.next:hover {
          background: var(--surface-100) !important;
          background-image: none !important;
          border-color: #b8cec6 !important;
          color: var(--brand-900) !important;
          box-shadow: none !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          background: var(--brand-700) !important;
          background-image: none !important;
          border-color: var(--brand-700) !important;
          color: #fff !important;
          box-shadow: none !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
        .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover {
          background: #fafbfa !important;
          background-image: none !important;
          border-color: var(--line-soft) !important;
          color: var(--ink-600) !important;
          opacity: .55;
          box-shadow: none !important;
        }
        .alert {
          border-radius: 8px;
          border: 1px solid rgba(176, 138, 46, .36);
          background: #fff9e8;
          color: #584312;
        }
        hr {
          border-color: var(--line-soft);
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
      "Solicitar análise",
      div(class = "page-wrap", mod_solicitante_ui("solicitante"))
    ),
    bslib::nav_panel(
      "Recepção",
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
