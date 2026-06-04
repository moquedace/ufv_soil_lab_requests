app_ui <- function() {
  bslib::page_navbar(
    id = "navbar_principal",
    title = "Análises de Solo · DPS-UFV",
    fillable = FALSE,
    theme = bslib::bs_theme(
      version = 5,
      primary = "#a6552f",
      base_font = bslib::font_google("Inter"),
      heading_font = bslib::font_google("Space Grotesk")
    ),
    header = tagList(
      tags$head(
        shinyjs::useShinyjs(),
        tags$link(
          rel = "icon",
          href = "data:image/svg+xml,%3Csvg%20xmlns='http://www.w3.org/2000/svg'%20viewBox='0%200%2016%2016'%3E%3Crect%20y='0'%20width='16'%20height='3'%20fill='%233a2a1e'/%3E%3Crect%20y='3'%20width='16'%20height='3'%20fill='%236b4a30'/%3E%3Crect%20y='6'%20width='16'%20height='4'%20fill='%239c5a38'/%3E%3Crect%20y='10'%20width='16'%20height='3'%20fill='%23be8a4e'/%3E%3Crect%20y='13'%20width='16'%20height='3'%20fill='%23d8c09a'/%3E%3C/svg%3E"
        ),
        tags$style(
          "
          :root {
            --hz-o: #3a2a1e;
            --hz-a: #6b4a30;
            --hz-b: #9c5a38;
            --hz-bc: #be8a4e;
            --hz-c: #d8c09a;
            --soil-900: #2e241c;
            --soil-800: #3d2e23;
            --soil-700: #5a4433;
            --soil-600: #7a5a3f;
            --clay-600: #a6552f;
            --clay-500: #ba6440;
            --ocre-500: #c0883e;
            --sand-50: #f9f5ee;
            --sand-100: #f3ece0;
            --sand-200: #ece2d2;
            --line: #e2d7c6;
            --surface: #ffffff;
            --ink-900: #2a211a;
            --ink-700: #4d4339;
            --ink-500: #8a7d6d;
            --shadow-soft: 0 2px 10px rgba(58, 42, 30, .07);
          }
          html, body { overflow-x: hidden; }
          body {
            background: var(--sand-50);
            color: var(--ink-900);
          }
          .full-bleed {
            width: 100vw;
            position: relative;
            left: 50%;
            transform: translateX(-50%);
          }
          h1, h2, h3, h4, h5, h6, .navbar-brand, .hero-title, .footer-inner h4 {
            font-family: 'Space Grotesk', system-ui, sans-serif;
          }
          /* ---- Navbar ---- */
          .navbar {
            background: var(--soil-900) !important;
            box-shadow: 0 2px 14px rgba(46, 36, 28, .18);
            min-height: 64px;
          }
          .navbar .container-fluid { gap: .75rem; }
          .navbar-brand {
            font-weight: 600;
            color: #fff !important;
            font-size: 1.15rem;
            letter-spacing: .01em;
          }
          .navbar .nav-link {
            color: rgba(255, 255, 255, .74) !important;
            font-weight: 500;
            border-radius: 7px;
            padding: .5rem .85rem;
            margin: 0 .05rem;
            transition: background-color .15s ease, color .15s ease;
          }
          .navbar .nav-link.active,
          .navbar .nav-link:focus,
          .navbar .nav-link:hover {
            background: rgba(255, 255, 255, .08);
            color: #fff !important;
          }
          .navbar .nav-link.active {
            box-shadow: inset 0 -2px 0 var(--clay-500);
          }
          .navbar-logos {
            display: flex;
            align-items: center;
            gap: .8rem;
            margin-left: 1rem;
            padding: .34rem .65rem;
            border-radius: 8px;
            background: rgba(255, 255, 255, .96);
            box-shadow: 0 4px 12px rgba(46, 36, 28, .12);
          }
          .navbar-logo { display: block; max-height: 34px; max-width: 140px; object-fit: contain; }
          .navbar-logo-dps { max-height: 38px; max-width: 118px; }
          /* ---- Faixa de horizontes ---- */
          .horizon-bar { height: 5px; display: flex; }
          .horizon-bar span { flex: 1; }
          .horizon-bar .o  { background: var(--hz-o); }
          .horizon-bar .a  { background: var(--hz-a); }
          .horizon-bar .b  { background: var(--hz-b); }
          .horizon-bar .bc { background: var(--hz-bc); }
          .horizon-bar .c  { background: var(--hz-c); }
          /* ---- Hero ---- */
          .hero {
            background: linear-gradient(180deg, var(--sand-100), var(--sand-50));
            border-bottom: 1px solid var(--line);
          }
          .hero-inner {
            max-width: 1180px;
            margin: 0 auto;
            padding: 1.3rem 1.5rem;
            display: flex;
            align-items: center;
            gap: 1.3rem;
          }
          .soil-profile {
            width: 30px;
            height: 62px;
            border-radius: 5px;
            overflow: hidden;
            box-shadow: 0 2px 8px rgba(58, 42, 30, .22);
            flex-shrink: 0;
            display: flex;
            flex-direction: column;
          }
          .soil-profile span { width: 100%; }
          .soil-profile .o  { flex: .8; background: var(--hz-o); }
          .soil-profile .a  { flex: 1.1; background: var(--hz-a); }
          .soil-profile .b  { flex: 1.5; background: var(--hz-b); }
          .soil-profile .bc { flex: 1.1; background: var(--hz-bc); }
          .soil-profile .c  { flex: 1; background: var(--hz-c); }
          .hero-label {
            font-size: .72rem;
            font-weight: 700;
            letter-spacing: .14em;
            text-transform: uppercase;
            color: var(--clay-600);
          }
          .hero-title {
            font-size: 1.35rem;
            font-weight: 600;
            color: var(--soil-900);
            margin: .1rem 0 0;
          }
          .hero-sub { font-size: .9rem; color: var(--ink-500); margin-top: .1rem; }
          /* ---- Page ---- */
          .page-wrap { max-width: 1180px; margin: 0 auto; padding: 1.8rem 1.5rem 2.5rem; }
          .section-title {
            margin-bottom: .15rem;
            color: var(--soil-900);
            font-weight: 600;
            font-size: 1.9rem;
            letter-spacing: -.01em;
          }
          .section-title + .muted-help { margin-bottom: 1.5rem; }
          .muted-help { color: var(--ink-500); font-size: .92rem; }
          h3 { font-size: 1.2rem; font-weight: 600; color: var(--soil-900); margin-bottom: .75rem; }
          h4 {
            font-size: .8rem; font-weight: 700; color: var(--soil-700);
            text-transform: uppercase; letter-spacing: .07em; margin-bottom: .65rem;
          }
          /* ---- Cards ---- */
          .card {
            border: 1px solid var(--line);
            border-radius: 11px;
            box-shadow: var(--shadow-soft);
            background: var(--surface);
          }
          .card-header {
            display: flex;
            align-items: center;
            gap: .55rem;
            background: var(--sand-50);
            border-bottom: 1px solid var(--line);
            border-left: 3px solid var(--clay-600);
            color: var(--soil-700);
            font-weight: 700;
            font-size: .8rem;
            text-transform: uppercase;
            letter-spacing: .07em;
            padding: .7rem 1.05rem;
          }
          .card-header .bi { color: var(--clay-600); font-size: 1.05rem; }
          .sample-card {
            border: 1px solid var(--line);
            border-radius: 11px;
            padding: 1.15rem;
            margin-bottom: 1.1rem;
            background: var(--surface);
            box-shadow: var(--shadow-soft);
          }
          .sample-card > .bslib-grid,
          .card-body > .bslib-grid { row-gap: .8rem; }
          /* ---- Inputs ---- */
          label, .control-label {
            color: var(--ink-700);
            font-size: .82rem;
            font-weight: 600;
            margin-bottom: .25rem;
            letter-spacing: .01em;
          }
          .form-control, .form-select, .selectize-input {
            border-color: #d3c6b3;
            border-radius: 8px;
            color: var(--ink-900);
            background-color: #fffdfa;
          }
          .form-control:focus, .form-select:focus, .selectize-input.focus {
            border-color: var(--clay-600);
            box-shadow: 0 0 0 .2rem rgba(166, 85, 47, .14);
          }
          .checkbox-inline, .radio-inline { margin-right: .95rem; }
          input[type='checkbox'], input[type='radio'] { accent-color: var(--clay-600); }
          /* ---- Buttons ---- */
          .btn {
            border-radius: 8px;
            font-weight: 600;
            border-width: 1px;
            box-shadow: none !important;
            font-size: .9rem;
          }
          .btn-primary {
            background: var(--clay-600);
            border-color: var(--clay-600);
            color: #fff;
          }
          .btn-primary:hover, .btn-primary:focus {
            background: var(--soil-700);
            border-color: var(--soil-700);
            color: #fff;
          }
          .btn-outline-primary {
            color: var(--clay-600);
            border-color: #d0b89e;
            background: #fff;
          }
          .btn-outline-primary:hover, .btn-outline-primary:focus {
            background: var(--clay-600);
            border-color: var(--clay-600);
            color: #fff;
          }
          .btn-secondary:not(.btn-primary),
          .btn-default:not(.btn-primary),
          .shiny-download-link {
            background: var(--sand-100) !important;
            border-color: #ddd0bb !important;
            color: var(--ink-700) !important;
          }
          .btn-secondary:not(.btn-primary):hover,
          .btn-default:not(.btn-primary):hover,
          .shiny-download-link:hover,
          .shiny-download-link:focus {
            background: var(--sand-200) !important;
            border-color: #cdbb9f !important;
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
          /* ---- Map ---- */
          .map-box {
            min-height: 420px;
            overflow: hidden;
            border: 1px solid var(--line);
            border-radius: 9px;
            background: #e9e0d0;
          }
          .leaflet-container { border-radius: 9px; font-family: Inter, system-ui, sans-serif; }
          /* ---- Review ---- */
          pre {
            color: var(--ink-900);
            background: var(--sand-50);
            border: 1px solid var(--line);
            border-radius: 8px;
            padding: .75rem;
          }
          .review-box {
            background: var(--sand-50);
            border: 1px solid var(--line);
            border-radius: 9px;
            padding: 1rem;
          }
          .review-box dl {
            display: grid;
            grid-template-columns: minmax(130px, .45fr) 1fr;
            gap: .45rem .9rem;
            margin: 0;
          }
          .review-box dt { color: var(--ink-500); font-size: .84rem; font-weight: 700; }
          .review-box dd { margin: 0; color: var(--ink-900); font-weight: 600; }
          /* ---- Status badges ---- */
          .status-badge {
            display: inline-block;
            padding: .18rem .6rem;
            border-radius: 999px;
            font-size: .78rem;
            font-weight: 600;
            border: 1px solid transparent;
            white-space: nowrap;
          }
          .status-recebida      { background: #eef3ed; color: #3d5a3a; border-color: #cfe0cb; }
          .status-aguardando    { background: #fbf1e0; color: #8a5a16; border-color: #ecd6ad; }
          .status-em-analise    { background: #e9f0f6; color: #2f567a; border-color: #c6d9ea; }
          .status-finalizada    { background: #e7f1ec; color: #1f6d4a; border-color: #bfe0cf; }
          .status-cancelada     { background: #f6e9e7; color: #8c3a2e; border-color: #ecc6bf; }
          .status-teste         { background: #efeae4; color: #6b5d4e; border-color: #ddd0bb; }
          /* ---- DataTables ---- */
          table.dataTable { border-collapse: separate !important; border-spacing: 0; }
          .dataTables_wrapper { color: var(--ink-900); }
          .dataTables_wrapper .dataTables_filter input,
          .dataTables_wrapper .dataTables_length select {
            border: 1px solid #d3c6b3;
            border-radius: 8px;
            padding: .28rem .5rem;
          }
          table.dataTable thead th {
            background: var(--sand-100);
            color: var(--soil-900);
            border-bottom: 1px solid var(--line) !important;
            font-size: .85rem;
            font-weight: 600;
          }
          table.dataTable tbody td { border-top: 1px solid #f0e8da; vertical-align: middle; }
          table.dataTable tbody tr:hover { background: var(--sand-50); }
          .page-link,
          .dataTables_wrapper .paginate_button a.page-link,
          .dataTables_wrapper .dataTables_paginate .paginate_button {
            color: var(--clay-600) !important;
            border: 1px solid var(--line) !important;
            background: var(--sand-50) !important;
            background-image: none !important;
            border-radius: 6px !important;
            margin: 0 2px;
            box-shadow: none !important;
          }
          .dataTables_wrapper .dataTables_paginate .paginate_button.current,
          .page-item.active .page-link {
            background: var(--clay-600) !important;
            border-color: var(--clay-600) !important;
            color: #fff !important;
          }
          .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
            background: var(--sand-200) !important;
            color: var(--soil-900) !important;
          }
          .dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
          .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover {
            background: #faf6ef !important;
            color: var(--ink-500) !important;
            opacity: .55;
          }
          /* ---- Alert ---- */
          .alert {
            border-radius: 9px;
            border: 1px solid rgba(192, 136, 62, .36);
            background: #fbf3e3;
            color: #6b4a18;
          }
          .alert-success {
            border-color: #bfe0cf;
            background: #ecf6f0;
            color: #1f6d4a;
          }
          hr { border-color: var(--line); opacity: 1; margin: 1.35rem 0; }
          /* ---- Footer ---- */
          .footer-body { background: var(--soil-900); color: rgba(255, 255, 255, .72); }
          .footer-inner {
            max-width: 1180px;
            margin: 0 auto;
            padding: 2rem 1.5rem;
            display: grid;
            grid-template-columns: 1.4fr 1fr 1fr;
            gap: 2rem;
            font-size: .85rem;
            line-height: 1.6;
          }
          .footer-inner h4 {
            color: #fff;
            font-size: 1rem;
            font-weight: 600;
            margin: 0 0 .6rem;
            text-transform: none;
            letter-spacing: 0;
          }
          .footer-inner a { color: var(--hz-c); text-decoration: none; }
          .footer-inner a:hover { color: #fff; }
          .footer-bottom {
            max-width: 1180px;
            margin: 0 auto;
            padding: 0 1.5rem 1.4rem;
            font-size: .78rem;
            color: rgba(255, 255, 255, .5);
            display: flex;
            justify-content: space-between;
            gap: 1rem;
          }
          @media (max-width: 760px) {
            .page-wrap { padding: 1.2rem .9rem 2rem; }
            .navbar { min-height: auto; }
            .review-box dl { grid-template-columns: 1fr; }
            .hero-inner { padding: 1rem 1rem; }
            .footer-inner { grid-template-columns: 1fr; gap: 1.2rem; }
            .footer-bottom { flex-direction: column; }
          }
          "
        )
      ),
      div(
        class = "full-bleed horizon-bar",
        tags$span(class = "o"), tags$span(class = "a"), tags$span(class = "b"),
        tags$span(class = "bc"), tags$span(class = "c")
      ),
      div(
        class = "full-bleed hero",
        div(
          class = "hero-inner",
          div(
            class = "soil-profile",
            tags$span(class = "o"), tags$span(class = "a"), tags$span(class = "b"),
            tags$span(class = "bc"), tags$span(class = "c")
          ),
          div(
            div(class = "hero-label", "Laboratório de Análises de Solo"),
            div(class = "hero-title", "Departamento de Solos · Universidade Federal de Viçosa"),
            div(class = "hero-sub", "Solicitação digital de análises — solo, vegetal, CHN, absorção atômica e ICP-OES.")
          )
        )
      )
    ),
    footer = tagList(
      div(
        class = "full-bleed horizon-bar",
        tags$span(class = "o"), tags$span(class = "a"), tags$span(class = "b"),
        tags$span(class = "bc"), tags$span(class = "c")
      ),
      tags$footer(
        class = "full-bleed footer-body",
        div(
          class = "footer-inner",
          div(
            tags$h4("Laboratório de Análises de Solo"),
            "Departamento de Solos (DPS)", tags$br(),
            "Universidade Federal de Viçosa", tags$br(),
            "Av. P. H. Rolfs s/n, Campus UFV, Viçosa-MG"
          ),
          div(
            tags$h4("Contato"),
            "laboratorio.solos@ufv.br", tags$br(),
            "Seg. a sex., 8h–17h"
          ),
          div(
            tags$h4("Sistema"),
            "Solicitação digital de análises", tags$br(),
            "Versão piloto 0.1", tags$br(),
            tags$a(href = "https://github.com/moquedace/ufv_soil_lab_requests", target = "_blank", "Código no GitHub")
          )
        ),
        div(
          class = "footer-bottom",
          tags$span("© 2026 Departamento de Solos — UFV"),
          tags$span("Feito para fins acadêmicos e de pesquisa")
        )
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
