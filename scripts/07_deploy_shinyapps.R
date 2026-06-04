source("R/config.R")
load_project_env()

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Instale o pacote 'rsconnect' com source('scripts/00_install_packages.R').", call. = FALSE)
}

app_name <- Sys.getenv("SHINYAPPS_APP_NAME", "ufv-soil-lab-requests")
app_title <- Sys.getenv("SHINYAPPS_APP_TITLE", "Solicitação de análises - DPS/UFV")
account <- Sys.getenv("SHINYAPPS_ACCOUNT", "")

if (!length(rsconnect::accounts()$name)) {
  stop(
    "Configure sua conta shinyapps.io antes do deploy. Use o comando rsconnect::setAccountInfo(...) da pagina Tokens do shinyapps.io.",
    call. = FALSE
  )
}

if (use_google_sheets()) {
  service_account_path <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
  if (!nzchar(service_account_path)) {
    default_service_account_path <- file.path("credentials", "google-service-account.json")
    if (file.exists(default_service_account_path)) {
      Sys.setenv(GOOGLE_SERVICE_ACCOUNT_JSON = default_service_account_path)
      service_account_path <- default_service_account_path
    }
  }

  if (!nzchar(service_account_path) || !file.exists(service_account_path)) {
    stop(
      "USE_GOOGLE_SHEETS=true requer uma service account para rodar no shinyapps.io. ",
      "Salve o JSON em credentials/google-service-account.json ou configure GOOGLE_SERVICE_ACCOUNT_JSON no .Renviron.",
      call. = FALSE
    )
  }
}

required_env <- c("GOOGLE_SHEET_ID", "USE_GOOGLE_SHEETS", "LAB_RECEPTION_PASSWORD")
missing_env <- required_env[!nzchar(Sys.getenv(required_env, ""))]
if (length(missing_env)) {
  stop("Configure no .Renviron: ", paste(missing_env, collapse = ", "), call. = FALSE)
}

app_files <- c(
  "app.R",
  ".Renviron",
  list.files("R", recursive = TRUE, full.names = TRUE),
  list.files("config", recursive = TRUE, full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE)
)

service_account_path <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
if (nzchar(service_account_path) && file.exists(service_account_path)) {
  app_files <- c(app_files, service_account_path)
}

app_files <- app_files[file.exists(app_files)]

deploy_args <- list(
  appDir = ".",
  appFiles = unique(app_files),
  appName = app_name,
  appTitle = app_title,
  launch.browser = TRUE
)

if (nzchar(account)) {
  deploy_args$account <- account
}

do.call(rsconnect::deployApp, deploy_args)
