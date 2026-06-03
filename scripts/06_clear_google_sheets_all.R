source("R/config.R")
load_project_env()
source("R/storage_google_sheets.R")

if (!use_google_sheets()) {
  stop(
    "Ative USE_GOOGLE_SHEETS=true e configure GOOGLE_SHEET_ID no .Renviron antes de limpar a planilha.",
    call. = FALSE
  )
}

clear_google_store(confirm = FALSE)
