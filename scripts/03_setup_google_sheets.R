source("R/config.R")
load_project_env()
source("R/storage_google_sheets.R")

setup_google_sheets()
message("Planilha Google preparada: ", Sys.getenv("GOOGLE_SHEET_ID"))
