source("R/storage_google_sheets.R")

setup_google_sheets()
message("Planilha Google preparada: ", Sys.getenv("GOOGLE_SHEET_ID"))
