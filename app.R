library(shiny)

source("R/config.R")
if (is_test_mode()) {
  Sys.setenv(USE_GOOGLE_SHEETS = "false")
} else {
  load_project_env()
}

source("R/sample_data.R")
source("R/exportacao.R")
source("R/storage_google_sheets.R")
source("R/mod_solicitante.R")
source("R/mod_amostras.R")
source("R/mod_recepcao.R")
source("R/app_ui.R")
source("R/app_server.R")

shiny::shinyApp(ui = app_ui(), server = app_server)
