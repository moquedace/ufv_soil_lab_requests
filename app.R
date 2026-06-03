library(shiny)

source("R/app_ui.R")
source("R/app_server.R")
source("R/config.R")
source("R/sample_data.R")
source("R/exportacao.R")
source("R/storage_google_sheets.R")
source("R/mod_solicitante.R")
source("R/mod_amostras.R")
source("R/mod_recepcao.R")

shiny::shinyApp(ui = app_ui(), server = app_server)
