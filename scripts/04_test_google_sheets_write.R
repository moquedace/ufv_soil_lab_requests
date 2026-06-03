source("R/config.R")
load_project_env()
source("R/storage_google_sheets.R")

if (!use_google_sheets()) {
  stop(
    "Ative USE_GOOGLE_SHEETS=true e configure GOOGLE_SHEET_ID no .Renviron antes de rodar este teste.",
    call. = FALSE
  )
}

test_id <- paste0("TESTE-GS-", format(Sys.time(), "%Y%m%d-%H%M%S"))

test_store <- list(
  solicitacoes = data.frame(
    solicitacao_id = test_id,
    data_hora_envio = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    nome_solicitante = "TESTE AUTOMATICO - NAO USAR",
    email = "teste.automatico@example.com",
    telefone = "(00) 00000-0000",
    cidade_solicitante = "Vicosa",
    status_interno = "Teste",
    stringsAsFactors = FALSE
  ),
  amostras = data.frame(
    amostra_id = paste0(test_id, "-AMS-001"),
    solicitacao_id = test_id,
    ordem_amostra = 1,
    referencia_amostra = "TESTE AUTOMATICO - AMOSTRA",
    municipio_amostra = "Vicosa",
    uf_amostra = "MG",
    localidade_descricao = "Teste de integracao Google Sheets",
    latitude_wgs84 = -20.7546,
    longitude_wgs84 = -42.8825,
    tipo_localizacao = "aproximada",
    tipo_material = "Solo",
    grupos_analise = "solo_rotina;vegetal",
    carbonato_presente = NA_character_,
    pre_tratamento_necessario = FALSE,
    stringsAsFactors = FALSE
  ),
  analises = data.frame(
    amostra_id = paste0(test_id, "-AMS-001"),
    laboratorio = c("solo_rotina", "vegetal"),
    analise_id = c("rotina_basica", "nitrogenio"),
    analise_nome = c(
      "Rotina: pH, P, K, Ca, Mg, Al, H + Al e P-rem",
      "Nitrogenio"
    ),
    stringsAsFactors = FALSE
  )
)

append_google_store(test_store)
store <- read_google_store()

found_request <- test_id %in% store$solicitacoes$solicitacao_id
found_sample <- test_id %in% store$amostras$solicitacao_id
found_analyses <- paste0(test_id, "-AMS-001") %in% store$analises$amostra_id

if (!found_request || !found_sample || !found_analyses) {
  stop(
    paste(
      "Falha ao validar escrita/leitura no Google Sheets.",
      "solicitacao=", found_request,
      "amostra=", found_sample,
      "analises=", found_analyses
    ),
    call. = FALSE
  )
}

message("Teste Google Sheets OK: ", test_id)
