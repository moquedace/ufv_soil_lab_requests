test_that("use_google_sheets only turns on with flag and sheet id", {
  withr::local_envvar(c(USE_GOOGLE_SHEETS = "false", GOOGLE_SHEET_ID = "abc"))
  expect_false(use_google_sheets())

  withr::local_envvar(c(USE_GOOGLE_SHEETS = "true", GOOGLE_SHEET_ID = ""))
  expect_false(use_google_sheets())

  withr::local_envvar(c(USE_GOOGLE_SHEETS = "true", GOOGLE_SHEET_ID = "abc"))
  expect_true(use_google_sheets())
})

test_that("sheet_columns contains all expected sheets", {
  expect_true(all(c("solicitacoes", "amostras", "analises_amostra") %in% names(sheet_columns)))
})

test_that("sheet_columns solicitacoes contains all required fields", {
  cols <- sheet_columns$solicitacoes
  required <- c(
    "solicitacao_id", "data_hora_envio", "nome_solicitante",
    "email", "telefone", "cpf_cnpj", "endereco", "bairro", "cep",
    "cidade_solicitante", "uf_solicitante", "vinculo", "vinculo_outro",
    "matricula", "instituicao", "orientador", "observacoes_solicitante",
    "consentimento_aceito",
    "status_interno", "data_entrada_lab", "numero_laboratorio",
    "custo_total_lab", "forma_pagamento_lab", "pedido_numero_lab",
    "observacoes_internas"
  )
  missing <- setdiff(required, cols)
  expect_equal(missing, character(0), info = paste("Missing:", paste(missing, collapse = ", ")))
})

test_that("sheet_columns amostras contains all required fields including new ones", {
  cols <- sheet_columns$amostras
  required <- c(
    "amostra_id", "solicitacao_id", "referencia_amostra",
    "tipo_material", "municipio_amostra", "uf_amostra",
    "latitude_wgs84", "longitude_wgs84", "tipo_localizacao",
    "grupos_analise", "carbonato_presente", "pre_tratamento_necessario",
    "tipo_amostra_vegetal", "cultura_planta",
    "percentual_c_estimado", "percentual_n_estimado", "numero_registro_projeto",
    "elementos_aa_icp", "tipo_digestao", "volume_apos_digestao",
    "aliquota", "diluicao", "volume_final",
    "departamento_origem", "projeto_registrado", "numero_registro_projeto_aa"
  )
  missing <- setdiff(required, cols)
  expect_equal(missing, character(0), info = paste("Missing:", paste(missing, collapse = ", ")))
})

test_that("empty_sheet_data creates the expected columns", {
  data <- empty_sheet_data("amostras")
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 0)
  expect_equal(names(data), sheet_columns$amostras)
})

test_that("empty sheet data for all sheets preserves schemas", {
  for (sheet in sheet_names) {
    data <- empty_sheet_data(sheet)
    expect_equal(nrow(data), 0)
    expect_equal(names(data), sheet_columns[[sheet]])
  }
})

test_that("remove_test_records removes linked test rows only", {
  store <- sample_store()
  test_request <- store$solicitacoes
  test_request$solicitacao_id <- "TESTE-GS-001"
  test_request$nome_solicitante <- "TESTE AUTOMATICO - NAO USAR"

  test_sample <- store$amostras
  test_sample$solicitacao_id <- "TESTE-GS-001"
  test_sample$amostra_id <- "TESTE-GS-001-AMS-001"

  test_analysis <- store$analises
  test_analysis$amostra_id <- "TESTE-GS-001-AMS-001"

  mixed <- list(
    solicitacoes = rbind(store$solicitacoes, test_request),
    amostras = rbind(store$amostras, test_sample),
    analises = rbind(store$analises, test_analysis)
  )

  result <- remove_test_records(mixed)

  expect_equal(result$removed$solicitacoes, 1)
  expect_equal(result$removed$amostras, 1)
  expect_equal(result$removed$analises, 1)
  expect_false("TESTE-GS-001" %in% result$store$solicitacoes$solicitacao_id)
  expect_true("SOL-20260603-0001" %in% result$store$solicitacoes$solicitacao_id)
})

test_that("remove_test_records returns zero removed when no test records", {
  result <- remove_test_records(sample_store())
  expect_equal(result$removed$solicitacoes, 0)
  expect_equal(result$removed$amostras, 0)
})

test_that("detect_test_request_ids finds records by name pattern", {
  store <- sample_store()
  store$solicitacoes$nome_solicitante[[1]] <- "Teste Automatizado"
  ids <- detect_test_request_ids(store$solicitacoes)
  expect_equal(length(ids), 1)
})

test_that("detect_test_request_ids returns empty for normal records", {
  expect_equal(length(detect_test_request_ids(sample_store()$solicitacoes)), 0)
})
