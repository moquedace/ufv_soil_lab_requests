test_that("use_google_sheets only turns on with flag and sheet id", {
  withr::local_envvar(c(
    USE_GOOGLE_SHEETS = "false",
    GOOGLE_SHEET_ID = "abc"
  ))
  expect_false(use_google_sheets())

  withr::local_envvar(c(
    USE_GOOGLE_SHEETS = "true",
    GOOGLE_SHEET_ID = ""
  ))
  expect_false(use_google_sheets())

  withr::local_envvar(c(
    USE_GOOGLE_SHEETS = "true",
    GOOGLE_SHEET_ID = "abc"
  ))
  expect_true(use_google_sheets())
})

test_that("append_google_rows keeps expected columns", {
  expect_true(all(c("solicitacoes", "amostras", "analises_amostra") %in% names(sheet_columns)))
  expect_true("solicitacao_id" %in% sheet_columns$solicitacoes)
  expect_true("numero_laboratorio" %in% sheet_columns$solicitacoes)
  expect_true("observacoes_internas" %in% sheet_columns$solicitacoes)
  expect_true("amostra_id" %in% sheet_columns$amostras)
  expect_true("analise_id" %in% sheet_columns$analises_amostra)
})

test_that("empty_sheet_data creates the expected columns", {
  data <- empty_sheet_data("amostras")

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 0)
  expect_equal(names(data), sheet_columns$amostras)
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

test_that("empty sheet data for all sheets preserves schemas", {
  for (sheet in sheet_names) {
    data <- empty_sheet_data(sheet)
    expect_equal(nrow(data), 0)
    expect_equal(names(data), sheet_columns[[sheet]])
  }
})
