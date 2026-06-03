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
  expect_true("amostra_id" %in% sheet_columns$amostras)
  expect_true("analise_id" %in% sheet_columns$analises_amostra)
})

test_that("empty_sheet_data creates the expected columns", {
  data <- empty_sheet_data("amostras")

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 0)
  expect_equal(names(data), sheet_columns$amostras)
})
