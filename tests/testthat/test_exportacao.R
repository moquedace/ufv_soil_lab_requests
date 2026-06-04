test_that("flatten_store returns exportable rows", {
  flat <- flatten_store(sample_store())
  expect_s3_class(flat, "data.frame")
  expect_gt(nrow(flat), 0)
  expect_true("solicitacao_id" %in% names(flat))
  expect_true("amostra_id" %in% names(flat))
  expect_true("analises_solicitadas" %in% names(flat))
})

test_that("flatten_store returns empty frame when store has no solicitacoes", {
  store <- list(
    solicitacoes = data.frame(),
    amostras = data.frame(),
    analises = data.frame()
  )
  expect_equal(nrow(flatten_store(store)), 0)
})

test_that("flatten_store_by_analysis returns one row per analysis", {
  flat <- flatten_store_by_analysis(sample_store())
  expect_s3_class(flat, "data.frame")
  expect_gt(nrow(flat), 0)
  expect_true("laboratorio" %in% names(flat))
  expect_true("analise_id" %in% names(flat))
  expect_true("solicitacao_id" %in% names(flat))
})

test_that("flatten_store_by_analysis returns empty frame when no analises", {
  store <- sample_store()
  store$analises <- data.frame()
  expect_equal(nrow(flatten_store_by_analysis(store)), 0)
})

test_that("flatten_store preserves new sample fields from schema", {
  store <- sample_store()
  store$amostras$tipo_amostra_vegetal <- "folha"
  store$amostras$cultura_planta <- "Eucalipto"
  flat <- flatten_store(store)
  expect_true("tipo_amostra_vegetal" %in% names(flat))
  expect_equal(flat$tipo_amostra_vegetal[[1]], "folha")
})

test_that("filter_reception_data filters by search, status and lab", {
  flat <- flatten_store_by_analysis(sample_store())

  expect_gt(nrow(filter_reception_data(flat, search = "Talhao 1")), 0)
  expect_gt(nrow(filter_reception_data(flat, status = "Recebida")), 0)
  expect_gt(nrow(filter_reception_data(flat, laboratorio = "solo_rotina")), 0)
  expect_true(all(filter_reception_data(flat, laboratorio = "solo_rotina")$laboratorio == "solo_rotina"))
  expect_equal(nrow(filter_reception_data(flat, search = "texto que nao existe")), 0)
})

test_that("filter_reception_data returns all rows with default args", {
  flat <- flatten_store_by_analysis(sample_store())
  expect_equal(nrow(filter_reception_data(flat)), nrow(flat))
})

test_that("filter_reception_data is case-insensitive", {
  flat <- flatten_store_by_analysis(sample_store())
  lower <- filter_reception_data(flat, search = "talhao 1")
  upper <- filter_reception_data(flat, search = "TALHAO 1")
  expect_equal(nrow(lower), nrow(upper))
  expect_gt(nrow(lower), 0)
})

test_that("filter_reception_data returns empty frame when no match on status", {
  flat <- flatten_store_by_analysis(sample_store())
  result <- filter_reception_data(flat, status = "status_que_nao_existe")
  expect_equal(nrow(result), 0)
})

test_that("write_export_csv produces valid UTF-8 output", {
  flat <- flatten_store(sample_store())
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write_export_csv(flat, tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  lines <- readLines(tmp, encoding = "UTF-8", n = 2)
  expect_match(lines[[1]], "solicitacao_id")
})

test_that("write_export_xlsx produces a valid file", {
  skip_if_not_installed("writexl")
  flat <- flatten_store(sample_store())
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  write_export_xlsx(flat, tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
})
