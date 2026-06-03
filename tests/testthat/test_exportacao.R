test_that("flatten_store returns exportable rows", {
  flat <- flatten_store(sample_store())

  expect_s3_class(flat, "data.frame")
  expect_gt(nrow(flat), 0)
  expect_true("solicitacao_id" %in% names(flat))
  expect_true("amostra_id" %in% names(flat))
  expect_true("analises_solicitadas" %in% names(flat))
})

test_that("flatten_store_by_analysis returns one row per analysis", {
  flat <- flatten_store_by_analysis(sample_store())

  expect_s3_class(flat, "data.frame")
  expect_gt(nrow(flat), 0)
  expect_true("laboratorio" %in% names(flat))
  expect_true("analise_id" %in% names(flat))
  expect_true("solicitacao_id" %in% names(flat))
})

test_that("filter_reception_data filters by search, status and lab", {
  flat <- flatten_store_by_analysis(sample_store())

  by_text <- filter_reception_data(flat, search = "Talhao 1")
  expect_gt(nrow(by_text), 0)

  by_status <- filter_reception_data(flat, status = "Recebida")
  expect_gt(nrow(by_status), 0)

  by_lab <- filter_reception_data(flat, laboratorio = "solo_rotina")
  expect_gt(nrow(by_lab), 0)
  expect_true(all(by_lab$laboratorio == "solo_rotina"))

  empty <- filter_reception_data(flat, search = "texto inexistente")
  expect_equal(nrow(empty), 0)
})
