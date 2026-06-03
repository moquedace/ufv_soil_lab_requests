test_that("flatten_store returns exportable rows", {
  flat <- flatten_store(sample_store())

  expect_s3_class(flat, "data.frame")
  expect_gt(nrow(flat), 0)
  expect_true("solicitacao_id" %in% names(flat))
  expect_true("amostra_id" %in% names(flat))
  expect_true("analises_solicitadas" %in% names(flat))
})
