test_that("ensure_request_internal_columns adds missing internal columns", {
  data <- data.frame(
    solicitacao_id = "SOL-1",
    status_interno = "Recebida",
    stringsAsFactors = FALSE
  )

  result <- ensure_request_internal_columns(data)

  expect_true("numero_laboratorio" %in% names(result))
  expect_true("observacoes_internas" %in% names(result))
  expect_equal(result$numero_laboratorio[[1]], "")
})

test_that("field_value handles missing and NA values", {
  data <- data.frame(
    status_interno = NA_character_,
    numero_laboratorio = "LAB-1",
    stringsAsFactors = FALSE
  )

  expect_equal(field_value(data, "numero_laboratorio"), "LAB-1")
  expect_equal(field_value(data, "status_interno", "Recebida"), "Recebida")
  expect_equal(field_value(data, "campo_inexistente", "x"), "x")
})
