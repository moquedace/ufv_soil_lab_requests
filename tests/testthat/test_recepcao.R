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

test_that("update_request_internal_fields changes only the selected request", {
  data <- sample_store()$solicitacoes

  result <- update_request_internal_fields(
    solicitacoes = data,
    request_id = "SOL-20260603-0001",
    values = list(
      numero_laboratorio = "LAB-TESTE-001",
      status_interno = "Em analise",
      observacoes_internas = "Teste automatizado"
    )
  )

  expect_equal(result$numero_laboratorio[[1]], "LAB-TESTE-001")
  expect_equal(result$status_interno[[1]], "Em analise")
  expect_equal(result$observacoes_internas[[1]], "Teste automatizado")
})
