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

test_that("ensure_request_internal_columns preserves existing values", {
  data <- data.frame(
    solicitacao_id = "SOL-1",
    numero_laboratorio = "LAB-99",
    stringsAsFactors = FALSE
  )
  result <- ensure_request_internal_columns(data)
  expect_equal(result$numero_laboratorio[[1]], "LAB-99")
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

test_that("field_value returns fallback for empty string", {
  data <- data.frame(custo_total_lab = "", stringsAsFactors = FALSE)
  expect_equal(field_value(data, "custo_total_lab", "0"), "")
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

test_that("update_request_internal_fields ignores unknown request id", {
  data <- sample_store()$solicitacoes
  result <- update_request_internal_fields(
    data,
    request_id = "ID-QUE-NAO-EXISTE",
    values = list(numero_laboratorio = "LAB-X")
  )
  expect_equal(result$numero_laboratorio[[1]], "")
})

test_that("update_request_internal_fields does not change non-editable fields", {
  data <- sample_store()$solicitacoes
  result <- update_request_internal_fields(
    data,
    request_id = "SOL-20260603-0001",
    values = list(nome_solicitante = "Nome alterado")
  )
  expect_equal(result$nome_solicitante[[1]], "Exemplo de solicitante")
})

test_that("recepcao_senha returns default when env var not set", {
  withr::local_envvar(c(LAB_RECEPTION_PASSWORD = ""))
  expect_equal(recepcao_senha(), "dps2024")
})

test_that("recepcao_senha returns custom value when env var is set", {
  withr::local_envvar(c(LAB_RECEPTION_PASSWORD = "senha_customizada"))
  expect_equal(recepcao_senha(), "senha_customizada")
})

test_that("recepcao_senha comparison is exact (case-sensitive)", {
  withr::local_envvar(c(LAB_RECEPTION_PASSWORD = "DPS2024"))
  expect_false(identical("dps2024", recepcao_senha()))
  expect_true(identical("DPS2024", recepcao_senha()))
})

test_that("status_badge_html maps known statuses to css classes", {
  expect_match(status_badge_html("Recebida"), "status-recebida")
  expect_match(status_badge_html("Em análise"), "status-em-analise")
  expect_match(status_badge_html("Finalizada"), "status-finalizada")
  expect_match(status_badge_html("Cancelada"), "status-cancelada")
  expect_match(status_badge_html("Aguardando amostra"), "status-aguardando")
})

test_that("status_badge_html falls back for unknown or empty status", {
  expect_match(status_badge_html(""), "status-recebida")
  expect_match(status_badge_html("Qualquer Coisa"), "status-teste")
})

test_that("status_badge_html escapes html in the status text", {
  out <- status_badge_html("<script>x</script>")
  expect_false(grepl("<script>", out, fixed = TRUE))
  expect_match(out, "&lt;script&gt;")
})

test_that("blank_value detects empty, NA and literal NA", {
  expect_true(blank_value(NULL))
  expect_true(blank_value(""))
  expect_true(blank_value("   "))
  expect_true(blank_value(NA))
  expect_true(blank_value(NA_character_))
  expect_true(blank_value("NA"))
  expect_false(blank_value("Viçosa"))
  expect_false(blank_value(3.5))
})

test_that("detail_dd returns NULL for blank and tags for filled", {
  expect_null(detail_dd("Cidade", ""))
  expect_null(detail_dd("Cidade", NA))
  out <- detail_dd("Cidade", "Viçosa")
  expect_false(is.null(out))
})

test_that("recepcao_code_label maps codes and falls back to raw", {
  m <- list(sim = "Sim", nao = "Não")
  expect_equal(recepcao_code_label("sim", m), "Sim")
  expect_equal(recepcao_code_label("nao", m), "Não")
  expect_equal(recepcao_code_label("outro_valor", m), "outro_valor")
})

test_that("vinculo_recepcao_label expands codes and handles outro", {
  expect_equal(vinculo_recepcao_label("mestrado"), "Mestrado")
  expect_equal(vinculo_recepcao_label("doutorado"), "Doutorado")
  expect_equal(vinculo_recepcao_label("outro", "Bolsista PNPD"), "Outro: Bolsista PNPD")
  expect_equal(vinculo_recepcao_label("outro", ""), "Outro")
})

test_that("format_request_address joins non-empty parts only", {
  req <- data.frame(
    endereco = "Rua A, 10", bairro = "Centro",
    cidade_solicitante = "Viçosa", uf_solicitante = "MG", cep = "36570-000",
    stringsAsFactors = FALSE
  )
  out <- format_request_address(req)
  expect_match(out, "Rua A, 10")
  expect_match(out, "Centro")
  expect_match(out, "Viçosa/MG")
  expect_match(out, "36570-000")

  req2 <- data.frame(
    endereco = "", bairro = "", cidade_solicitante = "Viçosa",
    uf_solicitante = "MG", cep = "", stringsAsFactors = FALSE
  )
  expect_equal(format_request_address(req2), "Viçosa/MG")
})

test_that("format_sample_location formats coordinates or returns empty", {
  s <- data.frame(latitude_wgs84 = -20.7546, longitude_wgs84 = -42.8825,
                  tipo_localizacao = "aproximada", stringsAsFactors = FALSE)
  out <- format_sample_location(s)
  expect_match(out, "-20.75460")
  expect_match(out, "aproximado")

  s_na <- data.frame(latitude_wgs84 = NA_real_, longitude_wgs84 = NA_real_,
                     tipo_localizacao = "aproximada", stringsAsFactors = FALSE)
  expect_equal(format_sample_location(s_na), "")
})
