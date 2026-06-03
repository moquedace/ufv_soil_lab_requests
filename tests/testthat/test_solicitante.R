test_that("build_review_table summarizes samples", {
  sample <- list(
    referencia_amostra = "Talhao 1",
    tipo_material = "Solo",
    municipio_amostra = "Vicosa",
    uf_amostra = "MG",
    latitude_wgs84 = -20.7546,
    longitude_wgs84 = -42.8825,
    tipo_localizacao = "aproximada",
    grupos_analise = c("solo_rotina", "vegetal"),
    analises_nomes = c("Rotina", "Nitrogenio")
  )

  result <- build_review_table(list(sample))

  expect_equal(nrow(result), 1)
  expect_equal(result$referencia[[1]], "Talhao 1")
  expect_equal(result$localizacao_mapa[[1]], "sim")
  expect_match(result$grupos[[1]], "solo_rotina")
})

test_that("has_sample_coordinates detects missing coordinates", {
  sample <- list(latitude_wgs84 = NA_real_, longitude_wgs84 = -42)
  expect_false(has_sample_coordinates(sample))

  sample <- list(latitude_wgs84 = -20, longitude_wgs84 = -42)
  expect_true(has_sample_coordinates(sample))
})

test_that("count_review_locations handles empty review tables", {
  expect_equal(count_review_locations(data.frame()), 0)
  expect_equal(count_review_locations(data.frame(localizacao_mapa = c("sim", "nao"))), 1)
})
