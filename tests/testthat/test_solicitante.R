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

test_that("build_review_table returns empty data frame for empty list", {
  expect_equal(nrow(build_review_table(list())), 0)
})

test_that("build_review_table marks localizacao_mapa as nao without coordinates", {
  sample <- list(
    referencia_amostra = "Sem coord",
    tipo_material = "Vegetal",
    municipio_amostra = "Vicosa",
    uf_amostra = "MG",
    latitude_wgs84 = NA_real_,
    longitude_wgs84 = NA_real_,
    tipo_localizacao = "municipio_regiao",
    grupos_analise = "vegetal",
    analises_nomes = "Nitrogenio"
  )
  result <- build_review_table(list(sample))
  expect_equal(result$localizacao_mapa[[1]], "nao")
})

test_that("build_review_table with multiple samples", {
  make_sample <- function(ref, lat = NA_real_) {
    list(
      referencia_amostra = ref,
      tipo_material = "Solo",
      municipio_amostra = "Vicosa",
      uf_amostra = "MG",
      latitude_wgs84 = lat,
      longitude_wgs84 = if (is.na(lat)) NA_real_ else -42.88,
      tipo_localizacao = "aproximada",
      grupos_analise = "solo_rotina",
      analises_nomes = "Rotina"
    )
  }
  samples <- list(make_sample("A", -20.75), make_sample("B"), make_sample("C", -20.76))
  result <- build_review_table(samples)
  expect_equal(nrow(result), 3)
  expect_equal(count_review_locations(result), 2)
})

test_that("has_sample_coordinates detects missing coordinates", {
  expect_false(has_sample_coordinates(list(latitude_wgs84 = NA_real_, longitude_wgs84 = -42)))
  expect_false(has_sample_coordinates(list(latitude_wgs84 = -20, longitude_wgs84 = NA_real_)))
  expect_false(has_sample_coordinates(list(latitude_wgs84 = NULL, longitude_wgs84 = -42)))
  expect_true(has_sample_coordinates(list(latitude_wgs84 = -20, longitude_wgs84 = -42)))
})

test_that("count_review_locations handles empty review tables", {
  expect_equal(count_review_locations(data.frame()), 0)
  expect_equal(count_review_locations(data.frame(localizacao_mapa = c("sim", "nao", "sim"))), 2)
  expect_equal(count_review_locations(data.frame(localizacao_mapa = c("nao", "nao"))), 0)
})

test_that("is_valid_email accepts well-formed addresses", {
  expect_true(is_valid_email("fulano@ufv.br"))
  expect_true(is_valid_email("a.b-c_d@dominio.com.br"))
  expect_true(is_valid_email("teste123@example.org"))
})

test_that("is_valid_email rejects malformed addresses", {
  expect_false(is_valid_email("sem-arroba"))
  expect_false(is_valid_email("fulano@gmail"))
  expect_false(is_valid_email("fulano@.com"))
  expect_false(is_valid_email("@dominio.com"))
  expect_false(is_valid_email("fulano @dominio.com"))
  expect_false(is_valid_email(""))
})

test_that("is_valid_phone accepts numbers with enough digits", {
  expect_true(is_valid_phone("(31) 99999-0000"))
  expect_true(is_valid_phone("3138991234"))
  expect_true(is_valid_phone("31 8888-7777"))
})

test_that("is_valid_phone rejects numbers that are too short", {
  expect_false(is_valid_phone("1234"))
  expect_false(is_valid_phone("(31)"))
  expect_false(is_valid_phone(""))
})
