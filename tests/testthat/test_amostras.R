test_that("remove_sample_at removes the selected sample", {
  sample <- list(referencia_amostra = "Amostra 1")
  samples <- list(sample, list(referencia_amostra = "Amostra 2"))
  result <- remove_sample_at(samples, 1)
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$referencia_amostra, "Amostra 2")
})

test_that("remove_sample_at returns empty list when only one sample", {
  samples <- list(list(referencia_amostra = "Unica"))
  result <- remove_sample_at(samples, 1)
  expect_equal(length(result), 0)
})

test_that("remove_sample_at ignores out-of-bounds index", {
  samples <- list(list(referencia_amostra = "A"))
  expect_equal(length(remove_sample_at(samples, 99)), 1)
  expect_equal(length(remove_sample_at(list(), 1)), 0)
})

test_that("duplicate_sample_at appends a copied sample", {
  sample <- list(referencia_amostra = "Amostra 1")
  result <- duplicate_sample_at(list(sample), 1)
  expect_equal(length(result), 2)
  expect_equal(result[[2]]$referencia_amostra, "Amostra 1 copia")
})

test_that("replace_sample_at updates the selected sample", {
  samples <- list(
    list(referencia_amostra = "Amostra 1"),
    list(referencia_amostra = "Amostra 2")
  )
  result <- replace_sample_at(samples, 2, list(referencia_amostra = "Amostra editada"))
  expect_equal(result[[1]]$referencia_amostra, "Amostra 1")
  expect_equal(result[[2]]$referencia_amostra, "Amostra editada")
})

test_that("selected_sample_index falls back to hidden test index", {
  input <- list(tabela_amostras_rows_selected = integer(), test_selected_sample_index = 2)
  expect_equal(selected_sample_index(input), 2L)
  input$tabela_amostras_rows_selected <- 1
  expect_equal(selected_sample_index(input), 1)
})

test_that("current_marker falls back to hidden test coordinates", {
  expect_equal(current_marker(list(lat = -1, lng = -2), list()), list(lat = -1, lng = -2))
  input <- list(test_map_lat = -20.7546, test_map_lng = -42.8825)
  expect_equal(current_marker(NULL, input), list(lat = -20.7546, lng = -42.8825))
  input <- list(test_map_lat = NA_real_, test_map_lng = -42.8825)
  expect_null(current_marker(NULL, input))
})

make_base_input <- function(groups, ...) {
  c(list(
    grupos_analise = groups,
    referencia_amostra = "Amostra teste",
    tipo_material = "Solo",
    municipio_amostra = "Vicosa",
    uf_amostra = "MG",
    localidade_descricao = "",
    tipo_localizacao = "aproximada"
  ), list(...))
}

test_that("build_sample_from_inputs returns correct structure for solo_rotina", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input("solo_rotina", analises_solo_rotina = "rotina_basica")
  result <- build_sample_from_inputs(input, app_config, list(lat = -20.75, lng = -42.88))

  expect_equal(result$referencia_amostra, "Amostra teste")
  expect_equal(result$grupos_analise, "solo_rotina")
  expect_equal(result$latitude_wgs84, -20.75)
  expect_equal(result$longitude_wgs84, -42.88)
  expect_equal(nrow(result$analises), 1)
  expect_equal(result$analises$analise_id, "rotina_basica")
  expect_true(is.na(result$tipo_amostra_vegetal))
  expect_true(is.na(result$elementos_aa_icp))
})

test_that("build_sample_from_inputs captures soil depth fields", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input(
    "solo_rotina",
    analises_solo_rotina = "rotina_basica",
    profundidade_de = 0,
    profundidade_ate = 20,
    camada = ""
  )
  result <- build_sample_from_inputs(input, app_config, NULL)
  expect_equal(result$profundidade_de, 0)
  expect_equal(result$profundidade_ate, 20)
  expect_equal(result$camada, "")
})

test_that("build_sample_from_inputs captures camada classification when depth unknown", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input(
    "solo_rotina",
    analises_solo_rotina = "rotina_basica",
    camada = "superficial"
  )
  result <- build_sample_from_inputs(input, app_config, NULL)
  expect_true(is.na(result$profundidade_de))
  expect_equal(result$camada, "superficial")
})

test_that("build_sample_from_inputs drops depth fields for non-soil material", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input(
    "vegetal",
    analises_vegetal = "nitrogenio",
    profundidade_de = 0,
    profundidade_ate = 20,
    camada = "superficial"
  )
  input$tipo_material <- "Vegetal"
  result <- build_sample_from_inputs(input, app_config, NULL)
  expect_true(is.na(result$profundidade_de))
  expect_true(is.na(result$profundidade_ate))
  expect_true(is.na(result$camada))
})

test_that("depth_order_ok validates top/base ordering", {
  expect_false(depth_order_ok(40, 20))
  expect_false(depth_order_ok(20, 20))
  expect_true(depth_order_ok(0, 20))
  expect_true(depth_order_ok(NA, 20))
  expect_true(depth_order_ok(NULL, NULL))
})

test_that("build_sample_from_inputs sets vegetal fields when vegetal in groups", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input(
    "vegetal",
    analises_vegetal = "nitrogenio",
    tipo_amostra_vegetal = "folha",
    cultura_planta = "Eucalipto"
  )
  result <- build_sample_from_inputs(input, app_config, NULL)

  expect_equal(result$tipo_amostra_vegetal, "folha")
  expect_equal(result$cultura_planta, "Eucalipto")
  expect_true(is.na(result$elementos_aa_icp))
})

test_that("build_sample_from_inputs sets NA for vegetal fields when solo_rotina only", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input("solo_rotina", analises_solo_rotina = "rotina_basica")
  result <- build_sample_from_inputs(input, app_config, NULL)

  expect_true(is.na(result$tipo_amostra_vegetal))
  expect_true(is.na(result$cultura_planta))
})

test_that("build_sample_from_inputs sets CHN fields and pre_tratamento correctly", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))

  input_org <- make_base_input(
    "chn",
    analises_chn = c("carbono_organico_total", "nitrogenio"),
    carbonato_presente = "sim",
    percentual_c_estimado = 3.5,
    percentual_n_estimado = 0.4,
    numero_registro_projeto = "PROJ-001"
  )
  result_org <- build_sample_from_inputs(input_org, app_config, NULL)
  expect_true(result_org$pre_tratamento_necessario)
  expect_equal(result_org$carbonato_presente, "sim")
  expect_equal(result_org$percentual_c_estimado, 3.5)
  expect_equal(result_org$percentual_n_estimado, 0.4)
  expect_equal(result_org$numero_registro_projeto, "PROJ-001")

  input_tot <- make_base_input("chn", analises_chn = "carbono_total", carbonato_presente = "nao")
  result_tot <- build_sample_from_inputs(input_tot, app_config, NULL)
  expect_false(result_tot$pre_tratamento_necessario)
})

test_that("build_sample_from_inputs sets AA/ICP fields when selected", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input(
    "absorcao_atomica",
    analises_absorcao_atomica = "elementos_absorcao_atomica",
    elementos_aa_icp = "Ca, Mg, Fe",
    tipo_digestao = "Nitrico-perclorica",
    volume_apos_digestao = 50,
    aliquota = 5,
    diluicao = "1:10",
    volume_final = 25,
    departamento_origem = "dps",
    projeto_registrado = "nao"
  )
  result <- build_sample_from_inputs(input, app_config, NULL)

  expect_equal(result$elementos_aa_icp, "Ca, Mg, Fe")
  expect_equal(result$tipo_digestao, "Nitrico-perclorica")
  expect_equal(result$volume_apos_digestao, 50)
  expect_equal(result$aliquota, 5)
  expect_equal(result$diluicao, "1:10")
  expect_equal(result$volume_final, 25)
  expect_equal(result$departamento_origem, "dps")
  expect_equal(result$projeto_registrado, "nao")
  expect_true(is.na(result$tipo_amostra_vegetal))
  expect_true(is.na(result$carbonato_presente))
})

test_that("build_sample_from_inputs sets NA for AA fields when not in groups", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input("solo_rotina", analises_solo_rotina = "rotina_basica")
  result <- build_sample_from_inputs(input, app_config, NULL)

  expect_true(is.na(result$elementos_aa_icp))
  expect_true(is.na(result$tipo_digestao))
  expect_true(is.na(result$departamento_origem))
})

test_that("build_sample_from_inputs handles NULL point as NA coordinates", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input("solo_rotina", analises_solo_rotina = "rotina_basica")
  result <- build_sample_from_inputs(input, app_config, NULL)

  expect_true(is.na(result$latitude_wgs84))
  expect_true(is.na(result$longitude_wgs84))
})

test_that("build_sample_from_inputs with multiple groups collects all analyses", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- make_base_input(
    c("solo_rotina", "vegetal"),
    analises_solo_rotina = "rotina_basica",
    analises_vegetal = c("nitrogenio", "boro"),
    tipo_amostra_vegetal = "galho",
    cultura_planta = "Pinus"
  )
  result <- build_sample_from_inputs(input, app_config, NULL)

  expect_equal(nrow(result$analises), 3)
  expect_equal(result$tipo_amostra_vegetal, "galho")
  expect_true(is.na(result$carbonato_presente))
})
