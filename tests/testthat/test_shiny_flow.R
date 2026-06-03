new_app_driver <- function(name) {
  withr::local_envvar(c(
    UFV_SOIL_LAB_TEST_MODE = "true",
    USE_GOOGLE_SHEETS = "false"
  ))

  shinytest2::AppDriver$new(
    app_dir = testthat::test_path("../.."),
    name = name,
    seed = 123
  )
}

fill_requester <- function(app, name = "Teste Automatizado") {
  app$set_inputs(
    `solicitante-nome_solicitante` = name,
    `solicitante-email` = "teste@example.com",
    `solicitante-telefone` = "(31) 99999-0000",
    `solicitante-cidade_solicitante` = "Vicosa"
  )
}

fill_sample_base <- function(app, reference = "Amostra teste 0-20 cm", groups = "solo_rotina") {
  app$set_inputs(
    `solicitante-amostras-referencia_amostra` = reference,
    `solicitante-amostras-tipo_material` = "Solo",
    `solicitante-amostras-grupos_analise` = groups,
    `solicitante-amostras-municipio_amostra` = "Vicosa",
    `solicitante-amostras-uf_amostra` = "MG",
    `solicitante-amostras-localidade_descricao` = "Talhao teste"
  )
  app$wait_for_idle()
}

test_that("solicitante cannot submit without samples", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_sem_amostra")
  on.exit(app$stop(), add = TRUE)

  fill_requester(app)
  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_equal(app$get_value(export = "solicitante-ultimo_envio_id"), "")
  expect_equal(app$get_value(export = "solicitacoes_count"), 1)
})

test_that("solicitante can add and submit one sample with multiple groups", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_multiplos_grupos")
  on.exit(app$stop(), add = TRUE)

  fill_requester(app)
  fill_sample_base(app, groups = c("solo_rotina", "vegetal"))

  app$set_inputs(
    `solicitante-amostras-analises_solo_rotina` = "rotina_basica",
    `solicitante-amostras-analises_vegetal` = "nitrogenio",
    `solicitante-amostras-mapa_click` = list(lat = -20.7546, lng = -42.8825)
  )

  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()
  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_match(app$get_value(export = "solicitante-ultimo_envio_id"), "SOL-", fixed = TRUE)
  expect_equal(app$get_value(export = "solicitacoes_count"), 2)
  expect_equal(app$get_value(export = "amostras_count"), 2)
  expect_equal(app$get_value(export = "analises_count"), 3)
})

test_that("solicitante can submit CHN with organic carbon and carbonate flag", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_chn")
  on.exit(app$stop(), add = TRUE)

  fill_requester(app, name = "Teste CHN")
  fill_sample_base(app, reference = "Material CHN teste", groups = "chn")

  app$set_inputs(
    `solicitante-amostras-analises_chn` = c("carbono_organico_total", "nitrogenio"),
    `solicitante-amostras-carbonato_presente` = "sim",
    `solicitante-amostras-mapa_click` = list(lat = -20.7546, lng = -42.8825)
  )

  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()
  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_match(app$get_value(export = "solicitante-ultimo_envio_id"), "SOL-", fixed = TRUE)
  expect_equal(app$get_value(export = "solicitacoes_count"), 2)
  expect_equal(app$get_value(export = "amostras_count"), 2)
  expect_equal(app$get_value(export = "analises_count"), 3)
})

test_that("recepcao can save internal fields for a selected request", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("recepcao_campos_internos")
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(`recepcao-solicitacoes_rows_selected` = 1)
  app$wait_for_idle()

  app$set_inputs(
    `recepcao-data_entrada_lab` = "2026-06-03",
    `recepcao-numero_laboratorio` = "LAB-TESTE-001",
    `recepcao-custo_total_lab` = "123,45",
    `recepcao-pedido_numero_lab` = "PED-001",
    `recepcao-status_interno_edit` = "Em analise",
    `recepcao-forma_pagamento_lab` = "PIX",
    `recepcao-observacoes_internas` = "Teste automatizado"
  )

  app$click("recepcao-salvar_interno")
  app$wait_for_idle()

  expect_match(app$get_value(export = "recepcao-ultimo_salvamento_id"), "SOL-", fixed = TRUE)
})
