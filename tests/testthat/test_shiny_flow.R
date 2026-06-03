test_that("solicitante can add and submit a sample", {
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("../.."),
    name = "solicitacao_basica",
    seed = 123
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(
    `solicitante-nome_solicitante` = "Teste Automatizado",
    `solicitante-email` = "teste@example.com",
    `solicitante-telefone` = "(31) 99999-0000",
    `solicitante-cidade_solicitante` = "Vicosa",
    `solicitante-amostras-referencia_amostra` = "Amostra teste 0-20 cm",
    `solicitante-amostras-tipo_material` = "Solo",
    `solicitante-amostras-laboratorio` = "solo_rotina",
    `solicitante-amostras-municipio_amostra` = "Vicosa",
    `solicitante-amostras-uf_amostra` = "MG",
    `solicitante-amostras-localidade_descricao` = "Talhao teste"
  )

  app$set_inputs(
    `solicitante-amostras-analises` = "rotina_basica",
    `solicitante-amostras-mapa_click` = list(lat = -20.7546, lng = -42.8825)
  )

  app$click("solicitante-amostras-adicionar")
  app$click("solicitante-enviar")

  expect_match(
    app$get_value(output = "solicitante-ultimo_envio_id"),
    "SOL-",
    fixed = TRUE
  )
})
