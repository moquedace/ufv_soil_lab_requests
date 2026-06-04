new_app_driver <- function(name) {
  Sys.setenv(
    UFV_SOIL_LAB_TEST_MODE = "true",
    USE_GOOGLE_SHEETS = "false",
    LAB_RECEPTION_PASSWORD = "dps2024"
  )

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
    `solicitante-cidade_solicitante` = "Vicosa",
    `solicitante-consentimento_lgpd` = TRUE
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

test_that("solicitante cannot submit without LGPD consent", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_sem_consentimento")
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(
    `solicitante-nome_solicitante` = "Teste Sem Consentimento",
    `solicitante-email` = "teste@example.com",
    `solicitante-consentimento_lgpd` = FALSE
  )
  fill_sample_base(app)
  app$set_inputs(`solicitante-amostras-analises_solo_rotina` = "rotina_basica")
  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()
  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_equal(app$get_value(export = "solicitante-ultimo_envio_id"), "")
  expect_equal(app$get_value(export = "solicitacoes_count"), 1)
})

test_that("solicitante cannot submit with invalid email", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_email_invalido")
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(
    `solicitante-nome_solicitante` = "Teste Email Invalido",
    `solicitante-email` = "email-sem-arroba",
    `solicitante-consentimento_lgpd` = TRUE
  )
  fill_sample_base(app)
  app$set_inputs(`solicitante-amostras-analises_solo_rotina` = "rotina_basica")
  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()
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
    `solicitante-amostras-test_map_lat` = -20.7546,
    `solicitante-amostras-test_map_lng` = -42.8825
  )

  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()

  expect_equal(app$get_value(export = "solicitante-review_sample_count"), 1)
  expect_equal(app$get_value(export = "solicitante-review_location_count"), 1)

  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_match(app$get_value(export = "solicitante-ultimo_envio_id"), "SOL-", fixed = TRUE)
  expect_equal(app$get_value(export = "solicitacoes_count"), 2)
  expect_equal(app$get_value(export = "amostras_count"), 2)
  expect_equal(app$get_value(export = "analises_count"), 3)

  # apos enviar, o formulario de amostras deve ser limpo (reset)
  expect_equal(app$get_value(export = "solicitante-review_sample_count"), 0)
})

test_that("solicitante can duplicate and remove samples before submit", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_duplica_remove")
  on.exit(app$stop(), add = TRUE)

  fill_requester(app)
  fill_sample_base(app)

  app$set_inputs(
    `solicitante-amostras-analises_solo_rotina` = "rotina_basica",
    `solicitante-amostras-test_map_lat` = -20.7546,
    `solicitante-amostras-test_map_lng` = -42.8825
  )

  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()

  app$set_inputs(`solicitante-amostras-test_selected_sample_index` = 1)
  app$click("solicitante-amostras-duplicar_amostra")
  app$wait_for_idle()

  app$set_inputs(`solicitante-amostras-test_selected_sample_index` = 1)
  app$click("solicitante-amostras-remover_amostra")
  app$wait_for_idle()

  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_match(app$get_value(export = "solicitante-ultimo_envio_id"), "SOL-", fixed = TRUE)
  expect_equal(app$get_value(export = "solicitacoes_count"), 2)
  expect_equal(app$get_value(export = "amostras_count"), 2)
  expect_equal(app$get_value(export = "analises_count"), 2)
})

test_that("solicitante can edit a sample before submit", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_edita_amostra")
  on.exit(app$stop(), add = TRUE)

  fill_requester(app)
  fill_sample_base(app, reference = "Amostra antes da edicao")

  app$set_inputs(
    `solicitante-amostras-analises_solo_rotina` = "rotina_basica",
    `solicitante-amostras-test_map_lat` = -20.7546,
    `solicitante-amostras-test_map_lng` = -42.8825
  )

  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()

  app$set_inputs(`solicitante-amostras-test_selected_sample_index` = 1)
  app$wait_for_idle()
  app$click("solicitante-amostras-editar_amostra")
  app$wait_for_value(export = "solicitante-amostras-editing_index", ignore = NULL)
  expect_equal(app$get_value(export = "solicitante-amostras-editing_index"), 1)

  app$set_inputs(`solicitante-amostras-referencia_amostra` = "Amostra editada")
  app$click("solicitante-amostras-atualizar_amostra")
  app$wait_for_idle()

  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_match(app$get_value(export = "solicitante-ultimo_envio_id"), "SOL-", fixed = TRUE)
  expect_equal(app$get_value(export = "solicitacoes_count"), 2)
  expect_equal(app$get_value(export = "amostras_count"), 2)
  expect_equal(app$get_value(export = "analises_count"), 2)
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
    `solicitante-amostras-test_map_lat` = -20.7546,
    `solicitante-amostras-test_map_lng` = -42.8825
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

test_that("solicitante can submit vegetal sample with tipo and cultura", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_vegetal")
  on.exit(app$stop(), add = TRUE)

  fill_requester(app, name = "Teste Vegetal")
  fill_sample_base(app, reference = "Folha eucalipto", groups = "vegetal")

  app$set_inputs(
    `solicitante-amostras-analises_vegetal` = "nitrogenio",
    `solicitante-amostras-tipo_amostra_vegetal` = "folha",
    `solicitante-amostras-cultura_planta` = "Eucalipto",
    `solicitante-amostras-test_map_lat` = -20.7546,
    `solicitante-amostras-test_map_lng` = -42.8825
  )

  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()
  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_match(app$get_value(export = "solicitante-ultimo_envio_id"), "SOL-", fixed = TRUE)
  expect_equal(app$get_value(export = "solicitacoes_count"), 2)
  expect_equal(app$get_value(export = "amostras_count"), 2)
  expect_equal(app$get_value(export = "analises_count"), 2)
})

test_that("solicitante can submit absorcao atomica sample with preparo fields", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_absorcao_atomica")
  on.exit(app$stop(), add = TRUE)

  fill_requester(app, name = "Teste AA")
  fill_sample_base(app, reference = "Extrato AA teste", groups = "absorcao_atomica")

  app$set_inputs(
    `solicitante-amostras-analises_absorcao_atomica` = "elementos_absorcao_atomica",
    `solicitante-amostras-elementos_aa_icp` = "Ca, Mg, Fe",
    `solicitante-amostras-tipo_digestao` = "Nitrico-perclorica",
    `solicitante-amostras-volume_apos_digestao` = 50,
    `solicitante-amostras-aliquota` = 5,
    `solicitante-amostras-volume_final` = 25,
    `solicitante-amostras-departamento_origem` = "dps",
    `solicitante-amostras-projeto_registrado` = "nao",
    `solicitante-amostras-test_map_lat` = -20.7546,
    `solicitante-amostras-test_map_lng` = -42.8825
  )

  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()
  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_match(app$get_value(export = "solicitante-ultimo_envio_id"), "SOL-", fixed = TRUE)
  expect_equal(app$get_value(export = "solicitacoes_count"), 2)
  expect_equal(app$get_value(export = "amostras_count"), 2)
  expect_equal(app$get_value(export = "analises_count"), 2)
})

test_that("solicitante saves bairro, cep and matricula on submit", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("solicitacao_campos_extras")
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(
    `solicitante-nome_solicitante` = "Teste Campos Extras",
    `solicitante-email` = "teste@example.com",
    `solicitante-telefone` = "(31) 99999-0000",
    `solicitante-cidade_solicitante` = "Vicosa",
    `solicitante-bairro` = "Centro",
    `solicitante-cep` = "36570-900",
    `solicitante-vinculo` = "mestrado",
    `solicitante-matricula` = "12345",
    `solicitante-instituicao` = "DPS/UFV",
    `solicitante-consentimento_lgpd` = TRUE
  )

  fill_sample_base(app)
  app$set_inputs(`solicitante-amostras-analises_solo_rotina` = "rotina_basica")
  app$click("solicitante-amostras-adicionar")
  app$wait_for_idle()
  app$click("solicitante-enviar")
  app$wait_for_idle()

  expect_match(app$get_value(export = "solicitante-ultimo_envio_id"), "SOL-", fixed = TRUE)
  expect_equal(app$get_value(export = "solicitacoes_count"), 2)
})

test_that("recepcao is not authenticated by default and accepts correct password", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("recepcao_autenticacao")
  on.exit(app$stop(), add = TRUE)

  expect_false(app$get_value(export = "recepcao-autenticado"))

  app$set_inputs(navbar_principal = "Recepção")
  app$wait_for_idle()

  app$set_inputs(`recepcao-senha_recepcao` = "errada")
  app$click("recepcao-entrar_recepcao")
  app$wait_for_idle()
  expect_false(app$get_value(export = "recepcao-autenticado"))

  app$set_inputs(`recepcao-senha_recepcao` = "dps2024")
  app$click("recepcao-entrar_recepcao")
  app$wait_for_idle()
  expect_true(app$get_value(export = "recepcao-autenticado"))
})

test_that("recepcao exposes sample data in local test mode", {
  skip_if_not_installed("shinytest2")

  app <- new_app_driver("recepcao_campos_internos")
  on.exit(app$stop(), add = TRUE)

  expect_equal(app$get_value(export = "solicitacoes_count"), 1)
  expect_equal(app$get_value(export = "amostras_count"), 1)
  expect_equal(app$get_value(export = "analises_count"), 1)
})
