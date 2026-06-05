mk_sample <- function(analise_ids, lab = "solo_rotina", ref = "A") {
  list(
    referencia_amostra = ref,
    analises = data.frame(
      laboratorio = lab,
      analise_id = analise_ids,
      analise_nome = analise_ids,
      stringsAsFactors = FALSE
    ),
    analises_ids = analise_ids,
    analises_nomes = analise_ids,
    grupos_analise = lab
  )
}

test_that("selected_sample_indices reads multiple selection", {
  expect_equal(selected_sample_indices(list(tabela_amostras_rows_selected = c(2, 4))), c(2L, 4L))
})

test_that("selected_sample_indices falls back to csv test input", {
  input <- list(tabela_amostras_rows_selected = integer(), test_selected_sample_indices = "1, 3, 5")
  expect_equal(selected_sample_indices(input), c(1L, 3L, 5L))
})

test_that("selected_sample_indices falls back to single test index", {
  expect_equal(selected_sample_indices(list(test_selected_sample_index = 2)), 2L)
  expect_equal(selected_sample_indices(list()), integer())
})

test_that("remove_samples_at removes all given indices", {
  samples <- list(mk_sample("x", ref = "A"), mk_sample("x", ref = "B"), mk_sample("x", ref = "C"))
  res <- remove_samples_at(samples, c(1, 3))
  expect_equal(length(res), 1)
  expect_equal(res[[1]]$referencia_amostra, "B")
})

test_that("remove_samples_at ignores invalid indices and can clear all", {
  samples <- list(mk_sample("x"), mk_sample("x"))
  expect_equal(length(remove_samples_at(samples, 99)), 2)
  expect_equal(length(remove_samples_at(samples, c(1, 2))), 0)
})

test_that("apply_analyses_to_samples appends without duplicating", {
  samples <- list(mk_sample("rotina_basica"), mk_sample("rotina_basica"))
  new <- data.frame(laboratorio = "chn", analise_id = "carbono_total",
                    analise_nome = "Carbono total", stringsAsFactors = FALSE)
  res <- apply_analyses_to_samples(samples, c(1, 2), new, "acrescentar")

  expect_equal(nrow(res[[1]]$analises), 2)
  expect_setequal(res[[1]]$analises$analise_id, c("rotina_basica", "carbono_total"))
  expect_setequal(res[[1]]$grupos_analise, c("solo_rotina", "chn"))
  expect_equal(res[[1]]$analises_ids, res[[1]]$analises$analise_id)
})

test_that("apply_analyses_to_samples does not duplicate existing analysis", {
  samples <- list(mk_sample("rotina_basica"))
  new <- data.frame(laboratorio = "solo_rotina", analise_id = "rotina_basica",
                    analise_nome = "Rotina", stringsAsFactors = FALSE)
  res <- apply_analyses_to_samples(samples, 1, new, "acrescentar")
  expect_equal(nrow(res[[1]]$analises), 1)
})

test_that("apply_analyses_to_samples replaces analyses in replace mode", {
  samples <- list(mk_sample("rotina_basica"), mk_sample("rotina_basica"))
  new <- data.frame(laboratorio = "chn", analise_id = "carbono_total",
                    analise_nome = "Carbono total", stringsAsFactors = FALSE)
  res <- apply_analyses_to_samples(samples, 1, new, "substituir")

  expect_equal(res[[1]]$analises$analise_id, "carbono_total")
  expect_equal(res[[1]]$grupos_analise, "chn")
  # a segunda amostra (nao selecionada) permanece intacta
  expect_equal(res[[2]]$analises$analise_id, "rotina_basica")
})

test_that("apply_analyses_to_samples ignores invalid indices and empty analyses", {
  samples <- list(mk_sample("rotina_basica"))
  new <- data.frame(laboratorio = "chn", analise_id = "carbono_total",
                    analise_nome = "Carbono total", stringsAsFactors = FALSE)
  empty <- new[0, , drop = FALSE]
  expect_identical(apply_analyses_to_samples(samples, 99, new), samples)
  expect_identical(apply_analyses_to_samples(samples, 1, empty), samples)
})

test_that("collect_analyses_from gathers selected analyses by prefix", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- list(
    aplicar_analises_solo_rotina = "rotina_basica",
    aplicar_analises_chn = c("carbono_total", "nitrogenio")
  )
  res <- collect_analyses_from(input, app_config, c("solo_rotina", "chn"), "aplicar_analises_")
  expect_equal(nrow(res), 3)
  expect_equal(res$laboratorio, c("solo_rotina", "chn", "chn"))
  expect_equal(res$analise_id, c("rotina_basica", "carbono_total", "nitrogenio"))
})

test_that("collect_analyses_from returns empty frame when nothing selected", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  res <- collect_analyses_from(list(), app_config, c("solo_rotina"), "aplicar_analises_")
  expect_equal(nrow(res), 0)
  expect_equal(names(res), c("laboratorio", "analise_id", "analise_nome"))
})
