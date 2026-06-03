test_that("collect_selected_analyses combines multiple analysis groups", {
  app_config <- load_app_config(testthat::test_path("../../config/analises.yml"))
  input <- list(
    analises_solo_rotina = "rotina_basica",
    analises_vegetal = c("nitrogenio", "boro")
  )

  result <- collect_selected_analyses(
    input = input,
    app_config = app_config,
    groups = c("solo_rotina", "vegetal")
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$laboratorio, c("solo_rotina", "vegetal", "vegetal"))
  expect_equal(result$analise_id, c("rotina_basica", "nitrogenio", "boro"))
})
