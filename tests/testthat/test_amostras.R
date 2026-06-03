test_that("remove_sample_at removes the selected sample", {
  sample <- list(referencia_amostra = "Amostra 1")
  samples <- list(sample, list(referencia_amostra = "Amostra 2"))

  result <- remove_sample_at(samples, 1)

  expect_equal(length(result), 1)
  expect_equal(result[[1]]$referencia_amostra, "Amostra 2")
})

test_that("duplicate_sample_at appends a copied sample", {
  sample <- list(referencia_amostra = "Amostra 1")
  samples <- list(sample)

  result <- duplicate_sample_at(samples, 1)

  expect_equal(length(result), 2)
  expect_equal(result[[2]]$referencia_amostra, "Amostra 1 copia")
})

test_that("replace_sample_at updates the selected sample", {
  samples <- list(
    list(referencia_amostra = "Amostra 1"),
    list(referencia_amostra = "Amostra 2")
  )

  result <- replace_sample_at(samples, 2, list(referencia_amostra = "Amostra editada"))

  expect_equal(length(result), 2)
  expect_equal(result[[1]]$referencia_amostra, "Amostra 1")
  expect_equal(result[[2]]$referencia_amostra, "Amostra editada")
})

test_that("selected_sample_index falls back to hidden test index", {
  input <- list(
    tabela_amostras_rows_selected = integer(),
    test_selected_sample_index = 2
  )

  expect_equal(selected_sample_index(input), 2L)

  input$tabela_amostras_rows_selected <- 1
  expect_equal(selected_sample_index(input), 1)
})
