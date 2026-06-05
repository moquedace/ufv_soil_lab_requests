test_that("split_lines splits and trims, dropping blanks", {
  expect_equal(split_lines("a\nb\nc"), c("a", "b", "c"))
  expect_equal(split_lines("  a \n\n  b  "), c("a", "b"))
  expect_equal(split_lines("x\r\ny"), c("x", "y"))
  expect_equal(split_lines(""), character())
  expect_equal(split_lines(NULL), character())
})

test_that("build_sample_references numeric mode pads and ranges", {
  refs <- build_sample_references("numerico", prefixo = "P", inicio = 1, fim = 3, digitos = 3)
  expect_equal(refs, c("P001", "P002", "P003"))
})

test_that("build_sample_references numeric mode applies suffix", {
  refs <- build_sample_references("numerico", prefixo = "T", inicio = 1, fim = 2, digitos = 2, sufixo = " 0-20")
  expect_equal(refs, c("T01 0-20", "T02 0-20"))
})

test_that("build_sample_references numeric handles single digit padding", {
  refs <- build_sample_references("numerico", prefixo = "A", inicio = 8, fim = 10, digitos = 1)
  expect_equal(refs, c("A8", "A9", "A10"))
})

test_that("build_sample_references layers mode combines points x depths in order", {
  refs <- build_sample_references(
    "camadas", prefixo = "P", inicio = 1, fim = 2, digitos = 1,
    profundidades = c("0-20", "20-40")
  )
  expect_equal(refs, c("P1 0-20", "P1 20-40", "P2 0-20", "P2 20-40"))
})

test_that("build_sample_references layers mode returns empty without depths", {
  refs <- build_sample_references("camadas", prefixo = "P", inicio = 1, fim = 3, profundidades = character())
  expect_equal(refs, character())
})

test_that("build_sample_references list mode trims and drops blanks", {
  refs <- build_sample_references("lista", lista = c(" Amostra A ", "", "Amostra B"))
  expect_equal(refs, c("Amostra A", "Amostra B"))
})

test_that("build_sample_references returns empty for invalid range", {
  expect_equal(build_sample_references("numerico", prefixo = "P", inicio = 5, fim = 2), character())
  expect_equal(build_sample_references("numerico", prefixo = "P", inicio = NA, fim = NA), character())
})

test_that("build_sample_references caps at the limit", {
  refs <- build_sample_references("numerico", prefixo = "P", inicio = 1, fim = 5000, digitos = 1, limite = 1000)
  expect_equal(length(refs), 1000)
})

test_that("build_sample_references handles large realistic batch", {
  refs <- build_sample_references("numerico", prefixo = "P", inicio = 1, fim = 400, digitos = 3)
  expect_equal(length(refs), 400)
  expect_equal(refs[[1]], "P001")
  expect_equal(refs[[400]], "P400")
})

test_that("preview_references summarizes count and examples", {
  expect_match(preview_references(character()), "Nenhuma referência")
  expect_match(preview_references(c("P1", "P2")), "2 amostras: P1, P2")
  big <- sprintf("P%03d", 1:400)
  out <- preview_references(big)
  expect_match(out, "400 amostras")
  expect_match(out, "P001, P002, P003")
  expect_match(out, "P399, P400")
})
