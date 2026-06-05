test_that("split_lines splits and trims, dropping blanks", {
  expect_equal(split_lines("a\nb\nc"), c("a", "b", "c"))
  expect_equal(split_lines("  a \n\n  b  "), c("a", "b"))
  expect_equal(split_lines("x\r\ny"), c("x", "y"))
  expect_equal(split_lines(""), character())
  expect_equal(split_lines(NULL), character())
})

test_that("parse_depth_layer extracts top/base from common formats", {
  a <- parse_depth_layer("0-20")
  expect_equal(a$de, 0); expect_equal(a$ate, 20); expect_equal(a$label, "0-20")

  b <- parse_depth_layer("0 a 20 cm")
  expect_equal(b$de, 0); expect_equal(b$ate, 20)

  d <- parse_depth_layer("20")
  expect_equal(d$de, 20); expect_true(is.na(d$ate))

  e <- parse_depth_layer("2,5-7,5")
  expect_equal(e$de, 2.5); expect_equal(e$ate, 7.5)
})

test_that("build_sample_references numeric returns a data frame of references", {
  res <- build_sample_references("numerico", prefixo = "P", inicio = 1, fim = 3, digitos = 3)
  expect_s3_class(res, "data.frame")
  expect_equal(res$referencia, c("P001", "P002", "P003"))
  expect_true(all(is.na(res$profundidade_de)))
  expect_true(all(is.na(res$profundidade_ate)))
})

test_that("build_sample_references numeric applies suffix", {
  res <- build_sample_references("numerico", prefixo = "T", inicio = 1, fim = 2, digitos = 2, sufixo = " sup")
  expect_equal(res$referencia, c("T01 sup", "T02 sup"))
})

test_that("build_sample_references numeric handles single digit padding", {
  res <- build_sample_references("numerico", prefixo = "A", inicio = 8, fim = 10, digitos = 1)
  expect_equal(res$referencia, c("A8", "A9", "A10"))
})

test_that("build_sample_references layers mode combines points x layers with depths", {
  res <- build_sample_references(
    "camadas", prefixo = "P", inicio = 1, fim = 2, digitos = 1,
    camadas = c("0-20", "20-40")
  )
  expect_equal(res$referencia, c("P1 0-20", "P1 20-40", "P2 0-20", "P2 20-40"))
  expect_equal(res$profundidade_de, c(0, 20, 0, 20))
  expect_equal(res$profundidade_ate, c(20, 40, 20, 40))
})

test_that("build_sample_references layers mode returns empty without layers", {
  res <- build_sample_references("camadas", prefixo = "P", inicio = 1, fim = 3, camadas = character())
  expect_equal(nrow(res), 0)
})

test_that("build_sample_references list mode trims and drops blanks", {
  res <- build_sample_references("lista", lista = c(" Amostra A ", "", "Amostra B"))
  expect_equal(res$referencia, c("Amostra A", "Amostra B"))
  expect_true(all(is.na(res$profundidade_de)))
})

test_that("build_sample_references returns empty for invalid range", {
  expect_equal(nrow(build_sample_references("numerico", prefixo = "P", inicio = 5, fim = 2)), 0)
  expect_equal(nrow(build_sample_references("numerico", prefixo = "P", inicio = NA, fim = NA)), 0)
})

test_that("build_sample_references caps at the limit", {
  res <- build_sample_references("numerico", prefixo = "P", inicio = 1, fim = 5000, digitos = 1, limite = 1000)
  expect_equal(nrow(res), 1000)
})

test_that("build_sample_references handles large realistic batch", {
  res <- build_sample_references("numerico", prefixo = "P", inicio = 1, fim = 400, digitos = 3)
  expect_equal(nrow(res), 400)
  expect_equal(res$referencia[[1]], "P001")
  expect_equal(res$referencia[[400]], "P400")
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

test_that("preview_references accepts the data frame form", {
  res <- build_sample_references("numerico", prefixo = "P", inicio = 1, fim = 2, digitos = 1)
  expect_match(preview_references(res), "2 amostras: P1, P2")
})

test_that("format_depth shows range, classification or dash", {
  expect_equal(format_depth(list(profundidade_de = 0, profundidade_ate = 20)), "0-20 cm")
  expect_equal(format_depth(list(profundidade_de = NA, profundidade_ate = NA, camada = "superficial")), "Superficial")
  expect_equal(format_depth(list(profundidade_de = NA, profundidade_ate = NA, camada = "")), "—")
  expect_equal(format_depth(list()), "—")
})
