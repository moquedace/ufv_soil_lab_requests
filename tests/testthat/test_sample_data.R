test_that("safe_rbind works with identical schemas", {
  a <- data.frame(x = 1, y = "a", stringsAsFactors = FALSE)
  b <- data.frame(x = 2, y = "b", stringsAsFactors = FALSE)
  result <- safe_rbind(a, b)
  expect_equal(nrow(result), 2)
  expect_equal(result$x, c(1, 2))
})

test_that("safe_rbind fills missing column in first frame with NA", {
  a <- data.frame(x = 1, stringsAsFactors = FALSE)
  b <- data.frame(x = 2, y = "novo", stringsAsFactors = FALSE)
  result <- safe_rbind(a, b)
  expect_equal(nrow(result), 2)
  expect_true("y" %in% names(result))
  expect_true(is.na(result$y[[1]]))
  expect_equal(result$y[[2]], "novo")
})

test_that("safe_rbind fills missing column in second frame with NA", {
  a <- data.frame(x = 1, y = "existente", stringsAsFactors = FALSE)
  b <- data.frame(x = 2, stringsAsFactors = FALSE)
  result <- safe_rbind(a, b)
  expect_equal(nrow(result), 2)
  expect_true("y" %in% names(result))
  expect_equal(result$y[[1]], "existente")
  expect_true(is.na(result$y[[2]]))
})

test_that("safe_rbind returns second when first is empty", {
  a <- data.frame()
  b <- data.frame(x = 1, y = "a", stringsAsFactors = FALSE)
  result <- safe_rbind(a, b)
  expect_equal(nrow(result), 1)
  expect_equal(result$x, 1)
})

test_that("safe_rbind returns first when second is empty", {
  a <- data.frame(x = 1, stringsAsFactors = FALSE)
  b <- data.frame()
  result <- safe_rbind(a, b)
  expect_equal(nrow(result), 1)
})

test_that("safe_rbind handles completely disjoint schemas", {
  a <- data.frame(col_a = "valor_a", stringsAsFactors = FALSE)
  b <- data.frame(col_b = "valor_b", stringsAsFactors = FALSE)
  result <- safe_rbind(a, b)
  expect_equal(nrow(result), 2)
  expect_true(all(c("col_a", "col_b") %in% names(result)))
  expect_true(is.na(result$col_b[[1]]))
  expect_true(is.na(result$col_a[[2]]))
})

test_that("safe_rbind preserves row order", {
  a <- data.frame(id = 1:3, stringsAsFactors = FALSE)
  b <- data.frame(id = 4:5, stringsAsFactors = FALSE)
  result <- safe_rbind(a, b)
  expect_equal(result$id, 1:5)
})
