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

test_that("next_request_id has expected prefix and random suffix", {
  id <- next_request_id()
  expect_match(id, "^SOL-[0-9]{8}-[0-9]{6}-[0-9a-f]{4}$")
})

test_that("next_request_id produces unique ids within the same second", {
  ids <- replicate(200, next_request_id())
  # com sufixo aleatorio, colisoes no mesmo segundo devem ser raras
  expect_gt(length(unique(ids)), 190)
})

test_that("next_sample_id derives from request id for global uniqueness", {
  request_id <- "SOL-20260604-101500-a1b2"
  expect_equal(next_sample_id(request_id, 1), "AMS-20260604-101500-a1b2-001")
  expect_equal(next_sample_id(request_id, 12), "AMS-20260604-101500-a1b2-012")
})

test_that("sample ids from different requests never collide", {
  id1 <- next_sample_id("SOL-20260604-101500-aaaa", 1)
  id2 <- next_sample_id("SOL-20260604-101500-bbbb", 1)
  expect_false(identical(id1, id2))
})

test_that("random_suffix returns lowercase hex of requested length", {
  expect_match(random_suffix(4), "^[0-9a-f]{4}$")
  expect_match(random_suffix(8), "^[0-9a-f]{8}$")
})
