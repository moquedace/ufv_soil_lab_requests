test_that("parse_decimal_coordinate accepts decimal point format", {
  result <- parse_decimal_coordinate("-22.7253, -47.6492")

  expect_equal(result$lat, -22.7253)
  expect_equal(result$lon, -47.6492)
})

test_that("parse_decimal_coordinate accepts decimal comma format", {
  result <- parse_decimal_coordinate("-22,7253 -47,6492")

  expect_equal(result$lat, -22.7253)
  expect_equal(result$lon, -47.6492)
})

test_that("parse_decimal_coordinate rejects invalid coordinates", {
  expect_null(parse_decimal_coordinate("Piracicaba"))
  expect_null(parse_decimal_coordinate("-200, -47"))
  expect_null(parse_decimal_coordinate("-22, -300"))
})
