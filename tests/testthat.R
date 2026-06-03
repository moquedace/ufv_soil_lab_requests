old_env <- Sys.getenv(c("UFV_SOIL_LAB_TEST_MODE", "USE_GOOGLE_SHEETS"))
on.exit({
  Sys.setenv(
    UFV_SOIL_LAB_TEST_MODE = old_env[["UFV_SOIL_LAB_TEST_MODE"]],
    USE_GOOGLE_SHEETS = old_env[["USE_GOOGLE_SHEETS"]]
  )
}, add = TRUE)

Sys.setenv(
  UFV_SOIL_LAB_TEST_MODE = "true",
  USE_GOOGLE_SHEETS = "false"
)

testthat::test_dir("tests/testthat")
