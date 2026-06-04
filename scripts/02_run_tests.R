old_env <- Sys.getenv(c("UFV_SOIL_LAB_TEST_MODE", "USE_GOOGLE_SHEETS", "LAB_RECEPTION_PASSWORD"))
on.exit({
  Sys.setenv(
    UFV_SOIL_LAB_TEST_MODE = old_env[["UFV_SOIL_LAB_TEST_MODE"]],
    USE_GOOGLE_SHEETS = old_env[["USE_GOOGLE_SHEETS"]],
    LAB_RECEPTION_PASSWORD = old_env[["LAB_RECEPTION_PASSWORD"]]
  )
}, add = TRUE)

Sys.setenv(
  UFV_SOIL_LAB_TEST_MODE = "true",
  USE_GOOGLE_SHEETS = "false",
  LAB_RECEPTION_PASSWORD = "dps2024"
)

testthat::test_dir("tests/testthat")
