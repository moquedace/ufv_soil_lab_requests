packages <- c(
  "shiny",
  "shinyjs",
  "bsicons",
  "bslib",
  "leaflet",
  "DT",
  "httr2",
  "jsonlite",
  "later",
  "yaml",
  "writexl",
  "googlesheets4",
  "googledrive",
  "testthat",
  "shinytest2",
  "withr"
)

installed <- rownames(installed.packages())
missing <- setdiff(packages, installed)

if (length(missing)) {
  install.packages(missing)
}

message("Pacotes prontos: ", paste(packages, collapse = ", "))
