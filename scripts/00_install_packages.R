packages <- c(
  "shiny",
  "bslib",
  "leaflet",
  "DT",
  "httr2",
  "jsonlite",
  "yaml",
  "writexl",
  "googlesheets4",
  "googledrive",
  "testthat",
  "shinytest2"
)

installed <- rownames(installed.packages())
missing <- setdiff(packages, installed)

if (length(missing)) {
  install.packages(missing)
}

message("Pacotes prontos: ", paste(packages, collapse = ", "))
