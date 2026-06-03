packages <- c(
  "shiny",
  "bslib",
  "leaflet",
  "leaflet.extras",
  "DT",
  "yaml",
  "writexl",
  "googlesheets4",
  "googledrive"
)

installed <- rownames(installed.packages())
missing <- setdiff(packages, installed)

if (length(missing)) {
  install.packages(missing)
}

message("Pacotes prontos: ", paste(packages, collapse = ", "))
