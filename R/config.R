load_app_config <- function(path = "config/analises.yml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Instale o pacote 'yaml' antes de rodar o app.", call. = FALSE)
  }

  yaml::read_yaml(path)
}

load_project_env <- function(path = ".Renviron") {
  if (file.exists(path)) {
    readRenviron(path)
  }

  invisible(TRUE)
}

choice_labels <- function(items) {
  stats::setNames(
    vapply(items, function(item) item$id, character(1)),
    vapply(items, function(item) item$nome, character(1))
  )
}

analysis_choices <- function(app_config, group_id) {
  group <- app_config$analises[[group_id]]
  if (is.null(group)) {
    return(character())
  }

  choice_labels(group$opcoes)
}

`%||%` <- function(value, fallback) {
  if (is.null(value)) fallback else value
}
