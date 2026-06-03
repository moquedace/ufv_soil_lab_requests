sheet_names <- c("solicitacoes", "amostras", "analises_amostra")

sheet_columns <- list(
  solicitacoes = c(
    "solicitacao_id",
    "data_hora_envio",
    "nome_solicitante",
    "email",
    "telefone",
    "cidade_solicitante",
    "status_interno",
    "data_entrada_lab",
    "numero_laboratorio",
    "custo_total_lab",
    "forma_pagamento_lab",
    "pedido_numero_lab",
    "observacoes_internas"
  ),
  amostras = c(
    "amostra_id",
    "solicitacao_id",
    "ordem_amostra",
    "referencia_amostra",
    "municipio_amostra",
    "uf_amostra",
    "localidade_descricao",
    "latitude_wgs84",
    "longitude_wgs84",
    "tipo_localizacao",
    "tipo_material",
    "grupos_analise",
    "carbonato_presente",
    "pre_tratamento_necessario"
  ),
  analises_amostra = c(
    "amostra_id",
    "laboratorio",
    "analise_id",
    "analise_nome"
  )
)

empty_sheet_data <- function(sheet) {
  as.data.frame(
    setNames(
      replicate(length(sheet_columns[[sheet]]), character(), simplify = FALSE),
      sheet_columns[[sheet]]
    ),
    stringsAsFactors = FALSE
  )
}

use_google_sheets <- function() {
  identical(tolower(Sys.getenv("USE_GOOGLE_SHEETS", "false")), "true") &&
    nzchar(Sys.getenv("GOOGLE_SHEET_ID", ""))
}

google_sheet_id <- function() {
  sheet_id <- Sys.getenv("GOOGLE_SHEET_ID", "")
  if (!nzchar(sheet_id)) {
    stop("Configure GOOGLE_SHEET_ID no arquivo .Renviron.", call. = FALSE)
  }

  sheet_id
}

ensure_google_auth <- function() {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Instale o pacote 'googlesheets4'.", call. = FALSE)
  }

  if (!googlesheets4::gs4_has_token()) {
    googlesheets4::gs4_auth()
  }
}

setup_google_sheets <- function(sheet_id = google_sheet_id()) {
  ensure_google_auth()

  existing <- googlesheets4::sheet_names(sheet_id)
  missing <- setdiff(sheet_names, existing)

  for (sheet in missing) {
    googlesheets4::sheet_add(sheet_id, sheet = sheet)
  }

  for (sheet in sheet_names) {
    raw <- read_google_sheet_raw(sheet_id, sheet)
    if (!nrow(raw) && !ncol(raw)) {
      googlesheets4::range_write(
        ss = sheet_id,
        data = empty_sheet_data(sheet),
        sheet = sheet,
        col_names = TRUE
      )
    } else if (length(setdiff(sheet_columns[[sheet]], names(raw)))) {
      write_google_sheet(sheet_id, sheet, read_google_sheet(sheet_id, sheet))
    }
  }

  invisible(TRUE)
}

read_google_store <- function(sheet_id = google_sheet_id()) {
  ensure_google_auth()

  list(
    solicitacoes = read_google_sheet(sheet_id, "solicitacoes"),
    amostras = read_google_sheet(sheet_id, "amostras"),
    analises = read_google_sheet(sheet_id, "analises_amostra")
  )
}

append_google_store <- function(new_store, sheet_id = google_sheet_id()) {
  ensure_google_auth()

  append_google_rows(sheet_id, "solicitacoes", new_store$solicitacoes)
  append_google_rows(sheet_id, "amostras", new_store$amostras)
  append_google_rows(sheet_id, "analises_amostra", new_store$analises)

  invisible(TRUE)
}

read_google_sheet <- function(sheet_id, sheet) {
  data <- read_google_sheet_raw(sheet_id, sheet)

  if (is.null(data)) {
    return(empty_sheet_data(sheet))
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  columns <- sheet_columns[[sheet]]
  missing <- setdiff(columns, names(data))
  for (column in missing) {
    data[[column]] <- NA
  }

  data[, columns, drop = FALSE]
}

read_google_sheet_raw <- function(sheet_id, sheet) {
  ensure_google_auth()

  data <- tryCatch(
    googlesheets4::read_sheet(sheet_id, sheet = sheet, .name_repair = "minimal"),
    error = function(error) NULL
  )

  if (is.null(data)) {
    return(NULL)
  }

  as.data.frame(data, stringsAsFactors = FALSE)
}

write_google_sheet <- function(sheet_id, sheet, data) {
  ensure_google_auth()

  columns <- sheet_columns[[sheet]]
  missing <- setdiff(columns, names(data))
  for (column in missing) {
    data[[column]] <- NA
  }

  data <- data[, columns, drop = FALSE]
  googlesheets4::range_write(
    ss = sheet_id,
    data = data,
    sheet = sheet,
    col_names = TRUE
  )

  invisible(TRUE)
}

append_google_rows <- function(sheet_id, sheet, data) {
  if (is.null(data) || !nrow(data)) {
    return(invisible(FALSE))
  }

  columns <- sheet_columns[[sheet]]
  missing <- setdiff(columns, names(data))
  for (column in missing) {
    data[[column]] <- NA
  }

  data <- data[, columns, drop = FALSE]
  googlesheets4::sheet_append(sheet_id, data = data, sheet = sheet)
  invisible(TRUE)
}
