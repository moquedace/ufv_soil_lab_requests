flatten_store <- function(store) {
  solicitacoes <- store$solicitacoes
  amostras <- store$amostras
  analises <- store$analises

  if (!nrow(solicitacoes) || !nrow(amostras)) {
    return(data.frame())
  }

  merged <- merge(amostras, solicitacoes, by = "solicitacao_id", all.x = TRUE)

  if (nrow(analises)) {
    analises_txt <- stats::aggregate(
      analise_nome ~ amostra_id,
      data = analises,
      FUN = function(values) paste(unique(values), collapse = " | ")
    )
    names(analises_txt)[2] <- "analises_solicitadas"
    merged <- merge(merged, analises_txt, by = "amostra_id", all.x = TRUE)
  }

  merged
}

write_export_csv <- function(data, path) {
  utils::write.csv2(data, file = path, row.names = FALSE, fileEncoding = "UTF-8")
}

write_export_xlsx <- function(data, path) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Instale o pacote 'writexl' para exportar XLSX.", call. = FALSE)
  }

  writexl::write_xlsx(data, path = path)
}
