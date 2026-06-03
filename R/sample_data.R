new_empty_store <- function() {
  list(
    solicitacoes = data.frame(),
    amostras = data.frame(),
    analises = data.frame()
  )
}

sample_store <- function() {
  list(
    solicitacoes = data.frame(
      solicitacao_id = "SOL-20260603-0001",
      data_hora_envio = "2026-06-03 10:45:00",
      nome_solicitante = "Exemplo de solicitante",
      email = "exemplo@email.com",
      telefone = "(31) 00000-0000",
      cidade_solicitante = "Vicosa",
      status_interno = "Recebida",
      data_entrada_lab = "",
      numero_laboratorio = "",
      custo_total_lab = "",
      forma_pagamento_lab = "",
      pedido_numero_lab = "",
      observacoes_internas = "",
      stringsAsFactors = FALSE
    ),
    amostras = data.frame(
      amostra_id = "AMS-0001",
      solicitacao_id = "SOL-20260603-0001",
      ordem_amostra = 1,
      referencia_amostra = "Talhao 1 - 0-20 cm",
      municipio_amostra = "Vicosa",
      uf_amostra = "MG",
      localidade_descricao = "Fazenda exemplo",
      latitude_wgs84 = -20.7546,
      longitude_wgs84 = -42.8825,
      tipo_localizacao = "aproximada",
      tipo_material = "Solo",
      grupos_analise = "solo_rotina",
      carbonato_presente = NA_character_,
      pre_tratamento_necessario = FALSE,
      stringsAsFactors = FALSE
    ),
    analises = data.frame(
      amostra_id = "AMS-0001",
      laboratorio = "solo_rotina",
      analise_id = "rotina_basica",
      analise_nome = "Rotina: pH, P, K, Ca, Mg, Al, H + Al e P-rem",
      stringsAsFactors = FALSE
    )
  )
}

safe_rbind <- function(a, b) {
  if (!nrow(a)) return(b)
  if (!nrow(b)) return(a)
  all_cols <- union(names(a), names(b))
  for (col in setdiff(all_cols, names(a))) a[[col]] <- NA
  for (col in setdiff(all_cols, names(b))) b[[col]] <- NA
  rbind(a[, all_cols, drop = FALSE], b[, all_cols, drop = FALSE])
}

next_request_id <- function() {
  paste0("SOL-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

next_sample_id <- function(index) {
  paste0("AMS-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", sprintf("%03d", index))
}
