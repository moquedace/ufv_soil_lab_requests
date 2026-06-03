# UFV Soil Lab Requests

<p align="center">
  <img src="www/img/logo_ufv.png" alt="Universidade Federal de Vicosa" width="310">
  &nbsp;&nbsp;&nbsp;&nbsp;
  <img src="www/img/logo_dps_ufv.png" alt="Departamento de Solos UFV" width="230">
</p>

Aplicacao web em R/Shiny para solicitacao de analises de amostras do Departamento de Solos da UFV.

## Objetivo

Substituir o fluxo inicial em papel por um formulario web simples, com historico em Google Sheets e exportacao para XLSX/CSV para apoiar a recepcao de amostras.

## Decisoes iniciais

- Solicitante sem login.
- Recepcao usada por funcionarios do laboratorio.
- Armazenamento piloto em Google Sheets.
- Uma solicitacao pode conter varias amostras.
- Cada amostra pode ter localizacao via mapa e varias analises associadas.

## Documentacao

- `docs/especificacao_mvp.md`: especificacao inicial do MVP.

## Como rodar localmente

No RStudio, abra esta pasta como projeto/diretorio de trabalho e rode:

```r
source("scripts/00_install_packages.R")
source("scripts/01_run_app.R")
```

O prototipo inicial ainda usa dados simulados em memoria. A integracao com
Google Sheets entra depois que validarmos o fluxo das telas.

No cadastro de amostras, use o campo "Buscar municipio, localidade, referencia
ou digitar a coordenada" para centralizar o mapa. Ele aceita texto ou coordenada
decimal, por exemplo `-22.72, -47.65`. Depois clique no ponto da amostra.

Uma mesma amostra pode ser associada a varios grupos de analise. Marque os
grupos desejados e, em seguida, selecione as analises dentro de cada grupo.

Na aba `Recepcao`, os filtros de busca/status/grupo de analise tambem definem o
conteudo exportado nos arquivos CSV e XLSX.
Essa aba tambem permite registrar campos internos do laboratorio, como data de
entrada, numero de laboratorio, custo, pagamento, pedido e observacoes internas.

## Como rodar os testes

```r
source("scripts/02_run_tests.R")
```

Os testes cobrem parsing de coordenadas, exportacao tabular e um fluxo basico
do Shiny com preenchimento de solicitante, amostra e envio.
Os testes automaticos rodam em modo local e nao escrevem na planilha Google.

## Google Sheets

Para gravar os dados do piloto na planilha Google:

1. Copie `.Renviron.example` para `.Renviron`.
2. Confira o ID da planilha em `GOOGLE_SHEET_ID`.
3. Altere `USE_GOOGLE_SHEETS=true`.
4. Reinicie a sessao do R.
5. Rode:

```r
source("R/config.R")
load_project_env()
Sys.getenv("GOOGLE_SHEET_ID")
source("scripts/03_setup_google_sheets.R")
source("scripts/01_run_app.R")
```

Na primeira execucao, o pacote `googlesheets4` pode abrir o fluxo de
autenticacao da sua conta Google.
Rode `scripts/03_setup_google_sheets.R` novamente quando novas colunas forem
incluidas no sistema.

Para testar escrita real na planilha, rode:

```r
source("scripts/04_test_google_sheets_write.R")
```

Esse teste grava uma solicitacao fake marcada como `TESTE AUTOMATICO - NAO USAR`
e valida a leitura de volta. Ele nao faz parte da suite comum para evitar
escritas acidentais a cada execucao.

Para limpar registros de teste da planilha, rode primeiro a previa:

```r
source("scripts/05_clean_google_sheets_tests.R")
```

Se a lista estiver correta, confirme a limpeza:

```r
clean_google_test_records(confirm = TRUE)
```

Para apagar todos os dados do piloto e manter apenas os cabecalhos das abas:

```r
source("scripts/06_clear_google_sheets_all.R")
clear_google_store(confirm = TRUE)
```

Esse comando limpa as linhas antigas das tres abas antes de reescrever os
cabecalhos.
