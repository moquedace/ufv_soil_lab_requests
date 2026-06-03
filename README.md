# UFV Soil Lab Requests

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

No cadastro de amostras, o mapa possui uma busca no canto superior esquerdo.
Digite um municipio ou localidade, aguarde o mapa centralizar e depois clique no
ponto da amostra.
