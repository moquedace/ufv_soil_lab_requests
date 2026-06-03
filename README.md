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
