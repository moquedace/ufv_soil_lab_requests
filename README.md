# UFV Soil Lab Requests

<p align="center">
  <img src="https://img.shields.io/badge/R-Shiny-276DC3?style=flat-square&logo=r&logoColor=white"/>
  <img src="https://img.shields.io/badge/storage-Google%20Sheets-34A853?style=flat-square&logo=googlesheets&logoColor=white"/>
  <img src="https://img.shields.io/badge/domain-soil%20laboratory-7A5A3F?style=flat-square"/>
  <img src="https://img.shields.io/badge/status-piloto-orange?style=flat-square"/>
  <img src="https://img.shields.io/badge/UFV-DPS-1F5F4A?style=flat-square"/>
</p>

<p align="center">
  <img src="www/img/logo_ufv.png" alt="Universidade Federal de Viçosa" width="310">
  &nbsp;&nbsp;&nbsp;&nbsp;
  <img src="www/img/logo_dps_ufv.png" alt="Departamento de Solos UFV" width="230">
</p>

<p align="center">
  Sistema web em <strong>R/Shiny</strong> para digitalizar a solicitação e recepção
  de análises de solo, vegetal, CHN, absorção atômica e ICP-OES no
  <strong>Departamento de Solos da UFV</strong>.
</p>

---

## Visão geral

O projeto substitui o fluxo baseado em fichas impressas por um formulário digital
com histórico em Google Sheets, exportação CSV/XLSX e área interna de recepção.

```
Solicitante                    Recepção DPS/UFV                  Dados
     │                                │                            │
     ├── dados pessoais + LGPD        │                            │
     ├── uma ou várias amostras       │                            │
     ├── mapa, município, profundidade│                            │
     ├── análises por laboratório     │                            │
     │                                │                            │
     └────────── envio ──────────────►│  conferência operacional   │
                                      ├── campos internos          │
                                      ├── filtros e detalhes       │
                                      └────────── exportação ─────► CSV/XLSX
                                                                  Google Sheets
```

---

## Funcionalidades

### Área do solicitante (sem login)

| Recurso | Descrição |
|---------|-----------|
| Dados do solicitante | Nome, contato, documento, endereço, vínculo, matrícula acadêmica quando aplicável e observações |
| Amostras múltiplas | Uma solicitação pode conter várias amostras, cada uma com material, profundidade/camada e localização |
| Geração em lote | Criação por intervalo numérico, lista colada ou pontos × camadas |
| Edição em bloco | Duplicar, remover e aplicar análises a várias amostras selecionadas |
| Localização | Busca por município/localidade/coordenada e mapa Leaflet com satélite e rótulos |
| LGPD | Consentimento obrigatório e aviso de privacidade antes do envio |
| Controle de envio | Validação de contato, IDs únicos e proteção contra duplo envio |

### Grupos de análise

| Grupo | Campos/observações |
|-------|--------------------|
| Solo rotina | Química e física: pH, P, K, granulometria, densidade, porosidade e outros |
| Vegetal | Tipo de amostra, cultura/planta e análises nutricionais |
| CHN | Carbono total/orgânico, nitrogênio, carbonato, %C/%N estimados e projeto |
| Absorção atômica | Elementos, digestão, volumes, departamento e projeto |
| ICP-OES | Elementos, preparo/digestão e instruções operacionais do equipamento |

### Área da recepção (protegida por senha)

| Recurso | Descrição |
|---------|-----------|
| Filtros | Busca por texto, status e grupo de análise |
| Detalhe completo | Dados do solicitante, amostras, profundidade/camada, localização e análises |
| Campos internos | Data de entrada, número de laboratório, custo, pagamento, pedido, status e observações |
| Status | Badges visuais para Recebida, Em análise, Finalizada, Cancelada etc. |
| Exportação | CSV e XLSX respeitando os filtros ativos |

---

## Identidade visual

Tema institucional inspirado em **perfis de solo**: paleta terrosa (húmus,
terracota, ocre, areia), tipografia **Space Grotesk** (títulos) + **Inter**
(corpo), faixa de horizontes de solo como assinatura, hero e rodapé
institucionais. Protótipos de design ficam em `prototipo/`.

---

## Estrutura

| Caminho | Papel |
|---------|-------|
| [`app.R`](app.R) | Ponto de entrada da aplicação |
| [`R/app_ui.R`](R/app_ui.R) · [`R/app_server.R`](R/app_server.R) | Tema, navegação, servidor e composição dos módulos |
| [`R/mod_solicitante.R`](R/mod_solicitante.R) | Formulário externo, revisão, LGPD e envio |
| [`R/mod_amostras.R`](R/mod_amostras.R) | Amostras, mapa, geração em lote, profundidade e análises |
| [`R/mod_recepcao.R`](R/mod_recepcao.R) | Área interna, senha, filtros, detalhes e campos do laboratório |
| [`R/storage_google_sheets.R`](R/storage_google_sheets.R) | Schema, leitura/escrita e limpeza do Google Sheets |
| [`R/exportacao.R`](R/exportacao.R) | Achatamento dos dados e exportação CSV/XLSX |
| [`config/analises.yml`](config/analises.yml) | Grupos e opções de análises |
| [`tests/testthat/`](tests/testthat) | Suíte automatizada |
| [`scripts/`](scripts) | Instalação, testes, Google Sheets, limpeza e deploy |

---

## Quickstart

No RStudio, abra esta pasta como projeto/diretório de trabalho e rode:

```r
source("scripts/00_install_packages.R")   # instala dependências
source("scripts/01_run_app.R")            # inicia o aplicativo
```

Sem configuração de Google Sheets, o app roda com **dados de exemplo em memória** —
útil para explorar a interface.

### Recepção

A aba **Recepção** é protegida por senha. A senha vem da variável de ambiente
`LAB_RECEPTION_PASSWORD`; se não definida, o padrão é `dps2024`. Para definir uma
senha própria, adicione ao seu `.Renviron`:

```
LAB_RECEPTION_PASSWORD=suasenha
```

> **Atenção:** defina uma senha própria antes de publicar o sistema. Não use o
> valor padrão em produção.

---

## Google Sheets

Os dados são gravados em três abas: `solicitacoes`, `amostras` e
`analises_amostra`.

1. Copie `.Renviron.example` para `.Renviron`.
2. Confira o `GOOGLE_SHEET_ID` e ajuste `USE_GOOGLE_SHEETS=true`.
3. Reinicie a sessão do R e rode:

```r
source("R/config.R")
load_project_env()
source("scripts/03_setup_google_sheets.R")   # cria/atualiza cabeçalhos
source("scripts/01_run_app.R")
```

Na primeira execução, o pacote `googlesheets4` abre o fluxo de autenticação da
conta Google. Rode `scripts/03_setup_google_sheets.R` novamente sempre que novas
colunas forem adicionadas ao sistema.

### Manutenção

```r
source("scripts/04_test_google_sheets_write.R")   # grava e lê um registro de teste
source("scripts/05_clean_google_sheets_tests.R")  # prévia da limpeza de testes
clean_google_test_records(confirm = TRUE)          # remove registros de teste

source("scripts/06_clear_google_sheets_all.R")     # prévia da limpeza total
clear_google_store(confirm = TRUE)                 # apaga tudo, mantém cabeçalhos
```

---

## Deploy

Para publicar o app no shinyapps.io, primeiro configure sua conta no RStudio/R:

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(
  name = "SUA_CONTA",
  token = "SEU_TOKEN",
  secret = "SEU_SECRET"
)
```

O comando completo fica disponível em **shinyapps.io > Account > Tokens**.

### Google Sheets no servidor

No shinyapps.io não há navegador interativo para autenticar o Google Sheets. Para
gravar na planilha durante o teste hospedado, use uma **service account** do
Google Cloud:

1. Crie/baixe o arquivo JSON da service account.
2. Salve localmente em `credentials/google-service-account.json`.
3. Compartilhe a planilha Google com o e-mail `client_email` que aparece dentro
   desse JSON, com permissão de edição.
4. No `.Renviron`, confira:

```text
USE_GOOGLE_SHEETS=true
GOOGLE_SHEET_ID=1BazpE4_5vJK2siiWxKBBFzptSj_GE9m_XWu2fLRapbA
GOOGLE_SERVICE_ACCOUNT_JSON=credentials/google-service-account.json
LAB_RECEPTION_PASSWORD=uma_senha_nao_padrao
SHINYAPPS_APP_NAME=ufv-soil-lab-requests
```

O arquivo `credentials/google-service-account.json` e o `.Renviron` são ignorados
pelo Git. O script de deploy envia esses arquivos somente no bundle do
shinyapps.io.

### Publicar

Depois de rodar os testes localmente:

```r
source("scripts/02_run_tests.R")
source("scripts/07_deploy_shinyapps.R")
```

Se algo falhar no ambiente hospedado, consulte os logs no painel do shinyapps.io
ou pelo R:

```r
rsconnect::showLogs(appName = "ufv-soil-lab-requests")
```

---

## Testes

```r
source("scripts/02_run_tests.R")
```

A suíte cobre parsing de coordenadas, configuração de análises,
construção e validação de amostras, exportação tabular, lógica da recepção,
schema do Google Sheets, geração de IDs e o fluxo Shiny ponta a ponta
(preenchimento, consentimento, envio, edição e autenticação da recepção). Os
testes rodam em modo local e **não escrevem na planilha Google**.

---

## Decisões e documentação

- Solicitante sem login; recepção para uso interno do laboratório.
- Uma solicitação agrupa várias amostras; cada amostra pode ter localização e
  vários grupos de análise.
- Especificação inicial: `docs/especificacao_mvp.md`.
