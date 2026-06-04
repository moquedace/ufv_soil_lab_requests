# UFV Soil Lab Requests

<p align="center">
  <img src="www/img/logo_ufv.png" alt="Universidade Federal de Viçosa" width="310">
  &nbsp;&nbsp;&nbsp;&nbsp;
  <img src="www/img/logo_dps_ufv.png" alt="Departamento de Solos UFV" width="230">
</p>

Aplicação web em **R/Shiny** para solicitação de análises de amostras do
**Departamento de Solos (DPS) da Universidade Federal de Viçosa**. Substitui o
fluxo em papel das fichas de recepção por um formulário digital, com histórico em
Google Sheets e exportação para CSV/XLSX.

---

## Funcionalidades

### Área do solicitante (sem login)

- **Dados do solicitante**: nome, e-mail, telefone, CPF/CNPJ, endereço completo
  (com bairro e CEP), cidade/UF, vínculo, matrícula (para vínculos acadêmicos),
  instituição, orientador e observações.
- **Múltiplas amostras por solicitação**, cada uma com referência, material e
  localização própria.
- **Cinco grupos de análise** com campos condicionais conforme a ficha original:
  - **Solo rotina** — química e física (pH, P, K, granulometria, densidade, etc.)
  - **Vegetal** — com tipo de amostra (folha, galho, casca, raiz...) e cultura/planta
  - **CHN** — carbono total/orgânico, carbonato, %C/%N estimados, nº de projeto
  - **Absorção atômica** — elementos, digestão, volumes, departamento, projeto
  - **ICP-OES** — mesmos campos da absorção atômica + instruções do equipamento
- **Mapa interativo** (Leaflet) com camadas de **satélite** e **rótulos**, busca
  por município/localidade ou coordenada decimal. Coordenadas salvas em WGS84.
- **Consentimento LGPD** (Lei nº 13.709/2018) obrigatório antes do envio e aviso
  de privacidade sobre o uso da localização.
- **Validação** de e-mail e telefone, **IDs únicos** por solicitação e **proteção
  contra envio duplicado**.

### Área da recepção (protegida por senha)

- Lista de solicitações com **filtros** por texto, status e grupo de análise.
- **Detalhe completo** da solicitação selecionada: todos os dados do solicitante e
  a ficha de cada amostra com seus campos específicos e análises.
- **Campos internos do laboratório**: data de entrada, nº de laboratório, custo,
  forma de pagamento, nº do pedido, status e observações internas.
- **Badges de status** coloridos (Recebida, Em análise, Finalizada, etc.).
- **Exportação CSV e XLSX** respeitando os filtros ativos.

---

## Identidade visual

Tema institucional inspirado em **perfis de solo**: paleta terrosa (húmus,
terracota, ocre, areia), tipografia **Space Grotesk** (títulos) + **Inter**
(corpo), faixa de horizontes de solo como assinatura, hero e rodapé
institucionais. Protótipos de design ficam em `prototipo/`.

---

## Estrutura do projeto

```
app.R                       # ponto de entrada
R/
  app_ui.R / app_server.R   # UI (tema, navbar, hero, rodapé) e servidor
  mod_solicitante.R         # formulário do solicitante + envio
  mod_amostras.R            # amostras, mapa e campos condicionais
  mod_recepcao.R            # área interna: lista, detalhe, campos internos
  config.R                  # carregamento de configuração e ambiente
  sample_data.R             # dados de exemplo + geração de IDs + safe_rbind
  storage_google_sheets.R   # leitura/escrita no Google Sheets
  exportacao.R              # achatamento de dados e exportação CSV/XLSX
config/analises.yml         # definição dos grupos e tipos de análise
tests/testthat/             # suíte de testes (262 testes)
scripts/                    # instalação, execução, testes e manutenção
docs/especificacao_mvp.md   # especificação inicial do MVP
prototipo/                  # protótipos de design (HTML)
www/img/                    # logos
```

---

## Como rodar localmente

No RStudio, abra esta pasta como projeto/diretório de trabalho e rode:

```r
source("scripts/00_install_packages.R")   # instala dependências
source("scripts/01_run_app.R")            # inicia o aplicativo
```

Sem configuração de Google Sheets, o app roda com **dados de exemplo em memória** —
útil para explorar a interface.

### Acesso à recepção

A aba **Recepção** é protegida por senha. A senha vem da variável de ambiente
`LAB_RECEPTION_PASSWORD`; se não definida, o padrão é `dps2024`. Para definir uma
senha própria, adicione ao seu `.Renviron`:

```
LAB_RECEPTION_PASSWORD=suasenha
```

> **Atenção:** defina uma senha própria antes de publicar o sistema. Não use o
> valor padrão em produção.

---

## Google Sheets (armazenamento do piloto)

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

### Manutenção da planilha

```r
source("scripts/04_test_google_sheets_write.R")   # grava e lê um registro de teste
source("scripts/05_clean_google_sheets_tests.R")  # prévia da limpeza de testes
clean_google_test_records(confirm = TRUE)          # remove registros de teste

source("scripts/06_clear_google_sheets_all.R")     # prévia da limpeza total
clear_google_store(confirm = TRUE)                 # apaga tudo, mantém cabeçalhos
```

---

## Testes

```r
source("scripts/02_run_tests.R")
```

A suíte (**262 testes**) cobre parsing de coordenadas, configuração de análises,
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
