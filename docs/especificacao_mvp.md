# Especificacao inicial do sistema de solicitacao de analises

Projeto: UFV Soil Lab Requests  
Laboratorio: Departamento de Solos - UFV  
Versao: rascunho MVP  
Data: 2026-06-03

## 1. Objetivo

Criar uma interface web simples para substituir o fluxo em papel das fichas de recepcao de amostras, permitindo que solicitantes preencham os dados antes da entrega ou no balcao. O sistema deve gerar historico em Google Sheets e permitir exportacao em XLSX e CSV, reduzindo retrabalho na recepcao e preparando os dados para copia ao sistema interno.

## 2. Decisoes iniciais

- Aplicacao em R/Shiny.
- Sem login para o solicitante.
- Area interna simples para funcionarios da recepcao.
- Armazenamento do piloto em Google Sheets.
- Exportacao em XLSX e CSV.
- CSV separado por ponto e virgula, com virgula decimal.
- Custo, pagamento confirmado, numero de laboratorio e data de entrada sao campos internos do laboratorio.
- Uma solicitacao pode conter varias amostras.
- Toda amostra pode ter localizacao, independentemente do tipo de analise.
- A coordenada deve ser salva em graus decimais WGS84.

## 3. Publicos do sistema

### Solicitante

Agricultor, pesquisador, estudante, professor, tecnico ou outro usuario que solicita analise. Pode preencher em casa ou com auxilio de funcionario na recepcao.

### Recepcao do laboratorio

Funcionarios que visualizam as solicitacoes enviadas, consultam historico e exportam planilhas.

## 4. Fluxo geral do solicitante

1. Escolhe o tipo de solicitacao ou as analises desejadas.
2. Preenche dados do solicitante.
3. Informa uma ou mais amostras.
4. Para cada amostra, informa referencia, tipo/material, analises e origem.
5. Opcionalmente marca a origem no mapa.
6. Revisa um resumo simples.
7. Envia a solicitacao.
8. Recebe uma confirmacao na tela.

Nao havera protocolo de acompanhamento no MVP.

## 5. Fluxo geral da recepcao

1. Abre a area interna.
2. Visualiza solicitacoes recebidas.
3. Filtra por data, tipo de analise, solicitante, status ou laboratorio.
4. Abre detalhes da solicitacao.
5. Ajusta campos internos quando necessario: data de entrada, custo, pagamento, numero de laboratorio, observacoes internas.
6. Exporta XLSX ou CSV.

Como nao ha etapa obrigatoria de conferencia antes da exportacao, a exportacao deve estar sempre disponivel.

## 6. Tipos de formulario contemplados

### Solo rotina

Campos observados na ficha:

- Cliente
- Endereco
- Bairro
- Cidade
- CEP
- Telefone
- E-mail
- CPF/CNPJ
- Identificacao
- Municipio
- Analises do Laboratorio de Rotina:
  - Rotina: pH, P, K, Ca, Mg, Al, H + Al e P-rem
  - Materia organica
  - Fe-Zn-Mn-Cu
  - Enxofre
  - Boro
  - Sodio
  - pH em KCl
  - Ni-Cd-Cr-Pb
  - Nitrogenio total
- Analises do Laboratorio de Fisica:
  - Analise granulometrica/textura
  - Argila dispersa em agua
  - Equivalente de umidade
  - Densidade do solo
  - Densidade de particulas
  - Porosidade
  - Retencao de agua - potenciais
  - Outras analises
- Referencias das amostras
- Campos internos: data de entrada, quantidade de amostras, registro do cliente, numeros de laboratorio, pagamento, pedido, orientador/chefia quando aplicavel.

### Vegetal

Campos observados na ficha:

- Cliente
- Endereco
- Bairro
- Cidade
- CEP
- Telefone
- E-mail
- CPF/CNPJ
- Identificacao
- Municipio
- Tipo de amostra:
  - Folha
  - Galho
  - Casca
  - Raiz
  - Serrapilheira
  - Outros
- Cultura/planta
- Analises:
  - Nitrogenio
  - P+K+Ca+Mg
  - Enxofre
  - Zn-Mn-Fe-Cu
  - Boro
  - Sodio
  - Cr-Ni-Cd-Pb
  - Carbono - metodo CHN
- Referencias das amostras
- Campos internos: data de entrada, quantidade de amostras, registro do cliente, numeros de laboratorio, pagamento, pedido, orientador/chefia quando aplicavel.

### CHN

Campos observados na ficha:

- Solicitante/matricula
- Vínculo: iniciacao cientifica, mestrado, doutorado, outro
- Professor/orientador
- Telefone de contato
- Endereco
- Instituicao/departamento/laboratorio
- Numero de amostras
- Material
- Porcentagem estimada: %C e %N
- Elementos a determinar: C e/ou N
- Presenca de carbono proveniente de carbonato
- Numero do registro do projeto de pesquisa
- Numero de solicitacao
- Numero de laboratorio
- Pagamento
- Data
- Assinatura do responsavel
- Observacoes

Regra nova para CHN:

- Se C for solicitado, o usuario deve informar o tipo de carbono:
  - Carbono total
  - Carbono organico total
- Se a opcao for Carbono organico total, o sistema deve marcar pre_tratamento_necessario = sim e mostrar aviso de que ha pre-tratamento antes da determinacao.
- Para carbonatos, incluir opcoes: Sim, Nao, Nao sei.

### Absorcao atomica

Campos observados na ficha:

- Equipamento: Absorcao Atomica
- Solicitante
- E-mail
- Telefone
- Endereco
- Departamento de origem: DPS ou outro
- Vinculo: iniciacao cientifica, mestrado, doutorado, outro
- Professor orientador
- Quantidade de amostras
- Elementos
- Descricao do material: solo, vegetal, outro
- Tipo de digestao realizada
- Volume apos digestao
- Diluicao, aliquota e volume final
- Projeto de pesquisa registrado: sim/nao e numero do registro
- Campos internos: custo, forma de pagamento, numero de solicitacao cliente, numero de laboratorio, assinaturas.

### ICP-OES

Campos observados na ficha:

- Equipamento: ICP-OES OPTIMA 8300
- Solicitante
- E-mail
- Telefone
- Endereco
- Departamento de origem: DPS ou outro
- Vinculo: iniciacao cientifica, mestrado, doutorado, outro
- Professor orientador
- Quantidade de amostras
- Elementos
- Descricao do material: solo, vegetal, outro
- Tipo de digestao realizada
- Volume apos digestao
- Diluicao, aliquota e volume final
- Projeto de pesquisa registrado: sim/nao e numero do registro
- Campos internos: custo, forma de pagamento, numero de solicitacao cliente, numero de laboratorio, assinaturas.
- Instrucoes operacionais da ficha original devem ficar disponiveis em texto informativo no formulario ICP, sem bloquear o preenchimento.

## 7. Modelo de solicitacao e amostras

A estrutura deve separar solicitacao e amostra.

### Solicitacao

Uma solicitacao agrupa dados do solicitante e pode conter uma ou mais amostras.

Campos sugeridos:

- solicitacao_id
- data_hora_envio
- tipo_solicitacao_principal
- nome_solicitante
- matricula
- cpf_cnpj
- email
- telefone
- endereco
- bairro
- cidade_solicitante
- uf_solicitante
- cep
- instituicao_departamento_laboratorio
- departamento_origem
- vinculo
- vinculo_outro
- professor_orientador
- projeto_registrado
- numero_registro_projeto
- observacoes_solicitante
- status_interno
- custo_total_lab
- forma_pagamento_lab
- pedido_numero_lab
- observacoes_internas

### Amostra

Cada amostra pertence a uma solicitacao.

Campos sugeridos:

- amostra_id
- solicitacao_id
- ordem_amostra
- referencia_amostra
- identificacao
- tipo_material
- tipo_amostra
- cultura_planta
- municipio_amostra
- uf_amostra
- localidade_descricao
- latitude_wgs84
- longitude_wgs84
- tipo_localizacao
- localizacao_informada
- mesma_localizacao_de
- analises_solicitadas
- laboratorio_destino
- elementos
- preparo_digestao
- volume_apos_digestao
- diluicao
- aliquota
- volume_final
- percentual_c_estimado
- percentual_n_estimado
- determinar_c
- determinar_n
- tipo_carbono
- carbonato_presente
- pre_tratamento_necessario
- numero_laboratorio

## 8. Localizacao das amostras

A localizacao deve ser oferecida para todas as amostras, mas de forma simples.

Campos de interface:

- Municipio da amostra: obrigatorio.
- UF da amostra: obrigatorio.
- Nome da propriedade, comunidade, talhao ou referencia: opcional.
- Mapa para marcar local: recomendado, mas nao obrigatorio.
- Classificacao do ponto marcado:
  - Local exato da coleta
  - Local aproximado da coleta
  - Apenas municipio/regiao

Comportamento esperado:

- O usuario pode buscar localidade/municipio e depois clicar no mapa.
- O usuario nao precisa digitar coordenada manualmente no fluxo principal.
- Depois do clique, o sistema salva latitude e longitude em graus decimais WGS84.
- Os campos numericos podem aparecer de forma discreta ou somente no resumo.
- Se a pessoa nao marcar o mapa, o sistema salva municipio/UF e localizacao_informada = nao.

Facilitadores para multiplas amostras:

- Botao: usar mesma localizacao da amostra anterior.
- Botao: copiar localizacao de outra amostra da solicitacao.
- Possibilidade de aplicar a mesma localizacao a varias amostras selecionadas.
- Ao duplicar uma amostra, copiar localizacao e analises por padrao, permitindo edicao posterior.

## 9. Analises compartilhadas e multiplos laboratorios

Uma mesma amostra pode ser direcionada para diferentes tipos de analise, por exemplo quimica, fisica e biologica, mantendo a mesma localizacao.

Para facilitar o uso:

- O formulario deve permitir adicionar uma amostra uma vez e marcar varias analises associadas a ela.
- Quando houver analises de laboratorios diferentes, o sistema deve gerar registros internos suficientes para exportacao por laboratorio, mas sem obrigar o solicitante a redigitar a amostra.
- A visualizacao da recepcao deve permitir filtrar por laboratorio ou tipo de analise.

## 10. Google Sheets como armazenamento do piloto

Sugestao de abas:

- solicitacoes
- amostras
- analises_amostra
- eventos_status
- configuracoes

Aba solicitacoes: uma linha por solicitacao.  
Aba amostras: uma linha por amostra.  
Aba analises_amostra: uma linha por analise marcada para cada amostra, quando for melhor normalizar dados.  
Aba eventos_status: historico simples de alteracoes internas.  
Aba configuracoes: listas de analises, laboratorios, municipios preferenciais e parametros.

## 11. Exportacoes

O sistema deve exportar:

- XLSX completo.
- CSV completo.
- CSV por laboratorio ou tipo de analise, quando necessario.

Regras para CSV:

- Separador: ponto e virgula.
- Decimal: virgula.
- Codificacao a testar com o sistema interno, preferencialmente UTF-8 com BOM no piloto.

## 12. Campos internos do laboratorio

Campos que nao devem ser solicitados ao usuario externo no MVP:

- Custo total.
- Confirmacao de pagamento.
- Numero de laboratorio.
- Numero de solicitacao interna, se houver.
- Data de entrada fisica da amostra.
- Pedido numero.
- Assinaturas.
- Observacoes internas.

## 13. Primeiras telas propostas

1. Inicio da solicitacao
   - Escolha de tipo de analise ou laboratorio.
   - Explicacao curta de que e possivel adicionar varias amostras.

2. Dados do solicitante
   - Dados comuns.
   - Campos academicos aparecem quando aplicavel.

3. Amostras
   - Lista de amostras ja adicionadas.
   - Botao adicionar amostra.
   - Botao duplicar amostra.
   - Botao usar mesma localizacao.

4. Detalhe da amostra
   - Referencia/identificacao.
   - Material/tipo.
   - Cultura/planta quando aplicavel.
   - Analises solicitadas.
   - Origem e mapa.

5. Revisao
   - Resumo por solicitacao e por amostra.
   - Alertas de campos importantes ausentes.

6. Confirmacao
   - Mensagem de envio concluido.

7. Area interna da recepcao
   - Tabela de solicitacoes.
   - Detalhes.
   - Campos internos.
   - Exportacao.

## 14. Pendencias para decisao

- Definir se havera senha simples para a area interna da recepcao ou se ficara apenas por URL pouco divulgada no piloto.
- Definir listas finais de analises biologicas, caso entrem no primeiro ciclo.
- Definir se pagamento deve aparecer como informativo ao solicitante ou somente interno.
- Definir o formato exato da planilha de exportacao depois de conhecer melhor o sistema interno.
- Escolher a camada de mapa/satelite que respeite custo e termos de uso.
- Decidir texto curto de consentimento/aviso sobre uso de localizacao.

## 15. Proxima etapa sugerida

Criar o esqueleto do projeto Shiny com:

- app.R ou estrutura modular em R/app_ui.R, R/app_server.R e modulos.
- configuracao de listas de analises.
- simulacao local de armazenamento antes da conexao Google Sheets.
- prototipo da tela de amostras com mapa Leaflet.
- exportacao inicial CSV/XLSX a partir de dados simulados.
