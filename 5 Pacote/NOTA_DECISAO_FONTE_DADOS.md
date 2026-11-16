# Nota de decisão — fonte de dados do pacote `mapemunicipios`

Data: 16/11/2026. Escrita depois de rodar, pela primeira vez, "3 Junção e
Transformação" e "4 Base Final" (nunca tinham sido executados — o Relatório
de Decisões dessas duas etapas foi escrito num ambiente sem R). Registra uma
decisão **pendente**, não implementada nesta rodada — `import.R` não foi
alterado.

## 1. O que existe hoje, dos dois lados

**`4 Base Final código.R` produz** (em `4 Base Final/`): um único arquivo
consolidado com as 17 dimensões já juntadas — `mape_municipios.{RData,
parquet,csv,xlsx,dta}` (200.556 linhas × 2.634 colunas) — mais
`dicionario_dimensoes.{csv,parquet}`, um crosswalk **só** com as colunas
`variavel`, `dimensao` (slug), `dimensao_nome` (sem descrição, natureza,
fonte etc.).

**`5 Pacote/mapemunicipios/R/import.R` espera**: um arquivo **por
dimensão** na fonte de dados (`mape_source()`), nomeado pelo slug —
`identificacao.parquet`, `saude.parquet` etc. (ou `.rds`) — que
`.read_dimension()` busca e `.join_tables()` junta sob demanda. Também
espera um `dicionario.xlsx` **único e plano**, com uma linha por variável de
**todas** as dimensões e uma coluna `dimensao_slug` (`import_mape_dictionary()`,
`import.R` linha 218).

Nenhum dos dois arquivos que `import.R` espera existe hoje: nem os 17
arquivos por dimensão, nem o `dicionario.xlsx` plano. O que existe é o
`mape_municipios.parquet` único e o `METADADOS.xlsx` com **17 abas
separadas** (uma por dimensão, sem coluna de slug).

Importante: os slugs em si **já batem** dos dois lados — a tabela
`.mape_dimensions()` do pacote (`utils.R`) usa exatamente os mesmos 17 slugs
de `dimensoes` em "3 Junção e Transformação código.R" (`assistencia_dh`,
`rh`, `transportes_mobilidade` etc.). O desalinhamento é só de **formato de
arquivo**, não de nomenclatura.

## 2. A decisão a tomar

**Opção A — manter um arquivo por dimensão.** Reexportar o painel final em
17 arquivos parquet separados (um `write_parquet()` por slug, selecionando
as colunas daquele `dimensao_slug` + id_municipio/ano) toda vez que "4 Base
Final" rodar. `import.R` não precisa mudar.
- Prós: zero mudança no pacote já escrito.
- Contras: mais um passo de exportação para manter sincronizado; dado
  duplicado (a mesma coluna existe no painel único e no arquivo da
  dimensão); o usuário que quer o painel inteiro ainda baixa 17 arquivos.

**Opção B — o pacote ler direto do `mape_municipios.parquet` único
(recomendada).** Trocar `.read_dimension()`/`.import()` em `import.R` para
ler o único parquet com `arrow::read_parquet(path, col_select = ...)`,
filtrando as colunas pelo `dimensao_slug` (lido de `dicionario_dimensoes
.parquet`, que já existe) em vez de baixar um arquivo por dimensão. O
parquet columnar já permite essa leitura seletiva sem carregar as ~2.600
colunas inteiras na memória.
- Prós: uma fonte de verdade só; nenhuma duplicação; "4 Base Final" já
  produz exatamente o que essa opção precisa, sem mudar nada lá.
- Contras: exige reescrever `.read_dimension()`, `.join_tables()` e
  `.apply_filters()` em `import.R` (a lógica de junção entre dimensões fica
  quase toda desnecessária, já que o painel já vem junto).

## 3. O `dicionario.xlsx` plano também precisa ser criado

Independente da opção A/B, falta um passo que nenhum script hoje faz:
combinar as 17 abas do METADADOS (empilhadas, com uma coluna `Dimensão`
já presente em cada aba) + o `dimensao_slug` de `dicionario_dimensoes.csv`
(join por nome da variável) num único `dicionario.xlsx` — é esse arquivo
plano, não o METADADOS.xlsx como está hoje, que `import_mape_dictionary()`
lê.

## 4. Recomendação

Opção B, mais o passo do item 3. Nenhuma das duas foi implementada aqui —
fica para quando o formato de distribuição dos dados (`mape_source()`,
ainda com placeholder `github.com/ORG/...`) for decidido, já que as duas
decisões são relacionadas (onde o(s) arquivo(s) vão morar e como o pacote
os lê são a mesma conversa).
