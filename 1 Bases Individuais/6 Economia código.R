# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(openxlsx)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(data.table)
library(janitor)
library(writexl)



# ── 2. Configuração do projeto no Google Cloud ────────────────────────────────
set_billing_id("dados-importacao") ## Substituir pelo seu projeto


# PIB 

# Para carregar o dado direto no R
query <- "
SELECT
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.ano as ano,
    dados.pib as pib,
    dados.impostos_liquidos as impostos_liquidos,
    dados.va as va,
    dados.va_agropecuaria as va_agropecuaria,
    dados.va_industria as va_industria,
    dados.va_servicos as va_servicos,
    dados.va_adespss as va_adespss
FROM `basedosdados.br_ibge_pib.municipio` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

pib <- read_sql(query, billing_project_id = get_billing_id())


### Criação de variáveis


pib_clean <- pib %>%
  mutate(
         impostos_pib = impostos_liquidos/pib,
         dependencia_adm = va_adespss/va,
         dependencia_industria = va_industria/va,
         dependencia_agro = va_agropecuaria/va,
         dependencia_servicos = va_servicos/va)

summary(pib_clean)


## Pesquisa Pecuária Municipal 


# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.tipo_rebanho as tipo_rebanho,
    dados.quantidade as quantidade
FROM `basedosdados.br_ibge_ppm.efetivo_rebanhos` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

rebanhos <- read_sql(query, billing_project_id = get_billing_id())

summary(rebanhos)

table(rebanhos$tipo_rebanho)

rebanhos <- rebanhos %>%
  mutate(
    ano = as.integer(ano),
    quantidade = as.numeric(quantidade)
  )


rebanhos_id <- rebanhos %>%
  filter(ano >= 1991) %>%
  group_by(ano, id_municipio) %>%
  summarise(
    total_rebanho = sum(quantidade, na.rm = TRUE),
    total_rebanho_bovino = sum(quantidade[tipo_rebanho == "Bovino"], na.rm = TRUE),
    total_rebanho_bubalino = sum(quantidade[tipo_rebanho == "Bubalino"], na.rm = TRUE),
    total_rebanho_caprino = sum(quantidade[tipo_rebanho == "Caprino"], na.rm = TRUE),
    total_rebanho_codornas = sum(quantidade[tipo_rebanho == "Codornas"], na.rm = TRUE),
    total_rebanho_equino = sum(quantidade[tipo_rebanho == "Equino"], na.rm = TRUE),
    total_rebanho_galinaceos = sum(quantidade[tipo_rebanho == "Galináceos - galinhas"], na.rm = TRUE),
    total_rebanho_suinos = sum(quantidade[tipo_rebanho == "Suíno - total"], na.rm = TRUE),
    .groups = "drop"
  )

summary(rebanhos_id)

### Produção Origem Animal

# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.produto as produto,
    dados.unidade as unidade,
    dados.quantidade as quantidade,
    dados.valor as valor
FROM `basedosdados.br_ibge_ppm.producao_origem_animal` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

producao_origem_animal <- read_sql(query, billing_project_id = get_billing_id())

class(producao_origem_animal$id_municipio)
class(producao_origem_animal$ano)

names(producao_origem_animal)

table(producao_origem_animal$produto)

table(producao_origem_animal$unidade)

table(producao_origem_animal$unidade, producao_origem_animal$produto)

summary(producao_origem_animal)

producao_origem_animal <- producao_origem_animal %>%
  mutate(
    ano = as.integer(ano),
    quantidade = as.numeric(quantidade),
    # CORREÇÃO (13/11/2026): "valor" não era convertido aqui (só "quantidade"),
    # e às vezes volta do BigQuery como integer64 — sum(valor, na.rm=TRUE)
    # então produz colunas integer64 em producao_origem_animal_id, que
    # sobrevivem ao merge e quebram fifelse(is.na(x), 0, x) mais na frente
    # (integer64 passa is.numeric(), mas tem classe diferente do literal 0).
    valor = as.numeric(valor)
  )


setDT(producao_origem_animal)

producao_origem_animal_id <- producao_origem_animal[ano >= 1991, .(
  total_producao        = sum(quantidade, na.rm = TRUE),
  total_valor_producao  = sum(valor, na.rm = TRUE),
  total_producao_la                 = sum(quantidade[produto == "Lã"], na.rm = TRUE),
  total_valor_producao_la           = sum(valor[produto == "Lã"], na.rm = TRUE),
  total_producao_leite              = sum(quantidade[produto == "Leite"], na.rm = TRUE),
  total_valor_producao_leite        = sum(valor[produto == "Leite"], na.rm = TRUE),
  total_producao_mel                = sum(quantidade[produto == "Mel de abelha"], na.rm = TRUE),
  total_valor_producao_mel          = sum(valor[produto == "Mel de abelha"], na.rm = TRUE),  # corrigido (era "Mel de Abelha")
  total_producao_ovos_codorna       = sum(quantidade[produto == "Ovos de codorna"], na.rm = TRUE),
  total_valor_producao_ovos_codorna = sum(valor[produto == "Ovos de codorna"], na.rm = TRUE),
  total_producao_ovos_galinha       = sum(quantidade[produto == "Ovos de galinha"], na.rm = TRUE),
  total_valor_producao_ovos_galinha = sum(valor[produto == "Ovos de galinha"], na.rm = TRUE),
  total_producao_bicho_seda         = sum(quantidade[produto == "Casulos do bicho-da-seda"], na.rm = TRUE),
  total_valor_producao_bicho_seda   = sum(valor[produto == "Casulos do bicho-da-seda"], na.rm = TRUE)
), by = .(ano, id_municipio)]

summary(producao_origem_animal_id)


## Produção pecuária

# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.ovinos_tosquiados as ovinos_tosquiados,
    dados.vacas_ordenhadas as vacas_ordenhadas
FROM `basedosdados.br_ibge_ppm.producao_pecuaria` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

producao_pecuaria <- read_sql(query, billing_project_id = get_billing_id())

names(producao_pecuaria)

producao_pecuaria_id <- producao_pecuaria %>%
  filter(ano >= 1991)



## Agricultura

## Lavouras Permanentes

## https://basedosdados.org/dataset/fc403b40-a7e1-40e7-9efe-910847b45a69?table=bd9b2960-361f-4465-be23-baaadaaf266d

# Para carregar o dado direto no R
query <- "
SELECT
    CAST(dados.ano AS INT64) as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.produto as produto,
    CAST(dados.area_destinada_colheita AS FLOAT64) as area_destinada_colheita,
    CAST(dados.area_colhida AS FLOAT64) as area_colhida,
    CAST(dados.quantidade_produzida AS FLOAT64) as quantidade_produzida,
    dados.rendimento_medio_producao as rendimento_medio_producao,
    CAST(dados.valor_producao AS FLOAT64) as valor_producao
FROM `basedosdados.br_ibge_pam.lavoura_permanente` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
WHERE dados.ano >= 1991
"

agricultura <- read_sql(query, billing_project_id = get_billing_id())


## Lavouras Temporárias


query <- "
SELECT
    CAST(dados.ano AS INT64) as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.produto as produto,
    CAST(dados.area_plantada AS FLOAT64) as area_plantada,
    CAST(dados.area_colhida AS FLOAT64) as area_colhida,
    CAST(dados.quantidade_produzida AS FLOAT64) as quantidade_produzida,
    dados.rendimento_medio_producao as rendimento_medio_producao,
    CAST(dados.valor_producao AS FLOAT64) as valor_producao
FROM `basedosdados.br_ibge_pam.lavoura_temporaria` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
WHERE dados.ano >= 1991
"

agricultura_t <- read_sql(query, billing_project_id = get_billing_id())

## Agregação na unidade id_municipio e ano


setDT(agricultura)
setDT(agricultura_t)

agricultura[, `:=`(
  ano = as.integer(ano),
  area_destinada_colheita = as.numeric(area_destinada_colheita),
  quantidade_produzida    = as.numeric(quantidade_produzida),
  valor_producao          = as.numeric(valor_producao)
)]

agricultura_t[, `:=`(
  ano = as.integer(ano),
  area_colhida         = as.numeric(area_colhida),
  quantidade_produzida = as.numeric(quantidade_produzida),
  valor_producao       = as.numeric(valor_producao)
)]

# make_clean_names() foi feito para limpar um vetor de nomes ÚNICOS (garante
# unicidade acrescentando sufixo _2, _3... a duplicatas). Aplicado direto na
# coluna `produto` -- que se repete em milhares de linhas -- ele "unicifica"
# cada repetição do mesmo produto, gerando produto_clean diferente por linha
# e explodindo o número de colunas no dcast. Por isso limpamos só os valores
# ÚNICOS de produto (um mapa produto -> produto_clean) e juntamos de volta.

produto_map <- data.table(
  produto = unique(c(agricultura$produto, agricultura_t$produto))
)
produto_map[, produto_clean := make_clean_names(produto)]

agricultura[produto_map, on = "produto", produto_clean := i.produto_clean]
agricultura_t[produto_map, on = "produto", produto_clean := i.produto_clean]

table(agricultura$produto)


# ============================================================
# LAVOURAS PERMANENTES
# ============================================================

# Passo 1: agrega primeiro (rápido, passada única) -> tabela pequena, ainda "longa"
perm_long <- agricultura[ano >= 1991, .(
  total_area       = sum(area_destinada_colheita, na.rm = TRUE),
  total_valor      = sum(valor_producao, na.rm = TRUE),
  total_quantidade = sum(quantidade_produzida, na.rm = TRUE)
), by = .(ano, id_municipio, produto_clean)]

# Passo 2: reshape puro (sem agregação) -> muito mais rápido
perm_produto <- dcast(
  perm_long,
  ano + id_municipio ~ produto_clean,
  value.var = c("total_area", "total_valor", "total_quantidade"),
  fill = 0
)

# Totais gerais (independe do produto)
perm_total <- agricultura[ano >= 1991, .(
  total_area_permanente       = sum(area_destinada_colheita, na.rm = TRUE),
  total_valor_permanente      = sum(valor_producao, na.rm = TRUE),
  total_quantidade_permanente = sum(quantidade_produzida, na.rm = TRUE)
), by = .(ano, id_municipio)]

lavouras_permanentes <- merge(perm_total, perm_produto, by = c("ano", "id_municipio"), all = TRUE)

# ============================================================
# LAVOURAS TEMPORÁRIAS (mesmo padrão)
# ============================================================

temp_long <- agricultura_t[ano >= 1991, .(
  total_area       = sum(area_colhida, na.rm = TRUE),
  total_valor      = sum(valor_producao, na.rm = TRUE),
  total_quantidade = sum(quantidade_produzida, na.rm = TRUE)
), by = .(ano, id_municipio, produto_clean)]

temp_produto <- dcast(
  temp_long,
  ano + id_municipio ~ produto_clean,
  value.var = c("total_area", "total_valor", "total_quantidade"),
  fill = 0
)

temp_total <- agricultura_t[ano >= 1991, .(
  total_area_temporaria       = sum(area_colhida, na.rm = TRUE),
  total_valor_temporaria      = sum(valor_producao, na.rm = TRUE),
  total_quantidade_temporaria = sum(quantidade_produzida, na.rm = TRUE)
), by = .(ano, id_municipio)]

lavouras_temporarias <- merge(temp_total, temp_produto, by = c("ano", "id_municipio"), all = TRUE)

# ============================================================
# JUNTAR TUDO: permanentes + temporárias + rebanhos_id + 
#              producao_origem_animal_id + producao_pecuaria_id
# unidade final = id_municipio x ano
# ============================================================

producao_pecuaria_id <- producao_pecuaria %>%
  filter(ano >= 1991) %>%
  select(-sigla_uf, -sigla_uf_nome, -id_municipio_nome)


setDT(producao_pecuaria_id)

producao_pecuaria_id[, `:=`(
  ano = as.integer(ano),
  id_municipio = as.character(id_municipio),
  # CORREÇÃO (13/11/2026): ovinos_tosquiados/vacas_ordenhadas vêm do BigQuery
  # como integer64 e nunca eram convertidos — mesmo problema de "valor" em
  # producao_origem_animal (ver nota acima). Sem isso, as duas colunas ficam
  # fora do preenchimento com 0 (NA em vez de 0 onde não houve produção).
  ovinos_tosquiados = as.numeric(ovinos_tosquiados),
  vacas_ordenhadas  = as.numeric(vacas_ordenhadas)
)]

setDT(producao_origem_animal_id)
producao_origem_animal_id[, `:=`(
  ano = as.integer(ano),
  id_municipio = as.character(id_municipio)
)]

# checagem de tipo em TODAS as peças antes de mergear
sapply(
  list(lavouras_permanentes, lavouras_temporarias, rebanhos_id,
       producao_origem_animal_id, producao_pecuaria_id),
  function(x) class(x$id_municipio)
)

lavouras <- merge(lavouras_permanentes, lavouras_temporarias, by = c("ano", "id_municipio"), all = TRUE)
painel_agro <- merge(lavouras, rebanhos_id, by = c("ano", "id_municipio"), all = TRUE)
painel_agro <- merge(painel_agro, producao_origem_animal_id, by = c("ano", "id_municipio"), all = TRUE)
painel_agro <- merge(painel_agro, producao_pecuaria_id, by = c("ano", "id_municipio"), all = TRUE)

cols_num <- setdiff(names(painel_agro), c("ano", "id_municipio", "sigla_uf", "sigla_uf_nome", "id_municipio_nome"))

# CORREÇÃO (13/11/2026): fifelse(is.na(x), 0, x) quebra se x não for numérico
# ou integer ("yes" e "no" com classes diferentes) — e sapply(class) pode
# mascarar isso quando a coluna tem mais de uma classe (ex.: integer64).
# Checamos is.numeric() coluna por coluna; o que não for numérico (character,
# logical, integer64 etc.) é sinalizado e fica fora do preenchimento com 0
# (mantido como está, NA onde não houver valor) — flagging over silencing,
# em vez de travar o script.
eh_numerico <- vapply(cols_num, function(nm) class(painel_agro[[nm]])[1] %in% c("numeric", "integer", "double"), logical(1))
cols_nao_numericos <- cols_num[!eh_numerico]
if (length(cols_nao_numericos) > 0) {
  classes_nao_num <- vapply(cols_nao_numericos, function(nm) paste(class(painel_agro[[nm]]), collapse = "/"), character(1))
  warning("Colunas de painel_agro nao numericas (nao preenchidas com 0): ",
          paste(paste0(cols_nao_numericos, " [", classes_nao_num, "]"), collapse = ", "))
}
cols_num <- cols_num[eh_numerico]
painel_agro[, (cols_num) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = cols_num]

# checagem final: uma linha por ano-município?
painel_agro[, .N, by = .(ano, id_municipio)][N > 1]  # deve retornar 0 linhas
summary(painel_agro)

names(painel_agro)


# Censo Agropecuário 

here("1 Bases Individuais", "6 Economia", "censoibge_corrigido", "base_censo_agropecuario.csv")
censo_data <- readr::read_csv(
  here("1 Bases Individuais", "6 Economia", "censoibge_corrigido", "base_censo_agropecuario.csv")
)

summary(censo_data)

table(censo_data$ano)

# ── 1. Carregar e tipar ──────────────────────────────────────────────────
censo_data <- readr::read_csv(
  here("1 Bases Individuais", "6 Economia", "censoibge_corrigido", "base_censo_agropecuario.csv"),
  show_col_types = FALSE
)

setDT(censo_data)
censo_data[, `:=`(
  ano          = as.integer(ano),
  id_municipio = as.character(id_municipio)
)]

# ── 2. Mapear cada ano do painel para o censo mais próximo ──────────────
anos_censo <- sort(unique(censo_data$ano))          # 1995 2006 2017
pontos_medios <- (head(anos_censo, -1) + tail(anos_censo, -1)) / 2  # 2000.5 2011.5

anos_painel <- sort(unique(painel_agro$ano))
mapa_ano_censo <- data.table(
  ano           = anos_painel,
  censo_ano_ref = anos_censo[findInterval(anos_painel, pontos_medios) + 1]
)
mapa_ano_censo[, censo_distancia_anos := abs(ano - censo_ano_ref)]
mapa_ano_censo[, censo_obs := ano == censo_ano_ref]

# checagem: algum ano do painel ficou a mais de 5 anos do censo mais próximo?
# (esperado só se o painel for além de 2022, já que 2017 é o último censo)
mapa_ano_censo[censo_distancia_anos > 5]

# ── 3. Juntar o mapa de anos ao painel ───────────────────────────────────
painel_agro <- merge(painel_agro, mapa_ano_censo, by = "ano", all.x = TRUE)

# ── 4. Juntar os dados do censo pelo censo_ano_ref ───────────────────────
censo_vars <- setdiff(names(censo_data), c("id_municipio", "nome_municipio", "sigla_uf", "ano"))
# "numero_estabelecimentos" "area_estabelecimentos_ha"

setnames(censo_data, "ano", "censo_ano_ref")

painel_agro <- merge(
  painel_agro,
  censo_data[, c("id_municipio", "censo_ano_ref", censo_vars), with = FALSE],
  by = c("id_municipio", "censo_ano_ref"),
  all.x = TRUE
)

# ── 5. Separar valor observado (_obs) de valor persistido (_persistido) ──
# _obs        -> preenchido só no ano exato do censo, NA nos demais
# _persistido -> preenchido em toda a janela de +/-5 anos com o valor do censo mais próximo
for (v in censo_vars) {
  obs_col  <- paste0("censo_", v, "_obs")
  pers_col <- paste0("censo_", v, "_persistido")
  painel_agro[, (obs_col)  := fifelse(censo_obs, get(v), NA_real_)]
  painel_agro[, (pers_col) := get(v)]
}
painel_agro[, (censo_vars) := NULL]

# checagem final: uma linha por ano-município?
painel_agro[, .N, by = .(ano, id_municipio)][N > 1]  # deve retornar 0 linhas
summary(painel_agro)

names(painel_agro)


## Juntar com PIB

setDT(pib_clean)

pib_clean[, `:=`(
  ano = as.integer(ano),
  id_municipio = as.character(id_municipio)
)]

painel_agro[, `:=`(
  ano = as.integer(ano),
  id_municipio = as.character(id_municipio)
)]

economia_brasileira <- merge(pib_clean, painel_agro, by = c("ano", "id_municipio"), all = TRUE)

# checagem: sobrou alguma coluna duplicada do merge?
names(economia_brasileira)[grepl("\\.x$|\\.y$", names(economia_brasileira))]

# se aparecer algo, remover a duplicata (mantendo a versão do PIB, que é a fonte de nome mais confiável)
if (any(grepl("\\.x$|\\.y$", names(economia_brasileira)))) {
  economia_brasileira[, id_municipio_nome := id_municipio_nome.x]
  economia_brasileira[, c("id_municipio_nome.x", "id_municipio_nome.y") := NULL]
}

# checagem final: uma linha por ano-município?
economia_brasileira[, .N, by = .(ano, id_municipio)][N > 1]  # deve retornar 0 linhas
summary(economia_brasileira)


# ── Diagnóstico de NA por variável ──────────────────────────────────────
na_summary <- data.table(
  variavel = names(economia_brasileira),
  n_na     = sapply(economia_brasileira, function(x) sum(is.na(x))),
  n_total  = nrow(economia_brasileira)
)
na_summary[, pct_na := round(100 * n_na / n_total, 2)]
setorder(na_summary, -n_na)

print(na_summary, nrows = Inf)

# salvar para conferência/anexar ao METADADOS
fwrite(na_summary, here("1 Bases Individuais", "6 Economia", "na_diagnostico_economia.csv"))

na_summary[pct_na > 0]          # só variáveis com algum NA
na_summary[pct_na >= 50]        # cruza com o limiar de 50% de cobertura que vocês já usam para escopo temporal válido

na_by_year <- economia_brasileira[, lapply(.SD, function(x) round(100 * sum(is.na(x)) / .N, 1)),
                                  by = ano, .SDcols = setdiff(names(economia_brasileira), c("ano", "id_municipio"))]

# formato longo, ordenado — mais fácil de ler que a matriz larga
na_by_year_long <- melt(na_by_year, id.vars = "ano", variable.name = "variavel", value.name = "pct_na")
setorder(na_by_year_long, -pct_na)
head(na_by_year_long, 30)



# ── Exportação ────────────────────────────────────────────────────────

dir.create(here("1 Bases Individuais", "6 Economia"), showWarnings = FALSE)

# writexl::write_xlsx() em vez de openxlsx::write.xlsx(): ver nota em
# "4 Corrupção e Transparência código.R" sobre a referência interna quebrada
# que o openxlsx (4.2.8.1) grava com write.xlsx().
write_xlsx(
  economia_brasileira,
  path = here("1 Bases Individuais", "6 Economia", "economia_brasileira.xlsx")
)

# ── Validação padronizada (2 Validação/6 Economia/) ──────────────────────
# Antes desta rodada, Economia era a única das 17 dimensões sem heatmap de
# cobertura e com o resumo de NA fora do padrão (na_diagnostico_economia.csv,
# na própria pasta "1 Bases Individuais/6 Economia", em vez de
# "2 Validação/6 Economia/6_economia_resumo_na.csv"). validar_dimensao()
# corrige os dois pontos; o na_diagnostico_economia.csv acima é mantido como
# diagnóstico de trabalho, não é mais o artefato oficial de validação.
validar_dimensao(economia_brasileira, n_dim = 6)








