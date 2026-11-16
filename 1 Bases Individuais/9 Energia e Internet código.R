# ══════════════════════════════════════════════════════════════════════════════
# BANCO DE DADOS: ENERGIA, INTERNET E TELEFONIA MUNICIPAL (COM REGIONALIZAÇÃO)
# Fontes: Anatel (Base dos Dados / BigQuery) e MME (Luz para Todos)
# ══════════════════════════════════════════════════════════════════════════════

# 1. PACOTES ───────────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(readr)
library(stringr)
library(openxlsx)
library(writexl)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))


# 2. CONFIGURAÇÃO DE AMBIENTE ──────────────────────────────────────────────────
# ID do projeto no Google Cloud Platform (BigQuery)
set_billing_id("dados-importacao") ## Substituir pelo seu projeto 


# ══════════════════════════════════════════════════════════════════════════════
# PARTE 1 — DIRETÓRIO COMPLETO DE MUNICÍPIOS (VARIÁVEIS GEOGRÁFICAS E REGIONAIS)
# ══════════════════════════════════════════════════════════════════════════════

# Baixa a tabela com todos os atributos municipais do IBGE/Base dos Dados
# (contém id_municipio_6, id_municipio_tse, microrregião, DDD, Amazônia Legal, etc.)
diretorio_completo <- read_sql("
  SELECT *
  FROM `basedosdados.br_bd_diretorios_brasil.municipio`
")


# ══════════════════════════════════════════════════════════════════════════════
# PARTE 2 — INTERNET E TELEFONIA (ANATEL VIA BIGQUERY)
# ══════════════════════════════════════════════════════════════════════════════

# https://basedosdados.org/dataset/4ba41417-ba19-4022-bc24-6837db973009?table=f8fd49ab-cf6e-40dc-9344-d251c22ee4ab

# ── 2.1 Banda Larga Fixa (Acessos Absolutos) ─────────────────────────────────
query_blf_absoluto <- "
  SELECT
    ano,
    mes,
    sigla_uf,
    id_municipio,
    SUM(acessos) AS acessos_banda_larga_fixa
  FROM `basedosdados.br_anatel_banda_larga_fixa.microdados`
  GROUP BY ano, mes, sigla_uf, id_municipio
  ORDER BY ano, mes, id_municipio
"

banda_larga_absoluto <- read_sql(query_blf_absoluto)

# ── 2.2 Banda Larga Fixa (Densidade - acessos por 100 domicílios) ────────────
query_blf_densidade <- "
  SELECT
    ano,
    mes,
    sigla_uf,
    id_municipio,
    densidade AS densidade_banda_larga_fixa
  FROM `basedosdados.br_anatel_banda_larga_fixa.densidade_municipio`
  ORDER BY ano, mes, id_municipio
"
banda_larga_densidade <- read_sql(query_blf_densidade)




# ── 2.3 Telefonia Móvel (Densidade - acessos por 100 hab) ────────────────────

# https://basedosdados.org/dataset/d3c86a88-d9a4-4fc0-bdec-08ab61e8f63c?table=bf94e55d-4685-4775-9e93-8644e8ecf76f


query_tel_densidade <- "
  SELECT
    ano,
    mes,
    sigla_uf,
    id_municipio,
    densidade AS densidade_telefonia_movel
  FROM `basedosdados.br_anatel_telefonia_movel.densidade_municipio`
  ORDER BY ano, mes, id_municipio
"
telefonia_movel_densidade <- read_sql(query_tel_densidade)



# ── 2.4 Consolidação das bases da Anatel ──────────────────────────────────────


# Agrupar por id_municipio e ano

summary(banda_larga_absoluto)

summary(banda_larga_densidade)

summary(telefonia_movel_densidade)


bd_absoluto_bl <- banda_larga_absoluto %>%
  group_by(ano, id_municipio) %>%
  summarise(soma_acessos_banda_larga_fixa = sum(acessos_banda_larga_fixa, na.rm = TRUE),
            media_acessos_banda_larga_fixa = mean(acessos_banda_larga_fixa, na.rm = TRUE)) %>%
  ungroup()

bd_densidade_bl <- banda_larga_densidade %>%
  group_by(ano, id_municipio) %>%
  summarise(media_densidade_banda_larga_fixa = mean(densidade_banda_larga_fixa, na.rm = TRUE)) %>%
  ungroup()

bd_densidade_tf <- telefonia_movel_densidade %>%
  group_by(ano, id_municipio) %>%
  summarise(media_densidade_telefonia_movel = mean(densidade_telefonia_movel, na.rm = TRUE)) %>%
  ungroup()

base_internet <- bd_absoluto_bl %>%
  full_join(bd_densidade_bl, by = c("ano", "id_municipio"))

base_internet <- base_internet %>%
  full_join(bd_densidade_tf, by = c("ano", "id_municipio"))

summary(base_internet)



# ══════════════════════════════════════════════════════════════════════════════
# PARTE 3 — ENERGIA (LUZ PARA TODOS / MME)
# ══════════════════════════════════════════════════════════════════════════════

# ── 3.1 Leitura dos dados do MME ─────────────────────────────────────────────
domicilios_atendidos <- read_csv2(
  "https://dadosabertos.mme.gov.br/dataset/418de37c-1689-46b7-832d-8eb5b5df9987/resource/3e832dfa-9985-4c5f-a36b-cc72479217fb/download/domicilios_atendidos.csv",
  locale = locale(encoding = "Latin1")
)

summary(domicilios_atendidos)

table(domicilios_atendidos$programa)

lpt <- domicilios_atendidos %>%
  group_by(ano, municipio, estado) %>%
  summarise(domicilios_lpt = sum(qtddomicilios, na.rm = TRUE),
            domicilios_lpt_amazonia_legal = sum(qtddomicilios[programa == "LPT - Regiões Remotas da Amazônia Legal"], na.rm = TRUE),
            domicilios_lpt_rural = sum(qtddomicilios[programa == "LPT - Rural"], na.rm = TRUE),
            domicilios_lpt_recurso_distribuidora = sum(qtddomicilios[programa == "Recurso da Distribuidora"], na.rm = TRUE)) %>%
  ungroup()




# ── 3.2 Funções e Tabelas de Apoio para Padronização de Texto ────────────────
remover_acentos <- function(x) {
  chartr(
    "áàâãäéèêëíìîïóòôõöúùûüçÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ",
    "aaaaaeeeeiiiiooooouuuucAAAAAEEEEIIIIOOOOOUUUUC",
    x
  )
}

de_para_uf <- tibble::tribble(
  ~estado, ~sigla_uf,
  "Acre", "AC", "Alagoas", "AL", "Amapá", "AP", "Amazonas", "AM",
  "Bahia", "BA", "Ceará", "CE", "Distrito Federal", "DF", "Espírito Santo", "ES",
  "Goiás", "GO", "Maranhão", "MA", "Mato Grosso", "MT", "Mato Grosso do Sul", "MS",
  "Minas Gerais", "MG", "Pará", "PA", "Paraíba", "PB", "Paraná", "PR",
  "Pernambuco", "PE", "Piauí", "PI", "Rio de Janeiro", "RJ", "Rio Grande do Norte", "RN",
  "Rio Grande do Sul", "RS", "Rondônia", "RO", "Roraima", "RR", "Santa Catarina", "SC",
  "São Paulo", "SP", "Sergipe", "SE", "Tocantins", "TO"
) %>%
  mutate(estado_padronizado = str_to_lower(remover_acentos(estado)))

correcao_municipios <- tibble::tribble(
  ~municipio, ~estado, ~municipio_correto,
  "Olho Dágua Grande", "Alagoas", "Olho D'Água Grande",
  "Olho Dágua das Flores", "Alagoas", "Olho D'Água das Flores",
  "Olho Dágua do Casado", "Alagoas", "Olho D'Água do Casado",
  "Tanque Darca", "Alagoas", "Tanque D'Arca",
  "Dias Dávila", "Bahia", "Dias D'Ávila",
  "São João Daliança", "Goiás", "São João D'Aliança",
  "Sítio Dabadia", "Goiás", "Sítio D'Abadia",
  "Olho Dágua das Cunhãs", "Maranhão", "Olho D'Água das Cunhãs",
  "Conquista Doeste", "Mato Grosso", "Conquista D'Oeste",
  "Figueirópolis Doeste", "Mato Grosso", "Figueirópolis D'Oeste",
  "Glória Doeste", "Mato Grosso", "Glória D'Oeste",
  "Lambari Doeste", "Mato Grosso", "Lambari D'Oeste",
  "Mirassol Doeste", "Mato Grosso", "Mirassol D'Oeste",
  "Brasópolis", "Minas Gerais", "Brazópolis",
  "Olhos-Dágua", "Minas Gerais", "Olhos-D'Água",
  "Pingo-Dágua", "Minas Gerais", "Pingo-D'Água",
  "Mãe Dágua", "Paraiba", "Mãe D'Água",
  "Olho Dágua", "Paraiba", "Olho D'Água",
  "Seridó", "Paraiba", "São Vicente do Seridó",
  "São Domingos de Pombal", "Paraiba", "São Domingos",
  "Diamante Doeste", "Paraná", "Diamante D'Oeste",
  "Itapejara Doeste", "Paraná", "Itapejara D'Oeste",
  "Pérola Doeste", "Paraná", "Pérola D'Oeste",
  "Rancho Alegre Doeste", "Paraná", "Rancho Alegre D'Oeste",
  "São Jorge Doeste", "Paraná", "São Jorge D'Oeste",
  "Eldorado dos Carajás", "Pará", "Eldorado do Carajás",
  "Pau Darco", "Pará", "Pau D'Arco",
  "Belém de São Francisco", "Pernambuco", "Belém do São Francisco",
  "Lagoa do Itaenga", "Pernambuco", "Lagoa de Itaenga",
  "Barra Dalcântara", "Piauí", "Barra D'Alcântara",
  "Olho Dágua do Piauí", "Piauí", "Olho D'Água do Piauí",
  "Pau Darco do Piauí", "Piauí", "Pau D'Arco do Piauí",
  "Augusto Severo", "Rio Grande do Norte", "Campo Grande",
  "Presidente Juscelino", "Rio Grande do Norte", "Serra Caiada",
  "Januário Cicco", "Rio Grande do Norte", "Boa Saúde",
  "Parati", "Rio de Janeiro", "Paraty",
  "Trajano de Morais", "Rio de Janeiro", "Trajano de Moraes",
  "Alta Floresta Doeste", "Rondônia", "Alta Floresta D'Oeste",
  "Alvorada Doeste", "Rondônia", "Alvorada D'Oeste",
  "Espigão Doeste", "Rondônia", "Espigão D'Oeste",
  "Machadinho Doeste", "Rondônia", "Machadinho D'Oeste",
  "Nova Brasilândia Doeste", "Rondônia", "Nova Brasilândia D'Oeste",
  "Santa Luzia Doeste", "Rondônia", "Santa Luzia D'Oeste",
  "São Felipe Doeste", "Rondônia", "São Felipe D'Oeste",
  "Santana do Livramento", "Rio Grande do Sul", "Sant'Ana do Livramento",
  "Herval Doeste", "Santa Catarina", "Herval D'Oeste",
  "Itaporanga Dajuda", "Sergipe", "Itaporanga D'Ajuda",
  "Moji Mirim", "São Paulo", "Mogi Mirim",
  "Aparecida Doeste", "São Paulo", "Aparecida D'Oeste",
  "Estrela Doeste", "São Paulo", "Estrela D'Oeste",
  "Guarani Doeste", "São Paulo", "Guarani D'Oeste",
  "Palmeira Doeste", "São Paulo", "Palmeira D'Oeste",
  "Santa Bárbara Doeste", "São Paulo", "Santa Bárbara D'Oeste",
  "Santa Clara Doeste", "São Paulo", "Santa Clara D'Oeste",
  "Santa Rita Doeste", "São Paulo", "Santa Rita D'Oeste",
  "São João do Pau Dalho", "São Paulo", "São João do Pau D'Alho",
  "São Valério da Natividade", "Tocantins", "São Valério",
  "Couto de Magalhães", "Tocantins", "Couto Magalhães",
  "Pau Darco", "Tocantins", "Pau D'Arco"
)

# ── 3.3 Padronização dos Nomes e Pareamento com IBGE ─────────────────────────
diretorio_simplificado <- diretorio_completo %>%
  select(id_municipio, nome, sigla_uf) %>%
  mutate(nome_padronizado = str_to_lower(remover_acentos(nome)))

domicilios_atendidos_com_id <- lpt %>%
  left_join(correcao_municipios, by = c("municipio", "estado")) %>%
  mutate(municipio = coalesce(municipio_correto, municipio)) %>%
  select(-municipio_correto) %>%
  mutate(estado_padronizado = str_to_lower(remover_acentos(estado))) %>%
  left_join(de_para_uf %>% select(sigla_uf, estado_padronizado), by = "estado_padronizado") %>%
  mutate(municipio_padronizado = str_to_lower(remover_acentos(municipio))) %>%
  left_join(
    diretorio_simplificado %>% select(id_municipio, nome_padronizado, sigla_uf),
    by = c("municipio_padronizado" = "nome_padronizado", "sigla_uf" = "sigla_uf")
  )

# ── 3.4 Agregação final da base de Luz ───────────────────────────────────────
# [UNICO] faltava o summarise() depois do group_by (o pipe parava no
# agrupamento): como nomes de município podem mudar (ver correcao_municipios)
# e dois nomes de origem podem resolver para o mesmo id_municipio, sem a
# soma explícita um mesmo município-ano podia aparecer em duas linhas.
base_luz <- domicilios_atendidos_com_id %>%
  filter(!is.na(id_municipio)) %>%
  group_by(ano, sigla_uf, id_municipio) %>%
  summarise(
    domicilios_lpt = sum(domicilios_lpt, na.rm = TRUE),
    domicilios_lpt_amazonia_legal = sum(domicilios_lpt_amazonia_legal, na.rm = TRUE),
    domicilios_lpt_rural = sum(domicilios_lpt_rural, na.rm = TRUE),
    domicilios_lpt_recurso_distribuidora = sum(domicilios_lpt_recurso_distribuidora, na.rm = TRUE),
    .groups = "drop"
  )

summary(base_luz)


# ══════════════════════════════════════════════════════════════════════════════
# PARTE 4 — CRUZAMENTO COM O DIRETÓRIO COMPLETO E EXPORTAÇÃO
# ══════════════════════════════════════════════════════════════════════════════

# ── 4.1 Unir cada base com todas as colunas de regionalização do IBGE ─────────

names(base_internet)

names(base_luz)

base_luz$ano <- as.numeric(base_luz$ano)

base_internet$ano <- as.numeric(base_internet$ano)

energia_internet <- full_join(base_internet, base_luz, by = c("ano", "id_municipio"))

summary(energia_internet)

# [UNICO] estado_padronizado/estado/municipio_padronizado/municipio já não
# sobrevivem ao summarise() de base_luz (corrigido acima) — só sigla_uf
# continua (é chave do group_by), e sai aqui como nos demais.
energia_internet <- energia_internet %>%
  select(-sigla_uf)

# [UNICO] `diretorio_completo` (Parte 1) era baixado mas nunca unido à base
# final — apesar do título do arquivo anunciar uma base "com
# regionalização", a exportação ficava só com ano, id_municipio e as
# variáveis de internet/energia (nem UF sobrava, por causa do select()
# acima). Reincorporado aqui.
energia_internet <- energia_internet %>%
  left_join(diretorio_completo, by = "id_municipio") %>%
  relocate(id_municipio, ano)


# ── 4.2 Exportação direta na pasta mape_municipios ────────────────────────────


dir.create(here("1 Bases Individuais", "9 Energia e Internet"), showWarnings = FALSE)

writexl::write_xlsx(
  energia_internet,
  path = here("1 Bases Individuais", "9 Energia e Internet", "energia_internet.xlsx")
)

# ── Validação padronizada (2 Validação/9 Energia e Internet/) ───────────────
validar_dimensao(energia_internet, n_dim = 9)

