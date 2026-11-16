
# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(openxlsx)
library(here)
library(writexl)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))

# ── 2. Configuração do projeto no Google Cloud ────────────────────────────────
set_billing_id("dados-importacao") ## Substituir pelo seu projeto


# 3. Diretórios 

# Para carregar o dado direto no R
query <- "
SELECT
    dados.id_municipio as id_municipio,
    dados.id_municipio_6 as id_municipio_6,
    dados.id_municipio_tse as id_municipio_tse,
    dados.nome as nome,
    dados.capital_uf as capital_uf,
    dados.id_regiao_saude as id_regiao_saude,
    dados.nome_regiao_saude as nome_regiao_saude,
    dados.id_regiao_imediata as id_regiao_imediata,
    dados.nome_regiao_imediata as nome_regiao_imediata,
    dados.id_regiao_intermediaria as id_regiao_intermediaria,
    dados.nome_regiao_intermediaria as nome_regiao_intermediaria,
    dados.id_microrregiao as id_microrregiao,
    dados.nome_microrregiao as nome_microrregiao,
    dados.id_mesorregiao as id_mesorregiao,
    dados.nome_mesorregiao as nome_mesorregiao,
    dados.id_regiao_metropolitana as id_regiao_metropolitana,
    dados.nome_regiao_metropolitana as nome_regiao_metropolitana,
    dados.ddd as ddd,
    dados.id_uf as id_uf,
    dados.sigla_uf as sigla_uf,
    dados.nome_uf as nome_uf,
    dados.nome_regiao as nome_regiao,
    dados.amazonia_legal as amazonia_legal,
    dados.centroide as centroide
FROM `basedosdados.br_bd_diretorios_brasil.municipio` AS dados
"

diretorios <- read_sql_retry(query, billing_project_id = get_billing_id())

str(diretorios)

# 4. Área total 

# Para carregar o dado direto no R

query <- "
SELECT
    dados.ano as ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.bioma as bioma,
    dados.area_total as area_total
FROM `basedosdados.br_inpe_prodes.municipio_bioma` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

area <- read_sql_retry(query, billing_project_id = get_billing_id())

table(area$ano)


# 5. AMC

# Para carregar o dado direto no R
query <- "
SELECT
    dados.id_amc as id_amc,
    dados.ano_de as ano_de,
    dados.ano_para as ano_para,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome
FROM `basedosdados.br_ibge_amc.municipio_de_para` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

amc <- read_sql_retry(query, billing_project_id = get_billing_id())

table(amc$ano_de, amc$ano_para)





# 7. Distâncias

here("1 Bases Individuais", "1 Identificação", "data.csv")


distancias <- readr::read_csv(
  here("1 Bases Individuais", "1 Identificação", "data.csv")
)

names(distancias)

distancias <- distancias %>%
  select(muncode, ln.dist.capital.sd, ln.dist.coast.sd, altitude.sd, longitude.sd, latitude.sd)


# 8. Juntar bases de dados

## 8.0 Padronizar tipos das chaves (evita joins silenciosamente vazios) ────────
diretorios <- diretorios %>% mutate(id_municipio = as.character(id_municipio))
area       <- area       %>% mutate(id_municipio = as.character(id_municipio))
amc        <- amc        %>% mutate(id_municipio = as.character(id_municipio))
distancias <- distancias %>% mutate(muncode = as.character(muncode))

## 8.1 Área total por município-ano (soma das áreas por bioma) ────────────────
area_mun_ano <- area %>%
  group_by(id_municipio, ano) %>%
  summarise(area_total = sum(area_total, na.rm = TRUE), .groups = "drop")

range(area_mun_ano$ano) # conferir cobertura real de anos da base de área

## 8.2 Distâncias: ligar primeiro pelo próprio id_municipio; só usar o        ─
##     crosswalk AMC para os municípios que não existem em data.csv (criados
##     depois de 2000).
##     CORREÇÃO (ver 1 Identificação/validacao/relatorio_validacao_distancias
##     .docx, 07/11/2026): a versão anterior ligava TODO município pelo AMC,
##     sem testar antes o id_municipio direto. Um mesmo id_amc aparece em
##     várias linhas da tabela (uma por par de anos, e às vezes mais de uma
##     dentro do mesmo par), então o inner_join "many-to-many" multiplicava
##     cada município por todas as áreas que compartilhavam o id_amc em
##     QUALQUER par, e o distinct(id_municipio, .keep_all = TRUE) seguinte
##     ficava com a primeira linha — arbitrária. Resultado: só 2.310 dos
##     5.571 municípios ficavam com o valor certo (conferido de novo nesta
##     validação, mesmo número do relatório). A correção abaixo é a sugerida
##     no relatório.

# (a) Município já existe em data.csv com o próprio código — liga direto.
#     Resolve 5.505 dos 5.571 municípios.
distancias_direto <- diretorios %>%
  distinct(id_municipio) %>%
  inner_join(distancias, by = c("id_municipio" = "muncode"))

n_distinct(distancias_direto$id_municipio)  # esperado: 5505

# (b) Os 66 que faltam (municípios criados depois de 2000) não têm linha em
#     data.csv. Para esses, usamos o par ano_de=2000/ano_para=2010 da tabela
#     AMC para achar com quais municípios de data.csv eles compartilham a
#     mesma área (id_amc) nesse par.
sem_direto <- diretorios %>%
  distinct(id_municipio) %>%
  filter(!id_municipio %in% distancias_direto$id_municipio)

amc_pair_2000_2010 <- amc %>%
  filter(ano_de == 2000, ano_para == 2010) %>%
  distinct(id_amc, id_municipio)

amc_sem_direto <- amc_pair_2000_2010 %>%
  filter(id_municipio %in% sem_direto$id_municipio)

candidatos_amc <- amc_pair_2000_2010 %>%
  filter(id_amc %in% amc_sem_direto$id_amc, id_municipio %in% distancias$muncode) %>%
  distinct(id_amc, muncode = id_municipio)

match_amc  <- amc_sem_direto %>% inner_join(candidatos_amc, by = "id_amc", relationship = "many-to-many")
contagem   <- match_amc %>% count(id_municipio, name = "n_candidatos")

## Só herda quando há exatamente 1 candidato na mesma área (34 municípios).
## Quando há mais de um (24 municípios) fica NA — não há uma regra definida
## no relatório de validação para escolher entre eles. Os 8 restantes (2 com
## id_amc mas sem nenhum município de data.csv na mesma área; 6 sem id_amc
## no par 2000-2010) também ficam NA. Todos continuam no painel final, só
## sem as variáveis de distância preenchidas — mantidos, não removidos
## (flagging over silencing).
candidatos_unicos <- match_amc %>%
  semi_join(contagem %>% filter(n_candidatos == 1), by = "id_municipio") %>%
  distinct(id_municipio, muncode)

n_distinct(candidatos_unicos$id_municipio)  # esperado: 34

distancias_herdadas <- candidatos_unicos %>%
  inner_join(distancias, by = "muncode") %>%
  select(-muncode)

## 8.3 Variáveis de distância finais (invariantes no tempo) ──────────────────
distancias_padronizadas <- bind_rows(distancias_direto, distancias_herdadas)

n_distinct(distancias_padronizadas$id_municipio)       # esperado: 5505 + 34 = 5539
anyDuplicated(distancias_padronizadas$id_municipio)    # esperado: 0

## 8.4 Painel balanceado município-ano (2000–2025), para os 5571 municípios  ──
##     atuais em `diretorios`
anos <- 2000:2025

painel <- diretorios %>%
  distinct(id_municipio) %>%
  tidyr::crossing(ano = anos)

## 8.5 Juntar área, imputando 2024 e 2025 com o último ano disponível (LOCF) ──
painel <- painel %>%
  left_join(area_mun_ano, by = c("id_municipio", "ano")) %>%
  arrange(id_municipio, ano) %>%
  group_by(id_municipio) %>%
  tidyr::fill(area_total, .direction = "down") %>%
  ungroup()

## 8.6 Juntar distâncias (invariantes no tempo) e diretórios ─────────────────
painel <- painel %>%
  left_join(distancias_padronizadas, by = "id_municipio") %>%
  left_join(diretorios, by = "id_municipio")

## 8.7 Checagens finais ───────────────────────────────────────────────────────
n_distinct(painel$id_municipio)                                    # esperado: 5571
painel %>% filter(ano == 2025) %>% nrow()                          # esperado: 5571
painel %>% filter(ano == 2025, !is.na(ln.dist.capital.sd)) %>%
  nrow()                                                            # esperado: 5539
painel %>% filter(ano == 2000, !is.na(area_total)) %>% nrow()
painel %>% filter(ano %in% c(2024, 2025)) %>%
  summarise(pct_area_preenchida = mean(!is.na(area_total)))


## Alterar ordem 

diretorios <- painel %>%
  select(ano, id_municipio, id_municipio_6, id_municipio_tse, nome, capital_uf, id_regiao_saude, nome_regiao_saude,
         id_regiao_imediata, nome_regiao_imediata, id_regiao_intermediaria, nome_regiao_intermediaria,
         id_microrregiao, nome_microrregiao, id_mesorregiao, nome_mesorregiao, id_regiao_metropolitana, nome_regiao_metropolitana,
         ddd, id_uf, sigla_uf, nome_uf, nome_regiao, amazonia_legal, centroide, area_total,
         ln.dist.capital.sd, ln.dist.coast.sd, longitude.sd, latitude.sd, altitude.sd)


str(diretorios)


# ── 9. Exportar .xlsx ────────────────────────────────────────────────────────
# writexl::write_xlsx() em vez de openxlsx::write.xlsx(): o openxlsx instalado
# nesta máquina (4.2.8.1) grava toda vez uma relação para um drawing/
# vmlDrawing que nunca é escrito no pacote .xlsx, deixando o arquivo com
# referência interna quebrada (abre no Excel/readxl, mas quebra em leitores
# mais estritos, como openpyxl em Python). Mesma observação em
# "4 Corrupção e Transparência código.R". `centroide` (classe wk_wkt) precisa
# virar texto antes, porque o writexl não serializa esse tipo.
dir.create(here("1 Bases Individuais", "1 Identificação"), showWarnings = FALSE)

diretorios <- diretorios %>%
  mutate(centroide = as.character(centroide))

write_xlsx(
  diretorios,
  path = here("1 Bases Individuais", "1 Identificação", "diretorios_brasileiros.xlsx")
)

# ── 10. Validação padronizada (2 Validação/1 Identificação/) ────────────────
validar_dimensao(diretorios, n_dim = 1)



