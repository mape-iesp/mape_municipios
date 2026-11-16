# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(openxlsx)
library(here)
library(censobr)
library(arrow)
library(writexl)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))

# ── 2. Configuração do projeto no Google Cloud ────────────────────────────────
set_billing_id("dados-importacao") ## Substituir pelo seu projeto

# ── 3. População IBGE  ────────────────────────────────

# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.populacao as populacao
FROM `basedosdados.br_ibge_populacao.municipio` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

populacao <- read_sql(query, billing_project_id = get_billing_id())


# ── 4. População Censo  ────────────────────────────────


# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.grupo_idade as grupo_idade,
    dados.sexo as sexo,
    dados.cor_raca as cor_raca,
    dados.populacao as populacao
FROM `basedosdados.br_ibge_censo_2022.populacao_grupo_idade_sexo_raca` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

populacao_censo <- read_sql(query, billing_project_id = get_billing_id())

summary(populacao_censo)

table(populacao_censo$cor_raca)

# Apesar do nome do dataset ("censo_2022"), a tabela
# br_ibge_censo_2022.populacao_grupo_idade_sexo_raca traz também o Censo 2010
# (recontagem/comparabilidade). Confirmar quais anos vêm nela:
table(populacao_censo$ano)

populacao_censo_bd <- populacao_censo %>%
  group_by(ano, id_municipio) %>%
  summarise(
    pessoas_pretas    = sum(populacao[which(cor_raca == 'Preta')],    na.rm = TRUE),
    pessoas_pardas    = sum(populacao[which(cor_raca == 'Parda')],    na.rm = TRUE),
    pessoas_brancas   = sum(populacao[which(cor_raca == 'Branca')],   na.rm = TRUE),
    pessoas_amarelas  = sum(populacao[which(cor_raca == 'Amarela')],  na.rm = TRUE),
    pessoas_indigenas = sum(populacao[which(cor_raca == 'Indígena')], na.rm = TRUE),
    populacao_total   = sum(populacao, na.rm = TRUE),
    .groups = "drop"
  )



#### Base de dados população 

### Censo 2010

#### Pessoa

pop <- read_population(year = 2010,
                       columns = c('abbrev_state', 'code_muni', 'V0606', 'V0010', 'V6400', 'V6527', 'V6529',
                                   'V6121'),
                       add_labels = 'pt',
                       showProgress = FALSE)

## 'V0606' = Cor, 'V0010' = Total, 'V6400' = Superior completo, 'V6527' = Rendimento mensal, 'V6529' = Rendimento Mensal Domicilio



df <- pop |>                                                   # (a)
  collect() |>
  group_by(code_muni) |>                                                                 # (b)
  summarize(educacao_superior = sum(V0010[which(V6400=="Superior completo")]) / sum(V0010), # (c)
            pop = sum(V0010),
            rendimento_mensal_media = mean(V6527, na.rm = TRUE),
            rendimento_mensal_media_domicilio = mean(V6529, na.rm = TRUE),
            pessoas_pretas = sum(V0010[which(V0606=='Preta')]),
            pessoas_pardas = sum(V0010[which(V0606=='Parda')]),
            pessoas_brancas = sum(V0010[which(V0606=='Branca')]),
            pessoas_amarelas = sum(V0010[which(V0606=='Amarela')]),
            pessoas_indigenas = sum(V0010[which(V0606=='Indígena')]),
            evangelicos_pentecostais_pop = sum(V0010[which(V6121 %in% c(300:530))]
            ),
            evangelicos_missao_pop = sum(V0010[which(V6121 %in% c(210:289))]
            ),
            matriz_africana_pop = sum(V0010[which(V6121 %in% c(620, 629, 630, 639,
                                                               640, 641, 649))]),
            catolica_pop = sum(V0010[which(V6121 %in% c(110:120, 199))]),
            espiritas_pop = sum(V0010[which(V6121 %in% c(610:619))])) |>
  mutate(prop_pessoas_negras = (pessoas_pretas + pessoas_pardas)/pop,
         prop_evangelicos_pentecostais = evangelicos_pentecostais_pop/pop,
         prop_pessoas_pretas = pessoas_pretas/pop,
         prop_pessoas_pardas = pessoas_pardas/pop,
         prop_pessoas_brancas = pessoas_brancas/pop,
         prop_pessoas_amarelas = pessoas_amarelas/pop,
         prop_pessoas_indigenas = pessoas_indigenas/pop,
         prop_matriz_africana = matriz_africana_pop/pop,
         prop_evangelicos_missao = evangelicos_missao_pop/pop,
         prop_catolicos = catolica_pop/pop,
         prop_espiritas = espiritas_pop/pop)|>
  collect()

summary(df)

### Censo 2000

### Pop

#### Pessoa

help("data_dictionary")

data_dictionary(year = 2000, dataset = 'population', showProgress = FALSE)

data_dictionary(year = 2010, dataset = 'population', showProgress = FALSE)

## 'V0606' = Cor, 'V0010' = Total, 'V6400' = Superior completo, 'V6527' = Rendimento mensal, 'V6529' = Rendimento Mensal Domicilio


## 'V0102', 'V0103', 'V0408', 'V0300', 'V0432', 'V4614', 'V4615', 'V4090'

pop00 <- read_population(year = 2000,
                         showProgress = FALSE,
                         cache = FALSE)


df00 <- pop00 |>                                                   # (a)
  collect() |>
  group_by(code_muni) |>                                                                 # (b)
  summarize(educacao_superior = sum(P001[which(V0432 >= 7)]) / sum(P001), # (c)
            pop = sum(P001),
            rendimento_mensal_media = mean(P001, na.rm = TRUE),
            rendimento_mensal_media_domicilio = mean(V4615, na.rm = TRUE),
            pessoas_pretas = sum(P001[which(V0408=='2')]),
            pessoas_pardas = sum(P001[which(V0408=='4')]), 
            pessoas_brancas = sum(P001[which(V0408=='1')]),
            pessoas_amarelas = sum(P001[which(V0408=='3')]), 
            pessoas_indigenas = sum(P001[which(V0408=='5')]),
            evangelicos_pentecostais_pop = sum(P001[which(V4090 %in% c(300:530))]),
            evangelicos_missao_pop = sum(P001[which(V4090 %in% c(210:289))]
            ),
            matriz_africana_pop = sum(P001[which(V4090 %in% c(620:649))]),
            catolica_pop = sum(P001[which(V4090 %in% c(110:120, 199))]),
            espiritas_pop = sum(P001[which(V4090 %in% c(590:619))])) |>
  mutate(prop_pessoas_negras = (pessoas_pretas + pessoas_pardas)/pop,
         prop_evangelicos_pentecostais = evangelicos_pentecostais_pop/pop,
         prop_pessoas_pretas = pessoas_pretas/pop,
         prop_pessoas_pardas = pessoas_pardas/pop,
         prop_matriz_africana = matriz_africana_pop/pop,
         prop_evangelicos_missao = evangelicos_missao_pop/pop,
         prop_catolicos = catolica_pop/pop,
         prop_espiritas = espiritas_pop/pop)|>
  collect()

summary(df00)


# ── 5. Comparação Censo 2010: basedosdados vs censobr ─────────────────────────
##     A tabela br_ibge_censo_2022.populacao_grupo_idade_sexo_raca traz o ano
##     de 2010 além de 2022 (ver table(populacao_censo$ano) acima). Comparamos
##     esses valores agregados por município com os obtidos via microdados do
##     censobr (objeto `df`, calculado na seção "Censo 2010" acima) para
##     confirmar que as duas fontes batem antes de decidir qual usar no banco
##     final.

censo_2010_bd <- populacao_censo_bd %>%
  filter(ano == 2010) %>%
  mutate(id_municipio = as.character(id_municipio))

censo_2010_censobr <- df %>%
  select(code_muni, pop, pessoas_pretas, pessoas_pardas, pessoas_brancas,
         pessoas_amarelas, pessoas_indigenas) %>%
  mutate(id_municipio = as.character(code_muni)) %>%
  select(-code_muni) %>%
  rename(populacao_total = pop)

comparacao_2010 <- censo_2010_bd %>%
  select(id_municipio, populacao_total, pessoas_pretas, pessoas_pardas,
         pessoas_brancas, pessoas_amarelas, pessoas_indigenas) %>%
  inner_join(censo_2010_censobr, by = "id_municipio", suffix = c("_bd", "_censobr")) %>%
  mutate(
    dif_populacao_total   = populacao_total_bd   - populacao_total_censobr,
    dif_pessoas_pretas    = pessoas_pretas_bd    - pessoas_pretas_censobr,
    dif_pessoas_pardas    = pessoas_pardas_bd    - pessoas_pardas_censobr,
    dif_pessoas_brancas   = pessoas_brancas_bd   - pessoas_brancas_censobr,
    dif_pessoas_amarelas  = pessoas_amarelas_bd  - pessoas_amarelas_censobr,
    dif_pessoas_indigenas = pessoas_indigenas_bd - pessoas_indigenas_censobr
  )

# número de municípios comparados e magnitude das divergências — checar antes
# de seguir (idealmente as diferenças devem ser próximas de zero)
comparacao_2010 %>%
  summarise(
    n_municipios              = n(),
    n_municipios_dif_pop      = sum(dif_populacao_total != 0, na.rm = TRUE),
    media_dif_populacao_total = mean(abs(dif_populacao_total), na.rm = TRUE),
    max_dif_populacao_total   = max(abs(dif_populacao_total), na.rm = TRUE),
    media_dif_pretas          = mean(abs(dif_pessoas_pretas), na.rm = TRUE),
    media_dif_pardas          = mean(abs(dif_pessoas_pardas), na.rm = TRUE),
    media_dif_brancas         = mean(abs(dif_pessoas_brancas), na.rm = TRUE),
    media_dif_amarelas        = mean(abs(dif_pessoas_amarelas), na.rm = TRUE),
    media_dif_indigenas       = mean(abs(dif_pessoas_indigenas), na.rm = TRUE)
  )

# municípios com maior divergência, para inspeção manual
comparacao_2010 %>%
  arrange(desc(abs(dif_populacao_total))) %>%
  select(id_municipio, populacao_total_bd, populacao_total_censobr, dif_populacao_total) %>%
  head(20)

##     Assumindo que as diferenças acima são pequenas/aceitáveis, o banco
##     final usa a versão da basedosdados para 2010 (mesma fonte/metodologia
##     do Censo 2022), mantendo a comparação acima só como checagem de
##     qualidade. Se as diferenças forem grandes, revisar antes de exportar.


# ── 6. Consolidar dados de censo (2000, 2010, 2022) ───────────────────────────

## As somas vindas do BigQuery voltam como integer64 (pacote bit64), enquanto
## as do censobr voltam como double — convertemos tudo para numeric antes de
## empilhar, senão bind_rows() falha ao combinar tipos diferentes.
contagem_cols <- c("populacao_censo", "pessoas_pretas", "pessoas_pardas",
                    "pessoas_brancas", "pessoas_amarelas", "pessoas_indigenas")

censo_2000_final <- df00 %>%
  select(code_muni, pop, pessoas_pretas, pessoas_pardas, pessoas_brancas,
         pessoas_amarelas, pessoas_indigenas) %>%
  mutate(id_municipio = as.character(code_muni), ano_censo = 2000) %>%
  select(-code_muni) %>%
  rename(populacao_censo = pop) %>%
  mutate(across(all_of(contagem_cols), as.numeric))

censo_2010_final <- censo_2010_bd %>%
  mutate(ano_censo = 2010) %>%
  select(id_municipio, ano_censo, populacao_censo = populacao_total,
         pessoas_pretas, pessoas_pardas, pessoas_brancas, pessoas_amarelas, pessoas_indigenas) %>%
  mutate(across(all_of(contagem_cols), as.numeric))

censo_2022_final <- populacao_censo_bd %>%
  filter(ano == 2022) %>%
  mutate(id_municipio = as.character(id_municipio), ano_censo = 2022) %>%
  select(id_municipio, ano_censo, populacao_censo = populacao_total,
         pessoas_pretas, pessoas_pardas, pessoas_brancas, pessoas_amarelas, pessoas_indigenas) %>%
  mutate(across(all_of(contagem_cols), as.numeric))

censo_consolidado <- bind_rows(censo_2000_final, censo_2010_final, censo_2022_final)

table(censo_consolidado$ano_censo) # conferir cobertura municipal por ano de censo


# ── 7. Painel município-ano: população estimada + censo imputado ─────────────
# CORREÇÃO (validação de 13/11/2026 — ver v2/2 Validação/2 População/
# relatorio_validacao_2_populacao.docx): a versão anterior usava
# `populacao %>% select(...)` direto como painel, sem passar por um esqueleto
# município x ano completo. Isso escondia dois problemas: (a) 1 linha com
# id_municipio em branco (um agregado que não é município, filtrado abaixo) e
# (b) município-ano ausentes ficavam simplesmente fora da base, em vez de
# aparecerem como NA — ao contrário do que o Protocolo recomenda ("painel
# primeiro, join depois").

populacao <- populacao %>%
  filter(!is.na(id_municipio)) %>%
  mutate(id_municipio = as.character(id_municipio)) %>%
  rename(populacao_estimativa = populacao)

range(populacao$ano) # conferir cobertura real de anos (esperado: 1991-2025)

## 7.1 Esqueleto do painel: Diretórios (5.571 municípios atuais) x 1991:2025 ──
## Municípios criados depois de 1991 ficam NA nos anos anteriores à criação —
## isso é esperado (o município não existia) e não é dado faltante por erro.
## Boa Esperança do Norte (MT, id_municipio 5101837, instalado em 2025) não
## tem nenhuma linha na tabela de estimativas do IBGE ainda em nenhum ano —
## fica NA em toda a série, mas continua no painel (flagging over silencing).
diretorios_query <- "
SELECT DISTINCT
    id_municipio,
    nome     AS id_municipio_nome,
    sigla_uf
FROM `basedosdados.br_bd_diretorios_brasil.municipio`
"

diretorios_pop <- read_sql(diretorios_query, billing_project_id = get_billing_id()) %>%
  mutate(id_municipio = as.character(id_municipio))

n_distinct(diretorios_pop$id_municipio)  # esperado: 5571

anos_painel <- 1991:2025

esqueleto_pop <- diretorios_pop %>%
  distinct(id_municipio, id_municipio_nome, sigla_uf) %>%
  crossing(ano = anos_painel)

## 7.2 Join da população estimada sobre o esqueleto ───────────────────────────
## id_municipio_nome/sigla_uf_nome vêm do esqueleto (Diretórios), não da
## tabela de estimativas, para existirem mesmo nos município-ano sem
## estimativa (ex.: antes da criação do município).
sigla_uf_nomes <- populacao %>% distinct(sigla_uf, sigla_uf_nome)

painel_pop <- esqueleto_pop %>%
  left_join(sigla_uf_nomes, by = "sigla_uf") %>%
  left_join(
    populacao %>% select(id_municipio, ano, populacao_estimativa),
    by = c("id_municipio", "ano")
  ) %>%
  relocate(ano, id_municipio, id_municipio_nome, sigla_uf, sigla_uf_nome)

## Janela de vigência de cada censo (valor constante do censo dentro da janela):
##   Censo 2000 -> anos 1995 a 2005
##   Censo 2010 -> anos 2006 a 2015
##   Censo 2022 -> anos 2016 a 2025
##   Fora dessas janelas (ex.: 1991-1994) fica NA, sem imputação (conforme
##   decidido — não há extrapolação do Censo 2000 para trás)
painel_pop <- painel_pop %>%
  mutate(ano_censo = case_when(
    ano >= 1995 & ano <= 2005 ~ 2000,
    ano >= 2006 & ano <= 2015 ~ 2010,
    ano >= 2016 & ano <= 2025 ~ 2022,
    TRUE ~ NA_real_
  ))

populacao_brasileira <- painel_pop %>%
  left_join(censo_consolidado, by = c("id_municipio", "ano_censo"))

str(populacao_brasileira)

## Checagens finais
nrow(populacao_brasileira) == n_distinct(diretorios_pop$id_municipio) * length(anos_painel)  # esperado: TRUE (194.985)
anyDuplicated(populacao_brasileira[c("id_municipio", "ano")])                                # esperado: 0
sum(is.na(populacao_brasileira$id_municipio))                                                # esperado: 0
all(nchar(populacao_brasileira$id_municipio) == 7)                                           # esperado: TRUE
range(populacao_brasileira$ano)
populacao_brasileira %>% filter(is.na(ano_censo)) %>% distinct(ano) # esperado: só 1991-1994, se existirem
populacao_brasileira %>%
  filter(!is.na(ano_censo)) %>%
  group_by(ano) %>%
  summarise(pct_censo_preenchido = mean(!is.na(populacao_censo))) %>%
  print(n = Inf)

# Município-ano sem estimativa de população (municípios ainda não criados,
# mais Boa Esperança do Norte em toda a série)
populacao_brasileira %>%
  filter(is.na(populacao_estimativa)) %>%
  count(ano) %>%
  print(n = Inf)


# ── 8. Exportar .xlsx ──────────────────────────────────────────────────────────
# writexl::write_xlsx() em vez de openxlsx::write.xlsx(): ver nota em
# "4 Corrupção e Transparência código.R" sobre a referência interna quebrada
# que o openxlsx (4.2.8.1) grava com write.xlsx().
dir.create(here("1 Bases Individuais", "2 População"), showWarnings = FALSE)

write_xlsx(
  populacao_brasileira,
  path = here("1 Bases Individuais", "2 População", "populacao_brasileira.xlsx")
)

# ── 9. Validação padronizada (2 Validação/2 População/) ─────────────────────
validar_dimensao(populacao_brasileira, n_dim = 2)







