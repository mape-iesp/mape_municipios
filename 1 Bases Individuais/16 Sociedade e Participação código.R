# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(openxlsx)
library(writexl)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(readxl)

# ── 2. Configuração do projeto no Google Cloud ────────────────────────────────
set_billing_id("dados-importacao") ## Substituir pelo seu projeto

# ── 3. Mapa OSC Data ───────────────────────────────────────────────────────────────

here("1 Bases Individuais", "16 Sociedade e Participação", "20260522_MOSC_baseDivulgacao.csv")

mapa_osc <- read_delim(
  here("1 Bases Individuais", "16 Sociedade e Participação", "20260522_MOSC_baseDivulgacao.csv"),
  delim = ";"
)

names(mapa_osc)
summary(mapa_osc)
str(mapa_osc)

table(mapa_osc$situacao_cadastral)

table(mapa_osc$Area_Desenvolvimento_e_defesa_de_direitos_e_interesses)


# 4. Expandir o banco 

library(data.table)
library(lubridate)

setDT(mapa_osc)

ano_referencia <- 2026  # ano da extração — ajuste se necessário

# ── 1. Base org-nível, com ano de início/fim de atividade ──────────────────
# [UNICO] corrigido o apontamento de validacao/relatorio_validacao_mapa_osc.docx
# (Felipe, 07/11/2026): 150 OSCs do arquivo bruto não têm município (cd_municipio
# vazio ou 0) e viravam 54 linhas com cod_municipio vazio/0 na base final,
# somando 3.574 organizações-ano que não pertencem a nenhum município real.
# Descartadas aqui, na origem.
osc_base <- mapa_osc[
  !is.na(dt_fundacao_osc) & !is.na(cd_municipio) & cd_municipio != 0,
  .(cnpj,
    cod_municipio = cd_municipio,
    ano_fundacao = year(dt_fundacao_osc),
    ano_fechamento,
    Area_Assistencia_social, Area_Associacoes_patronais_e_profissionais,
    Area_Cultura_e_recreacao, Area_Desenvolvimento_e_defesa_de_direitos_e_interesses,
    Area_Educacao_e_pesquisa, Area_Outras_atividades_associativas,
    Area_Religiao, Area_Saude)
]

# se não fechou, considera ativa até o ano de referência
osc_base[, ano_fim := fifelse(is.na(ano_fechamento), ano_referencia, ano_fechamento)]

# descarta fundação posterior ao fechamento (inconsistência de dados)
osc_base <- osc_base[ano_fundacao <= ano_fim]

# zera NA nas colunas de área (organização sem área classificada)
area_cols <- c("Area_Assistencia_social", "Area_Associacoes_patronais_e_profissionais",
               "Area_Cultura_e_recreacao", "Area_Desenvolvimento_e_defesa_de_direitos_e_interesses",
               "Area_Educacao_e_pesquisa", "Area_Outras_atividades_associativas",
               "Area_Religiao", "Area_Saude")
osc_base[, (area_cols) := lapply(.SD, function(x) fcoalesce(x, 0)), .SDcols = area_cols]

# ── 2. Empilha organização-ano ──────────────────────────────────────────────
painel_osc <- osc_base[
  , .(ano = seq(ano_fundacao, ano_fim)),
  by = .(cnpj, cod_municipio, Area_Assistencia_social, Area_Associacoes_patronais_e_profissionais,
         Area_Cultura_e_recreacao, Area_Desenvolvimento_e_defesa_de_direitos_e_interesses,
         Area_Educacao_e_pesquisa, Area_Outras_atividades_associativas,
         Area_Religiao, Area_Saude)
]

# ── 3a. Quantitativo total de organizações por ano-município ───────────────
quant_total_municipio <- painel_osc[
  , .(total_organizacoes = uniqueN(cnpj)),
  by = .(ano, cod_municipio)
]

# ── 3b. Quantitativo por área, ano-município ────────────────────────────────
painel_long <- melt(
  painel_osc,
  id.vars = c("cnpj", "cod_municipio", "ano"),
  measure.vars = area_cols,
  variable.name = "area",
  value.name = "flag_area"
)

quant_area_municipio <- painel_long[
  flag_area == 1,
  .(qtd_organizacoes = uniqueN(cnpj)),
  by = .(ano, cod_municipio, area)
]

# formato largo (uma coluna por área) para juntar ao mape_municipios
quant_area_municipio_wide <- dcast(
  quant_area_municipio,
  ano + cod_municipio ~ area,
  value.var = "qtd_organizacoes",
  fill = 0
)

# ── 4. Base final: total + por área, município-ano ─────────────────────────
mosc_municipio_ano <- merge(
  quant_total_municipio,
  quant_area_municipio_wide,
  by = c("ano", "cod_municipio"),
  all.x = TRUE
)

summary(mosc_municipio_ano)

osc_ano <- mosc_municipio_ano %>%
  filter(ano >= 1995 & ano <= 2025)

summary(osc_ano)

sociedade_brasileira <- osc_ano

## IVS

# Para carregar o dado direto no R
# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.raca_cor as raca_cor,
    dados.sexo as sexo,
    dados.localizacao as localizacao,
    dados.udh as udh,
    dados.nome_udh as nome_udh,
    dados.ivs as ivs,
    dados.ivs_infraestrutura_urbana as ivs_infraestrutura_urbana,
    dados.ivs_capital_humano as ivs_capital_humano,
    dados.ivs_renda_trabalho as ivs_renda_trabalho,
    dados.idhm as idhm,
    dados.indice_gini as indice_gini
FROM `basedosdados.br_ipea_avs.municipio` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

ivs <- read_sql(query, billing_project_id = get_billing_id())


summary(ivs)

# [UNICO] corrigido o bug apontado em validacao/relatorio_validacao_ivs.docx
# (Felipe, 07/11/2026, Resultado: Não validado): a tabela tem ~27 linhas por
# município-censo (o município como um todo + recortes por raça, sexo,
# localização, e as UDHs — áreas dentro do município); a versão anterior
# calculava a MÉDIA de todas essas linhas como "o" IVS do município, o que
# em 11.130 município-censo diferia do IVS municipal verdadeiro em até 0,21
# (escala de 0 a 1). Corrigido: usa só a linha do município como um todo
# (raca_cor/sexo/localizacao == "total", udh == "geral"); *_dp agora é o
# desvio-padrão entre as UDHs (dispersão interna do município), não entre
# município+recortes+UDHs misturados.
ivs_municipio <- ivs %>%
  filter(raca_cor == "total", sexo == "total", localizacao == "total", udh == "geral")

ivs_udh <- ivs %>%
  filter(raca_cor == "total", sexo == "total", localizacao == "total", udh != "geral")

atlas <- ivs_municipio %>%
  select(ano, id_municipio, ivs_media = ivs,
         ivs_infraestrutura_urbana_media = ivs_infraestrutura_urbana,
         ivs_capital_humano_media = ivs_capital_humano,
         ivs_renda_trabalho_media = ivs_renda_trabalho,
         idhm_media = idhm, gini_media = indice_gini) %>%
  left_join(
    ivs_udh %>%
      group_by(ano, id_municipio) %>%
      summarise(ivs_dp = sd(ivs, na.rm = TRUE),
                ivs_infraestrutura_urbana_dp = sd(ivs_infraestrutura_urbana, na.rm = TRUE),
                ivs_capital_humano_dp = sd(ivs_capital_humano, na.rm = TRUE),
                ivs_renda_trabalho_dp = sd(ivs_renda_trabalho, na.rm = TRUE),
                idhm_dp = sd(idhm, na.rm = TRUE),
                gini_dp = sd(indice_gini, na.rm = TRUE),
                .groups = "drop"),
    by = c("ano", "id_municipio")
  )

str(atlas)

atlas$ano <- as.numeric(atlas$ano)

table(atlas$ano)

painel_atlas <- atlas %>%
  mutate(ano_censo = case_when(
    ano >= 1995 & ano <= 2005 ~ 2000,
    ano >= 2006 & ano <= 2025 ~ 2010,
    TRUE ~ NA_real_
  ))



# 1. padroniza o tipo da chave de município nas duas bases
sociedade_brasileira <- sociedade_brasileira %>%
  mutate(cod_municipio = as.character(as.integer(cod_municipio)))

painel_atlas <- painel_atlas %>%
  mutate(id_municipio = as.character(id_municipio))

# 2. aplica a MESMA lógica de ano_censo na base de OSCs,
#    para poder casar pelo intervalo do censo, não pelo ano exato
sociedade_brasileira <- sociedade_brasileira %>%
  mutate(ano_censo = case_when(
    ano >= 1995 & ano <= 2005 ~ 2000,
    ano >= 2006 & ano <= 2025 ~ 2010,
    TRUE ~ NA_real_
  ))

# 3. join pelo município + ano_censo (não pelo "ano" original do IVS)
#    -> "carrega" o valor do censo para todos os anos do intervalo
painel_final <- sociedade_brasileira %>%
  left_join(
    painel_atlas %>% select(-ano),
    by = c("cod_municipio" = "id_municipio", "ano_censo" = "ano_censo")
  )

# 4. diagnóstico: quantos municípios-ano ficaram sem IVS depois do join?
painel_final %>%
  filter(is.na(ivs_media)) %>%
  count(ano)

sociedade_participacao <- painel_final


# ── Dados Conselhos ───────────────────────────────────────────────────────────────
here("1 Bases Individuais", "MUNIC", "munic_rh_conselhos_2004_2024.csv")
munic_data <- readr::read_csv(
  here("1 Bases Individuais", "MUNIC", "munic_rh_conselhos_2004_2024.csv")
)


names(munic_data)


conselhos_data <- munic_data %>%
  select(-rh_dir_total, -rh_dir_estatutario, -rh_dir_clt,
         -rh_dir_comissionado, -rh_dir_estagiario, -rh_dir_sem_vinculo, -rh_ind_existe, -rh_ind_total)

summary(conselhos_data)

summary(conselhos_data$cons_alim_escolar_existe)

summary(conselhos_data$cons_alim_escolar_corrigido)

# [UNICO] corrigido o bug apontado em validacao/relatorio_validacao_conselhos.docx
# (Felipe, 07/11/2026): a soma com "+" vira NA quando falta qualquer parcela —
# como vários conselhos só passaram a ser perguntados depois de 2004, o total
# saía NA em 100% das linhas de 2004 a 2018; incluía cons_alim_escolar_reuniao_12m
# (mede funcionamento, não existência); e deixava de fora assistência social,
# Fundeb e conselho tutelar. Corrigido: soma todas as colunas cons_*_existe
# (as 25 que existem na base harmonizada, selecionadas dinamicamente, não por
# uma lista fixa) com na.rm = TRUE. NA só quando NENHUM conselho tem resposta
# no município-ano; município-ano com pelo menos 1 resposta conta os demais
# como "não perguntado ainda" = 0 (subestima o total nos anos iniciais, em que
# menos conselhos eram pesquisados — documentado no dicionário).
colunas_existe <- grep("^cons_.*_existe$", names(conselhos_data), value = TRUE)
conselhos_data <- conselhos_data %>%
  mutate(
    total_conselhos = rowSums(across(all_of(colunas_existe)), na.rm = TRUE),
    total_conselhos = if_else(
      rowSums(!is.na(across(all_of(colunas_existe)))) == 0, NA_real_, total_conselhos
    )
  )

summary(conselhos_data$total_conselhos)
conselhos_data %>% group_by(ano) %>% summarise(pct_na = round(100 * mean(is.na(total_conselhos)), 1))


# ── Juntar conselhos à base ────────────────────────────────────────────────────
# Entram todas as variáveis cons_* da MUNIC (2004–2024) e total_conselhos
# (corrigido nesta validação, ver nota acima). O full_join mantém os
# municípios-ano que não têm OSC; nessas linhas, OSC e IVS ficam NA.
conselhos_base <- conselhos_data %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano)) %>%
  select(id_municipio, ano, starts_with("cons_"), total_conselhos)

sociedade_participacao <- sociedade_participacao %>%
  mutate(ano = as.integer(ano)) %>%
  full_join(conselhos_base, by = c("cod_municipio" = "id_municipio", "ano")) %>%
  arrange(cod_municipio, ano)


# ── Exportação ────────────────────────────────────────────────────────


dir.create(here("1 Bases Individuais", "16 Sociedade e Participação"), showWarnings = FALSE)

write_xlsx(
  sociedade_participacao,
  path = here("1 Bases Individuais", "16 Sociedade e Participação", "sociedade_participacao.xlsx")
)

# ── Validação padronizada (2 Validação/16 Sociedade e Participação/) ───────
# Única dimensão cuja coluna de município se chama cod_municipio, não
# id_municipio (ver Relatório de Decisões, Etapas 3 e 4, seção 3.2).
validar_dimensao(sociedade_participacao, n_dim = 16, id = "cod_municipio")
