# ══════════════════════════════════════════════════════════════════════════
# mape_municipios — Dimensão Habitação e Zoneamento
# MUNIC/IBGE (política habitacional e de zoneamento) + Minha Casa, Minha Vida
# (MCMV Subsidiado/OGU e MCMV Financiado/FGTS)
# Grão final: id_municipio (IBGE, 7 dígitos) x ano
# ══════════════════════════════════════════════════════════════════════════
#
# UNIFICADO em 13/11/2026 (validação): este arquivo agora junta duas partes
# que antes rodavam separadas:
#   - a consulta à MUNIC/IBGE e o painel de habitação (já existentes aqui);
#   - o pipeline do MCMV ("11 Habitação e Zoneamento/mcmv/1_script/mcmv.Rmd",
#     de Tomás Paixão Borges, atualizado em 05/11/2026 com os dados mais
#     recentes do Ministério das Cidades — mcmv_subsidiado_202606302.zip e
#     mcmv_financ_sintetico_20260724_v2.zip, cobrindo até jun/jul de 2026).
# Duas correções nesta validação, marcadas com [UNICO] nos próprios trechos:
#   - o script de MUNIC/Habitação nunca exportava nada (terminava em
#     summary(painel_habitacao), sem write — Etapa 4 do protocolo estava
#     faltando); a Etapa 7 (Exportação), abaixo, é nova;
#   - o pipeline do MCMV deflacionava os valores monetários pelo IPCA
#     ("ipca(.x, ..., '12/2023')", nas seções 3 e 4 do .Rmd original) — o que
#     contraria a regra do projeto de manter todo valor em R$ nominal
#     (corrente), nunca deflacionado. As duas chamadas a ipca() foram
#     removidas; os valores ficam em reais correntes de cada contrato.
# O .Rmd original (e o .csv que ele gerava, com os valores deflacionados)
# fica como está, para registro do pipeline isolado antes da correção — o
# arquivo final da dimensão é o exportado na Etapa 7 abaixo.

# ── 1. Pacotes ────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(openxlsx)
library(writexl)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(readxl)
library(data.table)
library(stringr)
library(ggplot2)

# ── 2. Configuração do projeto no Google Cloud ────────────────────────────
set_billing_id("dados-importacao") ## Substituir pelo seu projeto

PASTA_HABITACAO <- here("1 Bases Individuais", "11 Habitação e Zoneamento")


# ══════════════════════════════════════════════════════════════════════════
# 3. MUNIC/IBGE — POLÍTICA HABITACIONAL E DE ZONEAMENTO
# ══════════════════════════════════════════════════════════════════════════

# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.orgao_especifico_implementacao_politica_habitacional_existencia as orgao_especifico_implementacao_politica_habitacional_existencia,
    dados.caracterizacao_orgao_gestor_planejamento_urbano as caracterizacao_orgao_gestor_planejamento_urbano,
    dados.orgao_administracao_indireta_habitacao_existencia as orgao_administracao_indireta_habitacao_existencia,
    dados.plano_diretor_existencia as plano_diretor_existencia,
    dados.plano_diretor_ano as plano_diretor_ano,
    dados.plano_diretor_foi_revisto as plano_diretor_foi_revisto,
    dados.plano_diretor_ano_ultima_atualizacao as plano_diretor_ano_ultima_atualizacao,
    dados.plano_diretor_numero_lei as plano_diretor_numero_lei,
    dados.plano_diretor_elaborando_revendo as plano_diretor_elaborando_revendo,
    dados.plano_diretor_revendo as plano_diretor_revendo,
    dados.plano_diretor_elaborando as plano_diretor_elaborando,
    dados.plano_diretor_instrumentos_participacao_coordenacao_compartilhada as plano_diretor_instrumentos_participacao_coordenacao_compartilhada,
    dados.plano_diretor_instrumentos_participacao_conselho_politica_urbana as plano_diretor_instrumentos_participacao_conselho_politica_urbana,
    dados.plano_diretor_instrumentos_participacao_conferencia_cidade as plano_diretor_instrumentos_participacao_conferencia_cidade,
    dados.plano_diretor_instrumentos_participacao_discussao_segmentos_especificos as plano_diretor_instrumentos_participacao_discussao_segmentos_especificos,
    dados.plano_diretor_instrumentos_participacao_discussao_tematica as plano_diretor_instrumentos_participacao_discussao_tematica,
    dados.plano_diretor_instrumentos_participacao_discussao_bairros_distritos_setores as plano_diretor_instrumentos_participacao_discussao_bairros_distritos_setores,
    dados.plano_diretor_instrumentos_participacao_atividades_capacitacao as plano_diretor_instrumentos_participacao_atividades_capacitacao,
    dados.plano_diretor_instrumentos_participacao_outros as plano_diretor_instrumentos_participacao_outros,
    dados.plano_diretor_instrumentos_participacao_nenhum as plano_diretor_instrumentos_participacao_nenhum,
    dados.plano_diretor_orientacao_normas_tecnicas_acessibilidade as plano_diretor_orientacao_normas_tecnicas_acessibilidade,
    dados.plano_diretor_contempla_instrumentos_parcelamento_solo as plano_diretor_contempla_instrumentos_parcelamento_solo,
    dados.plano_diretor_contempla_instrumentos_zoneamento_ocupacao_solo as plano_diretor_contempla_instrumentos_zoneamento_ocupacao_solo,
    dados.plano_diretor_contempla_instrumentos_codigo_obras as plano_diretor_contempla_instrumentos_codigo_obras,
    dados.plano_diretor_contempla_instrumentos_contribuicao_melhorias as plano_diretor_contempla_instrumentos_contribuicao_melhorias,
    dados.plano_diretor_contempla_instrumentos_operacao_urbana_consorciada as plano_diretor_contempla_instrumentos_operacao_urbana_consorciada,
    dados.plano_diretor_contempla_instrumentos_lei_solo_criado as plano_diretor_contempla_instrumentos_lei_solo_criado,
    dados.plano_diretor_contempla_instrumentos_estudo_impacto_vizinhanca as plano_diretor_contempla_instrumentos_estudo_impacto_vizinhanca,
    dados.plano_diretor_contempla_instrumentos_codigo_posturas as plano_diretor_contempla_instrumentos_codigo_posturas,
    dados.plano_diretor_contempla_instrumentos_zona_area_interesse_social as plano_diretor_contempla_instrumentos_zona_area_interesse_social,
    dados.plano_diretor_contempla_instrumentos_zona_area_especial as plano_diretor_contempla_instrumentos_zona_area_especial,
    dados.plano_diretor_contempla_instrumentos_solo_criado as plano_diretor_contempla_instrumentos_solo_criado,
    dados.plano_municipal_habitacao_existencia as plano_municipal_habitacao_existencia,
    dados.conferencia_municipal_habitacao_realizada_ultimos_4anos as conferencia_municipal_habitacao_realizada_ultimos_4anos,
    dados.conferencia_municipal_habitacao_elementos_elaboracao as conferencia_municipal_habitacao_elementos_elaboracao,
    dados.demais_instrumentos as demais_instrumentos,
    dados.demais_instrumentos_zona_area_interesse_social_existencia as demais_instrumentos_zona_area_interesse_social_existencia,
    dados.demais_instrumentos_zona_area_interesse_social_ano_lei as demais_instrumentos_zona_area_interesse_social_ano_lei,
    dados.demais_instrumentos_zona_area_interesse_especial_existencia as demais_instrumentos_zona_area_interesse_especial_existencia,
    dados.demais_instrumentos_zona_area_interesse_especial_ano_lei as demais_instrumentos_zona_area_interesse_especial_ano_lei,
    dados.demais_instrumentos_lei_iptu_progressivo_existencia as demais_instrumentos_lei_iptu_progressivo_existencia,
    dados.demais_instrumentos_lei_iptu_progressivo_ano_lei as demais_instrumentos_lei_iptu_progressivo_ano_lei,
    dados.demais_instrumentos_zoneamento_ambiental_existencia as demais_instrumentos_zoneamento_ambiental_existencia,
    dados.demais_instrumentos_zoneamento_ambiental_ano_lei as demais_instrumentos_zoneamento_ambiental_ano_lei,
    dados.demais_instrumentos_regularizacao_fundiaria_existencia as demais_instrumentos_regularizacao_fundiaria_existencia,
    dados.demais_instrumentos_regularizacao_fundiaria_ano_lei as demais_instrumentos_regularizacao_fundiaria_ano_lei,
    dados.lei_organica_ano_lei as lei_organica_ano_lei,
    dados.lei_diretrizes_orcamentarias as lei_diretrizes_orcamentarias,
    dados.lei_orcamento_anual as lei_orcamento_anual,
    dados.utiliza_instrumentos_politica_urbana_estatuto_cidades as utiliza_instrumentos_politica_urbana_estatuto_cidades,
    dados.conselho_municipal_politica_urbana_existencia as conselho_municipal_politica_urbana_existencia,
    dados.conselho_municipal_politica_urbana_lei_criacao as conselho_municipal_politica_urbana_lei_criacao,
    dados.conselho_municipal_politica_urbana_ano_criacao as conselho_municipal_politica_urbana_ano_criacao,
    dados.conselho_municipal_politica_urbana_situacao as conselho_municipal_politica_urbana_situacao,
    dados.conselho_municipal_politica_urbana_periodicidade_reunioes as conselho_municipal_politica_urbana_periodicidade_reunioes,
    dados.conselho_municipal_politica_urbana_reuniao_ultimos_12meses as conselho_municipal_politica_urbana_reuniao_ultimos_12meses,
    dados.conselho_municipal_politica_urbana_reunioes_ultimos_12meses as conselho_municipal_politica_urbana_reunioes_ultimos_12meses,
    dados.conselho_municipal_politica_urbana_carater_consultivo as conselho_municipal_politica_urbana_carater_consultivo,
    dados.conselho_municipal_politica_urbana_carater_deliberativo as conselho_municipal_politica_urbana_carater_deliberativo,
    dados.conselho_municipal_politica_urbana_carater_normativo as conselho_municipal_politica_urbana_carater_normativo,
    dados.conselho_municipal_politica_urbana_carater_fiscalizador as conselho_municipal_politica_urbana_carater_fiscalizador,
    dados.conselho_municipal_politica_urbana_conselheiros as conselho_municipal_politica_urbana_conselheiros,
    dados.fundo_municipal_habitacao_existencia as fundo_municipal_habitacao_existencia,
    dados.fundo_municipal_habitacao_financiou_acoes_ultimos_12meses as fundo_municipal_habitacao_financiou_acoes_ultimos_12meses,
    dados.loteamentos_irregulares_existencia as loteamentos_irregulares_existencia,
    dados.loteamentos_irregulares_criterios_sem_aprovacao_previa as loteamentos_irregulares_criterios_sem_aprovacao_previa,
    dados.programas as programas,
    dados.programas_construcao_unidades_habitacionais as programas_construcao_unidades_habitacionais,
    dados.programas_construcao_unidades_habitacionais_iniciativa_prefeitura as programas_construcao_unidades_habitacionais_iniciativa_prefeitura,
    dados.programas_construcao_unidades_habitacionais_convenio as programas_construcao_unidades_habitacionais_convenio,
    dados.programas_construcao_unidades_habitacionais_convenio_governo_federal as programas_construcao_unidades_habitacionais_convenio_governo_federal,
    dados.programas_construcao_unidades_habitacionais_convenio_governo_estadual as programas_construcao_unidades_habitacionais_convenio_governo_estadual,
    dados.programas_construcao_unidades_habitacionais_convenio_outro_municipio as programas_construcao_unidades_habitacionais_convenio_outro_municipio,
    dados.programas_construcao_unidades_habitacionais_convenio_iniciativa_privada as programas_construcao_unidades_habitacionais_convenio_iniciativa_privada,
    dados.programas_aquisicao_unidades_habitacionais as programas_aquisicao_unidades_habitacionais,
    dados.programas_aquisicao_unidades_habitacionais_iniciativa_prefeitura as programas_aquisicao_unidades_habitacionais_iniciativa_prefeitura,
    dados.programas_aquisicao_unidades_habitacionais_convenio_governo_federal as programas_aquisicao_unidades_habitacionais_convenio_governo_federal,
    dados.programas_aquisicao_unidades_habitacionais_convenio_governo_estadual as programas_aquisicao_unidades_habitacionais_convenio_governo_estadual,
    dados.programas_aquisicao_unidades_habitacionais_convenio_outro_municipio as programas_aquisicao_unidades_habitacionais_convenio_outro_municipio,
    dados.programas_aquisicao_unidades_habitacionais_convenio_iniciativa_privada as programas_aquisicao_unidades_habitacionais_convenio_iniciativa_privada,
    dados.programas_regularizacao_fundiaria as programas_regularizacao_fundiaria,
    dados.programas_regularizacao_fundiaria_iniciativa_prefeitura as programas_regularizacao_fundiaria_iniciativa_prefeitura,
    dados.programas_regularizacao_fundiaria_convenio as programas_regularizacao_fundiaria_convenio,
    dados.programas_regularizacao_fundiaria_convenio_governo_federal as programas_regularizacao_fundiaria_convenio_governo_federal,
    dados.programas_regularizacao_fundiaria_convenio_governo_estadual as programas_regularizacao_fundiaria_convenio_governo_estadual,
    dados.programas_regularizacao_fundiaria_convenio_outro_municipio as programas_regularizacao_fundiaria_convenio_outro_municipio,
    dados.programas_regularizacao_fundiaria_convenio_iniciativa_privada as programas_regularizacao_fundiaria_convenio_iniciativa_privada,
    dados.programas_regularizacao_fundiaria_areas_beneficiadas_urbana as programas_regularizacao_fundiaria_areas_beneficiadas_urbana,
    dados.programas_regularizacao_fundiaria_areas_beneficiadas_rural as programas_regularizacao_fundiaria_areas_beneficiadas_rural,
    dados.emitiu_licencas_novos_loteamentos as emitiu_licencas_novos_loteamentos,
    dados.emitiu_licencas_construcao as emitiu_licencas_construcao,
    dados.emitiu_alvaras_habitacao as emitiu_alvaras_habitacao,
    dados.emitiu_nada as emitiu_nada
FROM `basedosdados.br_ibge_munic.habitacao` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"


habitacao <- read_sql(query, billing_project_id = get_billing_id())

names(habitacao)

table(habitacao$ano)

table(habitacao$ano, habitacao$caracterizacao_orgao_gestor_planejamento_urbano, useNA = "always")

table(habitacao$caracterizacao_orgao_gestor_planejamento_urbano)

table(habitacao$plano_diretor_ano)

table(habitacao$programas_regularizacao_fundiaria, useNA = "always")

table(habitacao$programas_regularizacao_fundiaria, habitacao$ano, useNA = "always")

table(habitacao$programas_aquisicao_unidades_habitacionais, habitacao$ano, useNA = "always")

table(habitacao$programas_regularizacao_fundiaria, habitacao$ano, useNA = "always")


# ============================================================================
# HABITAÇÃO — Painel ano x id_municipio | mape_municipios
# ----------------------------------------------------------------------------
# Input esperado: objeto `habitacao` já carregado (ver consulta acima, via
# basedosdados/bigrquery — base br_ibge_munic.habitacao).
#
# ÍNDICE
#   1. Setup
#   2. Esqueleto do painel (id_municipio x ano)
#   3. Diagnóstico de cobertura por variável (pergunta aplicada em quais anos)
#   4. Variáveis "existência + ano de aprovação" -> vigência retroativa
#   5. Variáveis binárias Sim/Não sem ano -> persistência entre ondas (LOCF)
#   6. Padronização de "órgão gestor" (categórica multi-nível)
#   7. Consolidação do painel final
#   8. Validação / QA
# ============================================================================

setDT(habitacao)

# ano chega como integer64 (bit64) do BigQuery via bq_table_download().
# Convertendo para integer aqui, uma única vez, evita choques de tipo em
# TODAS as junções por "ano" no resto do script (ex.: o operador ":" não
# tem método para integer64 e acaba coagindo silenciosamente para double
# em outro ponto, gerando tipos inconsistentes entre painel e habitacao)
habitacao[, ano := as.integer(ano)]


# ----------------------------------------------------------------------------
# 1. Setup
# ----------------------------------------------------------------------------

id_vars <- c("ano", "sigla_uf", "id_municipio")

anos_pesquisa <- sort(unique(habitacao$ano))            # anos em que a MUNIC foi a campo
anos_painel   <- min(anos_pesquisa):max(anos_pesquisa)   # painel anual completo


# ----------------------------------------------------------------------------
# 2. Esqueleto do painel
# ----------------------------------------------------------------------------

municipios <- unique(habitacao[, .(id_municipio, sigla_uf)])

painel_habitacao <- CJ(id_municipio = municipios$id_municipio, ano = anos_painel)
painel_habitacao[municipios, on = "id_municipio", sigla_uf := i.sigla_uf]
setcolorder(painel_habitacao, c("id_municipio", "sigla_uf", "ano"))


# ----------------------------------------------------------------------------
# 3. Diagnóstico de cobertura por variável
# ----------------------------------------------------------------------------
# Objetivo: para cada variável, identificar em quais anos a pergunta foi
# efetivamente aplicada (não 100% NA) vs. anos em que aquele bloco temático
# não entrou na edição da MUNIC daquele ano. Evita confundir "não perguntado"
# com "perguntado mas não respondido" (item non-response genuíno, via
# Recusa/Não informou/Ignorado).

vars_conteudo <- setdiff(names(habitacao), c(id_vars, "sigla_uf_nome", "id_municipio_nome"))

cobertura <- habitacao[, lapply(.SD, function(x) mean(!is.na(x))),
                       by = ano, .SDcols = vars_conteudo]

cobertura_long <- melt(cobertura, id.vars = "ano",
                       variable.name = "variavel", value.name = "pct_nao_na")
cobertura_long[, aplicada_no_ano := pct_nao_na > 0]

# tabela-resumo: em quais anos cada variável foi de fato aplicada
resumo_cobertura <- cobertura_long[, .(
  anos_aplicados   = paste(ano[aplicada_no_ano], collapse = ", "),
  n_anos_aplicados = sum(aplicada_no_ano)
), by = variavel]

# Ex. de conferência pontual (o achado que motivou este script):
resumo_cobertura[variavel %in% c("programas_aquisicao_unidades_habitacionais",
                                 "programas_regularizacao_fundiaria")]

# heatmap diagnóstico — útil para inspecionar visualmente antes de decidir
# o tratamento de cada variável nova que entrar no painel
ggplot(cobertura_long[variavel %in% c("programas_aquisicao_unidades_habitacionais",
                                      "programas_regularizacao_fundiaria",
                                      "plano_diretor_existencia",
                                      "caracterizacao_orgao_gestor_planejamento_urbano")],
       aes(x = factor(ano), y = variavel, fill = pct_nao_na)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "grey90", high = "steelblue", name = "% não-NA") +
  labs(x = "Ano", y = NULL,
       title = "Cobertura por variável e ano — Habitação (MUNIC)") +
  theme_minimal()


# ----------------------------------------------------------------------------
# 4. Variáveis "existência + ano de aprovação" -> vigência retroativa
# ----------------------------------------------------------------------------
# Ex.: plano_diretor_existencia + plano_diretor_ano
#      demais_instrumentos_*_existencia + demais_instrumentos_*_ano_lei
#      conselho_municipal_politica_urbana_existencia + ..._ano_criacao
# Regra: se aprovado em Y, vale 1 de Y até o fim da série (mesmo antes da
# onda em que foi captado). Se nunca teve (Sim nunca observado, mas Não
# observado em alguma onda), vale 0 a partir da primeira onda observada.
# Antes da primeira onda observada, permanece NA (não há base para inferir).

sufixos_ano <- c("_ano_lei", "_ano_criacao", "_ano")

detectar_pares_vigencia <- function(nomes) {
  pares <- rbindlist(lapply(sufixos_ano, function(sufixo) {
    cand  <- nomes[str_ends(nomes, sufixo)]
    base  <- str_remove(cand, paste0(sufixo, "$"))
    exist <- paste0(base, "_existencia")
    data.table(var_ano = cand, var_existencia = exist)[exist %in% nomes]
  }))
  unique(pares)
}

pares_vigencia <- detectar_pares_vigencia(names(habitacao))
# plano_diretor_ano/plano_diretor_existencia é tratado à parte logo abaixo
# (nome de saída "plano_diretor_vigente"); removido daqui para não gerar
# "plano_diretor_existencia_vigente" como coluna duplicada
pares_vigencia <- pares_vigencia[var_existencia != "plano_diretor_existencia"]
# CONFERIR MANUALMENTE antes de rodar em massa — variáveis como
# plano_diretor_ano_ultima_atualizacao não devem aparecer aqui (não têm
# par "_existencia" correspondente direto)
print(pares_vigencia)

calcular_vigencia <- function(dt, var_existencia, var_ano, valores_sim = "Sim") {
  # as.numeric() força double em ano/ano_lei — fifelse() exige que os dois
  # ramos (yes/no) tenham a mesma classe, e colunas de ano vindas do BigQuery
  # costumam chegar como integer, o que quebra contra NA_real_ (double)
  tmp <- dt[, .(id_municipio, ano = as.numeric(ano),
                existencia = get(var_existencia),
                ano_lei    = as.numeric(get(var_ano)))]

  tmp[, .(
    ano_vigencia      = suppressWarnings(min(ano_lei, na.rm = TRUE)),
    teve_sim          = any(existencia %in% valores_sim, na.rm = TRUE),
    teve_nao          = any(!existencia %in% valores_sim & !is.na(existencia)),
    primeira_onda_obs = suppressWarnings(min(ano[!is.na(existencia)], na.rm = TRUE))
  ), by = id_municipio
  ][, `:=`(
    ano_vigencia      = fifelse(is.finite(ano_vigencia), ano_vigencia, NA_real_),
    primeira_onda_obs = fifelse(is.finite(primeira_onda_obs), primeira_onda_obs, NA_real_)
  )]
}

aplicar_vigencia <- function(painel, dt_origem, var_existencia, var_ano, nome_saida) {
  resumo <- calcular_vigencia(dt_origem, var_existencia, var_ano)
  painel[resumo, on = "id_municipio", `:=`(
    .v = i.ano_vigencia, .s = i.teve_sim, .n = i.teve_nao, .p = i.primeira_onda_obs
  )]
  painel[, (nome_saida) := fifelse(
    !is.na(.v) & ano >= .v, 1L,
    fifelse(.n & !.s & ano >= .p, 0L, NA_integer_)
  )]
  painel[, c(".v", ".s", ".n", ".p") := NULL]
  painel
}

# plano_diretor tratado à parte por clareza de nome de saída
painel_habitacao <- aplicar_vigencia(painel_habitacao, habitacao,
                                     "plano_diretor_existencia", "plano_diretor_ano",
                                     "plano_diretor_vigente")

for (i in seq_len(nrow(pares_vigencia))) {
  nome_saida <- paste0(pares_vigencia$var_existencia[i], "_vigente")
  painel_habitacao <- aplicar_vigencia(painel_habitacao, habitacao,
                                       pares_vigencia$var_existencia[i],
                                       pares_vigencia$var_ano[i],
                                       nome_saida)
}


# ----------------------------------------------------------------------------
# 5. Variáveis binárias Sim/Não sem ano -> persistência entre ondas (LOCF)
# ----------------------------------------------------------------------------
# Caso de programas_aquisicao_unidades_habitacionais e
# programas_regularizacao_fundiaria: existência de um programa na onda em
# que foi perguntado. Entre ondas, assume-se que o estado persiste até
# evidência em contrário — mesma lógica já usada para conselho de segurança
# em seguranca_brasileira.
#
# IMPORTANTE: essa suposição é mais forte para programas (que podem começar
# ou parar sem serem captados por uma onda) do que para leis/planos vigentes.
# Por isso mantemos DUAS versões de cada variável:
#   - "_obs"        : só o que foi de fato observado naquela onda (NA no resto)
#   - "_persistido"  : versão com LOCF aplicado
# Use "_obs" para análises que exigem precisão temporal; "_persistido" para
# preencher lacunas em modelos que toleram essa suposição.

niveis_sim_nao <- c("Sim", "Não", "Não aplicável", "Recusa", "Não informou", "Ignorado")

vars_ja_tratadas <- c(pares_vigencia$var_existencia, "plano_diretor_existencia")

detectar_binarias_sim_nao <- function(dt, excluir) {
  candidatos <- setdiff(names(dt), excluir)
  candidatos[sapply(dt[, ..candidatos], function(x) {
    is.character(x) && length(na.omit(unique(x))) > 0 &&
      all(na.omit(unique(x)) %in% niveis_sim_nao)
  })]
}

vars_binarias <- detectar_binarias_sim_nao(
  habitacao,
  excluir = c(id_vars, "sigla_uf_nome", "id_municipio_nome",
              "caracterizacao_orgao_gestor_planejamento_urbano",
              vars_ja_tratadas)
)
print(vars_binarias)

# "Não aplicável" tratado como NA — ambíguo entre "não se aplica ao
# município" e um artefato de mudança de questionário entre ondas.
# Decisão documentada aqui e a repetir no METADADOS; revisar caso a caso
# se alguma análise específica exigir distinguir isso de item non-response.
recodificar_sim_nao <- function(x) {
  fifelse(x == "Sim", 1L,
          fifelse(x == "Não", 0L, NA_integer_))
}

aplicar_persistencia_binaria <- function(painel, dt_origem, var) {
  obs <- dt_origem[, .(id_municipio, ano, valor = recodificar_sim_nao(get(var)))]
  obs <- obs[!is.na(valor)]

  var_obs  <- paste0(var, "_obs")
  var_pers <- paste0(var, "_persistido")

  painel[obs, on = c("id_municipio", "ano"), (var_obs) := i.valor]
  setorder(painel, id_municipio, ano)
  painel[, (var_pers) := nafill(get(var_obs), type = "locf"), by = id_municipio]
  painel
}

for (v in vars_binarias) {
  painel_habitacao <- aplicar_persistencia_binaria(painel_habitacao, habitacao, v)
}


# ----------------------------------------------------------------------------
# 6. Padronização de "órgão gestor" (categórica multi-nível)
# ----------------------------------------------------------------------------
# Múltiplas grafias entre ondas para a mesma categoria (variação de espaço,
# capitalização e redação do IBGE). Harmonizar antes de qualquer análise.

habitacao[, orgao_gestor_raw := str_squish(caracterizacao_orgao_gestor_planejamento_urbano)]

# CONFERIR MANUALMENTE antes de aplicar o dicionário — garanta que nada
# ficou de fora da lista abaixo
print(sort(unique(habitacao$orgao_gestor_raw)))

dicionario_orgao_gestor <- c(
  "Não possui estrutura"                                            = "Sem estrutura específica",
  "Não possui estrutura específica"                                 = "Sem estrutura específica",
  "Secretaria exclusiva"                                            = "Secretaria exclusiva",
  "Secretaria municipal exclusiva"                                  = "Secretaria exclusiva",
  "Secretaria em conjunto com outra política"                       = "Secretaria compartilhada",
  "Secretaria em conjunto com outras políticas"                     = "Secretaria compartilhada",
  "Secretaria em conjunto com outras políticas setoriais"           = "Secretaria compartilhada",
  "Secretaria municipal em conjunto com outras políticas"           = "Secretaria compartilhada",
  "Secretaria municipal em conjunto com outras políticas setoriais" = "Secretaria compartilhada",
  "Setor subordinado a outra secretaria"                            = "Subordinado a outra secretaria",
  "Setor subordinado à chefia do executivo"                         = "Subordinado diretamente ao Executivo",
  "Setor subordinado à chefia do Executivo"                         = "Subordinado diretamente ao Executivo",
  "Setor subordinado diretamente à chefia do executivo"             = "Subordinado diretamente ao Executivo",
  "Setor subordinado diretamente à chefia do Executivo"             = "Subordinado diretamente ao Executivo",
  "Setor subordinado diretamente ao chefe do Executivo"             = "Subordinado diretamente ao Executivo",
  "Órgão da administração indireta"                                 = "Administração indireta",
  "Não informou" = NA_character_,
  "Ignorado"     = NA_character_,
  "Recusa"       = NA_character_
)

habitacao[, orgao_gestor_padronizado := dicionario_orgao_gestor[orgao_gestor_raw]]

# Sobrou algo fora do dicionário? Deve retornar character(0)
habitacao[!is.na(orgao_gestor_raw) &
            !orgao_gestor_raw %in% names(dicionario_orgao_gestor),
          unique(orgao_gestor_raw)]

obs_orgao <- habitacao[!is.na(orgao_gestor_padronizado),
                       .(id_municipio, ano, orgao_gestor_padronizado)]

painel_habitacao[obs_orgao, on = c("id_municipio", "ano"),
                 orgao_gestor_obs := i.orgao_gestor_padronizado]
painel_habitacao[, orgao_gestor_observado := !is.na(orgao_gestor_obs)]

locf_chr <- function(x) {
  idx <- which(!is.na(x))
  if (!length(idx)) return(x)
  # NAs antes da primeira observação permanecem NA (mesma regra do nafill
  # usado na seção 5) — sem isso, o vetor preenchido fica mais curto que o
  # grupo sempre que a 1a onda observada não é a 1a linha do painel
  c(rep(NA_character_, idx[1] - 1),
    rep(x[idx], diff(c(idx, length(x) + 1))))
}
setorder(painel_habitacao, id_municipio, ano)
painel_habitacao[, orgao_gestor_persistido := locf_chr(orgao_gestor_obs), by = id_municipio]


# ----------------------------------------------------------------------------
# 7. Consolidação do painel final (MUNIC)
# ----------------------------------------------------------------------------

setcolorder(painel_habitacao, c("id_municipio", "sigla_uf", "ano"))
setorder(painel_habitacao, id_municipio, ano)


# ----------------------------------------------------------------------------
# 8. Validação / QA (MUNIC)
# ----------------------------------------------------------------------------

# 8.1 — total de linhas deve bater com n_municipios x n_anos
stopifnot(nrow(painel_habitacao) == uniqueN(painel_habitacao$id_municipio) * length(anos_painel))

# 8.2 — plano_diretor_vigente: preenchimento por ano (mede o ganho da
# vigência retroativa vs. só as ondas em que a pergunta foi feita)
painel_habitacao[, .(pct_preenchido = mean(!is.na(plano_diretor_vigente))), by = ano]

# 8.3 — vigência deve ser monotônica (uma vez 1, nunca deveria voltar a 0/NA)
checagem_monotonia <- painel_habitacao[order(id_municipio, ano),
                                       .(quebra = any(diff(as.integer(!is.na(plano_diretor_vigente) &
                                                                        plano_diretor_vigente == 1)) < 0)),
                                       by = id_municipio]
checagem_monotonia[quebra == TRUE]  # deve retornar 0 linhas

# 8.4 — binários: comparar cobertura observada vs. persistida por ano
painel_habitacao[, .(
  pct_obs        = mean(!is.na(programas_aquisicao_unidades_habitacionais_obs)),
  pct_persistido = mean(!is.na(programas_aquisicao_unidades_habitacionais_persistido))
), by = ano]

# 8.5 — órgão gestor: observado vs. persistido por ano
painel_habitacao[, .(
  pct_observado  = mean(orgao_gestor_observado),
  pct_persistido = mean(!is.na(orgao_gestor_persistido))
), by = ano]


names(painel_habitacao)

summary(painel_habitacao)


# ══════════════════════════════════════════════════════════════════════════
# 4. MCMV — MINHA CASA, MINHA VIDA (Subsidiado/OGU + Financiado/FGTS)
# ══════════════════════════════════════════════════════════════════════════
# Copiado de "mcmv/1_script/mcmv.Rmd" (Tomás Paixão Borges, atualizado em
# 05/11/2026 com os zips mais recentes do Ministério das Cidades — Subsidiado
# até 30/06/2026, Financiado até 24/07/2026). [UNICO] As duas chamadas a
# ipca() do .Rmd original foram REMOVIDAS: o projeto nunca deflaciona valores
# monetários (regra já aplicada em todas as demais dimensões); os valores
# ficam em reais correntes de cada contrato, igual ao resto do painel.

PASTA_MCMV <- file.path(PASTA_HABITACAO, "mcmv")

zip_subsidiado <- file.path(PASTA_MCMV, "0_dados", "brutos_2026", "mcmv_subsidiado_202606302.zip")
zip_financiado <- file.path(PASTA_MCMV, "0_dados", "brutos_2026", "mcmv_financ_sintetico_20260724_v2.zip")

# Período do painel do MCMV:
# - começa em 2009 (lançamento do programa);
# - termina em 2025, porque 2026 ainda está incompleto na fonte (dados até
#   jun/jul de 2026).
anos_painel_mcmv <- 2009:2025

# ── 4.1 Leitura ────────────────────────────────────────────────────────────
# read_delim() lê o CSV direto de dentro do zip. Tudo é lido como texto e
# convertido depois, para não perder zeros à esquerda nem errar os decimais.
subsidiado_bruto <- read_delim(zip_subsidiado, delim = ";",
                               col_types = cols(.default = col_character()))

financiado_bruto <- read_delim(zip_financiado, delim = ";",
                               col_types = cols(.default = col_character()))

# Diretórios do próprio pipeline do MCMV: usado para passar o código IBGE de
# 6 dígitos (que vem na fonte) para o id_municipio de 7 dígitos (padrão do banco)
diretorios_mcmv <- read_excel(file.path(PASTA_MCMV, "0_dados", "diretorios.xlsx"), col_types = "text") %>%
  select(id_municipio, id_municipio_6)

nrow(subsidiado_bruto)
nrow(financiado_bruto)

# ── 4.2 Limpeza — MCMV Subsidiado (OGU) ──────────────────────────────────
# Observações sobre a base:
# - Linhas duplicadas: 1.087 operações da modalidade Rural aparecem 4 vezes,
#   com todas as colunas idênticas, inclusive o cod_operacao (identificador
#   único do empreendimento) — erro da extração do ministério. Sem as
#   cópias, o Rural de 2013 soma 55.929 UH, perto do número oficial da SNH
#   (56.607); com as cópias, seriam 66.249. Essas cópias são removidas.
# - Linhas sem cod_operacao (Oferta Pública e FAR - Compra Assistida) são
#   mantidas mesmo quando idênticas: sem o código, não dá para saber se são
#   duplicatas ou contratos diferentes do mesmo tamanho, no mesmo dia.
# - Números em formato brasileiro ("13.898.046,25"); cod_ibge com 6 dígitos
#   e ponto ("520.140").
subsidiado <- bind_rows(
  subsidiado_bruto %>% filter(!is.na(cod_operacao)) %>% distinct(),
  subsidiado_bruto %>% filter(is.na(cod_operacao))
)

# Quantas linhas foram removidas?
nrow(subsidiado_bruto) - nrow(subsidiado)

formato_br <- locale(decimal_mark = ",", grouping_mark = ".")

subsidiado <- subsidiado %>%
  mutate(
    id_municipio_6 = str_remove_all(cod_ibge, "\\."),          # "520.140" -> "520140"
    data_contrato  = dmy(dt_assinatura),                       # data de assinatura do contrato
    ano            = as.integer(year(data_contrato)),
    across(c(qtd_uh, qtd_uh_entregues, qtd_uh_vigentes, qtd_uh_distratadas,
             val_contratado_total, val_desembolsado),
           ~ parse_number(.x, locale = formato_br))
  ) %>%
  left_join(diretorios_mcmv, by = "id_municipio_6") %>%
  filter(ano %in% anos_painel_mcmv)

# Municípios que não casaram com os diretórios (deve ser vazio)
subsidiado %>%
  filter(is.na(id_municipio)) %>%
  count(cod_ibge, txt_nome_municipio, txt_sigla_uf)

# [UNICO] valores em reais correntes do mês do contrato — sem deflação
# (removida a chamada a ipca(..., "12/2023") do .Rmd original).

# ── 4.3 Limpeza — MCMV Financiado (FGTS) ─────────────────────────────────
# Observações sobre a base:
# - Já vem agregada por município x ano x mês x faixa de renda.
# - Os valores já usam ponto como decimal ("45340.11").
# - cod_ibge já tem 6 dígitos sem ponto.
# - Poucas linhas não têm município (cod_ibge vazio). Elas não entram no painel.
financiado <- financiado_bruto %>%
  mutate(
    id_municipio_6 = cod_ibge,
    ano = as.integer(num_ano),
    mes = as.integer(num_mes),
    across(c(qtd_uh_financiadas, vlr_financiamento, vlr_subsidio), as.numeric)
  ) %>%
  left_join(diretorios_mcmv, by = "id_municipio_6") %>%
  filter(ano %in% anos_painel_mcmv)

# Linhas sem município (cod_ibge vazio ou que não casou com os diretórios)
financiado %>%
  filter(is.na(id_municipio)) %>%
  count(cod_ibge, txt_municipio, wt = qtd_uh_financiadas, name = "uh_financiadas")

# [UNICO] valores em reais correntes do mês do contrato — sem deflação
# (removida a chamada a ipca(..., "12/2023") do .Rmd original).

# ── 4.4 Agregação para município x ano ───────────────────────────────────
subsidiado_mun_ano <- subsidiado %>%
  filter(!is.na(id_municipio)) %>%
  group_by(id_municipio, ano) %>%
  summarise(
    mcmv_sub_qtd_uh_contratadas = sum(qtd_uh, na.rm = TRUE),
    mcmv_sub_qtd_uh_entregues   = sum(qtd_uh_entregues, na.rm = TRUE),
    mcmv_sub_qtd_uh_vigentes    = sum(qtd_uh_vigentes, na.rm = TRUE),
    mcmv_sub_qtd_uh_distratadas = sum(qtd_uh_distratadas, na.rm = TRUE),
    mcmv_sub_val_contratado     = sum(val_contratado_total, na.rm = TRUE),
    mcmv_sub_val_desembolsado   = sum(val_desembolsado, na.rm = TRUE),
    .groups = "drop"
  )

financiado_mun_ano <- financiado %>%
  filter(!is.na(id_municipio)) %>%
  group_by(id_municipio, ano) %>%
  summarise(
    mcmv_fin_qtd_uh_financiadas = sum(qtd_uh_financiadas, na.rm = TRUE),
    mcmv_fin_val_financiamento  = sum(vlr_financiamento, na.rm = TRUE),
    mcmv_fin_val_subsidio       = sum(vlr_subsidio, na.rm = TRUE),
    .groups = "drop"
  )

# ── 4.5 Painel do MCMV: primeiro o esqueleto, depois o join ─────────────
# O esqueleto tem todos os municípios x todos os anos. Assim, os
# municípios-ano sem nenhum contrato aparecem no painel (em vez de sumirem).
painel_mcmv <- crossing(id_municipio = diretorios_mcmv$id_municipio,
                        ano          = anos_painel_mcmv)

mcmv <- painel_mcmv %>%
  left_join(subsidiado_mun_ano, by = c("id_municipio", "ano")) %>%
  left_join(financiado_mun_ano, by = c("id_municipio", "ano"))

# ── 4.6 Diagnóstico de dados faltantes (antes de preencher com 0) ───────
# Neste ponto, NA significa "o município não teve nenhum contrato no ano".
heatmap_na_mcmv <- mcmv %>%
  pivot_longer(starts_with("mcmv_"), names_to = "variavel", values_to = "valor") %>%
  group_by(variavel, ano) %>%
  summarise(pct_na = 100 * mean(is.na(valor)), .groups = "drop") %>%
  ggplot(aes(x = factor(ano), y = variavel, fill = pct_na)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(pct_na)), size = 2.5) +
  scale_fill_gradient(low = "white", high = "firebrick", limits = c(0, 100)) +
  labs(x = "Ano", y = NULL, fill = "% NA",
       title = "MCMV: % de municípios sem contrato no ano (antes de preencher com 0)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

heatmap_na_mcmv
ggsave(file.path(PASTA_MCMV, "2_output", "heatmap_na_mcmv.png"),
       heatmap_na_mcmv, width = 9, height = 5, bg = "white")

# Como as bases listam TODOS os contratos do programa, município-ano sem
# contrato é um zero verdadeiro (zero estrutural), e por isso NA vira 0.
# Isso está registrado na coluna "Transformações" do dicionário.
mcmv <- mcmv %>%
  mutate(across(starts_with("mcmv_"), ~ replace_na(.x, 0)))

# ── 4.7 Checagens (MCMV) ──────────────────────────────────────────────────
stopifnot(is.character(mcmv$id_municipio),
          all(nchar(mcmv$id_municipio) == 7),
          is.integer(mcmv$ano))

stopifnot(!anyDuplicated(mcmv[c("id_municipio", "ano")]))

stopifnot(sum(mcmv$mcmv_sub_qtd_uh_contratadas) ==
            sum(subsidiado$qtd_uh[!is.na(subsidiado$id_municipio)], na.rm = TRUE))
stopifnot(sum(mcmv$mcmv_fin_qtd_uh_financiadas) ==
            sum(financiado$qtd_uh_financiadas[!is.na(financiado$id_municipio)], na.rm = TRUE))

# Totais por ano (para conferir com os números oficiais do programa)
mcmv %>%
  group_by(ano) %>%
  summarise(uh_subsidiadas = sum(mcmv_sub_qtd_uh_contratadas),
            uh_financiadas = sum(mcmv_fin_qtd_uh_financiadas),
            municipios_com_contrato_sub = sum(mcmv_sub_qtd_uh_contratadas > 0),
            municipios_com_contrato_fin = sum(mcmv_fin_qtd_uh_financiadas > 0))

# Cópia isolada do MCMV, como o .Rmd original já exportava (registro do
# pipeline isolado, sem deflação, antes da junção com a MUNIC).
write_csv(mcmv, file.path(PASTA_MCMV, "0_dados", "processed", "mcmv_municipio_ano.csv"))


# ══════════════════════════════════════════════════════════════════════════
# 5. INTEGRAÇÃO FINAL — MUNIC (habitação/zoneamento) + MCMV
# ══════════════════════════════════════════════════════════════════════════
# [UNICO] junção nova: o painel final da dimensão passa a reunir as duas
# fontes por id_municipio x ano. O painel MUNIC cobre 2017-2025 (ondas da
# pesquisa); o MCMV cobre 2009-2025 — fora da janela MUNIC, as variáveis de
# habitação/zoneamento ficam NA (não existe pergunta feita naquele ano).

habitacao_zoneamento <- merge(
  painel_habitacao,
  as.data.table(mcmv),
  by = c("id_municipio", "ano"),
  all = TRUE
)
setcolorder(habitacao_zoneamento, c("id_municipio", "sigla_uf", "ano"))
setorder(habitacao_zoneamento, id_municipio, ano)

# Checagem de granularidade: cada id_municipio-ano deve ser único
stopifnot(!anyDuplicated(habitacao_zoneamento[, .(id_municipio, ano)]))

names(habitacao_zoneamento)
dim(habitacao_zoneamento)
summary(habitacao_zoneamento)


# ══════════════════════════════════════════════════════════════════════════
# 6. EXPORTAÇÃO
# ══════════════════════════════════════════════════════════════════════════
# [UNICO] etapa inteiramente nova — o script original terminava em
# summary(painel_habitacao) sem nunca escrever um arquivo de saída.

dir.create(PASTA_HABITACAO, showWarnings = FALSE, recursive = TRUE)

write_xlsx(
  habitacao_zoneamento,
  path = file.path(PASTA_HABITACAO, "habitacao_zoneamento.xlsx")
)

# ── Validação padronizada (2 Validação/11 Habitação e Zoneamento/) ──────────
validar_dimensao(habitacao_zoneamento, n_dim = 11)
