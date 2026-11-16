# ══════════════════════════════════════════════════════════════════════════
# mape_municipios — Dimensão Meio-Ambiente
# Desmatamento (INPE/PRODES) + Saneamento (SNIS) + Chuva (INMET) +
# Desastres Naturais (Atlas Digital de Desastres) + AdaptaBrasil (MCTI)
# Grão final: id_municipio (IBGE, 7 dígitos) x ano
# ══════════════════════════════════════════════════════════════════════════
#
# UNIFICADO em 13/11/2026 (validação): o código desta dimensão só tinha
# desmatamento (seção 3, abaixo, sem alterações de lógica). As outras quatro
# fontes já existiam, prontas, em subpastas próprias, mas nunca entravam no
# código principal nem eram exportadas como uma base única. Esta validação
# junta as cinco, com as correções abaixo (marcadas [UNICO] no próprio trecho):
#   - Saneamento ("Saneamento/saneamento.R") e Desastres
#     ("Desastres/desastres_ambientais.R") deflacionavam valores monetários
#     pelo IPCA — removido (regra do projeto: nunca deflacionar);
#   - Desastres passou a ler "BD_Atlas_1991_2025_v1.1_2026.08.06_Consolidado.xlsx"
#     (o mais recente na subpasta; o antigo "BD_Atlas_1991_2023_v1.0..."
#     cobria só até 2023) — a coluna de vítimas foi renomeada pela fonte entre
#     as duas versões (DH_total_danos_humanos -> DH_total_danos_humanos_diretos);
#   - AdaptaBrasil ("Adapta Brasil MCTI/AdaptaBR.R") juntava os 10 indicadores
#     por id+geocod_ibge+nome+ANO; como 8 têm ano de referência 2015 e 2 têm
#     2016 (ver tabela na seção 7), juntar pelo ano quebrava cada município em
#     2 linhas (2015 só com os 8, 2016 só com os 2), e até os consumidores
#     dessa base (o antigo "meio_ambiente.R") descartavam os 2 de 2016 por
#     causa disso. Corrigido: junção só por município (cada arquivo tem
#     exatamente 1 linha por município), com o ano de referência guardado por
#     fora. Depois, a pedido da validação, cada indicador é imputado para até
#     10 anos de distância do seu próprio ano de referência (seção 7.3) — como
#     o AdaptaBrasil só tem 1 "foto" por indicador (2015 ou 2016), e não uma
#     série temporal, sem essa imputação a variável ficaria NA em todo o
#     resto do painel.
# Chuva ("Chuva/chuva_municipios.R") reaproveita o .RData já processado (INMET
# 2010-2024, rodado em 19/09/2025) — o download/parse do INMET (anos inteiros
# de dados horários) não foi refeito nesta validação; para atualizar, rode
# aquele script separadamente e esta seção lerá o resultado novo.

# ── 1. Pacotes ────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(openxlsx)
library(writexl)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(readxl)

# ── 2. Configuração do projeto no Google Cloud ────────────────────────────
set_billing_id("dados-importacao") ## Substituir pelo seu projeto

PASTA_MEIO_AMBIENTE <- here("1 Bases Individuais", "12 Meio-Ambiente")


# ══════════════════════════════════════════════════════════════════════════
# 3. DESMATAMENTO (INPE/PRODES)
# ══════════════════════════════════════════════════════════════════════════
# Sem alterações de lógica nesta validação (o pedido foi incluir as outras
# quatro fontes; uma versão alternativa mais completa — área oficial do IBGE
# e os 6 biomas, não só Amazônia — existe em "Desmatamento/desmatamento.R" e
# não foi adotada aqui, ver relatório de validação).

query <- "
SELECT
    ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    bioma,
    area_total,
    desmatado,
    vegetacao_natural,
    nao_vegetacao_natural,
    hidrografia
FROM `basedosdados.br_inpe_prodes.municipio_bioma` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

desmatamento <- read_sql(query, billing_project_id = get_billing_id())

summary(desmatamento)


table(desmatamento$bioma)

names(desmatamento)

desmatamento_id <- desmatamento %>%
  group_by(ano, id_municipio) %>%
  summarise(area_total_municipio = sum(area_total, na.rm = TRUE),
            area_total_amazonia = sum(area_total[bioma == "Amazônia"], na.rm = TRUE),
            area_vegetacao_natural = sum(vegetacao_natural, na.rm = TRUE),
            area_vegetacao_natural_amazonia = sum(vegetacao_natural[bioma == "Amazônia"], na.rm = TRUE),
            desmatado_total_municipio = sum(desmatado, na.rm = TRUE),
            desmatado_total_amazonia = sum(desmatado[bioma == "Amazônia"], na.rm = TRUE)) %>%
  ungroup()

summary(desmatamento_id)


desmatamento_id <- desmatamento_id %>%
  mutate(proporcao_desmatamento = desmatado_total_municipio/area_total_municipio,
         razao_desmatamento_natural = desmatado_total_municipio/area_vegetacao_natural)

summary(desmatamento_id)


desmatamento_id <- desmatamento_id %>%
  arrange(id_municipio, ano) %>%  # Garantir a ordem correta por município e ano
  group_by(id_municipio) %>%  # Agrupar por município para calcular a defasagem corretamente
  mutate(lag_total_desmatamento = lag(desmatado_total_municipio, 1),
         lag_proporcao_desmatamento = lag(proporcao_desmatamento, 1)) %>%
  ungroup()

summary(desmatamento_id)


desmatamento_id <- desmatamento_id %>%
  mutate(taxa_crescimento_desmatamento_abs =
           ifelse(!is.na(lag_total_desmatamento) & lag_total_desmatamento != 0,
                  ((desmatado_total_municipio - lag_total_desmatamento) / lag_total_desmatamento) * 100,
                  NA),
         crescimento_desmatamento_pro = proporcao_desmatamento - lag_proporcao_desmatamento,
         crescimento_desmatamento_abs = desmatado_total_municipio - lag_total_desmatamento)

summary(desmatamento_id)

## Testes

desmatamento_negativo <- desmatamento_id %>%
  filter(taxa_crescimento_desmatamento_abs < 0)

summary(desmatamento_negativo)

desmatamento_id <- desmatamento_id %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano))


# ══════════════════════════════════════════════════════════════════════════
# 4. SANEAMENTO (SNIS, via basedosdados)
# ══════════════════════════════════════════════════════════════════════════
# Copiado de "Saneamento/saneamento.R". [UNICO] removida a deflação por IPCA
# (despesa_pessoal, arrecadacao_total, investimento_*) — valores em reais
# correntes de cada ano, como no resto do painel.

query <- "
SELECT
    dados.ano as ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.populacao_atendida_agua as populacao_atendida_agua,
    dados.populacao_atentida_esgoto as populacao_atentida_esgoto,
    dados.populacao_urbana as populacao_urbana,
    dados.populacao_urbana_residente_agua as populacao_urbana_residente_agua,
    dados.populacao_urbana_atendida_agua as populacao_urbana_atendida_agua,
    dados.populacao_urbana_atendida_agua_ibge as populacao_urbana_atendida_agua_ibge,
    dados.populacao_urbana_residente_esgoto as populacao_urbana_residente_esgoto,
    dados.populacao_urbana_atendida_esgoto as populacao_urbana_atendida_esgoto,
    dados.populacao_urbana_residente_esgoto_ibge as populacao_urbana_residente_esgoto_ibge,
    dados.extensao_rede_agua as extensao_rede_agua,
    dados.extensao_rede_esgoto as extensao_rede_esgoto,
    dados.quantidade_sede_municipal_agua as quantidade_sede_municipal_agua,
    dados.quantidade_sede_municipal_esgoto as quantidade_sede_municipal_esgoto,
    dados.quantidade_localidade_agua as quantidade_localidade_agua,
    dados.quantidade_localidade_esgoto as quantidade_localidade_esgoto,
    dados.quantidade_ligacao_ativa_agua as quantidade_ligacao_ativa_agua,
    dados.quantidade_ligacao_ativa_esgoto as quantidade_ligacao_ativa_esgoto,
    dados.quantidade_ligacao_total_agua as quantidade_ligacao_total_agua,
    dados.quantidade_ligacao_total_esgoto as quantidade_ligacao_total_esgoto,
    dados.quantidade_economia_ativa_agua as quantidade_economia_ativa_agua,
    dados.quantidade_economia_ativa_esgoto as quantidade_economia_ativa_esgoto,
    dados.quantidade_ligacao_micromedia_ativa_agua as quantidade_ligacao_micromedia_ativa_agua,
    dados.quantidade_economia_residencial_ativa_agua as quantidade_economia_residencial_ativa_agua,
    dados.quantidade_economia_residencial_ativa_esgoto as quantidade_economia_residencial_ativa_esgoto,
    dados.quantidade_economia_micromedida_ativa_agua as quantidade_economia_micromedida_ativa_agua,
    dados.quantidade_economia_residencial_micromedida_ativa_agua as quantidade_economia_residencial_micromedida_ativa_agua,
    dados.volume_agua_produzido as volume_agua_produzido,
    dados.volume_agua_tratada_eta as volume_agua_tratada_eta,
    dados.volume_agua_micromedido as volume_agua_micromedido,
    dados.volume_agua_consumido as volume_agua_consumido,
    dados.volume_agua_faturado as volume_agua_faturado,
    dados.volume_agua_macromedido as volume_agua_macromedido,
    dados.volume_agua_tratada_desinfeccao as volume_agua_tratada_desinfeccao,
    dados.volume_agua_bruta_exportado as volume_agua_bruta_exportado,
    dados.volume_agua_tratada_importado as volume_agua_tratada_importado,
    dados.volume_agua_tratada_exportado as volume_agua_tratada_exportado,
    dados.volume_agua_micromedido_economia_ativa as volume_agua_micromedido_economia_ativa,
    dados.volume_servico_agua as volume_servico_agua,
    dados.volume_agua_fluoretada as volume_agua_fluoretada,
    dados.consumo_eletrico_sistemas_agua as consumo_eletrico_sistemas_agua,
    dados.volume_esgoto_coletado as volume_esgoto_coletado,
    dados.volume_esgoto_tratado as volume_esgoto_tratado,
    dados.volume_esgoto_faturado as volume_esgoto_faturado,
    dados.volume_esgoto_bruto_exportado as volume_esgoto_bruto_exportado,
    dados.volume_esgoto_bruto_importado as volume_esgoto_bruto_importado,
    dados.volume_esgoto_importado as volume_esgoto_importado,
    dados.volume_esgoto_bruto_exportado_tratamento_importador as volume_esgoto_bruto_exportado_tratamento_importador,
    dados.consumo_eletrico_sistemas_esgoto as consumo_eletrico_sistemas_esgoto,
    dados.indice_agua_ligacao as indice_agua_ligacao,
    dados.indice_hidrometracao as indice_hidrometracao,
    dados.indice_macromedicao_volume_disponibilizado as indice_macromedicao_volume_disponibilizado,
    dados.indice_macromedicao as indice_macromedicao,
    dados.indice_perda_faturamento as indice_perda_faturamento,
    dados.indice_micromedido_economia as indice_micromedido_economia,
    dados.indice_coleta_esgoto as indice_coleta_esgoto,
    dados.indice_tratamento_esgoto as indice_tratamento_esgoto,
    dados.indice_consumo_agua_faturado as indice_consumo_agua_faturado,
    dados.indice_extensao_agua_ligacao as indice_extensao_agua_ligacao,
    dados.indice_extensao_esgoto_ligacao as indice_extensao_esgoto_ligacao,
    dados.indice_consumo_agua_per_capita as indice_consumo_agua_per_capita,
    dados.indice_atendimento_urbano_agua as indice_atendimento_urbano_agua,
    dados.indice_atendimento_agua_esgoto as indice_atendimento_agua_esgoto,
    dados.indice_volume_agua_economia as indice_volume_agua_economia,
    dados.indice_faturamento_agua as indice_faturamento_agua,
    dados.indice_participacao_economia_agua as indice_participacao_economia_agua,
    dados.indice_micromedicao_relativo_agua as indice_micromedicao_relativo_agua,
    dados.indice_esgotamento_agua_consumida as indice_esgotamento_agua_consumida,
    dados.indice_atendimento_esgoto_esgoto as indice_atendimento_esgoto_esgoto,
    dados.indice_perda_distribuicao_agua as indice_perda_distribuicao_agua,
    dados.indice_perda_linear_agua as indice_perda_linear_agua,
    dados.indice_perda_ligacao_agua as indice_perda_ligacao_agua,
    dados.indice_consumo_agua as indice_consumo_agua,
    dados.indice_consumo_medio_agua as indice_consumo_medio_agua,
    dados.indice_atendimento_total_agua as indice_atendimento_total_agua,
    dados.indice_atendimento_esgoto_agua as indice_atendimento_esgoto_agua,
    dados.indice_fluoretacao_agua as indice_fluoretacao_agua,
    dados.indice_consumo_energia_agua as indice_consumo_energia_agua,
    dados.indice_consumo_energia_esgoto as indice_consumo_energia_esgoto,
    dados.receita_operacional_direta as receita_operacional_direta,
    dados.receita_operacional_direta_agua as receita_operacional_direta_agua,
    dados.receita_operacional_direta_esgoto as receita_operacional_direta_esgoto,
    dados.receita_operacional_indireta as receita_operacional_indireta,
    dados.receita_operacional_direta_agua_exportada as receita_operacional_direta_agua_exportada,
    dados.receita_operacional as receita_operacional,
    dados.receita_operacional_direta_esgoto_importado as receita_operacional_direta_esgoto_importado,
    dados.arrecadacao_total as arrecadacao_total,
    dados.credito_areceber as credito_areceber,
    dados.despesa_pessoal as despesa_pessoal,
    dados.quantidade_empregado as quantidade_empregado,
    dados.despesa_produto_quimico as despesa_produto_quimico,
    dados.despesa_energia as despesa_energia,
    dados.despesa_servico_terceiro as despesa_servico_terceiro,
    dados.despesa_exploracao as despesa_exploracao,
    dados.despesas_juros_divida as despesas_juros_divida,
    dados.despesa_total_servico as despesa_total_servico,
    dados.despesa_ativo as despesa_ativo,
    dados.despesa_agua_importada as despesa_agua_importada,
    dados.despesa_fiscal as despesa_fiscal,
    dados.despesa_fiscal_nao_computada as despesa_fiscal_nao_computada,
    dados.despesa_exploracao_outro as despesa_exploracao_outro,
    dados.despesa_servico_outro as despesa_servico_outro,
    dados.despesa_amortizacao_divida as despesa_amortizacao_divida,
    dados.despesas_juros_divida_excecao as despesas_juros_divida_excecao,
    dados.despesa_divida_variacao as despesa_divida_variacao,
    dados.despesa_divida_total as despesa_divida_total,
    dados.despesa_esgoto_exportado as despesa_esgoto_exportado,
    dados.despesa_capitalizavel_municipio as despesa_capitalizavel_municipio,
    dados.despesa_capitalizavel_estado as despesa_capitalizavel_estado,
    dados.despesa_capitalizavel_prestador as despesa_capitalizavel_prestador,
    dados.investimento_agua_prestador as investimento_agua_prestador,
    dados.investimento_esgoto_prestador as investimento_esgoto_prestador,
    dados.investimento_outro_prestador as investimento_outro_prestador,
    dados.investimento_recurso_proprio_prestador as investimento_recurso_proprio_prestador,
    dados.investimento_recurso_oneroso_prestador as investimento_recurso_oneroso_prestador,
    dados.investimento_recurso_nao_oneroso_prestador as investimento_recurso_nao_oneroso_prestador,
    dados.investimento_total_prestador as investimento_total_prestador,
    dados.investimento_agua_municipio as investimento_agua_municipio,
    dados.investimento_esgoto_municipio as investimento_esgoto_municipio,
    dados.investimento_outro_municipio as investimento_outro_municipio,
    dados.investimento_recurso_proprio_municipio as investimento_recurso_proprio_municipio,
    dados.investimento_recurso_oneroso_municipio as investimento_recurso_oneroso_municipio,
    dados.investimento_recurso_nao_oneroso_municipio as investimento_recurso_nao_oneroso_municipio,
    dados.investimento_total_municipio as investimento_total_municipio,
    dados.investimento_agua_estado as investimento_agua_estado,
    dados.investimento_esgoto_estado as investimento_esgoto_estado,
    dados.investimento_outro_estado as investimento_outro_estado,
    dados.investimento_recurso_proprio_estado as investimento_recurso_proprio_estado,
    dados.investimento_recurso_oneroso_estado as investimento_recurso_oneroso_estado,
    dados.investimento_recurso_nao_oneroso_estado as investimento_recurso_nao_oneroso_estado,
    dados.investimento_total_estado as investimento_total_estado
FROM `basedosdados.br_mdr_snis.municipio_agua_esgoto` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
"

saneamento <- read_sql(query, billing_project_id = get_billing_id())

summary(saneamento)

names(saneamento)

saneamento <- saneamento %>%
  select(
   id_municipio, ano, populacao_atendida_agua, populacao_atentida_esgoto,
   populacao_urbana, populacao_urbana_residente_agua, populacao_urbana_atendida_esgoto,
   extensao_rede_agua, extensao_rede_esgoto, quantidade_sede_municipal_agua,
   quantidade_sede_municipal_esgoto,
   quantidade_ligacao_total_agua, quantidade_ligacao_total_esgoto,
   quantidade_empregado, despesa_pessoal, arrecadacao_total,
   investimento_agua_municipio, investimento_agua_estado, investimento_agua_prestador,
   investimento_esgoto_municipio, investimento_esgoto_estado, investimento_esgoto_prestador,
   indice_atendimento_total_agua,
   indice_atendimento_esgoto_agua
  ) %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano))

# [UNICO] valores em reais correntes de cada ano — removida a deflação por
# IPCA do script original (despesa_pessoal, arrecadacao_total,
# investimento_agua_*, investimento_esgoto_*).

summary(saneamento)

names(saneamento)


# ══════════════════════════════════════════════════════════════════════════
# 5. CHUVA (INMET, via pacote climateBR)
# ══════════════════════════════════════════════════════════════════════════
# [UNICO] reaproveita o .RData já processado por "Chuva/chuva_municipios.R"
# (INMET 2010-2024, rodado em 19/09/2025) — o download/parse do INMET (dados
# horários de 15 anos, por estação) não foi refeito nesta validação. Para
# atualizar: rode aquele script separadamente (ele sobrescreve o .RData) e
# esta seção lerá o resultado novo na próxima execução.

e_chuva <- new.env()
load(file.path(PASTA_MEIO_AMBIENTE, "Chuva", "chuva_municipios.RData"), envir = e_chuva)
chuva_municipios <- e_chuva[["chuva_municipios_final"]] %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano))

summary(chuva_municipios)


# ══════════════════════════════════════════════════════════════════════════
# 6. DESASTRES NATURAIS (Atlas Digital de Desastres no Brasil)
# ══════════════════════════════════════════════════════════════════════════
# Copiado de "Desastres/desastres_ambientais.R". [UNICO]:
#   - lê "BD_Atlas_1991_2025_v1.1_2026.08.06_Consolidado.xlsx" (o mais recente
#     na subpasta — cobre 1991-2025; o antigo ia só até 2023);
#   - lido com readxl::read_excel() em vez de openxlsx::read.xlsx(): o
#     arquivo novo já traz Data_Evento/Data_Registro como data-hora (a leitura
#     antiga, com as.Date(x, origin = "1899-12-30"), pressupunha o valor
#     numérico de serial do Excel — a troca evita essa suposição);
#   - "DH_total_danos_humanos" foi renomeada pela fonte para
#     "DH_total_danos_humanos_diretos" entre as duas versões do Atlas;
#   - removida a deflação por IPCA (PEPL_total_publico, PEPR_total_privado) —
#     valores em reais correntes do ano do evento.

desastres <- read_excel(file.path(PASTA_MEIO_AMBIENTE, "Desastres",
                                  "BD_Atlas_1991_2025_v1.1_2026.08.06_Consolidado.xlsx"))

summary(desastres)

glimpse(desastres)

desastres <- desastres %>%
  mutate(ano = year(Data_Evento))

glimpse(desastres)

table(desastres$ano)

table(desastres$grupo_de_desastre)

summary(desastres)

# [UNICO] sem deflação — valores em reais correntes do ano do evento.

desastres$controle <- 1

table(desastres$grupo_de_desastre)

desastres_municipios <- desastres %>%
  group_by(ano, Cod_IBGE_Mun) %>%
  summarise(
    total_desastres = sum(controle, na.rm = TRUE),
    total_pessoas_afetadas = sum(DH_total_danos_humanos_diretos, na.rm = TRUE),
    total_danos_materiais = sum(DM_total_danos_materiais, na.rm = TRUE),
    total_prejuizos_publicos = sum(PEPL_total_publico, na.rm = TRUE),
    total_prejuizos_privados = sum(PEPR_total_privado, na.rm = TRUE),

    # Climatológico
    total_desastres_climatologicos = sum(controle[grupo_de_desastre == "Climatológico"], na.rm = TRUE),
    total_pessoas_afetadas_climatologicos = sum(DH_total_danos_humanos_diretos[grupo_de_desastre == "Climatológico"], na.rm = TRUE),
    total_danos_materiais_climatologicos = sum(DM_total_danos_materiais[grupo_de_desastre == "Climatológico"], na.rm = TRUE),
    total_prejuizos_publicos_climatologicos = sum(PEPL_total_publico[grupo_de_desastre == "Climatológico"], na.rm = TRUE),
    total_prejuizos_privados_climatologicos = sum(PEPR_total_privado[grupo_de_desastre == "Climatológico"], na.rm = TRUE),

    # Hidrológico
    total_desastres_hidrologicos = sum(controle[grupo_de_desastre == "Hidrológico"], na.rm = TRUE),
    total_pessoas_afetadas_hidrologicos = sum(DH_total_danos_humanos_diretos[grupo_de_desastre == "Hidrológico"], na.rm = TRUE),
    total_danos_materiais_hidrologicos = sum(DM_total_danos_materiais[grupo_de_desastre == "Hidrológico"], na.rm = TRUE),
    total_prejuizos_publicos_hidrologicos = sum(PEPL_total_publico[grupo_de_desastre == "Hidrológico"], na.rm = TRUE),
    total_prejuizos_privados_hidrologicos = sum(PEPR_total_privado[grupo_de_desastre == "Hidrológico"], na.rm = TRUE),

    # Meteorológico
    total_desastres_meteorologicos = sum(controle[grupo_de_desastre == "Meteorológico"], na.rm = TRUE),
    total_pessoas_afetadas_meteorologicos = sum(DH_total_danos_humanos_diretos[grupo_de_desastre == "Meteorológico"], na.rm = TRUE),
    total_danos_materiais_meteorologicos = sum(DM_total_danos_materiais[grupo_de_desastre == "Meteorológico"], na.rm = TRUE),
    total_prejuizos_publicos_meteorologicos = sum(PEPL_total_publico[grupo_de_desastre == "Meteorológico"], na.rm = TRUE),
    total_prejuizos_privados_meteorologicos = sum(PEPR_total_privado[grupo_de_desastre == "Meteorológico"], na.rm = TRUE),

    # Outros
    total_desastres_outros = sum(controle[grupo_de_desastre == "Outros"], na.rm = TRUE),
    total_pessoas_afetadas_outros = sum(DH_total_danos_humanos_diretos[grupo_de_desastre == "Outros"], na.rm = TRUE),
    total_danos_materiais_outros = sum(DM_total_danos_materiais[grupo_de_desastre == "Outros"], na.rm = TRUE),
    total_prejuizos_publicos_outros = sum(PEPL_total_publico[grupo_de_desastre == "Outros"], na.rm = TRUE),
    total_prejuizos_privados_outros = sum(PEPR_total_privado[grupo_de_desastre == "Outros"], na.rm = TRUE),

    .groups = "drop"
  )

glimpse(desastres_municipios)

desastres_municipios <- desastres_municipios %>%
  rename(id_municipio = Cod_IBGE_Mun) %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano))


# ══════════════════════════════════════════════════════════════════════════
# 7. ADAPTABRASIL (MCTI — https://sistema.adaptabrasil.mcti.gov.br/)
# ══════════════════════════════════════════════════════════════════════════
# Copiado de "Adapta Brasil MCTI/AdaptaBR.R". [UNICO] reescrito: o original
# juntava os 10 indicadores por id+geocod_ibge+nome+ANO; como 8 têm ano de
# referência 2015 e 2 têm 2016 (coluna abaixo), juntar pelo ano quebrava cada
# município em duas linhas (uma só com os 8 de 2015, outra só com os 2 de
# 2016). Corrigido: junção só por município (cada um dos 10 arquivos tem
# exatamente 1 linha por município — confirmado, 5571 linhas em todos).
#
# O AdaptaBrasil não é uma série temporal: cada indicador é uma "foto" única,
# no ano de referência da tabela abaixo. Para entrar no painel anual da
# dimensão, cada indicador é imputado (repetido) para até 10 anos de
# distância do seu próprio ano de referência — pedido da validação ("ver
# quais os anos que temos disponíveis e fazer uma imputação de até 10 anos").
# Fora dessa janela de 21 anos (ref-10 a ref+10), fica NA: não há base para
# supor que o indicador de 2015/2016 ainda descreva o município 15+ anos
# depois. Mesma lógica já usada para o Censo Agropecuário (6 Economia),
# só que com janela de 10 anos em vez de 5 (ver dicionário).

PASTA_ADAPTABRASIL <- file.path(PASTA_MEIO_AMBIENTE, "Adapta Brasil MCTI", "Bases")

adaptabrasil_indicadores <- tribble(
  ~slug, ~arquivo, ~ano_referencia, ~descricao,
  "adaptabrasil_capacidade_investimento_renda",
  "AdaptaBrasil_desastres_geo-hidrologicos_capacidade_de_investimento_publico_municipal_e_renda_BR_municipio_2015_csv.CSV",
  2015, "Capacidade de investimento público municipal e renda da população (ameaça geo-hidrológica de inundações/enxurradas/alagamentos)",

  "adaptabrasil_capacidade_adaptativa_inundacao",
  "AdaptaBrasil_desastres_geo-hidrologicos_indice_de_capacidade_adaptativa_BR_municipio_2015_csv.CSV",
  2015, "Capacidade do sistema socioecológico de se ajustar a desastres geo-hidrológicos de inundações, enxurradas e alagamentos",

  "adaptabrasil_risco_inundacao",
  "AdaptaBrasil_desastres_geo-hidrologicos_indice_de_risco_para_inundacoes_enxurradas_e_alagamentos_BR_municipio_2015_csv.CSV",
  2015, "Risco de impacto das mudanças climáticas via desastres geo-hidrológicos de inundações, enxurradas e alagamentos",

  "adaptabrasil_vulnerabilidade_inundacao",
  "AdaptaBrasil_desastres_geo-hidrologicos_indice_de_vulnerabilidade_BR_municipio_2015_csv.CSV",
  2015, "Vulnerabilidade da população aos impactos de desastres geo-hidrológicos de inundações, enxurradas e alagamentos",

  "adaptabrasil_investimento_percapita_protecao_ambiental",
  "AB5_investimento_percapita_protecao_ambiental_2016.CSV",
  2016, "Valor per capita das transferências governamentais para proteção ambiental em políticas de adaptação e infraestrutura",

  "adaptabrasil_adesao_cidades_resilientes",
  "AdaptaBrasil_recursos_hidricos_adesao_ao_programa_cidades_resilientes_BR_municipio_2015_csv.CSV",
  2015, "Adesão ao Programa Cidades Resilientes (capacidade adaptativa de planejamento e gestão de risco para recursos hídricos), 2014-2016",

  "adaptabrasil_capacidade_adaptativa_seca",
  "AdaptaBrasil_recursos_hidricos_indice_de_capacidade_adaptativa_BR_municipio_2015_csv.CSV",
  2015, "Capacidade do sistema socioecológico de se ajustar a ameaças climáticas de seca",

  "adaptabrasil_risco_seca",
  "AdaptaBrasil_recursos_hidricos_indice_de_risco_de_impacto_para_seca_BR_municipio_2015_csv.CSV",
  2015, "Risco de impacto das mudanças climáticas via ameaça climática de seca",

  "adaptabrasil_vulnerabilidade_seca",
  "AdaptaBrasil_recursos_hidricos_indice_de_vulnerabilidade_BR_municipio_2015_csv.CSV",
  2015, "Suscetibilidade do sistema socioecológico aos efeitos das mudanças climáticas que resultam em seca",

  "adaptabrasil_plano_saneamento_basico",
  "AdaptaBrasil_recursos_hidricos_nivel_de_implementacao_e_articulacao_do_plano_municipal_de_saneamento_basico_BR_municipio_2016_csv.CSV",
  2016, "Nível de implementação e articulação de planos municipais de saneamento básico para gestão dos recursos hídricos"
)

# ── 7.1 Leitura e padronização de cada indicador ─────────────────────────
# [UNICO] o nome original de um dos 10 arquivos (investimento per capita em
# políticas de adaptação...) passava de 260 caracteres de caminho completo
# (limite clássico do Windows/MAX_PATH), e nem read_delim() nem file.copy()
# conseguiam abri-lo ("does not exist" mesmo com o arquivo presente). Uma
# cópia com nome curto (AB5_investimento_percapita_protecao_ambiental_2016.CSV,
# mesmo conteúdo) foi salva na própria pasta Bases/ e é ela que entra na
# tabela de indicadores acima; o arquivo original (bruto) continua intacto.
ler_adaptabrasil <- function(arquivo, slug) {
  read_delim(file.path(PASTA_ADAPTABRASIL, arquivo), delim = ";",
             locale = locale(encoding = "UTF-8"),
             col_types = cols(geocod_ibge = "c", valor = "d", classe = "c",
                              .default = "c")) %>%
    transmute(id_municipio = geocod_ibge,
              !!paste0(slug, "_valor")  := valor,
              !!paste0(slug, "_classe") := classe)
}

adaptabrasil_lista <- map2(adaptabrasil_indicadores$arquivo, adaptabrasil_indicadores$slug,
                           ler_adaptabrasil)

# Cada arquivo tem 1 linha por município (5571) — junção simples por
# id_municipio, sem risco de duplicar linhas.
adaptabrasil_base <- reduce(adaptabrasil_lista, full_join, by = "id_municipio")

# [UNICO] os 10 arquivos trazem 5570 municípios (não 5571) — confirmado nos
# 10, mesma contagem e sem duplicidade; a fonte do AdaptaBrasil não cobre
# 1 município do diretório atual (sinalizado, não investigado a fundo aqui).
stopifnot(nrow(adaptabrasil_base) == 5570, !anyDuplicated(adaptabrasil_base$id_municipio))

# ── 7.2 Checagem de intervalo (valor deve estar em [0, 1]; classe, com rótulo) ─
summary(adaptabrasil_base)

# ── 7.3 Imputação de até 10 anos (por indicador, a partir do seu próprio
#        ano de referência) ────────────────────────────────────────────────
# Esta parte só cria a TABELA de referência (ano_referencia, janela válida);
# a imputação em si (repetir o valor nos anos da janela) acontece na seção 8,
# junto com o esqueleto do painel — como o AdaptaBrasil não tem variação por
# ano na origem, não há "join por ano" aqui, só o registro de até quando cada
# indicador vale.
JANELA_IMPUTACAO_ADAPTABRASIL <- 10L
print(adaptabrasil_indicadores %>% select(slug, ano_referencia, descricao))


# ══════════════════════════════════════════════════════════════════════════
# 8. INTEGRAÇÃO FINAL — painel primeiro, join depois
# ══════════════════════════════════════════════════════════════════════════
# Esqueleto: todos os municípios (diretório oficial do IBGE) x todos os anos
# observados em qualquer uma das quatro fontes com série temporal
# (desmatamento, saneamento, chuva, desastres). O AdaptaBrasil entra por
# fora, com sua própria janela de imputação (seção 7.3).

municipios_diretorio <- read_sql(
  "SELECT DISTINCT id_municipio FROM `basedosdados.br_bd_diretorios_brasil.municipio`",
  billing_project_id = get_billing_id()
) %>%
  mutate(id_municipio = as.character(id_municipio))

anos_painel_ambiente <- range(c(desmatamento_id$ano, saneamento$ano,
                                chuva_municipios$ano, desastres_municipios$ano))
anos_painel_ambiente <- anos_painel_ambiente[1]:anos_painel_ambiente[2]

painel_ambiente <- crossing(id_municipio = municipios_diretorio$id_municipio,
                            ano          = anos_painel_ambiente)

meio_ambiente <- painel_ambiente %>%
  left_join(desmatamento_id,      by = c("id_municipio", "ano")) %>%
  left_join(saneamento,           by = c("id_municipio", "ano")) %>%
  left_join(chuva_municipios,     by = c("id_municipio", "ano")) %>%
  left_join(desastres_municipios, by = c("id_municipio", "ano"))

# Desastres: a fonte lista TODOS os eventos registrados, então município-ano
# sem nenhuma linha é um zero verdadeiro (nenhum desastre), não dado
# faltante — mesmo raciocínio já usado para emendas parlamentares e MCMV
# nesta validação. Saneamento/chuva/desmatamento permanecem NA (ausência
# genuína: município não respondeu a pesquisa / sem estação próxima / fora
# do bioma monitorado naquele ano).
colunas_desastres <- setdiff(names(desastres_municipios), c("id_municipio", "ano"))
meio_ambiente <- meio_ambiente %>%
  mutate(across(all_of(colunas_desastres), ~ replace_na(.x, 0)))

# AdaptaBrasil: imputado para até 10 anos de distância do ano de referência
# de CADA indicador (seção 7.3) — fora da janela, NA.
for (i in seq_len(nrow(adaptabrasil_indicadores))) {
  slug      <- adaptabrasil_indicadores$slug[i]
  ano_ref   <- adaptabrasil_indicadores$ano_referencia[i]
  col_valor <- paste0(slug, "_valor")
  col_classe<- paste0(slug, "_classe")
  col_obs   <- paste0(slug, "_valor_obs")

  # [UNICO] renomeia explicitamente para "_fonte" antes do join: o suffix
  # do left_join só é aplicado quando já existe uma coluna com o MESMO nome
  # dos dois lados — na 1ª iteração (e em todas, já que cada slug só existe
  # em adaptabrasil_base) não há colisão, então o suffix era ignorado e
  # col_valor/col_classe entravam SEM o sufixo "_fonte" esperado pelo mutate().
  tmp <- adaptabrasil_base %>%
    select(id_municipio, all_of(c(col_valor, col_classe))) %>%
    rename(!!paste0(col_valor, "_fonte") := all_of(col_valor),
           !!paste0(col_classe, "_fonte") := all_of(col_classe))

  meio_ambiente <- meio_ambiente %>%
    left_join(tmp, by = "id_municipio") %>%
    mutate(
      !!col_obs    := if_else(ano == ano_ref, .data[[paste0(col_valor, "_fonte")]], NA_real_),
      !!col_valor  := if_else(abs(ano - ano_ref) <= JANELA_IMPUTACAO_ADAPTABRASIL,
                              .data[[paste0(col_valor, "_fonte")]], NA_real_),
      !!col_classe := if_else(abs(ano - ano_ref) <= JANELA_IMPUTACAO_ADAPTABRASIL,
                              .data[[paste0(col_classe, "_fonte")]], NA_character_)
    ) %>%
    select(-all_of(paste0(c(col_valor, col_classe), "_fonte")))
}

# Checagem de granularidade: cada id_municipio-ano deve ser único
n_dup_final <- meio_ambiente %>% count(id_municipio, ano) %>% filter(n > 1) %>% nrow()
if (n_dup_final > 0) warning(n_dup_final, " município-ano duplicados no painel final — investigar antes de exportar")

names(meio_ambiente)
dim(meio_ambiente)
summary(meio_ambiente)


# ══════════════════════════════════════════════════════════════════════════
# 9. EXPORTAÇÃO
# ══════════════════════════════════════════════════════════════════════════

dir.create(PASTA_MEIO_AMBIENTE, showWarnings = FALSE, recursive = TRUE)

write_xlsx(
  meio_ambiente,
  path = file.path(PASTA_MEIO_AMBIENTE, "meio_ambiente.xlsx")
)

# ── Validação padronizada (2 Validação/12 Meio-Ambiente/) ───────────────────
validar_dimensao(meio_ambiente, n_dim = 12)
