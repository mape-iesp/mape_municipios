# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(openxlsx)
library(writexl)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))

# ── 2. Configuração do projeto no Google Cloud ────────────────────────────────
set_billing_id("dados-importacao") ## Substituir pelo seu projeto


# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.adm_direta as adm_direta,
    dados.adm_direta_sem_instrucao as adm_direta_sem_instrucao,
    dados.adm_direta_ensino_fundamental as adm_direta_ensino_fundamental,
    dados.adm_direta_ensino_medio as adm_direta_ensino_medio,
    dados.adm_direta_ensino_superior as adm_direta_ensino_superior,
    dados.adm_direta_pos_graduacao as adm_direta_pos_graduacao,
    dados.adm_direta_nivel_auxiliar as adm_direta_nivel_auxiliar,
    dados.estatutario_adm_direta as estatutario_adm_direta,
    dados.estatutario_adm_direta_sem_instrucao as estatutario_adm_direta_sem_instrucao,
    dados.estatutario_adm_direta_ensino_fundamental as estatutario_adm_direta_ensino_fundamental,
    dados.estatutario_adm_direta_ensino_medio as estatutario_adm_direta_ensino_medio,
    dados.estatutario_adm_direta_ensino_superior as estatutario_adm_direta_ensino_superior,
    dados.estatutario_adm_direta_pos_graduacao as estatutario_adm_direta_pos_graduacao,
    dados.clt_adm_direta as clt_adm_direta,
    dados.clt_adm_direta_sem_instrucao as clt_adm_direta_sem_instrucao,
    dados.clt_adm_direta_ensino_fundamental as clt_adm_direta_ensino_fundamental,
    dados.clt_adm_direta_ensino_medio as clt_adm_direta_ensino_medio,
    dados.clt_adm_direta_ensino_superior as clt_adm_direta_ensino_superior,
    dados.clt_adm_direta_pos_graduacao as clt_adm_direta_pos_graduacao,
    dados.comissionado_adm_direta as comissionado_adm_direta,
    dados.comissionado_adm_direta_sem_instrucao as comissionado_adm_direta_sem_instrucao,
    dados.comissionado_adm_direta_ensino_fundamental as comissionado_adm_direta_ensino_fundamental,
    dados.comissionado_adm_direta_ensino_medio as comissionado_adm_direta_ensino_medio,
    dados.comissionado_adm_direta_ensino_superior as comissionado_adm_direta_ensino_superior,
    dados.comissionado_adm_direta_pos_graduacao as comissionado_adm_direta_pos_graduacao,
    dados.estagio_adm_direta as estagio_adm_direta,
    dados.estagio_adm_direta_ensino_fundamental as estagio_adm_direta_ensino_fundamental,
    dados.estagio_adm_direta_ensino_medio as estagio_adm_direta_ensino_medio,
    dados.estagio_adm_direta_ensino_superior as estagio_adm_direta_ensino_superior,
    dados.estagio_adm_direta_pos_graduacao as estagio_adm_direta_pos_graduacao,
    dados.sem_vinculo_permanente_adm_direta as sem_vinculo_permanente_adm_direta,
    dados.sem_vinculo_permanente_adm_direta_sem_instrucao as sem_vinculo_permanente_adm_direta_sem_instrucao,
    dados.sem_vinculo_permanente_adm_direta_ensino_fundamental as sem_vinculo_permanente_adm_direta_ensino_fundamental,
    dados.sem_vinculo_permanente_adm_direta_ensino_medio as sem_vinculo_permanente_adm_direta_ensino_medio,
    dados.sem_vinculo_permanente_adm_direta_ensino_superior as sem_vinculo_permanente_adm_direta_ensino_superior,
    dados.sem_vinculo_permanente_adm_direta_pos_graduacao as sem_vinculo_permanente_adm_direta_pos_graduacao,
    dados.outro_vinculo_adm_direta as outro_vinculo_adm_direta,
    dados.existencia_unidade_chefiada_mulher_adm_direta as existencia_unidade_chefiada_mulher_adm_direta,
    dados.existencia_registro_deficiencia_adm_direta as existencia_registro_deficiencia_adm_direta,
    dados.registro_deficiencia_adm_direta as registro_deficiencia_adm_direta,
    dados.existencia_adm_indireta as existencia_adm_indireta,
    dados.adm_indireta as adm_indireta,
    dados.adm_indireta_sem_instrucao as adm_indireta_sem_instrucao,
    dados.adm_indireta_ensino_fundamental as adm_indireta_ensino_fundamental,
    dados.adm_indireta_ensino_medio as adm_indireta_ensino_medio,
    dados.adm_indireta_ensino_superior as adm_indireta_ensino_superior,
    dados.adm_indireta_pos_graduacao as adm_indireta_pos_graduacao,
    dados.adm_indireta_nivel_auxiliar as adm_indireta_nivel_auxiliar,
    dados.estatutario_adm_indireta as estatutario_adm_indireta,
    dados.estatutario_adm_indireta_sem_instrucao as estatutario_adm_indireta_sem_instrucao,
    dados.estatutario_adm_indireta_ensino_fundamental as estatutario_adm_indireta_ensino_fundamental,
    dados.estatutario_adm_indireta_ensino_medio as estatutario_adm_indireta_ensino_medio,
    dados.estatutario_adm_indireta_ensino_superior as estatutario_adm_indireta_ensino_superior,
    dados.estatutario_adm_indireta_pos_graduacao as estatutario_adm_indireta_pos_graduacao,
    dados.clt_adm_indireta as clt_adm_indireta,
    dados.clt_adm_indireta_sem_instrucao as clt_adm_indireta_sem_instrucao,
    dados.clt_adm_indireta_ensino_fundamental as clt_adm_indireta_ensino_fundamental,
    dados.clt_adm_indireta_ensino_medio as clt_adm_indireta_ensino_medio,
    dados.clt_adm_indireta_ensino_superior as clt_adm_indireta_ensino_superior,
    dados.clt_adm_indireta_pos_graduacao as clt_adm_indireta_pos_graduacao,
    dados.comissionado_adm_indireta as comissionado_adm_indireta,
    dados.comissionado_adm_indireta_sem_instrucao as comissionado_adm_indireta_sem_instrucao,
    dados.comissionado_adm_indireta_ensino_fundamental as comissionado_adm_indireta_ensino_fundamental,
    dados.comissionado_adm_indireta_ensino_medio as comissionado_adm_indireta_ensino_medio,
    dados.comissionado_adm_indireta_ensino_superior as comissionado_adm_indireta_ensino_superior,
    dados.comissionado_adm_indireta_pos_graduacao as comissionado_adm_indireta_pos_graduacao,
    dados.estagio_adm_indireta as estagio_adm_indireta,
    dados.estagio_adm_indireta_ensino_fundamental as estagio_adm_indireta_ensino_fundamental,
    dados.estagio_adm_indireta_ensino_medio as estagio_adm_indireta_ensino_medio,
    dados.estagio_adm_indireta_ensino_superior as estagio_adm_indireta_ensino_superior,
    dados.estagio_adm_indireta_pos_graduacao as estagio_adm_indireta_pos_graduacao,
    dados.sem_vinculo_permanente_adm_indireta as sem_vinculo_permanente_adm_indireta,
    dados.sem_vinculo_permanente_adm_indireta_sem_instrucao as sem_vinculo_permanente_adm_indireta_sem_instrucao,
    dados.sem_vinculo_permanente_adm_indireta_ensino_fundamental as sem_vinculo_permanente_adm_indireta_ensino_fundamental,
    dados.sem_vinculo_permanente_adm_indireta_ensino_medio as sem_vinculo_permanente_adm_indireta_ensino_medio,
    dados.sem_vinculo_permanente_adm_indireta_ensino_superior as sem_vinculo_permanente_adm_indireta_ensino_superior,
    dados.sem_vinculo_permanente_adm_indireta_pos_graduacao as sem_vinculo_permanente_adm_indireta_pos_graduacao,
    dados.outro_vinculo_adm_indireta as outro_vinculo_adm_indireta,
    dados.fundacao_adm_indireta as fundacao_adm_indireta,
    dados.autarquia_adm_indireta as autarquia_adm_indireta,
    dados.empresa_publica_adm_indireta as empresa_publica_adm_indireta,
    dados.sociedade_economia_mista_adm_indireta as sociedade_economia_mista_adm_indireta,
    dados.existencia_contratacao_24_meses as existencia_contratacao_24_meses,
    dados.existencia_realizacao_concurso_24_meses as existencia_realizacao_concurso_24_meses,
    dados.existencia_reserva_concurso_deficientes as existencia_reserva_concurso_deficientes,
    dados.existencia_reserva_concurso_negros as existencia_reserva_concurso_negros,
    dados.existencia_reserva_concurso_quilombolas as existencia_reserva_concurso_quilombolas,
    dados.existencia_reserva_concurso_indigenas as existencia_reserva_concurso_indigenas,
    dados.existencia_reserva_concurso_ciganos as existencia_reserva_concurso_ciganos,
    dados.existencia_reserva_concurso_nao_sabia_informar as existencia_reserva_concurso_nao_sabia_informar,
    dados.existencia_reserva_concurso_nao_houve as existencia_reserva_concurso_nao_houve,
    dados.existencia_priorizacao_cargo_mulher as existencia_priorizacao_cargo_mulher,
    dados.existencia_fundo_municipal_previdencia as existencia_fundo_municipal_previdencia,
    dados.aposentado as aposentado,
    dados.pesionista as pesionista
FROM `basedosdados.br_ibge_munic.recursos_humanos` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

recursos_humanos <- read_sql(query, billing_project_id = get_billing_id())

table(recursos_humanos$ano)

summary(recursos_humanos)

# 1. Proporção de NA por ano, para cada variável
na_por_ano <- recursos_humanos %>%
  group_by(ano) %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  ungroup()

# 2. Formato longo para visualizar/filtrar
na_long <- na_por_ano %>%
  pivot_longer(-ano, names_to = "variavel", values_to = "prop_na")

# 3. Heatmap de NA por ano x variável (seu padrão de diagnóstico)
ggplot(na_long, aes(x = factor(ano), y = variavel, fill = prop_na)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "firebrick", labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6)) +
  labs(x = "Ano", y = NULL, fill = "% NA")

limiar <- 0.5  # ex: aceitar até 50% de NA no ano

escopo_por_variavel <- na_long %>%
  filter(prop_na < limiar) %>%
  group_by(variavel) %>%
  summarise(
    ano_min = min(ano),
    ano_max = max(ano),
    anos_validos = paste(sort(unique(ano)), collapse = ", "),
    .groups = "drop"
  )


# ══════════════════════════════════════════════════════════════════════════
# VALIDAÇÃO CRUZADA — basedosdados.br_ibge_munic.indicadores_quantidade_vinculo
# ══════════════════════════════════════════════════════════════════════════
# [UNICO] pedido da validação: cruzar esta base com outra visão da própria
# MUNIC sobre o mesmo bloco de Recursos Humanos. A tabela "recursos_humanos"
# usada acima é a MUNIC "larga" (1 coluna por tipo de vínculo x escolaridade);
# "indicadores_quantidade_vinculo" é a MESMA pesquisa em formato longo
# (ano, id_municipio, tipo_administracao, tipo_vinculo, escolaridade,
# quantidade_vinculo) — não é uma fonte independente, mas valida se a leitura
# em colunas acima (ex.: somar adm_direta + adm_indireta) bate com o total
# que o próprio Base dos Dados calcula a partir da mesma pesquisa. Não há,
# em basedosdados, nenhuma fonte de headcount municipal fora da MUNIC.
query_validacao <- "
SELECT ano, id_municipio, tipo_administracao, quantidade_vinculo
FROM `basedosdados.br_ibge_munic.indicadores_quantidade_vinculo`
WHERE recusa IS NULL OR recusa = 0
"
validacao_vinculo <- read_sql(query_validacao, billing_project_id = get_billing_id())

total_vinculo_longo <- validacao_vinculo %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano)) %>%
  group_by(ano, id_municipio, tipo_administracao) %>%
  summarise(total_quantidade_vinculo = sum(quantidade_vinculo, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = tipo_administracao, values_from = total_quantidade_vinculo,
              names_prefix = "total_longo_")

names(total_vinculo_longo)

comparacao_adm <- recursos_humanos %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano)) %>%
  select(ano, id_municipio, adm_direta, adm_indireta) %>%
  inner_join(total_vinculo_longo, by = c("ano", "id_municipio"))

names(comparacao_adm)

# Direta e indireta: diferença entre a soma da base larga (usada no painel)
# e o total da visão longa do Base dos Dados, para o mesmo município-ano.
# Esperado: diferença 0 (ou muito próxima) na grande maioria dos casos —
# divergências vão para o relatório de validação, não são corrigidas aqui
# (não há como saber qual das duas visões da MUNIC está certa sem ir à fonte).
# CONFERIR nomes de tipo_administracao antes de rodar em produção — o grep
# com "_direta$"/"_indireta$" (exigindo "_" antes de "direta") evita que
# "Indireta" (que termina em "direta") seja confundida com "Direta".
col_longo_direta   <- grep("_direta$",   names(comparacao_adm), value = TRUE, ignore.case = TRUE)[1]
col_longo_indireta <- grep("_indireta$", names(comparacao_adm), value = TRUE, ignore.case = TRUE)[1]
stopifnot(!is.na(col_longo_direta), !is.na(col_longo_indireta))

comparacao_adm <- comparacao_adm %>%
  mutate(
    dif_adm_direta   = adm_direta   - .data[[col_longo_direta]],
    dif_adm_indireta = adm_indireta - .data[[col_longo_indireta]]
  )

resumo_validacao_vinculo <- comparacao_adm %>%
  summarise(
    n_municipio_ano           = n(),
    pct_adm_direta_identica   = mean(dif_adm_direta == 0, na.rm = TRUE) * 100,
    pct_adm_indireta_identica = mean(dif_adm_indireta == 0, na.rm = TRUE) * 100,
    max_dif_abs_adm_direta    = max(abs(dif_adm_direta), na.rm = TRUE),
    max_dif_abs_adm_indireta  = max(abs(dif_adm_indireta), na.rm = TRUE)
  )

print(resumo_validacao_vinculo)

write_csv(comparacao_adm,
         here("1 Bases Individuais", "13 Recursos Humanos",
              "validacao_cruzada_indicadores_quantidade_vinculo.csv"))


# ── Exportar .xlsx ────────────────────────────────────────────────────────
dir.create(here("1 Bases Individuais", "13 Recursos Humanos"), showWarnings = FALSE)

write_xlsx(
  recursos_humanos,
  path = here("1 Bases Individuais", "13 Recursos Humanos", "recursos_humanos.xlsx")
)

# ── Validação padronizada (2 Validação/13 Recursos Humanos/) ────────────────
validar_dimensao(recursos_humanos, n_dim = 13)


