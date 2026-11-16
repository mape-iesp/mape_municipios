# ══════════════════════════════════════════════════════════════════════════════
# 4 Corrupção e Transparência
#
# Gera o painel município-ano da dimensão, cruzando o diretório de municípios
# (Identificação) com as bases de corrupção/transparência já coletadas.
#
# Bases que entram:
#   • Programa de Fiscalização em Entes Federativos (FEF) — CGU, 2006-2018.
#     Bruto: cgu/br_cgu_fef_microdados.csv.gz.
#     A conta (total de ações, falhas graves, montante fiscalizado) é a mesma
#     de cgu/cgu.R; aqui ela entra num painel município x ano completo, com
#     id_municipio como caractere (padrão da dimensão 1 Identificação), em vez
#     de só as linhas com ao menos uma ação.
#   • Escala Brasil Transparente (EBT) — CGU, 2020-2021.
#     Fonte: basedosdados.br_cgu_ebt.municipio (BigQuery, projeto
#     "dados-importacao"). A EBT avalia só uma parte dos municípios (665 de
#     5.571, nos dois anos) — quem não é avaliado fica NA, não é 0: não ser
#     avaliado não é o mesmo que ter nota mínima de transparência.
#
# Decisões:
#   • Painel = Diretórios (5.571 municípios) x 2006:2021 (une a cobertura do
#     FEF, 2006-2018, com a da EBT, 2020-2021; 2019 fica no meio, sem
#     nenhuma das duas fontes, e todas as colunas ficam NA nesse ano).
#     Município-ano sem fiscalização/avaliação no período fica NA — não é 0,
#     porque não dá para distinguir "não foi sorteado/avaliado" de "não teve
#     falha/nota baixa".
#   • montante_fiscalizado e montante_fiscalizado_falha_grave ficam em R$
#     NOMINAIS, sem deflacionar — por instrução da coordenação (mesmo padrão
#     de tarifa_transporte_publico na dimensão 17 Mobilidade). O cgu/cgu.R
#     original deflacionava pelo IPCA para 12/2023 (deflateBR); essa etapa
#     foi removida aqui e cgu/cgu.R/cgu/corrupcao.xlsx ficam desatualizados
#     nesse ponto — vale corrigi-los ou apagá-los depois.
#   • Exportação via writexl::write_xlsx() em vez de openxlsx::write.xlsx():
#     o openxlsx instalado nesta máquina (4.2.8.1) grava toda vez uma relação
#     para um drawing/vmlDrawing que nunca é escrito no pacote .xlsx, deixando
#     o arquivo com referência interna quebrada (abre no Excel/readxl, mas
#     quebra em leitores mais estritos, como openpyxl em Python). Isso afeta
#     a maior parte dos .xlsx já exportados pelo projeto — vale revisar depois.
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(tidyverse)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(basedosdados)
library(writexl)

set_billing_id("dados-importacao") ## Substituir pelo seu projeto, se não for o mesmo

pasta_dimensao <- here("1 Bases Individuais", "4 Corrupção e Transparência")


# ── 2. Esqueleto do painel (Diretórios x anos do FEF + EBT) ───────────────────
diretorios <- openxlsx::read.xlsx(
  here("1 Bases Individuais", "1 Identificação", "diretorios_brasileiros.xlsx")
) %>%
  as_tibble() %>%
  distinct(id_municipio) %>%
  mutate(id_municipio = as.character(id_municipio))

n_distinct(diretorios$id_municipio)  # esperado: 5571

anos_dimensao <- 2006:2021

esqueleto <- diretorios %>%
  crossing(ano = anos_dimensao)


# ── 3. Ler e agregar o bruto do FEF ───────────────────────────────────────────
fef_bruto <- read_csv(
  file.path(pasta_dimensao, "cgu", "br_cgu_fef_microdados.csv.gz"),
  col_types = cols(id_municipio = "c", ano_evento = "i")
)

range(fef_bruto$ano_evento)  # esperado: 2006-2018
table(fef_bruto$tipo_constatacao)

fef <- fef_bruto %>%
  mutate(
    ano          = ano_evento,
    falha_grave  = if_else(tipo_constatacao == "Falha Grave", 1, 0)
  ) %>%
  group_by(ano, id_municipio) %>%
  summarise(
    total_acao                        = n(),
    total_falhas_graves                = sum(falha_grave, na.rm = TRUE),
    montante_fiscalizado               = sum(montante_fiscalizado, na.rm = TRUE),
    montante_fiscalizado_falha_grave   = sum(montante_fiscalizado[falha_grave == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_falhas_graves      = total_falhas_graves / total_acao,
    montante_falhas_graves  = montante_fiscalizado_falha_grave / montante_fiscalizado
  )

# montante_fiscalizado e montante_fiscalizado_falha_grave ficam em R$
# nominais do ano de cada evento — sem deflacionar (instrução da coordenação).


# ── 4. Ler a Escala Brasil Transparente (BigQuery) ────────────────────────────
ebt_bruto <- read_sql(
  "SELECT ano, id_municipio, ranking, nota
   FROM `basedosdados.br_cgu_ebt.municipio`",
  billing_project_id = get_billing_id()
)

ebt <- ebt_bruto %>%
  mutate(
    ano                      = as.integer(ano),
    id_municipio             = as.character(id_municipio),
    ebt_ranking              = as.integer(ranking),
    ebt_nota_transparencia   = nota
  ) %>%
  select(ano, id_municipio, ebt_ranking, ebt_nota_transparencia)

range(ebt$ano)                                    # esperado: 2020-2021
n_distinct(ebt$id_municipio)                      # esperado: 665
anyDuplicated(ebt[c("id_municipio", "ano")])      # esperado: 0


# ── 5. Juntar ao esqueleto do painel ──────────────────────────────────────────
corrupcao <- esqueleto %>%
  left_join(fef, by = c("id_municipio", "ano")) %>%
  left_join(ebt, by = c("id_municipio", "ano")) %>%
  arrange(id_municipio, ano)


# ── 6. Checagens ──────────────────────────────────────────────────────────────
nrow(corrupcao) == n_distinct(diretorios$id_municipio) * length(anos_dimensao)  # esperado: TRUE
anyDuplicated(corrupcao[c("id_municipio", "ano")])                        # esperado: 0
all(nchar(corrupcao$id_municipio) == 7)                                   # esperado: TRUE
is.character(corrupcao$id_municipio)                                      # esperado: TRUE

# Município-ano sem nenhuma ação de fiscalização no período (FEF)
corrupcao %>%
  filter(is.na(total_acao)) %>%
  summarise(n_municipio_ano_sem_fiscalizacao = n())

# Município-ano sem avaliação na EBT (só 665 municípios, só 2020-2021)
corrupcao %>%
  filter(ano %in% c(2020, 2021)) %>%
  summarise(n_avaliados = sum(!is.na(ebt_nota_transparencia)),
            n_total     = n())

# 2019: nenhuma das duas fontes cobre esse ano
corrupcao %>%
  filter(ano == 2019) %>%
  summarise(todas_na = all(is.na(total_acao) & is.na(ebt_nota_transparencia)))  # esperado: TRUE

summary(corrupcao)


# ── 7. Exportar ───────────────────────────────────────────────────────────────
save(corrupcao, file = file.path(pasta_dimensao, "corrupcao.RData"))

write_xlsx(
  corrupcao,
  path = file.path(pasta_dimensao, "corrupcao.xlsx")
)

# ── 8. Validação padronizada (2 Validação/4 Corrupção e Transparência/) ─────
validar_dimensao(corrupcao, n_dim = 4)
