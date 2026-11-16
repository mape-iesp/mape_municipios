# ══════════════════════════════════════════════════════════════════════════════
# MUNIC (IBGE) – Harmonização 2004–2024: Recursos Humanos e Conselhos Municipais
#
# Gera uma base município-ano (2004–2024) com:
#   • Recursos humanos: quadro de pessoal da adm. direta (total e por vínculo)
#     e existência/total da adm. indireta. Sem imputação: anos sem pesquisa = NA.
#   • Conselhos municipais perguntados em ao menos 3 edições: existência e
#     funcionamento (ano de criação, paritário, deliberativo, reuniu nos
#     últimos 12 meses). Os anos sem pergunta recebem a informação da última
#     edição em que o conselho foi investigado (last observation carried
#     forward), até ser perguntado novamente. A coluna cons_<x>_ano_ref indica
#     de qual edição veio a informação.
#   • Correção pelo ano de criação: se uma edição posterior informa que o
#     conselho foi criado num ano já coberto por um "não" imputado, esses anos
#     passam a existe = 1 (cons_<x>_corrigido = 1).
#
# A correspondência entre códigos de cada edição e as variáveis harmonizadas
# está em MUNIC/harmonizacao/munic_crosswalk_rh_conselhos.csv (editável).
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(tidyverse)
library(readxl)
library(here)

# ── 2. Caminhos e parâmetros ──────────────────────────────────────────────────
dir_munic <- here("v2", "1 Bases Individuais", "MUNIC")

anos_painel <- 2004:2024

# correção pelo ano de criação (seção 8b): também preencher anos anteriores à
# primeira edição em que o conselho foi perguntado?
corrigir_antes_primeira_edicao <- FALSE

crosswalk <- read_csv(
  file.path(dir_munic, "harmonizacao", "munic_crosswalk_rh_conselhos.csv"),
  col_types = cols(.default = "c", ano = "i"),
  trim_ws = FALSE   # alguns nomes de aba terminam com espaço
)

# arquivo "Base" de cada edição (ignora .zip e as tabelas agregadas)
arquivos <- tibble(ano = list.files(dir_munic, pattern = "^\\d{4}$")) |>
  mutate(
    arquivo = map_chr(ano, \(a) {
      f <- list.files(file.path(dir_munic, a), pattern = "^(base|munic).*\\.xlsx?$",
                      ignore.case = TRUE, full.names = TRUE)
      if (length(f) == 0) NA_character_ else f[1]
    }),
    ano = as.integer(ano)
  ) |>
  filter(!is.na(arquivo))

# ── 3. Funções auxiliares ─────────────────────────────────────────────────────
normalizar <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
}

# Sim/Não → 1/0 (Recusa, Não informou, Não aplicável, "-" etc. → NA)
recode_sim_nao <- function(v) {
  case_when(
    str_detect(v, "^sim") ~ 1L,
    v == "nao"            ~ 0L,
    TRUE                  ~ NA_integer_
  )
}

# Contagens (nº de funcionários) – só números inteiros válidos
recode_numero <- function(v) {
  as.numeric(if_else(str_detect(v, "^\\d+(\\.0+)?$"), v, NA_character_))
}

recode_ano <- function(v) {
  a <- suppressWarnings(as.integer(str_extract(v, "^\\d{4}")))
  if_else(between(a, 1900L, max(anos_painel)), a, NA_integer_)
}

# 2004–2012: "Sim/Não"; 2017+: "Paritário / Tem maior representação ..."
recode_paritario <- function(v) {
  case_when(
    str_detect(v, "^sim|^(e )?paritario")    ~ 1L,
    v == "nao" | str_detect(v, "^tem maior") ~ 0L,
    TRUE                                     ~ NA_integer_
  )
}

# até 2014: "Sim/Não"; 2017+: quantidade de reuniões nos últimos 12 meses
recode_reuniao <- function(v) {
  n <- suppressWarnings(as.numeric(if_else(str_detect(v, "^\\d+$"), v, NA_character_)))
  coalesce(recode_sim_nao(v), as.integer(n > 0))
}

ler_aba <- function(arquivo, aba, codigos) {
  df <- read_excel(arquivo, sheet = aba, col_types = "text", .name_repair = "minimal")
  names(df) <- str_trim(names(df))
  # coluna do código do município: em geral a 1ª, mas não em todas as abas (ex.: 2018 Cultura)
  col_id <- str_which(names(df), regex("^(a1|codmun|cod ?munic|codigo ?munic)", ignore_case = TRUE))[1]
  if (is.na(col_id)) col_id <- 1
  names(df)[col_id] <- "id_original"
  df |>
    select(id_original, any_of(codigos)) |>
    filter(str_detect(id_original, "^\\d{6,7}")) |>
    pivot_longer(-id_original, names_to = "codigo", values_to = "valor")
}

# ── 4. Leitura das bases (só as abas/variáveis do crosswalk) ──────────────────
leituras <- crosswalk |>
  distinct(ano, aba) |>
  inner_join(arquivos, by = "ano")

munic_long <- leituras |>
  mutate(dados = pmap(list(arquivo, aba, ano), \(arquivo, aba, a) {
    message("Lendo ", a, " – ", aba)
    ler_aba(arquivo, aba, crosswalk$codigo[crosswalk$ano == a & crosswalk$aba == aba])
  })) |>
  select(ano, aba, dados) |>
  unnest(dados) |>
  inner_join(crosswalk |> select(ano, aba, codigo, bloco, conselho, variavel),
             by = c("ano", "aba", "codigo")) |>
  mutate(valor = normalizar(valor))

# conferência: toda variável do crosswalk foi encontrada?
faltando <- anti_join(crosswalk, distinct(munic_long, ano, codigo), by = c("ano", "codigo"))
if (nrow(faltando) > 0) {
  warning("Códigos do crosswalk não encontrados nas bases:\n",
          paste(faltando$ano, faltando$codigo, collapse = "\n"))
}

# ── 5. Código do município (6 dígitos até 2013 → 7 dígitos IBGE) ─────────────
municipios <- read_excel(
  arquivos$arquivo[arquivos$ano == 2024], sheet = "Recursos humanos", col_types = "text"
) |>
  transmute(id_municipio = CodMun, sigla_uf = UF, nome_municipio = Mun)

munic_long <- munic_long |>
  mutate(id6 = str_sub(id_original, 1, 6)) |>
  left_join(municipios |> transmute(id6 = str_sub(id_municipio, 1, 6), id_municipio),
            by = "id6")

sem_id <- munic_long |> filter(is.na(id_municipio)) |> distinct(ano, id_original)
if (nrow(sem_id) > 0) message(nrow(sem_id), " registros sem correspondência de código (descartados)")
munic_long <- munic_long |> filter(!is.na(id_municipio))

# ── 6. Recursos Humanos (sem imputação) ───────────────────────────────────────
rh <- munic_long |>
  filter(bloco == "recursos_humanos") |>
  mutate(valor_num = if_else(variavel == "rh_ind_existe",
                             as.numeric(recode_sim_nao(valor)),
                             recode_numero(valor))) |>
  select(id_municipio, ano, variavel, valor_num) |>
  pivot_wider(names_from = variavel, values_from = valor_num) |>
  # adm. indireta inexistente → 0 funcionários
  mutate(rh_ind_total = if_else(rh_ind_existe == 0 & is.na(rh_ind_total), 0, rh_ind_total)) |>
  relocate(id_municipio, ano, rh_dir_total, rh_dir_estatutario, rh_dir_clt,
           rh_dir_comissionado, rh_dir_estagiario, rh_dir_sem_vinculo,
           rh_ind_existe, rh_ind_total)

# ── 7. Conselhos: recodificação por edição ───────────────────────────────────
conselhos_obs <- munic_long |>
  filter(bloco == "conselhos") |>
  mutate(item = str_remove(variavel, paste0("^cons_", conselho, "_"))) |>
  mutate(valor_rec = case_when(
    item %in% c("existe", "deliberativo") ~ recode_sim_nao(valor),
    item == "ano_criacao"                 ~ recode_ano(valor),
    item == "paritario"                   ~ recode_paritario(valor),
    item == "reuniao_12m"                 ~ recode_reuniao(valor)
  )) |>
  select(id_municipio, conselho, ano, item, valor_rec) |>
  pivot_wider(names_from = item, values_from = valor_rec) |>
  # edição só conta como observação se a existência foi informada
  filter(!is.na(existe)) |>
  # funcionamento só se aplica a conselho existente
  mutate(across(c(any_of(c("ano_criacao", "paritario", "deliberativo", "reuniao_12m"))),
                \(x) if_else(existe == 1L, x, NA_integer_))) |>
  rename(ano_ref = ano)

# ── 8. Conselhos: imputação para os anos sem pergunta (LOCF) ─────────────────
# cada município-ano recebe o bloco completo da última edição observada (ano_ref <= ano)
conselhos_painel <- expand_grid(
  id_municipio = municipios$id_municipio,
  conselho     = unique(conselhos_obs$conselho),
  ano          = anos_painel
) |>
  left_join(conselhos_obs |> distinct(id_municipio, conselho, ano_ref) |> mutate(ano = ano_ref),
            by = c("id_municipio", "conselho", "ano")) |>
  group_by(id_municipio, conselho) |>
  arrange(ano, .by_group = TRUE) |>
  fill(ano_ref, .direction = "down") |>
  ungroup() |>
  left_join(conselhos_obs, by = c("id_municipio", "conselho", "ano_ref"))

# ── 8b. Correção pelo ano de criação informado em edições posteriores ────────
# Ex.: "não" em 2005 e, em 2009, "criado em 2006" → 2006–2008 passam a existe = 1.
# Só altera anos imputados (nunca uma resposta observada) e só usa o ano de
# criação informado pela PRÓXIMA edição observada.
criacao_obs <- conselhos_obs |>
  filter(existe == 1L, !is.na(ano_criacao)) |>
  select(id_municipio, conselho, ano_obs = ano_ref, ano_criacao_obs = ano_criacao)

correcoes <- conselhos_painel |>
  group_by(id_municipio, conselho) |>
  arrange(ano, .by_group = TRUE) |>
  mutate(ano_prox = if_else(ano == ano_ref, ano, NA_integer_)) |>
  fill(ano_prox, .direction = "up") |>
  ungroup() |>
  filter(ano != ano_ref | is.na(ano_ref)) |>
  filter(existe == 0L | (is.na(ano_ref) & corrigir_antes_primeira_edicao)) |>
  inner_join(criacao_obs, by = c("id_municipio", "conselho", "ano_prox" = "ano_obs")) |>
  filter(ano_criacao_obs <= ano) |>
  transmute(id_municipio, conselho, ano, ano_criacao_corr = ano_criacao_obs)

conselhos_painel <- conselhos_painel |>
  left_join(correcoes, by = c("id_municipio", "conselho", "ano")) |>
  mutate(
    corrigido   = if_else(is.na(ano_criacao_corr), 0L, 1L),
    existe      = if_else(corrigido == 1L, 1L, existe),
    ano_criacao = if_else(corrigido == 1L, ano_criacao_corr, ano_criacao),
    # funcionamento desconhecido nos anos corrigidos
    across(c(paritario, deliberativo, reuniao_12m), \(x) if_else(corrigido == 1L, NA_integer_, x)),
    corrigido   = if_else(is.na(existe), NA_integer_, corrigido)
  ) |>
  select(-ano_criacao_corr)

message(nrow(correcoes), " município-ano-conselho corrigidos pelo ano de criação")

conselhos <- conselhos_painel |>
  pivot_wider(
    id_cols     = c(id_municipio, ano),
    names_from  = conselho,
    values_from = c(existe, ano_criacao, paritario, deliberativo, reuniao_12m, ano_ref, corrigido),
    names_glue  = "cons_{conselho}_{.value}"
  ) |>
  # remove colunas inteiramente vazias (item nunca perguntado para o conselho)
  select(where(\(x) !all(is.na(x))))

# ordena as colunas por conselho: existe, ano_criacao, paritario, deliberativo, reuniao_12m, ano_ref
ordem_itens <- c("existe", "ano_criacao", "paritario", "deliberativo", "reuniao_12m", "ano_ref", "corrigido")
ordem_cols  <- expand_grid(conselho = sort(unique(conselhos_obs$conselho)), item = ordem_itens) |>
  transmute(col = paste0("cons_", conselho, "_", item)) |>
  pull(col)
conselhos <- conselhos |> select(id_municipio, ano, any_of(ordem_cols))

# ── 9. Base final município-ano ───────────────────────────────────────────────
munic_rh_conselhos <- expand_grid(id_municipio = municipios$id_municipio, ano = anos_painel) |>
  left_join(municipios, by = "id_municipio") |>
  mutate(munic_realizada = ano %in% arquivos$ano) |>
  left_join(rh, by = c("id_municipio", "ano")) |>
  left_join(conselhos, by = c("id_municipio", "ano")) |>
  arrange(id_municipio, ano)

# ── 10. Conferências rápidas ──────────────────────────────────────────────────
# % de municípios com cada conselho, por ano (após imputação)
munic_rh_conselhos |>
  group_by(ano) |>
  summarise(across(ends_with("_existe") & starts_with("cons_"), \(x) round(mean(x, na.rm = TRUE) * 100))) |>
  print(width = Inf)

# soma dos vínculos x total informado (adm. direta)
munic_rh_conselhos |>
  filter(munic_realizada) |>
  mutate(soma = rowSums(pick(rh_dir_estatutario:rh_dir_sem_vinculo), na.rm = TRUE)) |>
  group_by(ano) |>
  summarise(total_medio = mean(rh_dir_total, na.rm = TRUE),
            pct_soma_igual_total = mean(soma == rh_dir_total, na.rm = TRUE)) |>
  print()

# ── 11. Exportação ────────────────────────────────────────────────────────────
write_rds(munic_rh_conselhos, file.path(dir_munic, "munic_rh_conselhos_2004_2024.rds"), compress = "gz")
write_csv(munic_rh_conselhos, file.path(dir_munic, "munic_rh_conselhos_2004_2024.csv"), na = "")
