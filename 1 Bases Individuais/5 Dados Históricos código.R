# =============================================================================
# mape_municipios — 16 Dados Históricos
# Ano de primeira aparição no Censo (proxy de fundação) dos 5.570 municípios
#
# Fontes:
#   (1) IBGE (2011). Evolução da divisão territorial do Brasil 1872–2010
#       -> planilha genealógica: IBGE_1872_2010_atualizado.xlsx (arquivo local)
#   (2) Ehrl (2017). Minimum comparable areas for the period 1872–2010.
#       Estudos Econômicos 47(1). -> via Base dos Dados (br_ibge_amc.municipio_de_para)
#   (3) Diretório de municípios da Base dos Dados (esqueleto de 5.570)
#
# Lógica de leitura das células da planilha IBGE (Ehrl 2017, seção 2):
#   - nome do município ............ município existe naquele censo
#   - "desmembrado de X" ........... criado ENTRE este censo e o seguinte
#                                    (ainda não existe neste censo)
#   - "anexado a X" ................ extinto/absorvido (não existe)
#   - "sede em X" .................. unidade existe, mas com sede em outra localidade
#   - "litígio ..." / "contestado" . território em disputa (não existe como município)
#   - "*" .......................... incerteza reconhecida pelo próprio IBGE
# =============================================================================

library(tidyverse)
library(basedosdados)
library(openxlsx)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(stringi)
library(writexl)

# -----------------------------------------------------------------------------
# 0. Caminhos e parâmetros
# -----------------------------------------------------------------------------

# Entrada: ajuste para a pasta onde está a planilha do IBGE no Drive
dir_entrada <- here("1 Bases Individuais", "5 Dados Históricos", "dados_historicos")
arq_ibge    <- file.path(dir_entrada, "IBGE_1872_2010_atualizado.xlsx")

stopifnot("Planilha IBGE não encontrada" = file.exists(arq_ibge))

set_billing_id("dados-importacao")

anos_censo <- c(1872, 1900, 1911, 1920, 1933, 1940, 1950,
                1960, 1970, 1980, 1991, 2000, 2010)

# Primeiro valor não-NA; devolve NA (e não vetor vazio) quando não há nenhum.
# Evita o erro "must be size 1, not 0" de first(na.omit(x)) em versões recentes do dplyr
primeiro_valido <- function(x) x[!is.na(x)][1]

# Normalização de nomes para matching
norm_nome <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> str_to_lower() |> str_squish()
}

# -----------------------------------------------------------------------------
# 1. Esqueleto: 5.570 municípios (Base dos Dados)
# -----------------------------------------------------------------------------

diretorio <- read_sql(
  "SELECT id_municipio, nome, sigla_uf
   FROM `basedosdados.br_bd_diretorios_brasil.municipio`",
  billing_project_id = get_billing_id()
) |>
  mutate(id_municipio = as.integer(as.character(id_municipio)))  # evita integer64

# -----------------------------------------------------------------------------
# 2. Planilha genealógica do IBGE (1872–2010)
# -----------------------------------------------------------------------------

ibge_raw <- read.xlsx(arq_ibge) |>
  mutate(id_municipio = as.integer(id_municipio))

cat("Linhas na planilha IBGE:", nrow(ibge_raw),
    "| ids únicos:", n_distinct(ibge_raw$id_municipio), "\n")

# 2.1 Formato longo (id × censo)
ibge_long <- ibge_raw |>
  select(id_municipio, mun_name, starts_with("mun1"), starts_with("mun2")) |>
  pivot_longer(matches("^mun\\d{4}$"), names_to = "ano", values_to = "celula",
               names_prefix = "mun", names_transform = as.integer) |>
  mutate(celula = na_if(str_squish(celula), ""))

# 2.2 Duplicatas: alguns ids aparecem em 2 linhas (quase todos em TO: uma linha
#     cobre o período em GO até 1980 e outra o período em TO a partir de 1991).
#     Ehrl (2017, nota 4): informação não conflitante -> fundir as linhas.
ibge_long <- ibge_long |>
  group_by(id_municipio, ano) |>
  summarise(
    mun_name        = primeiro_valido(str_squish(mun_name)),
    n_valores       = n_distinct(na.omit(celula)),
    celula          = primeiro_valido(celula),
    .groups = "drop"
  )

conflitos_dup <- ibge_long |> filter(n_valores > 1)
cat("Células com conflito entre linhas duplicadas:", nrow(conflitos_dup), "\n")

# 2.3 Classificação do status de cada célula
ibge_long <- ibge_long |>
  mutate(
    incerteza = str_detect(coalesce(celula, ""), fixed("*")),
    cel       = str_squish(str_remove_all(celula, fixed("*"))),
    status = case_when(
      is.na(cel)                                                  ~ "inexistente",
      str_detect(cel, regex("^(lit[ií]gio|contestado)", TRUE))    ~ "litigio",
      str_detect(cel, regex("^sede em", TRUE)) &
        str_detect(cel, regex("desmembrad", TRUE))                ~ "desmembrado",
      str_detect(cel, regex("^sede em", TRUE))                    ~ "sede_outra",
      str_detect(cel, regex("^anexad", TRUE))                     ~ "anexado",
      str_detect(cel, regex("^desmembrad", TRUE))                 ~ "desmembrado",
      TRUE                                                        ~ "existe"
    )
  ) |>
  arrange(id_municipio, ano)

print(count(ibge_long, status))

# -----------------------------------------------------------------------------
# 3. Variáveis de fundação por município
# -----------------------------------------------------------------------------

censo_anterior <- function(a) {
  i <- match(a, anos_censo)
  ifelse(is.na(i) | i == 1, NA_integer_, anos_censo[pmax(i - 1, 1)])
}

fundacao <- ibge_long |>
  group_by(id_municipio) |>
  summarise(
    mun_name = first(mun_name),
    
    # Principal: primeiro censo em que aparece com nome próprio
    ano_primeiro_censo = suppressWarnings(min(ano[status == "existe"])),
    
    # Alternativa ampla: conta também "sede em outra localidade"
    ano_primeiro_censo_amplo =
      suppressWarnings(min(ano[status %in% c("existe", "sede_outra")])),
    
    # Início do período contínuo de existência que chega a 2010
    ult_interrupcao = suppressWarnings(max(ano[status != "existe"])),
    existe_2010     = any(ano == 2010 & status == "existe"),
    
    nome_primeiro_censo = primeiro_valido(cel[status == "existe"]),
    n_censos_existente  = sum(status == "existe"),
    flag_incerteza_ibge = any(incerteza),
    flag_litigio        = any(status == "litigio"),
    flag_conflito_dup   = any(n_valores > 1),
    
    # guarda a sequência de status para auditoria
    trajetoria = paste0(ano, ":", status, collapse = " | "),
    .groups = "drop"
  ) |>
  mutate(
    across(c(ano_primeiro_censo, ano_primeiro_censo_amplo),
           ~ if_else(is.infinite(.x), NA_integer_, as.integer(.x))),
    ano_instalacao_atual = case_when(
      !existe_2010                     ~ NA_integer_,
      is.infinite(ult_interrupcao)     ~ ano_primeiro_censo,
      TRUE ~ as.integer(anos_censo[match(ult_interrupcao, anos_censo) + 1])
    ),
    flag_reinstalado  = !is.na(ano_instalacao_atual) &
      ano_instalacao_atual > ano_primeiro_censo,
    flag_sede_outra   = !is.na(ano_primeiro_censo_amplo) &
      ano_primeiro_censo_amplo < ano_primeiro_censo,
    ano_censo_anterior = censo_anterior(ano_primeiro_censo),
    extinto            = str_sub(id_municipio, 3, 5) == "999"
  ) |>
  select(-ult_interrupcao)

# 3.1 Origem (município-mãe) = célula do censo anterior à primeira aparição
origem <- ibge_long |>
  filter(status == "desmembrado") |>
  semi_join(fundacao |> select(id_municipio, ano = ano_censo_anterior),
            by = c("id_municipio", "ano")) |>
  transmute(
    id_municipio,
    desmembrado_de = cel |>
      str_remove(regex("^.*?desmembrad[oa]s? d[eoa]s?\\s+", TRUE)) |>
      str_squish()
  )

fundacao <- fundacao |>
  left_join(origem, by = "id_municipio") |>
  mutate(n_origens = if_else(
    is.na(desmembrado_de), NA_integer_,
    str_count(desmembrado_de, ",|\\s+e\\s+") + 1L
  ))

# 3.2 Comparação com a estimativa original da planilha (flag, não correção silenciosa)
estim_orig <- ibge_raw |>
  group_by(id_municipio) |>
  summarise(estimativa_ano_fundacao_original = min(estimativa_ano_fundacao, na.rm = TRUE),
            .groups = "drop")

fundacao <- fundacao |>
  left_join(estim_orig, by = "id_municipio") |>
  mutate(flag_diverge_original =
           coalesce(estimativa_ano_fundacao_original != ano_primeiro_censo, TRUE))

cat("Divergências com a estimativa original:",
    sum(fundacao$flag_diverge_original & !fundacao$extinto), "\n")

# -----------------------------------------------------------------------------
# 4. Municípios criados depois de 2010 (ausentes da planilha IBGE)
#    Instalados em 2013; primeira aparição em Censo = 2022
# -----------------------------------------------------------------------------

novos <- diretorio |> anti_join(fundacao, by = "id_municipio")
cat("Municípios do diretório ausentes da planilha IBGE:", nrow(novos), "\n")
print(novos)

# Municípios-mãe (conferir nas leis de criação)
#  - 5 instalados em 2013 -> primeira aparição em Censo = 2022
#  - Boa Esperança do Norte (MT): Lei estadual 7.264/2000, validada pelo STF
#    (ADPF 819, 2023), instalada em 01/01/2025 -> ainda não recenseada
maes_pos2010 <- tribble(
  ~nome_novo,               ~uf,  ~ano_instalacao, ~ano_primeiro_censo, ~maes,
  "Mojuí dos Campos",       "PA", 2013L,           2022L,               "Santarém",
  "Pescaria Brava",         "SC", 2013L,           2022L,               "Laguna",
  "Balneário Rincão",       "SC", 2013L,           2022L,               "Içara",
  "Pinto Bandeira",         "RS", 2013L,           2022L,               "Bento Gonçalves",
  "Paraíso das Águas",      "MS", 2013L,           2022L,               "Água Clara; Costa Rica; Chapadão do Sul",
  "Boa Esperança do Norte", "MT", 2025L,           NA_integer_,         "Sorriso; Nova Ubiratã"
)

novos <- novos |>
  mutate(chave = norm_nome(nome)) |>
  left_join(maes_pos2010 |> mutate(chave = norm_nome(nome_novo)),
            by = c("chave", "sigla_uf" = "uf"))

if (any(is.na(novos$maes))) {
  warning("Há municípios ausentes da planilha IBGE sem origem cadastrada: ",
          paste(novos$nome[is.na(novos$maes)], collapse = ", "))
}

fundacao_novos <- novos |>
  transmute(
    id_municipio,
    mun_name                 = nome,
    ano_primeiro_censo,                                   # NA se ainda não recenseado
    ano_primeiro_censo_amplo = ano_primeiro_censo,
    ano_instalacao_atual     = ano_instalacao,
    ano_censo_anterior       = if_else(is.na(ano_primeiro_censo), 2022L, 2010L),
    nome_primeiro_censo      = if_else(is.na(ano_primeiro_censo), NA_character_, nome),
    desmembrado_de           = str_replace_all(maes, ";", ","),
    n_origens                = str_count(maes, ";") + 1L,
    extinto = FALSE, flag_reinstalado = FALSE, flag_sede_outra = FALSE,
    flag_incerteza_ibge = FALSE, flag_litigio = FALSE, flag_conflito_dup = FALSE,
    flag_diverge_original = NA,
    fonte_fundacao = paste0("Manual: instalado em ", ano_instalacao,
                            ", fora de IBGE (2011)")
  )

fundacao <- fundacao |>
  mutate(fonte_fundacao = "IBGE (2011) Evolução da divisão territorial 1872-2010") |>
  bind_rows(fundacao_novos)

# -----------------------------------------------------------------------------
# 5. Áreas Mínimas Comparáveis (Ehrl 2017) — Base dos Dados
#    Uma coluna de AMC por ano inicial, sempre com ano final 2010
# -----------------------------------------------------------------------------

amc <- read_sql(
  "SELECT
      CAST(dados.id_amc AS STRING)       AS id_amc,
      dados.ano_de                       AS ano_de,
      dados.ano_para                     AS ano_para,
      CAST(dados.id_municipio AS STRING) AS id_municipio
   FROM `basedosdados.br_ibge_amc.municipio_de_para` AS dados
   WHERE dados.ano_para = 2010",
  billing_project_id = get_billing_id()
) |>
  mutate(id_municipio = as.integer(id_municipio),
         ano_de       = as.integer(as.character(ano_de)),
         ano_para     = as.integer(as.character(ano_para)))

cat("Pares (ano_de, ano_para) disponíveis:\n"); print(count(amc, ano_de, ano_para))

dup_amc <- amc |> count(ano_de, id_municipio) |> filter(n > 1)
if (nrow(dup_amc) > 0) warning("id_municipio repetido dentro de um período AMC: ", nrow(dup_amc))

amc_wide <- amc |>
  distinct(id_municipio, ano_de, id_amc) |>
  pivot_wider(id_cols = id_municipio, names_from = ano_de,
              values_from = id_amc, names_glue = "id_amc_{ano_de}_2010")

# 5.1 AMC dos municípios pós-2010: herdam a AMC das mães quando todas coincidem
maes_long <- fundacao_novos |>
  select(id_municipio, mae = desmembrado_de) |>
  left_join(novos |> select(id_municipio, sigla_uf), by = "id_municipio") |>
  separate_longer_delim(mae, delim = ",") |>
  mutate(chave = norm_nome(mae)) |>
  left_join(diretorio |> transmute(id_mae = id_municipio, sigla_uf, chave = norm_nome(nome)),
            by = c("sigla_uf", "chave"))

stopifnot("Município-mãe não encontrado no diretório" = !any(is.na(maes_long$id_mae)))

amc_novos <- maes_long |>
  select(id_municipio, id_mae) |>
  left_join(amc_wide, by = c("id_mae" = "id_municipio")) |>
  group_by(id_municipio) |>
  summarise(across(starts_with("id_amc_"),
                   ~ if (n_distinct(.x) == 1) first(.x) else NA_character_),
            .groups = "drop")

amc_wide <- amc_wide |>
  anti_join(amc_novos, by = "id_municipio") |>
  bind_rows(amc_novos)

# -----------------------------------------------------------------------------
# 6. Base final: esqueleto de 5.570 + fundação + AMC
# -----------------------------------------------------------------------------

stopifnot(class(diretorio$id_municipio) == class(fundacao$id_municipio),
          class(diretorio$id_municipio) == class(amc_wide$id_municipio))

historia_brasileira <- diretorio |>
  rename(nome_municipio = nome) |>
  left_join(fundacao |> filter(!extinto) |> select(-extinto, -mun_name),
            by = "id_municipio") |>
  left_join(amc_wide, by = "id_municipio") |>
  mutate(
    anos_desde_primeiro_censo_2010 =
      if_else(ano_primeiro_censo <= 2010, 2010L - ano_primeiro_censo, NA_integer_),
    decada_primeiro_censo = paste0(ano_primeiro_censo %/% 10 * 10, "s")
  ) |>
  relocate(id_municipio, nome_municipio, sigla_uf,
           ano_primeiro_censo, ano_censo_anterior, ano_primeiro_censo_amplo,
           ano_instalacao_atual, nome_primeiro_censo, desmembrado_de, n_origens,
           anos_desde_primeiro_censo_2010, decada_primeiro_censo,
           starts_with("id_amc_"), starts_with("flag_"),
           estimativa_ano_fundacao_original, fonte_fundacao, trajetoria)

# Checagens finais
stopifnot(nrow(historia_brasileira) == 5571,
          !anyDuplicated(historia_brasileira$id_municipio))
cat("NA em ano_primeiro_censo:", sum(is.na(historia_brasileira$ano_primeiro_censo)), "\n")
print(count(historia_brasileira, ano_primeiro_censo))

# Municípios extintos (código xx999xx) — úteis para reconstruir AMCs, fora do painel
municipios_extintos <- fundacao |>
  filter(extinto) |>
  select(id_municipio, mun_name, ano_primeiro_censo, trajetoria)

### Dados 

names(historia_brasileira)

### Capacidade estatal 

dir_entrada <- here("1 Bases Individuais", "5 Dados Históricos", "dados_historicos")
arq_capacidade    <- file.path(dir_entrada, "data.csv")

capacidade_raw <- read.csv(arq_capacidade) |>
  mutate(id_municipio = as.integer(muncode))

names(capacidade_raw)

capacidade <- capacidade_raw %>%
  select(id_municipio, ln.admpub.1920.sd, ln.forcapub.1920.sd,
         ln.rail1920.sd, ln.pc.receita1920.sd)


historia_brasileira <- left_join(historia_brasileira, capacidade, by = "id_municipio")


# -----------------------------------------------------------------------------
# 7. Exportação
# -----------------------------------------------------------------------------

dir.create(here("1 Bases Individuais", "5 Dados Históricos"), showWarnings = FALSE)

# writexl::write_xlsx() em vez de openxlsx::write.xlsx(): ver nota em
# "4 Corrupção e Transparência código.R" sobre a referência interna quebrada
# que o openxlsx (4.2.8.1) grava com write.xlsx().
write_xlsx(
  historia_brasileira,
  path = here("1 Bases Individuais", "5 Dados Históricos", "historia_brasileira.xlsx")
)

# ── 8. Validação padronizada (2 Validação/5 Dados Históricos/) ──────────────
# Base cross-sectional (uma linha por município, sem coluna de ano).
validar_dimensao(historia_brasileira, n_dim = 5, ano = NULL)