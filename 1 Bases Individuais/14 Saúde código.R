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

# ── 3. IEPS Data ───────────────────────────────────────────────────────────────
here("1 Bases Individuais", "14 Saúde", "export.csv")
ieps_data <- readr::read_csv(
  here("1 Bases Individuais", "14 Saúde", "export.csv")
)

names(ieps_data)
summary(ieps_data)

# ── 4. Codebook: nomes e blocos das variáveis ─────────────────────────────────
# Ajuste o caminho abaixo para onde o codebook.xlsx estiver salvo no projeto

here("1 Bases Individuais", "14 Saúde", "codebook.xlsx")
codebook <- read_excel(
  here("1 Bases Individuais", "14 Saúde", "codebook.xlsx")
)


# Dimensões de saúde a manter
dimensoes_saude <- c(
  "Atenção Primária",
  "Mortalidade & Morbidade",
  "Recursos",
  "Saúde Suplementar",
  "Gastos"
)

codebook_saude <- codebook %>%
  filter(Bloco %in% dimensoes_saude)

# Vetor nomeado novo_nome = nome_no_export, a partir do codebook
rename_vec <- setNames(codebook_saude$`Abreviações`, codebook_saude$Variável)

# ATENÇÃO: no export, "Atendimentos médicos por 1000 hab." aparece duas vezes
# com grafias quase idênticas (espaço duplo x espaço simples), e o codebook
# mapeia atend_medico_1000hab e atend_enfermeiro_1000hab para o MESMO rótulo.
# Resolvendo manualmente pela ordem em que aparecem no export (médicos antes
# de enfermeiros, como no restante da base) — confirme se está correto:
rename_vec <- rename_vec[!names(rename_vec) %in% c("atend_medico_1000hab", "atend_enfermeiro_1000hab")]

# ATENÇÃO: o codebook tem uma única variável genérica para mortalidade
# infantil (tx_mort_inf_ibge -> "Mortalidade Infantil (Por 1.000 NVs)"),
# mas o export traz duas versões separadas (IBGE e SIM/SINASC). Mantendo
# as duas, com nomes distintos:
rename_vec <- rename_vec[names(rename_vec) != "tx_mort_inf_ibge"]

rename_vec_manual <- c(
  atend_medico_1000hab     = "Atendimentos médicos por 1000  hab.", # espaço duplo
  atend_enfermeiro_1000hab = "Atendimentos médicos por 1000 hab.",  # espaço simples
  tx_mort_inf_ibge          = "Mortalidade Infantil (Por 1.000 NVs - IBGE)",
  tx_mort_inf_sinasc        = "Mortalidade Infantil (Por 1.000 NVs - SIM/SINASC)"
)
rename_vec <- c(rename_vec, rename_vec_manual)

# Mantém só as variáveis que de fato existem no export
# (as 21 variáveis do codebook sem correspondência no export.csv - contagens
# absolutas, despesas não deflacionadas e vacinação em dia - ficam de fora)
rename_vec <- rename_vec[rename_vec %in% names(ieps_data)]

# ── 5. Limpeza do banco - só dados de saúde ───────────────────────────────────
saude <- ieps_data %>%
  select(Ano, `Código IBGE`, all_of(unname(rename_vec))) %>%
  rename(!!!rename_vec)

names(saude)

# [UNICO] "Código IBGE" do IEPS vem com 6 dígitos (sem o dígito verificador),
# não com os 7 dígitos padrão do projeto — sem esta correção, as 5.571 x 16
# linhas de "saude" nunca batiam com atencao_basica/vacinacao (ambas com
# id_municipio de 7 dígitos) nos joins abaixo, e ficavam com essas duas bases
# inteiramente NA (achado da validação: 89.136 linhas com id_municipio de 6
# dígitos na base final). Corrigido via de-para do diretório oficial do IBGE.
diretorio_6_7 <- openxlsx::read.xlsx(
  here("1 Bases Individuais", "1 Identificação", "diretorios_brasileiros.xlsx")
) %>%
  filter(ano == 2024) %>%
  select(id_municipio, id_municipio_6)

n_saude_antes <- nrow(saude)
saude <- saude %>%
  mutate(id_municipio_6 = as.character(`Código IBGE`)) %>%
  left_join(diretorio_6_7, by = "id_municipio_6")

# Confere que nenhuma linha se perdeu nem duplicou no de-para (esperado: TRUE, TRUE)
nrow(saude) == n_saude_antes
sum(is.na(saude$id_municipio)) == 0


# 6. Imunizações 

# Para carregar o dado direto no R
query <- "
SELECT
    dados.ano as ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.doses_total as doses_total,
    dados.cobertura_total as cobertura_total
FROM `basedosdados.br_ms_imunizacoes.municipio` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

vacinacao <- read_sql(query, billing_project_id = get_billing_id())


# 7. Cobertura Atenção Básica 

# Para carregar o dado direto no R
query <- "
SELECT
    dados.quantidade_equipes_saude_familia as quantidade_equipes_saude_familia,
    dados.carga_horaria_medica_atencao_basica_tradicional as carga_horaria_medica_atencao_basica_tradicional,
    dados.quantidade_equipes_atencao_basica_equivalente as quantidade_equipes_atencao_basica_equivalente,
    dados.carga_horaria_enfermagem_atencao_basica_tradicional as carga_horaria_enfermagem_atencao_basica_tradicional,
    dados.ano as ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.quantidade_equipes_atencao_basica_total as quantidade_equipes_atencao_basica_total,
    dados.mes as mes
FROM `basedosdados.br_ms_atencao_basica.municipio` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

ab <- read_sql(query, billing_project_id = get_billing_id())


names(ab)

atencao_basica <- ab %>%
  group_by(ano, id_municipio) %>%
  summarise(media_quantidade_equipes_saude_familia = mean(quantidade_equipes_saude_familia, na.rm = TRUE),
            media_quantidade_equipes_atencao_basica_equivalente = mean(quantidade_equipes_atencao_basica_equivalente, na.rm = TRUE),
            media_quantidade_equipes_atencao_basica_total = mean(quantidade_equipes_atencao_basica_total, na.rm = TRUE),
            media_carga_horaria_medica_atencao_basica_tradicional = mean(carga_horaria_medica_atencao_basica_tradicional, na.rm = TRUE),
            media_carga_horaria_enfermagem_atencao_basica_tradicional = mean(carga_horaria_enfermagem_atencao_basica_tradicional, na.rm = TRUE),
            dp_quantidade_equipes_saude_familia = sd(quantidade_equipes_saude_familia, na.rm = TRUE),
            dp_quantidade_equipes_atencao_basica_equivalente = sd(quantidade_equipes_atencao_basica_equivalente, na.rm = TRUE),
            dp_quantidade_equipes_atencao_basica_total = sd(quantidade_equipes_atencao_basica_total, na.rm = TRUE),
            dp_carga_horaria_medica_atencao_basica_tradicional = sd(carga_horaria_medica_atencao_basica_tradicional, na.rm = TRUE),
            dp_carga_horaria_enfermagem_atencao_basica_tradicional = sd(carga_horaria_enfermagem_atencao_basica_tradicional, na.rm = TRUE)) %>%
  ungroup()
  



# Juntar bases de dados


names(saude)

# [UNICO] id_municipio já vem correto (7 dígitos) do de-para feito acima;
# "Código IBGE" (6 dígitos) e id_municipio_6 (auxiliar do de-para) descartados.
saude <- saude %>%
  rename(ano = Ano) %>%
  select(-`Código IBGE`, -id_municipio_6)

names(atencao_basica)

saude$ano <- as.numeric(saude$ano)

atencao_basica$ano <- as.numeric(atencao_basica$ano)

saude_brasileira <- left_join(saude, atencao_basica, by = c("ano", "id_municipio"))

names(vacinacao)

vacinacao$ano <- as.numeric(vacinacao$ano)

saude_brasileira <- full_join(saude_brasileira, vacinacao, by = c("ano", "id_municipio"))

str(saude_brasileira)

# [UNICO] checagens do protocolo antes de exportar (não existiam no script original)
n_dup <- saude_brasileira %>% count(id_municipio, ano) %>% filter(n > 1) %>% nrow()
if (n_dup > 0) warning(n_dup, " município-ano duplicados na base final — investigar antes de exportar")

n_7digitos <- sum(nchar(saude_brasileira$id_municipio) != 7, na.rm = TRUE)
if (n_7digitos > 0) warning(n_7digitos, " id_municipio sem 7 dígitos — investigar")

dim(saude_brasileira)
range(saude_brasileira$ano, na.rm = TRUE)


# ── 8. Exportar .xlsx ──────────────────────────────────────────────────────────
dir.create(here("1 Bases Individuais", "14 Saúde"), showWarnings = FALSE)

write_xlsx(
  saude_brasileira,
  path = here("1 Bases Individuais", "14 Saúde", "saude_brasileira.xlsx")
)

# ── Validação padronizada (2 Validação/14 Saúde/) ────────────────────────────
validar_dimensao(saude_brasileira, n_dim = 14)









