# =============================================================================
# Gera "4 Base Final/dicionario.xlsx" — dicionário plano (uma linha por
# variável, todas as dimensões, com dimensao_slug) a partir de:
#   - as 17 abas de dimensão de "mape_municipios METADADOS.xlsx";
#   - o crosswalk variável -> dimensão de "dicionario_dimensoes.csv"
#     (gerado por "3 Junção e Transformação código.R").
#
# É esse arquivo — não o METADADOS.xlsx como está hoje, com 17 abas
# separadas — que import_mape_dictionary() (5 Pacote/mapemunicipios/
# R/import.R) vai ler. Ver 5 Pacote/NOTA_DECISAO_FONTE_DADOS.md.
#
# Sem as colunas Responsável/Validação (nomes de pessoas): ficam só no
# METADADOS.xlsx, que não é distribuído com o pacote.
#
# Rodar de novo sempre que o METADADOS ou "3 Junção e Transformação" for
# atualizado.
# =============================================================================

library(here)
library(readxl)
library(dplyr)

meta_file <- here("mape_municipios METADADOS.xlsx")

# Aba do METADADOS -> slug canônico (mesmos de .mape_dimensions() em
# "5 Pacote/mapemunicipios/R/utils.R" e de `dimensoes` em
# "3 Junção e Transformação código.R").
abas_slug <- c(
  "1 Identificação"                 = "identificacao",
  "2 População"                     = "populacao",
  "3 AS e DH"                       = "assistencia_dh",
  "4 Corrupção e Transparência"     = "corrupcao_transparencia",
  "5 Dados Históricos"              = "historia",
  "6 Economia"                      = "economia",
  "7 Educação"                      = "educacao",
  "8 Eleições e Partidos - Gerais"  = "eleicao_partidos",
  "8 Eleições e Partidos - Municip" = "eleicao_partidos",
  "9 Energia e Internet"            = "energia_internet",
  "10 Finanças"                     = "financas",
  "11 Habitação e Zoneamento"       = "habitacao_zoneamento",
  "12 Meio-Ambiente"                = "meio_ambiente",
  "13 Recursos Humanos"             = "rh",
  "14 Saúde"                        = "saude",
  "15 Segurança"                    = "seguranca",
  "16 Sociedade e Participação"     = "sociedade_participacao",
  "17 Mobilidade e Transportes"     = "transportes_mobilidade"
)

cols_esperadas <- c("Dimensão", "Variável", "Descrição", "Escopo Temporal", "Natureza",
                     "Objetivo", "Transformações", "Tipo", "Fonte Original",
                     "Fonte Extração", "Link", "Responsável", "Nome do arquivo", "Validação")

ler_aba <- function(aba) {
  df <- read_excel(meta_file, sheet = aba, col_types = "text")
  faltando <- setdiff(cols_esperadas, names(df))
  for (c in faltando) df[[c]] <- NA_character_
  df <- df[cols_esperadas]
  df$aba_origem <- aba
  df$dimensao_slug_aba <- abas_slug[[aba]]
  df
}

dicionario <- bind_rows(lapply(names(abas_slug), ler_aba)) %>%
  filter(!is.na(Variável))

cat("Total de linhas empilhadas (17 abas):", nrow(dicionario), "\n")
cat("Variáveis únicas:", n_distinct(dicionario$Variável), "\n")

# Crosswalk variável -> dimensão REALMENTE usada no painel final. Identificação
# e População não entram nele (não são "dimensão de política" filtrável) —
# ficam com o slug já atribuído pela própria aba de origem.
crosswalk <- read.csv(here("4 Base Final", "dicionario_dimensoes.csv"),
                      stringsAsFactors = FALSE, encoding = "UTF-8")
cat("\nLinhas no crosswalk (dicionario_dimensoes.csv):", nrow(crosswalk), "\n")

dicionario <- dicionario %>%
  left_join(crosswalk %>% select(variavel, dimensao, dimensao_nome),
            by = c("Variável" = "variavel")) %>%
  mutate(
    dimensao_slug = coalesce(dimensao, dimensao_slug_aba),
    # id_municipio/ano se repetem em toda aba (são a chave de junção); o
    # slug é sempre o de Identificação/População, nunca o de outra
    # dimensão que porventura tenha repetido a coluna no METADADOS.
    dimensao_slug = case_when(
      Variável == "id_municipio" ~ "identificacao",
      Variável == "ano"          ~ "populacao",
      TRUE                       ~ dimensao_slug
    )
  ) %>%
  select(-dimensao, -dimensao_nome, -dimensao_slug_aba) %>%
  rename(
    dimensao        = Dimensão,
    variavel        = Variável,
    descricao       = Descrição,
    escopo_temporal = `Escopo Temporal`,
    natureza        = Natureza,
    objetivo        = Objetivo,
    transformacoes  = Transformações,
    tipo            = Tipo,
    fonte_original  = `Fonte Original`,
    fonte_extracao  = `Fonte Extração`,
    link            = Link,
    responsavel     = Responsável,
    nome_arquivo    = `Nome do arquivo`,
    validacao       = Validação
  )

n_sem_slug <- sum(is.na(dicionario$dimensao_slug))
cat("\nLinhas sem dimensao_slug encontrado (deveria ser 0):", n_sem_slug, "\n")
if (n_sem_slug > 0) {
  print(dicionario %>% filter(is.na(dimensao_slug)) %>% distinct(variavel, dimensao, aba_origem))
}

cat("\nVariáveis em mais de uma aba (esperado nas colunas compartilhadas,\n",
    "ex. sigla_uf, nome, id_municipio_nome — sinalizado, não é erro):\n", sep = "")
dup <- dicionario %>% count(variavel) %>% filter(n > 1) %>% arrange(desc(n))
print(dup, n = 20)

# Sem nomes de pessoas no arquivo distribuído com o pacote.
dicionario <- dicionario %>% select(-responsavel, -validacao)

writexl::write_xlsx(dicionario, here("4 Base Final", "dicionario.xlsx"))
cat("\nSalvo:", here("4 Base Final", "dicionario.xlsx"), "\n")
cat("Total de linhas:", nrow(dicionario), "| colunas:", ncol(dicionario), "\n")
