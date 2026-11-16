# =============================================================================
# mape_municipios — Etapa 4: Base Final
#
# Exporta o painel unificado (Etapa 3: "3 Junção e Transformação/
# painel_mape_municipios.RData") em múltiplos formatos para uso fora do R —
# RData (uso interno), parquet (leitura rápida e colunar; formato recomendado
# para o import_mape() do pacote), csv, xlsx e dta (Stata).
#
# A etiqueta de dimensão (atributo `dimensao`, aplicado na Etapa 3 em cada
# variável) é uma convenção do R e não sobrevive a formatos fora dele (csv,
# parquet, dta, xlsx apagam atributos customizados ao ler de volta). Por
# isso `dicionario_dimensoes` é exportado junto, em todo formato — é ele a
# fonte de verdade que o pacote vai usar para mapear variável -> dimensão
# fora do R.
#
# Nenhum valor monetário é deflacionado — toda variável em R$ permanece
# nominal, no valor do ano de referência (mesma convenção da Etapa 3; não é
# um passo que falta, é deliberado em todo o banco).
# =============================================================================

library(tidyverse)
library(here)
library(arrow)
library(writexl)
library(haven)

# Remova algum formato daqui se não precisar dele — "xlsx" em particular
# pode ficar pesado/lento dado o tamanho do painel (~2 mil colunas).
# [CORREÇÃO 16/11/2026] Tentado com "xlsx": o arquivo passa de 1,2 GB e o
# writexl (libxlsxwriter) falha ao fechar o zip ("ZIP_BADZIPFILE... use_zip64
# option may be required") — exatamente o problema que este comentário já
# previa. Removido de FORMATOS; parquet/csv/RData cobrem os casos de uso.
FORMATOS <- c("rdata", "parquet", "csv", "dta")

dir_final <- here("4 Base Final")
dir.create(dir_final, showWarnings = FALSE)

load(here("3 Junção e Transformação", "painel_mape_municipios.RData"))  # -> painel, dicionario_dimensoes
cat("Painel final:", nrow(painel), "linhas x", ncol(painel), "colunas\n")

# ── 1. RData (uso interno em R — preserva a etiqueta `dimensao`) ───────────
if ("rdata" %in% FORMATOS) {
  save(painel, dicionario_dimensoes, file = file.path(dir_final, "mape_municipios.RData"))
}

# ── 2. Parquet (formato recomendado para import_mape() no pacote) ─────────
if ("parquet" %in% FORMATOS) {
  write_parquet(painel, file.path(dir_final, "mape_municipios.parquet"))
  write_parquet(dicionario_dimensoes, file.path(dir_final, "dicionario_dimensoes.parquet"))
}

# ── 3. CSV ───────────────────────────────────────────────────────────────
if ("csv" %in% FORMATOS) {
  write_csv(painel, file.path(dir_final, "mape_municipios.csv"))
  write_csv(dicionario_dimensoes, file.path(dir_final, "dicionario_dimensoes.csv"))
}

# ── 4. XLSX ──────────────────────────────────────────────────────────────
# Aviso: o painel tem ~2 mil colunas x ~195 mil linhas — a exportação pode
# demorar bastante e gerar um arquivo pesado. Se isso for um problema na
# prática, remova "xlsx" de FORMATOS; parquet/csv já cobrem consulta e
# compartilhamento para a maioria dos casos de uso.
if ("xlsx" %in% FORMATOS) {
  write_xlsx(painel, file.path(dir_final, "mape_municipios.xlsx"))
}

# ── 5. DTA (Stata) ───────────────────────────────────────────────────────
# Stata permite no máximo 32 caracteres por nome de variável — várias
# variáveis do banco (ex.: dimensões Habitação, Mobilidade, Meio-Ambiente)
# são mais longas que isso. Truncar sem cuidado geraria colisões (duas
# variáveis diferentes acabando com o mesmo nome truncado); em vez disso,
# truncamos preservando unicidade dentro do limite de 32 caracteres e
# salvamos um de/para (nome original -> nome no .dta) para quem precisar
# cruzar os dois — flagging, não silencing.
if ("dta" %in% FORMATOS) {

  #' Trunca `nomes` em até `max_len` caracteres, preservando unicidade. Ao
  #' contrário de make.unique(), reserva espaço para o sufixo numérico ANTES
  #' de truncar, para o resultado nunca passar de `max_len`.
  #'
  #' [CORREÇÃO 16/11/2026] A versão anterior garantia unicidade só DENTRO de
  #' cada grupo de nomes com o mesmo prefixo de 32 caracteres (via
  #' ave(..., FUN = seq_along)), mas não conferia colisão ENTRE grupos
  #' diferentes depois de truncar+sufixar — com ~2.600 variáveis (muitas com
  #' prefixos longos repetidos, ex. "acesso_CMA*" em Mobilidade), dois nomes
  #' de grupos diferentes podiam acabar com o mesmo resultado final, e
  #' write_dta() falhava ("Nomes truncados para .dta não ficaram únicos").
  #' Reescrita para checar unicidade global: cada nome é truncado e, se já
  #' usado por QUALQUER nome anterior (do mesmo grupo ou não), tenta sufixos
  #' _1, _2, ... até achar um ainda não usado.
  #'
  #' [CORREÇÃO 16/11/2026, 2] Stata também não aceita caractere fora de
  #' letra/dígito/underscore em nome de variável — "ln.dist.capital.sd" e
  #' outras 4 colunas de distância (dimensão Identificação, nomeadas com
  #' ponto no estilo da fonte original, Kustov & Pardelli 2024) travavam
  #' write_dta() com "A provided name contains an illegal character", antes
  #' mesmo de chegar no limite de 32 caracteres. Todo caractere fora de
  #' [A-Za-z0-9_] é trocado por "_" antes de truncar.
  truncar_nomes_unicos <- function(nomes, max_len = 32) {
    nomes_sanitizados <- gsub("[^A-Za-z0-9_]", "_", nomes)
    resultado <- character(length(nomes))
    usados <- character(0)
    for (i in seq_along(nomes_sanitizados)) {
      candidato <- substr(nomes_sanitizados[i], 1, max_len)
      if (candidato %in% usados) {
        k <- 1
        repeat {
          sufixo <- paste0("_", k)
          candidato <- paste0(substr(nomes_sanitizados[i], 1, max_len - nchar(sufixo)), sufixo)
          if (!(candidato %in% usados)) break
          k <- k + 1
        }
      }
      resultado[i] <- candidato
      usados[i] <- candidato
    }
    resultado
  }

  nomes_dta <- truncar_nomes_unicos(names(painel))
  stopifnot("Nomes truncados para .dta não ficaram únicos" = !anyDuplicated(nomes_dta))

  divergentes <- tibble(nome_original = names(painel), nome_dta = nomes_dta) %>%
    filter(nome_original != nome_dta)

  if (nrow(divergentes) > 0) {
    message(
      nrow(divergentes), " nome(s) de variável truncado(s) para exportar em .dta ",
      "(limite de 32 caracteres do Stata) — ver 'de_para_nomes_dta.csv'."
    )
    write_csv(divergentes, file.path(dir_final, "de_para_nomes_dta.csv"))
  }

  painel_dta <- painel

  # [CORREÇÃO 16/11/2026, 3] NaN literal (ex.: 0/0 em alguma média da fonte
  # de Mobilidade/Acesso a Oportunidades, coluna acesso_TMIST_bicicleta,
  # linha 4025) faz write_dta() falhar ("provided tag value was outside the
  # range of allowed values") — haven só reconhece os NaN com payload
  # específico que usa para representar valores ausentes do Stata; um NaN
  # "cru" vindo de uma divisão não é um desses. Stata não distingue NaN de
  # ausente; convertido para NA antes de exportar (só aqui, no .dta — RData/
  # parquet/csv já exportados preservam o NaN original, sinalizado e não
  # alterado silenciosamente).
  n_nan <- sum(vapply(painel_dta, function(x) if (is.numeric(x)) sum(is.nan(x)) else 0L, integer(1)))
  if (n_nan > 0) {
    message(n_nan, " valor(es) NaN (não NA) encontrado(s) no painel — convertidos para NA só na ",
            "exportação .dta, que não aceita NaN. Preservados como NaN em RData/parquet/csv.")
    painel_dta[] <- lapply(painel_dta, function(x) {
      if (is.numeric(x)) { x[is.nan(x)] <- NA }
      x
    })
  }

  names(painel_dta) <- nomes_dta
  write_dta(painel_dta, file.path(dir_final, "mape_municipios.dta"))
}

cat("Exportação concluída em:", dir_final, "\n")
