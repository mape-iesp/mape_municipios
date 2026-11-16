# =============================================================================
# Funções compartilhadas pelos 17 códigos de dimensão (1 Bases Individuais)
# =============================================================================
#
# Fonte única da validação padronizada de cada dimensão, gravada em
# "2 Validação/<n> <Dimensão>/". Antes desta rodada, cada dimensão era
# validada manualmente (pessoas diferentes, formatos diferentes — ver
# inconsistência da pasta "6 Economia", sem heatmap e com nome de arquivo
# fora do padrão). validar_dimensao() substitui esse passo manual.
#
# Uso, no fim de cada "<n> <Dimensão> código.R", depois do write_xlsx()/save()
# final:
#   source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
#   validar_dimensao(minha_base_final, n_dim = 14)
#
# Para dimensões que exportam mais de um arquivo final (ex. Eleições: gerais
# e municipais), chamar uma vez por arquivo com `sufixo`:
#   validar_dimensao(eleicoes_gerais, n_dim = 8, sufixo = "gerais")
#   validar_dimensao(eleicoes_municipais, n_dim = 8, sufixo = "municipais")
#
# Para a dimensão cross-sectional (Dados Históricos, uma linha por
# município, sem coluna de ano): validar_dimensao(df, n_dim = 5, ano = NULL)
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# Slugs canônicos por dimensão — os mesmos usados em
# "3 Junção e Transformação código.R" (ver Relatório de Decisões - Etapas 3 e
# 4, seção 2, coluna "Slug (etiqueta)") para que os nomes de arquivo de
# validação fiquem consistentes com a etiqueta `dimensao` que o painel final
# e o futuro pacote usam. Antes desta padronização, os nomes de arquivo de
# "2 Validação" usavam abreviações diferentes (ex. "recursos_humanos" em vez
# de "rh", "mobilidade" em vez de "transportes_mobilidade") — mais uma
# variante de nome que este arquivo elimina.
.MAPE_DIMENSOES <- list(
  "1"  = list(pasta = "1 Identificação",                         slug = "identificacao"),
  "2"  = list(pasta = "2 População",                              slug = "populacao"),
  "3"  = list(pasta = "3 Assistência Social e Direitos Humanos",  slug = "assistencia_dh"),
  "4"  = list(pasta = "4 Corrupção e Transparência",              slug = "corrupcao_transparencia"),
  "5"  = list(pasta = "5 Dados Históricos",                       slug = "historia"),
  "6"  = list(pasta = "6 Economia",                               slug = "economia"),
  "7"  = list(pasta = "7 Educação",                               slug = "educacao"),
  "8"  = list(pasta = "8 Eleições e Partidos",                    slug = "eleicao_partidos"),
  "9"  = list(pasta = "9 Energia e Internet",                     slug = "energia_internet"),
  "10" = list(pasta = "10 Finanças",                              slug = "financas"),
  "11" = list(pasta = "11 Habitação e Zoneamento",                slug = "habitacao_zoneamento"),
  "12" = list(pasta = "12 Meio-Ambiente",                         slug = "meio_ambiente"),
  "13" = list(pasta = "13 Recursos Humanos",                      slug = "rh"),
  "14" = list(pasta = "14 Saúde",                                 slug = "saude"),
  "15" = list(pasta = "15 Segurança",                             slug = "seguranca"),
  "16" = list(pasta = "16 Sociedade e Participação",              slug = "sociedade_participacao"),
  "17" = list(pasta = "17 Mobilidade",                            slug = "transportes_mobilidade")
)

#' Valida e exporta os artefatos padrão de uma dimensão em "2 Validação/"
#'
#' @param df data.frame/data.table com a base final da dimensão (ou uma das
#'   bases, quando a dimensão exporta mais de um arquivo — ver `sufixo`).
#' @param n_dim "1".."17" (chave de .MAPE_DIMENSOES; aceita número ou string).
#' @param id nome da coluna de município (padrão "id_municipio").
#' @param ano nome da coluna de ano; NULL para base cross-sectional (sem
#'   heatmap por ano — só o %NA geral por variável).
#' @param sufixo sufixo opcional no nome do arquivo, para dimensões com mais
#'   de uma base final (ex. "gerais"/"municipais" em Eleições e Partidos).
#' @return lista invisível: duplicatas, resumo_na (data.frame) e vars_100_na.
validar_dimensao <- function(df, n_dim, id = "id_municipio", ano = "ano", sufixo = NULL) {
  n_dim <- as.character(n_dim)
  info <- .MAPE_DIMENSOES[[n_dim]]
  if (is.null(info)) stop("n_dim desconhecido: '", n_dim, "'. Use \"1\" a \"17\".", call. = FALSE)

  slug <- if (is.null(sufixo)) info$slug else paste0(info$slug, "_", sufixo)
  dir_valid <- here::here("2 Validação", info$pasta)
  dir.create(dir_valid, showWarnings = FALSE, recursive = TRUE)

  df <- as.data.frame(df)

  cat("\n========== Validação:", n_dim, "-", info$pasta,
      if (!is.null(sufixo)) paste0("(", sufixo, ")") else "", "==========\n")

  # 1. Tipos e duplicidade de id_municipio [x ano] --------------------------
  if (id %in% names(df)) {
    cat("class(", id, "): ", class(df[[id]]),
        " | 7 dígitos: ", all(nchar(as.character(df[[id]])) == 7, na.rm = TRUE), "\n", sep = "")
  } else {
    cat("AVISO: coluna '", id, "' não encontrada na base — checagens de id puladas.\n", sep = "")
  }

  dup <- NULL
  if (!is.null(ano) && id %in% names(df) && ano %in% names(df)) {
    cat("class(", ano, "): ", class(df[[ano]]), "\n", sep = "")
    dup <- df %>% dplyr::count(.data[[id]], .data[[ano]]) %>% dplyr::filter(n > 1)
    cat("Linhas ", id, " x ", ano, " duplicadas: ", nrow(dup), "\n", sep = "")
  } else if (id %in% names(df)) {
    dup <- df %>% dplyr::count(.data[[id]]) %>% dplyr::filter(n > 1)
    cat("Linhas ", id, " duplicadas (base cross-sectional): ", nrow(dup), "\n", sep = "")
  }
  if (!is.null(dup) && nrow(dup) > 0) {
    cat("AVISO: há duplicidade — conferir antes de considerar a dimensão validada.\n")
  }

  # 2. % de NA por variável (geral) ------------------------------------------
  resumo_na <- data.frame(
    variavel = names(df),
    pct_na   = vapply(df, function(x) round(100 * mean(is.na(x)), 2), numeric(1)),
    row.names = NULL
  )
  resumo_na <- resumo_na[order(-resumo_na$pct_na), ]

  arq_resumo <- file.path(dir_valid, paste0(n_dim, "_", slug, "_resumo_na.csv"))
  write.csv(resumo_na, arq_resumo, row.names = FALSE)
  cat("Resumo de NA salvo em:", arq_resumo, "\n")

  vars_100 <- resumo_na$variavel[resumo_na$pct_na == 100]
  cat("Variáveis 100% NA:", length(vars_100), "\n")
  if (length(vars_100) > 0) cat(" -", paste(vars_100, collapse = ", "), "\n")

  # 3. Heatmap ano x variável (só para bases em painel, com coluna de ano) ---
  if (!is.null(ano) && ano %in% names(df)) {
    na_long <- df %>%
      dplyr::group_by(.data[[ano]]) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(is.na(.x))), .groups = "drop") %>%
      tidyr::pivot_longer(-dplyr::all_of(ano), names_to = "variavel", values_to = "pct_na")

    g <- ggplot2::ggplot(na_long, ggplot2::aes(x = factor(.data[[ano]]), y = variavel, fill = pct_na)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "white", high = "firebrick", limits = c(0, 1), name = "% NA") +
      ggplot2::labs(x = ano, y = NULL,
                    title = paste("Cobertura —", info$pasta,
                                   if (!is.null(sufixo)) paste0("(", sufixo, ")") else "")) +
      ggplot2::theme_minimal(base_size = 7) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

    n_vars <- dplyr::n_distinct(na_long$variavel)
    arq_heatmap <- file.path(dir_valid, paste0(n_dim, "_", slug, "_heatmap_na.png"))
    ggplot2::ggsave(arq_heatmap, g, width = 10, height = max(6, n_vars * 0.12), limitsize = FALSE)
    cat("Heatmap salvo em:", arq_heatmap, "\n")
  } else {
    cat("Base cross-sectional (sem coluna de ano) — heatmap por ano não gerado.\n")
  }

  invisible(list(duplicatas = dup, resumo_na = resumo_na, vars_100_na = vars_100))
}

#' Roda basedosdados::read_sql() com retentativa em caso de erro transitório
#' de rede/backend do BigQuery (ex. "[backendError]" em
#' bigrquery::bq_table_download() -> curl::multi_run()). Observado em
#' algumas sessões, sobretudo com várias consultas seguidas no mesmo
#' processo R; uma nova tentativa, com uma pequena espera, normalmente
#' resolve (não é um problema do código da consulta em si).
#'
#' @param query string com o SQL a rodar.
#' @param billing_project_id ver basedosdados::get_billing_id().
#' @param tentativas número máximo de tentativas (padrão 3).
#' @param espera_seg segundos de espera entre tentativas (padrão 15).
read_sql_retry <- function(query, billing_project_id, tentativas = 3, espera_seg = 15) {
  for (i in seq_len(tentativas)) {
    resultado <- tryCatch(
      basedosdados::read_sql(query, billing_project_id = billing_project_id),
      error = function(e) e
    )
    if (!inherits(resultado, "error")) return(resultado)
    cat("read_sql_retry: tentativa", i, "de", tentativas, "falhou (",
        conditionMessage(resultado), ") — ",
        if (i < tentativas) paste("tentando de novo em", espera_seg, "s...") else "desistindo.",
        "\n")
    if (i < tentativas) Sys.sleep(espera_seg)
  }
  stop("read_sql_retry: todas as ", tentativas, " tentativas falharam para a consulta:\n", query, call. = FALSE)
}

#' Remove artefatos de validação com o nome antigo (fora do padrão atual),
#' quando a dimensão usava heatmap_na_<n>_<slug>.png ou outro nome ad hoc.
#' Chamar manualmente uma vez por dimensão, depois de confirmar que o novo
#' arquivo (<n>_<slug>_heatmap_na.png) foi gerado com sucesso.
limpar_validacao_antiga <- function(n_dim, arquivos_antigos) {
  n_dim <- as.character(n_dim)
  info <- .MAPE_DIMENSOES[[n_dim]]
  dir_valid <- here::here("2 Validação", info$pasta)
  for (f in arquivos_antigos) {
    p <- file.path(dir_valid, f)
    if (file.exists(p)) {
      file.remove(p)
      cat("Removido artefato de validação antigo:", p, "\n")
    }
  }
}
