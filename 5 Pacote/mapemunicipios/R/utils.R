# =============================================================================
# Funcoes internas compartilhadas (nao exportadas)
# =============================================================================

#' @importFrom rlang .data :=
#' @importFrom dplyr %>%
NULL

utils::globalVariables(c(".ref", ".x_", ".grupo_n"))

# -----------------------------------------------------------------------------
# Tabela de dimensoes
#   slug     : nome usado nas funcoes (import_mape_dimension("saude"))
#   nome     : nome por extenso
#   aliases  : nomes aceitos na coluna Dimensao do dicionario
# -----------------------------------------------------------------------------
.mape_dimensions <- function() {
  data.frame(
    slug = c("identificacao", "populacao", "assistencia_dh", "corrupcao_transparencia",
             "historia", "economia", "educacao", "eleicao_partidos", "energia_internet",
             "financas", "habitacao_zoneamento", "meio_ambiente", "rh", "saude",
             "seguranca", "sociedade_participacao", "transportes_mobilidade"),
    nome = c("Identifica\u00e7\u00e3o", "Popula\u00e7\u00e3o",
             "Assist\u00eancia Social e Direitos Humanos", "Corrup\u00e7\u00e3o e Transpar\u00eancia",
             "Dados Hist\u00f3ricos", "Economia", "Educa\u00e7\u00e3o", "Elei\u00e7\u00f5es e Partidos",
             "Energia e Internet", "Finan\u00e7as", "Habita\u00e7\u00e3o e Zoneamento",
             "Meio-Ambiente", "Recursos Humanos", "Sa\u00fade", "Seguran\u00e7a",
             "Sociedade e Participa\u00e7\u00e3o", "Transportes e Mobilidade"),
    aliases = I(list(
      "identificacao", "populacao", "assistencia social e direitos humanos",
      c("corrupcao e transparencia", "corrupcao"), c("dados historicos", "historia"),
      "economia", "educacao", c("eleicoes e partidos", "eleicoes"), "energia e internet",
      "financas", "habitacao e zoneamento", c("meio-ambiente", "meio ambiente"),
      "recursos humanos", "saude", "seguranca",
      c("sociedade e participacao", "sociedade"), c("transportes e mobilidade", "transportes")
    )),
    stringsAsFactors = FALSE
  )
}

# Dimensoes que sempre acompanham qualquer importacao
.mape_core <- c("identificacao", "populacao")

# Normaliza texto: minusculas, sem acento, espacos simples
.norm <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(trimws(gsub("\\s+", " ", x)))
  x
}

# Converte o nome da dimensao do dicionario para o slug
.dimension_slug <- function(nome) {
  dims <- .mape_dimensions()
  n <- .norm(nome)
  vapply(n, function(v) {
    hit <- which(vapply(dims$aliases, function(a) v %in% a, logical(1)))
    if (length(hit) == 0) NA_character_ else dims$slug[hit[1]]
  }, character(1), USE.NAMES = FALSE)
}

.check_dimension <- function(dimension) {
  dims <- .mape_dimensions()$slug
  bad <- setdiff(dimension, dims)
  if (length(bad) > 0) {
    stop("Dimens\u00e3o n\u00e3o reconhecida: ", paste(bad, collapse = ", "),
         "\nUse list_mape_dimensions() para ver as op\u00e7\u00f5es.", call. = FALSE)
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Padronizacao do dicionario
#   Aceita cabecalhos dos diferentes arquivos do projeto (DICIONARIO,
#   METADADOS de 14 colunas etc.) e devolve nomes padronizados.
# -----------------------------------------------------------------------------
.dict_columns <- list(
  variavel         = c("variavel", "nome_banco", "nome", "nome_original", "indicador"),
  dimensao         = c("dimensao"),
  descricao        = c("descricao"),
  natureza         = c("natureza"),
  tipo             = c("tipo", "operacionalizacao"),
  escopo_temporal  = c("escopo_temporal"),
  objetivo         = c("objetivo"),
  transformacoes   = c("transformacoes"),
  fonte_original   = c("fonte_original"),
  fonte_extracao   = c("fonte_extracao"),
  link             = c("link"),
  responsavel      = c("responsavel"),
  nome_arquivo     = c("nome_do_arquivo", "nome_arquivo"),
  validacao        = c("validacao")
)

.standardize_dictionary <- function(dic) {
  dic <- as.data.frame(dic, stringsAsFactors = FALSE)
  clean <- gsub("[^a-z0-9]+", "_", .norm(names(dic)))
  clean <- gsub("^_|_$", "", clean)
  out <- list()
  for (col in names(.dict_columns)) {
    hit <- which(clean %in% .dict_columns[[col]])
    # usa a primeira coluna candidata que nao seja formula/vazia
    val <- NA_character_
    for (h in hit) {
      v <- as.character(dic[[h]])
      if (!all(is.na(v) | grepl("^=", v))) { val <- v; break }
    }
    out[[col]] <- if (length(val) == 1 && is.na(val)) rep(NA_character_, nrow(dic)) else val
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out$variavel <- tolower(trimws(out$variavel))
  out$dimensao_slug <- .dimension_slug(out$dimensao)
  out[!is.na(out$variavel) & out$variavel != "", , drop = FALSE]
}

# Dicionario da sessao (carregado uma vez e guardado em memoria)
.mape_env <- new.env(parent = emptyenv())

.get_dictionary <- function(dictionary = NULL) {
  if (!is.null(dictionary)) return(.standardize_dictionary(dictionary))
  if (is.null(.mape_env$dictionary)) {
    .mape_env$dictionary <- import_mape_dictionary()
  }
  .mape_env$dictionary
}

# -----------------------------------------------------------------------------
# Validacoes e agrupamentos
# -----------------------------------------------------------------------------
.check_vars <- function(data, vars, numeric = TRUE) {
  faltando <- setdiff(vars, names(data))
  if (length(faltando) > 0) {
    stop("Vari\u00e1vel(is) n\u00e3o encontrada(s) na base: ",
         paste(faltando, collapse = ", "), call. = FALSE)
  }
  if (numeric) {
    nao_num <- vars[!vapply(data[vars], is.numeric, logical(1))]
    if (length(nao_num) > 0) {
      stop("Vari\u00e1vel(is) n\u00e3o num\u00e9rica(s): ", paste(nao_num, collapse = ", "),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

.check_panel <- function(data, id = "id_municipio", time = "ano") {
  faltando <- setdiff(c(id, time), names(data))
  if (length(faltando) > 0) {
    stop("A base precisa das colunas ", paste(faltando, collapse = " e "),
         " (formato munic\u00edpio \u00d7 ano).", call. = FALSE)
  }
  dup <- duplicated(data[c(id, time)])
  if (any(dup)) {
    stop("H\u00e1 ", sum(dup), " linhas duplicadas de munic\u00edpio \u00d7 ano. ",
         "Use check_mape() para diagnosticar.", call. = FALSE)
  }
  invisible(TRUE)
}

.mape_groups <- function(data, group, by_year) {
  if (!is.null(group)) {
    faltando <- setdiff(group, names(data))
    if (length(faltando) > 0) {
      stop("Agrupamento n\u00e3o encontrado na base: ", paste(faltando, collapse = ", "),
           call. = FALSE)
    }
  }
  if (by_year) {
    if (!"ano" %in% names(data)) {
      stop("A base n\u00e3o tem a coluna 'ano'. Use by_year = FALSE para dados de corte transversal.",
           call. = FALSE)
    }
    group <- c("ano", group)
  }
  group
}

# id_microrregiao -> microrregiao; sigla_uf -> uf; nome_regiao -> regiao; NULL -> brasil
.group_label <- function(group) {
  if (is.null(group)) return("brasil")
  paste(sub("^(id|sigla|nome|cod|codigo)_", "", group), collapse = "_")
}

# Aplica uma funcao vetorial dentro de cada grupo (ano + territorio)
.by_group <- function(data, groups, var, fun, name) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::mutate(!!name := fun(.data[[var]])) %>%
    dplyr::ungroup()
}

# -----------------------------------------------------------------------------
# Tempo: valor do mesmo municipio k anos antes/depois, baseado no ANO
# (e nao na posicao da linha), de modo que lacunas no painel sao respeitadas.
# -----------------------------------------------------------------------------
.value_at_year <- function(data, var, k, id = "id_municipio", time = "ano") {
  chave_atual <- paste(data[[id]], data[[time]] - k, sep = "__")
  chave_base  <- paste(data[[id]], data[[time]], sep = "__")
  data[[var]][match(chave_atual, chave_base)]
}

# -----------------------------------------------------------------------------
# Tipos: resolve integer64 (BigQuery) e padroniza id_municipio como texto
# -----------------------------------------------------------------------------
.fix_types <- function(data) {
  for (v in names(data)) {
    if (inherits(data[[v]], "integer64")) {
      data[[v]] <- as.numeric(as.character(data[[v]]))
    }
  }
  if ("id_municipio" %in% names(data)) {
    data$id_municipio <- as.character(data$id_municipio)
  }
  if ("ano" %in% names(data)) {
    data$ano <- as.integer(data$ano)
  }
  data
}
