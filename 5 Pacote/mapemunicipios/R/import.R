# =============================================================================
# Carregar base de dados
# =============================================================================
#
# PROVISORIO: o local definitivo dos arquivos ainda precisa ser definido
# (GitHub Releases, Zenodo, Dataverse, bucket do Google Cloud...). Por ora,
# a fonte e configuravel por opcao:
#
#   options(mape.source = "G:/Drives compartilhados/mape_municipios/pacote")
#   options(mape.source = "https://github.com/<org>/mape_municipios/releases/download/v1.0")
#
# Estrutura esperada na fonte, um arquivo por dimensao:
#   identificacao.parquet, populacao.parquet, saude.parquet, ...
#   dicionario.xlsx
# Arquivos .rds tambem sao aceitos no lugar de .parquet.
# =============================================================================

.default_source <- "https://github.com/ORG/mape_municipios/releases/download/v0.1"

#' Local de origem dos arquivos do mape_municipios
#'
#' Retorna a pasta local ou URL de onde os arquivos são lidos, definida por
#' `options(mape.source = ...)`.
#'
#' @return Texto com o caminho ou URL.
#' @export
mape_source <- function() {
  getOption("mape.source", .default_source)
}

#' Versão do mape_municipios
#'
#' Retorna a versão dos dados em uso, lida do arquivo `VERSION` na fonte,
#' quando existir. Deve ser registrada nas análises para garantir a
#' replicabilidade.
#'
#' @return Texto com a versão.
#' @export
mape_version <- function() {
  path <- tryCatch(.fetch_file("VERSION"), error = function(e) NULL)
  if (is.null(path)) return(NA_character_)
  trimws(readLines(path, n = 1, warn = FALSE))
}

# Pasta de cache local
.cache_dir <- function() {
  d <- getOption("mape.cache", tools::R_user_dir("mapemunicipios", "cache"))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# Obtem um arquivo da fonte (pasta local ou URL com cache) e devolve o caminho local
.fetch_file <- function(file, refresh = FALSE) {
  src <- mape_source()
  if (dir.exists(src)) {
    path <- file.path(src, file)
    if (!file.exists(path)) stop("Arquivo n\u00e3o encontrado: ", path, call. = FALSE)
    return(path)
  }
  dest <- file.path(.cache_dir(), file)
  if (!file.exists(dest) || refresh) {
    url <- paste0(sub("/$", "", src), "/", file)
    message("Baixando ", file, " ...")
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

# Le a tabela de uma dimensao (parquet ou rds) e corrige tipos
.read_dimension <- function(slug, refresh = FALSE) {
  path <- tryCatch(.fetch_file(paste0(slug, ".parquet"), refresh),
                   error = function(e) NULL)
  if (!is.null(path)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Instale o pacote 'arrow' para ler arquivos .parquet.", call. = FALSE)
    }
    data <- as.data.frame(arrow::read_parquet(path))
  } else {
    data <- readRDS(.fetch_file(paste0(slug, ".rds"), refresh))
  }
  .fix_types(as.data.frame(data))
}

# Aplica os filtros de ano, UF e municipio
.apply_filters <- function(data, years, uf, id_municipio) {
  if (!is.null(years) && "ano" %in% names(data)) {
    data <- data[data$ano %in% years, , drop = FALSE]
  }
  if (!is.null(uf) && "sigla_uf" %in% names(data)) {
    data <- data[data$sigla_uf %in% toupper(uf), , drop = FALSE]
  }
  if (!is.null(id_municipio) && "id_municipio" %in% names(data)) {
    data <- data[data$id_municipio %in% as.character(id_municipio), , drop = FALSE]
  }
  data
}

# Junta as tabelas: Identificacao (id) + Populacao (id x ano) + dimensoes.
# O esqueleto e formado por todas as combinacoes municipio x ano presentes em
# qualquer tabela carregada; as dimensoes sao unidas a ele por left join.
.join_tables <- function(tables) {
  com_ano <- Filter(function(t) "ano" %in% names(t), tables)
  if (length(com_ano) == 0) {
    esqueleto <- unique(tables[[1]]["id_municipio"])
  } else {
    esqueleto <- unique(do.call(rbind, lapply(com_ano, function(t) t[c("id_municipio", "ano")])))
  }
  out <- esqueleto
  for (t in tables) {
    by <- intersect(c("id_municipio", "ano"), names(t))
    novas <- setdiff(names(t), names(out))
    out <- dplyr::left_join(out, t[c(by, novas)], by = by)
  }
  if ("ano" %in% names(out)) out <- dplyr::arrange(out, .data$id_municipio, .data$ano)
  out
}

.import <- function(dims, years, uf, id_municipio, refresh) {
  tables <- lapply(dims, .read_dimension, refresh = refresh)
  names(tables) <- dims
  # a UF so existe em Identificacao: filtra os municipios por la
  if (!is.null(uf)) {
    ids_uf <- tables$identificacao$id_municipio[tables$identificacao$sigla_uf %in% toupper(uf)]
    id_municipio <- if (is.null(id_municipio)) ids_uf else intersect(as.character(id_municipio), ids_uf)
  }
  tables <- lapply(tables, .apply_filters, years = years, uf = NULL, id_municipio = id_municipio)
  .join_tables(tables)
}

#' Importar a base completa do mape_municipios
#'
#' Importa todas as dimensões, unidas no formato município × ano.
#'
#' @param years Vetor de anos a manter (ex.: `2010:2020`). NULL = todos.
#' @param uf Siglas das UFs a manter (ex.: `c("RJ", "SP")`). NULL = todas.
#' @param id_municipio Códigos IBGE de 7 dígitos a manter. NULL = todos.
#' @param refresh Se TRUE, baixa novamente os arquivos, ignorando o cache.
#'
#' @return Um data.frame com `id_municipio` como texto e `ano` como inteiro.
#' @examples
#' \dontrun{
#' bd <- import_mape(years = 2010:2020, uf = "RJ")
#' }
#' @export
import_mape <- function(years = NULL, uf = NULL, id_municipio = NULL, refresh = FALSE) {
  .import(.mape_dimensions()$slug, years, uf, id_municipio, refresh)
}

#' Importar a base por dimensão
#'
#' Importa uma ou mais dimensões. Identificação e População sempre vêm junto.
#'
#' @param dimension Uma ou mais dimensões (ver [list_mape_dimensions()]),
#'   por exemplo `"saude"` ou `c("saude", "educacao")`.
#' @inheritParams import_mape
#'
#' @examples
#' \dontrun{
#' bd <- import_mape_dimension("meio_ambiente", years = 2000:2023)
#' }
#' @export
import_mape_dimension <- function(dimension, years = NULL, uf = NULL,
                                  id_municipio = NULL, refresh = FALSE) {
  .check_dimension(dimension)
  .import(unique(c(.mape_core, dimension)), years, uf, id_municipio, refresh)
}

#' Importar variáveis específicas
#'
#' Importa apenas as variáveis pedidas, de qualquer dimensão, sempre com
#' Identificação e População. A dimensão de cada variável é
#' identificada pelo dicionário.
#'
#' @param variables Vetor com os nomes das variáveis.
#' @inheritParams import_mape
#'
#' @examples
#' \dontrun{
#' bd <- import_mape_variables(c("pib", "taxa_homicidios", "ideb_ai"),
#'                             years = 2005:2021)
#' }
#' @export
import_mape_variables <- function(variables, years = NULL, uf = NULL,
                                  id_municipio = NULL, refresh = FALSE) {
  dic <- .get_dictionary()
  variables <- tolower(variables)
  faltando <- setdiff(variables, dic$variavel)
  if (length(faltando) > 0) {
    stop("Vari\u00e1vel(is) n\u00e3o encontrada(s) no dicion\u00e1rio: ",
         paste(faltando, collapse = ", "), ". Use search_mape() para procurar.",
         call. = FALSE)
  }
  dims <- unique(dic$dimensao_slug[dic$variavel %in% variables])
  core <- .import(.mape_core, years, uf, id_municipio, refresh)
  extra <- setdiff(dims, .mape_core)
  if (length(extra) == 0) return(core)
  outras <- lapply(extra, function(d) {
    t <- .read_dimension(d, refresh)
    t <- .apply_filters(t, years, NULL, unique(core$id_municipio))
    t[intersect(c("id_municipio", "ano", variables), names(t))]
  })
  .join_tables(c(list(core), outras))
}

#' Importar o dicionário de variáveis
#'
#' @param refresh Se TRUE, baixa novamente o arquivo.
#' @return Um data.frame com uma linha por variável e colunas padronizadas:
#'   `variavel`, `dimensao`, `descricao`, `natureza`, `tipo`, `escopo_temporal`,
#'   `objetivo`, `transformacoes`, `fonte_original`, `fonte_extracao`, `link`,
#'   `responsavel`, `nome_arquivo`, `validacao` e `dimensao_slug`.
#'   Colunas ausentes no arquivo de origem vêm como NA.
#' @export
import_mape_dictionary <- function(refresh = FALSE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Instale o pacote 'readxl' para ler o dicion\u00e1rio.", call. = FALSE)
  }
  dic <- readxl::read_excel(.fetch_file("dicionario.xlsx", refresh))
  dic <- .standardize_dictionary(dic)
  .mape_env$dictionary <- dic
  dic
}

#' Importar o dicionário por dimensão
#'
#' Identificação e População sempre vêm junto.
#'
#' @inheritParams import_mape_dimension
#' @export
import_mape_dictionary_dimension <- function(dimension, refresh = FALSE) {
  .check_dimension(dimension)
  dic <- import_mape_dictionary(refresh)
  dic[dic$dimensao_slug %in% unique(c(.mape_core, dimension)), , drop = FALSE]
}
