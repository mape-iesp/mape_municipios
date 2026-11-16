# =============================================================================
# Cálculos
# =============================================================================

#' Deflacionar valores monetários
#'
#' Converte valores nominais em valores reais a preços de uma data de
#' referência, usando o pacote deflateBR. Cada ano é deflacionado a partir de
#' dezembro daquele ano (`month`), como no fluxo usado na construção da base.
#'
#' Só se aplica a variáveis de natureza "Numérico – Monetário". Quando `vars`
#' não é informado, a função seleciona automaticamente as variáveis
#' monetárias presentes na base a partir do dicionário.
#'
#' O fator de deflação é calculado uma única vez por ano e depois aplicado a
#' todas as variáveis, o que evita consultas repetidas à série do índice.
#'
#' @param data Base no formato município × ano.
#' @param vars Variáveis a deflacionar. NULL = todas as monetárias do dicionário.
#' @param ref Data de referência no formato "MM/AAAA", por exemplo "12/2025".
#' @param index Índice de preços: "ipca" (padrão), "inpc", "igpm", "igpdi" ou "ipc".
#' @param month Mês de cada ano usado como data nominal (padrão: 12).
#' @param suffix Sufixo das novas colunas. Use `suffix = ""` para substituir as
#'   colunas originais.
#' @param dictionary Dicionário já carregado (opcional).
#'
#' @return A base com as variáveis deflacionadas.
#' @examples
#' \dontrun{
#' bd <- deflate_monetary(bd, c("pib", "va_industria"), ref = "12/2025")
#' # -> pib_real, va_industria_real
#' }
#' @export
deflate_monetary <- function(data, vars = NULL, ref, index = c("ipca", "inpc", "igpm", "igpdi", "ipc"),
                             month = 12, suffix = "_real", dictionary = NULL) {
  index <- match.arg(index)
  if (!grepl("^\\d{2}/\\d{4}$", ref)) stop("'ref' deve estar no formato \"MM/AAAA\".", call. = FALSE)
  if (!"ano" %in% names(data)) stop("A base precisa da coluna 'ano'.", call. = FALSE)

  dic <- tryCatch(.get_dictionary(dictionary), error = function(e) NULL)
  monetarias <- if (!is.null(dic)) dic$variavel[grepl("monet", .norm(dic$natureza))] else character(0)

  if (is.null(vars)) {
    if (is.null(dic)) stop("Informe 'vars' ou um dicion\u00e1rio para identificar as vari\u00e1veis monet\u00e1rias.",
                           call. = FALSE)
    vars <- intersect(names(data), monetarias)
    if (length(vars) == 0) stop("Nenhuma vari\u00e1vel monet\u00e1ria encontrada na base.", call. = FALSE)
    message("Deflacionando: ", paste(vars, collapse = ", "))
  } else if (!is.null(dic)) {
    nao_monet <- setdiff(intersect(vars, dic$variavel), monetarias)
    if (length(nao_monet) > 0) {
      warning("Segundo o dicion\u00e1rio, estas vari\u00e1veis n\u00e3o s\u00e3o monet\u00e1rias: ",
              paste(nao_monet, collapse = ", "), call. = FALSE)
    }
  }
  .check_vars(data, vars)

  anos <- sort(unique(stats::na.omit(data$ano)))
  datas <- as.Date(sprintf("%d-%02d-01", anos, month))
  fator <- .deflator(datas, ref, index)
  fator_linha <- fator[match(data$ano, anos)]

  for (v in vars) data[[paste0(v, suffix)]] <- data[[v]] * fator_linha
  data
}

# Fator multiplicativo que leva R$ 1 de cada data nominal para a data de referência
.deflator <- function(datas, ref, index) {
  if (!requireNamespace("deflateBR", quietly = TRUE)) {
    stop("Instale o pacote 'deflateBR' para deflacionar valores.", call. = FALSE)
  }
  deflateBR::deflate(rep(1, length(datas)), datas, ref, index = index)
}

# Motor comum das ponderações: numerador / denominador * per
.calc_rate <- function(data, var, denominator, per, name) {
  .check_vars(data, c(var, denominator))
  den <- data[[denominator]]
  data[[name]] <- ifelse(is.na(den) | den == 0, NA_real_, data[[var]] / den * per)
  data
}

#' Ponderar um indicador pela população
#'
#' Divide um indicador pela população do município, com multiplicador opcional
#' (por exemplo, `per = 100000` para taxas por 100 mil habitantes).
#' Municípios com população zero ou ausente recebem NA.
#'
#' @param data Base no formato município × ano.
#' @param var Indicador a ponderar.
#' @param population Coluna de população: "populacao_estimativa" ou
#'   "populacao_censo" (ou qualquer outra coluna de população da base).
#' @param per Multiplicador (1 = per capita; 1000; 100000 ...).
#' @param name Nome da nova coluna. Se NULL: "<var>_pc" quando `per = 1` ou
#'   "<var>_por_<per>" nos demais casos.
#'
#' @examples
#' \dontrun{
#' calculate_population_weighting(bd, "pib", "populacao_estimativa")   # pib_pc
#' calculate_population_weighting(bd, "homicidios", "populacao_estimativa",
#'                                per = 100000)                        # homicidios_por_100000
#' }
#' @export
calculate_population_weighting <- function(data, var, population = "populacao_estimativa",
                                           per = 1, name = NULL) {
  if (is.null(name)) name <- .rate_name(var, per)
  .calc_rate(data, var, population, per, name)
}

#' Ponderar um indicador pelo tamanho de um grupo racial
#'
#' Divide um indicador pelo tamanho de um grupo racial na população do
#' município. Por exemplo, homicídios de pessoas pretas pelo total de pessoas
#' pretas (`pessoas_pretas`).
#'
#' @inheritParams calculate_population_weighting
#' @param race_group Coluna com o tamanho do grupo racial, por exemplo
#'   "pessoas_pretas".
#'
#' @examples
#' \dontrun{
#' calculate_population_race_weighting(bd, "homicidios_pretos", "pessoas_pretas",
#'                                     per = 100000)
#' }
#' @export
calculate_population_race_weighting <- function(data, var, race_group, per = 1, name = NULL) {
  if (is.null(name)) name <- .rate_name(var, per)
  .calc_rate(data, var, race_group, per, name)
}

#' Ponderar um indicador pela área do município
#'
#' Divide um indicador pela área do município (`area_total`, em km²).
#' Por exemplo, o rebanho bovino por km².
#'
#' @inheritParams calculate_population_weighting
#' @param area Coluna de área (padrão: "area_total").
#' @param name Nome da nova coluna. Se NULL: "<var>_km2".
#' @export
calculate_area_weighting <- function(data, var, area = "area_total", per = 1, name = NULL) {
  if (is.null(name)) name <- if (per == 1) paste0(var, "_km2") else paste0(var, "_por_", per, "_km2")
  .calc_rate(data, var, area, per, name)
}

.rate_name <- function(var, per) {
  if (per == 1) paste0(var, "_pc") else paste0(var, "_por_", format(per, scientific = FALSE, big.mark = ""))
}
