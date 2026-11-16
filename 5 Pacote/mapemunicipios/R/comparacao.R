# =============================================================================
# Comparação
#
# Todas as funções calculam a referência dentro de cada ano (by_year = TRUE)
# e do agrupamento territorial escolhido (group = NULL compara com o Brasil).
# Nomes de saída: <variavel>_<operacao>_<referencia>_<grupo>
# =============================================================================

.stat_label <- function(stat) c(mean = "media", median = "mediana")[[stat]]
# é = "é" (escape ASCII, exigido para pacotes no CRAN)
.stat_texto <- function(stat) c(mean = "da m\u00e9dia", median = "da mediana")[[stat]]

# Valor de referência (média ou mediana) do grupo, com opção de excluir o próprio município
.mape_reference <- function(x, stat, exclude_self) {
  # Grupos com menos de 2 municípios válidos não têm com quem comparar
  if (sum(!is.na(x)) < 2) return(rep(NA_real_, length(x)))
  if (!exclude_self) {
    ref <- if (stat == "mean") mean(x, na.rm = TRUE) else stats::median(x, na.rm = TRUE)
    ref <- rep(ref, length(x))
  } else if (stat == "mean") {
    n <- sum(!is.na(x))
    s <- sum(x, na.rm = TRUE)
    ref <- ifelse(is.na(x), s / n, (s - x) / (n - 1))
  } else {
    ref <- vapply(seq_along(x),
                  function(i) stats::median(x[-i], na.rm = TRUE),
                  numeric(1))
  }
  ref[!is.finite(ref)] <- NA_real_   # grupos vazios ou com um único município
  ref
}

# Classificação binária: 1 = acima, 0 = abaixo; empates segundo 'ties'
.mape_classify <- function(x, ref, ties) {
  empate <- switch(ties, abaixo = 0L, acima = 1L, na = NA_integer_)
  out <- ifelse(x > ref, 1L, ifelse(x < ref, 0L, empate))
  out[is.na(x) | is.na(ref)] <- NA_integer_
  as.integer(out)
}

# -----------------------------------------------------------------------------
# compare_group
# -----------------------------------------------------------------------------

#' Comparar o município com a média ou mediana de um agrupamento territorial
#'
#' Para cada município (e, por padrão, cada ano), compara o valor de um indicador
#' com a média ou a mediana dos municípios do mesmo agrupamento territorial.
#'
#' @param data Base no formato município × ano.
#' @param var Nome do indicador (texto), por exemplo "dependencia_adm".
#' @param group Agrupamento territorial: "id_microrregiao", "sigla_uf", "nome_regiao"
#'   etc. NULL (padrão) compara com todos os municípios do país.
#' @param stat "mean" (média) ou "median" (mediana).
#' @param by_year Se TRUE (padrão), a referência é calculada separadamente para cada ano.
#' @param exclude_self Se TRUE, a referência exclui o próprio município (leave-one-out).
#'   Recomendado para agrupamentos pequenos, como microrregiões.
#' @param ties Tratamento de valores iguais à referência: "abaixo" (padrão: só é
#'   acima quem está estritamente acima), "acima" ou "na".
#' @param output "binary" (1 = acima, 0 = abaixo), "diff" (valor - referência)
#'   ou "ratio" (valor / referência).
#' @param keep_reference Se TRUE, mantém a coluna com o valor de referência do grupo.
#' @param ... Em `compare_mean()` e `compare_median()`, outros argumentos de
#'   `compare_group()` (by_year, exclude_self, ties, output, keep_reference, name).
#' @param name Nome da coluna de saída. Se NULL, é gerado automaticamente,
#'   por exemplo "dependencia_adm_acima_media_uf".
#'
#' @return A base original com a(s) nova(s) coluna(s). Recebem NA os municípios
#'   com NA no indicador e os de grupos com menos de 2 municípios válidos
#'   (não há com quem comparar).
#' @export
compare_group <- function(data, var, group = NULL,
                          stat = c("mean", "median"),
                          by_year = TRUE,
                          exclude_self = FALSE,
                          ties = c("abaixo", "acima", "na"),
                          output = c("binary", "diff", "ratio"),
                          keep_reference = FALSE,
                          name = NULL) {
  stat   <- match.arg(stat)
  ties   <- match.arg(ties)
  output <- match.arg(output)
  .check_vars(data, var)
  groups <- .mape_groups(data, group, by_year)

  sufixo <- paste(.stat_label(stat), .group_label(group), sep = "_")
  if (is.null(name)) {
    op <- c(binary = "acima", diff = "dif", ratio = "razao")[[output]]
    name <- paste(var, op, sufixo, sep = "_")
  }
  ref_name <- paste(var, "ref", sufixo, sep = "_")

  out <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::mutate(.ref = .mape_reference(.data[[var]], stat, exclude_self)) |>
    dplyr::ungroup()

  out[[name]] <- switch(output,
    binary = .mape_classify(out[[var]], out$.ref, ties),
    diff   = out[[var]] - out$.ref,
    ratio  = ifelse(out$.ref == 0, NA_real_, out[[var]] / out$.ref)
  )

  if (keep_reference) {
    out[[ref_name]] <- out$.ref
  }
  out$.ref <- NULL
  out
}

#' @rdname compare_group
#' @export
compare_mean <- function(data, var, group = NULL, ...) {
  compare_group(data, var, group = group, stat = "mean", ...)
}

#' @rdname compare_group
#' @export
compare_median <- function(data, var, group = NULL, ...) {
  compare_group(data, var, group = group, stat = "median", ...)
}

# -----------------------------------------------------------------------------
# ranking_mape
# -----------------------------------------------------------------------------

#' Ranquear o município em um indicador dentro de um agrupamento territorial
#'
#' @param data Base no formato município × ano.
#' @param var Nome do indicador (texto).
#' @param group Agrupamento territorial (ver compare_group). NULL = Brasil.
#' @param by_year Se TRUE (padrão), o ranking é feito separadamente para cada ano.
#' @param desc Se TRUE (padrão), o maior valor recebe a posição 1. Use FALSE
#'   quando valores menores forem "melhores" (por exemplo, taxa de homicídios).
#' @param ties_method Tratamento de empates: "min" (padrão, 1, 2, 2, 4),
#'   "dense" (1, 2, 2, 3), "average", "first" ou "max".
#' @param percentile Se TRUE, acrescenta a posição em percentil (0 a 100, em que
#'   100 é a melhor posição segundo 'desc').
#' @param name Nome da coluna de ranking. Se NULL: "<var>_rank_<grupo>".
#'
#' @return A base original com a posição do município e o número de municípios
#'   ranqueados no grupo ("<var>_n_<grupo>"), para que a posição possa ser
#'   interpretada (3º de 5 é diferente de 3º de 853). Municípios com NA no
#'   indicador não são ranqueados.
#' @export
ranking_mape <- function(data, var, group = NULL,
                         by_year = TRUE,
                         desc = TRUE,
                         ties_method = c("min", "dense", "average", "first", "max"),
                         percentile = FALSE,
                         name = NULL) {
  ties_method <- match.arg(ties_method)
  .check_vars(data, var)
  groups <- .mape_groups(data, group, by_year)

  g <- .group_label(group)
  if (is.null(name)) name <- paste(var, "rank", g, sep = "_")
  n_name   <- paste(var, "n", g, sep = "_")
  pct_name <- paste(var, "pct", g, sep = "_")

  rank_fun <- function(x) {
    xo <- if (desc) -x else x
    if (ties_method == "dense") dplyr::dense_rank(xo)
    else rank(xo, ties.method = ties_method, na.last = "keep")
  }

  out <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::mutate(
      !!name   := rank_fun(.data[[var]]),
      !!n_name := sum(!is.na(.data[[var]]))
    )

  if (percentile) {
    out <- out |>
      dplyr::mutate(
        !!pct_name := 100 * dplyr::percent_rank(if (desc) .data[[var]] else -.data[[var]]),
        !!pct_name := ifelse(.data[[n_name]] < 2, NA_real_, .data[[pct_name]])
      )
  }
  dplyr::ungroup(out)
}

# -----------------------------------------------------------------------------
# typology_mape
# -----------------------------------------------------------------------------

#' Criar tipologia 2x2 a partir de dois indicadores
#'
#' Classifica cada município segundo sua posição (acima ou abaixo da média ou
#' mediana do agrupamento territorial) em dois indicadores ao mesmo tempo.
#'
#' Tipos:
#'   1 = acima em var_x  e acima em var_y
#'   2 = acima em var_x  e abaixo em var_y
#'   3 = abaixo em var_x e acima em var_y
#'   4 = abaixo em var_x e abaixo em var_y
#'
#' O rótulo sempre indica os indicadores e a referência, por exemplo
#' "pib acima da média | desmatamento abaixo da média".
#'
#' @param data Base no formato município × ano.
#' @param var_x,var_y Nomes dos dois indicadores (texto).
#' @param group Agrupamento territorial (ver compare_group). NULL = Brasil.
#' @param stat "mean" ou "median". Aceita um vetor de dois elementos para usar
#'   referências diferentes em cada indicador, por exemplo c("median", "mean").
#' @param by_year,exclude_self,ties Ver compare_group.
#' @param keep_components Se TRUE, mantém as duas colunas binárias usadas na tipologia.
#' @param name Nome base das colunas de saída. Se NULL: "tipo_<var_x>_<var_y>_<grupo>".
#'
#' @return A base original com duas colunas: o código do tipo (inteiro, 1 a 4) e
#'   o rótulo (fator ordenado de 1 a 4, com os quatro níveis sempre presentes).
#'   Municípios com NA em qualquer um dos indicadores recebem NA.
#' @export
typology_mape <- function(data, var_x, var_y, group = NULL,
                          stat = "mean",
                          by_year = TRUE,
                          exclude_self = FALSE,
                          ties = c("abaixo", "acima", "na"),
                          keep_components = FALSE,
                          name = NULL) {
  ties <- match.arg(ties)
  if (identical(var_x, var_y)) stop("var_x e var_y devem ser indicadores diferentes.", call. = FALSE)
  if (!length(stat) %in% 1:2 || !all(stat %in% c("mean", "median"))) {
    stop("stat deve ser 'mean', 'median' ou um vetor com dois desses valores.", call. = FALSE)
  }
  stat <- rep_len(stat, 2)
  .check_vars(data, c(var_x, var_y))

  g <- .group_label(group)
  if (is.null(name)) name <- paste("tipo", var_x, var_y, g, sep = "_")
  bx_name <- paste(var_x, "acima", .stat_label(stat[1]), g, sep = "_")
  by_name <- paste(var_y, "acima", .stat_label(stat[2]), g, sep = "_")

  out <- data |>
    compare_group(var_x, group, stat = stat[1], by_year = by_year,
                  exclude_self = exclude_self, ties = ties, name = bx_name) |>
    compare_group(var_y, group, stat = stat[2], by_year = by_year,
                  exclude_self = exclude_self, ties = ties, name = by_name)

  bx <- out[[bx_name]]
  by <- out[[by_name]]
  tipo <- ifelse(bx == 1 & by == 1, 1L,
          ifelse(bx == 1 & by == 0, 2L,
          ifelse(bx == 0 & by == 1, 3L, 4L)))
  tipo[is.na(bx) | is.na(by)] <- NA_integer_

  pos <- function(v, s, acima) paste(v, if (acima) "acima" else "abaixo", .stat_texto(s))
  niveis <- c(
    paste(pos(var_x, stat[1], TRUE),  "|", pos(var_y, stat[2], TRUE)),
    paste(pos(var_x, stat[1], TRUE),  "|", pos(var_y, stat[2], FALSE)),
    paste(pos(var_x, stat[1], FALSE), "|", pos(var_y, stat[2], TRUE)),
    paste(pos(var_x, stat[1], FALSE), "|", pos(var_y, stat[2], FALSE))
  )

  out[[name]] <- tipo
  out[[paste0(name, "_rotulo")]] <- factor(niveis[tipo], levels = niveis)

  if (!keep_components) {
    out[[bx_name]] <- NULL
    out[[by_name]] <- NULL
  }
  out
}


# -----------------------------------------------------------------------------
# standardize_mape
# -----------------------------------------------------------------------------

#' Padronizar um indicador (z-score) dentro de um agrupamento territorial
#'
#' Calcula `(x - média do grupo) / desvio-padrão do grupo`, dentro de cada ano
#' e agrupamento. O resultado indica a quantos desvios-padrão o município está
#' da média dos municípios comparáveis.
#'
#' @inheritParams compare_group
#' @param robust Se TRUE, usa mediana e desvio absoluto mediano (MAD) no lugar
#'   de média e desvio-padrão, o que reduz a influência de valores extremos
#'   (comum em indicadores municipais, como PIB).
#' @param name Nome da nova coluna. Padrão: "<var>_z_<grupo>".
#'
#' @return A base com a coluna padronizada. Grupos com menos de 2 municípios
#'   válidos ou sem variação recebem NA.
#' @export
standardize_mape <- function(data, var, group = NULL, by_year = TRUE, robust = FALSE, name = NULL) {
  .check_vars(data, var)
  groups <- .mape_groups(data, group, by_year)
  if (is.null(name)) name <- paste(var, if (robust) "zrob" else "z", .group_label(group), sep = "_")
  z <- function(x) {
    if (sum(!is.na(x)) < 2) return(rep(NA_real_, length(x)))
    centro <- if (robust) stats::median(x, na.rm = TRUE) else mean(x, na.rm = TRUE)
    escala <- if (robust) stats::mad(x, na.rm = TRUE) else stats::sd(x, na.rm = TRUE)
    if (is.na(escala) || escala == 0) return(rep(NA_real_, length(x)))
    (x - centro) / escala
  }
  .by_group(data, groups, var, z, name)
}

# -----------------------------------------------------------------------------
# calculate_quantile
# -----------------------------------------------------------------------------

#' Classificar o município em quantis dentro de um agrupamento territorial
#'
#' Divide os municípios de cada agrupamento e ano em `n` grupos de tamanho
#' aproximadamente igual (quintis com `n = 5`, decis com `n = 10`).
#' O quantil 1 reúne os menores valores. Municípios com o mesmo valor ficam
#' sempre no mesmo quantil.
#'
#' @inheritParams compare_group
#' @param n Número de quantis.
#' @param name Nome da nova coluna. Padrão: "<var>_q<n>_<grupo>".
#'
#' @return A base com o quantil (inteiro de 1 a `n`). Grupos com menos de `n`
#'   municípios válidos recebem NA.
#' @export
calculate_quantile <- function(data, var, group = NULL, n = 5, by_year = TRUE, name = NULL) {
  .check_vars(data, var)
  groups <- .mape_groups(data, group, by_year)
  if (is.null(name)) name <- paste0(var, "_q", n, "_", .group_label(group))
  q <- function(x) {
    validos <- sum(!is.na(x))
    if (validos < n) return(rep(NA_integer_, length(x)))
    r <- rank(x, ties.method = "min", na.last = "keep")
    as.integer(pmin(n, floor(n * (r - 1) / validos) + 1))
  }
  .by_group(data, groups, var, q, name)
}

# -----------------------------------------------------------------------------
# calculate_share
# -----------------------------------------------------------------------------

#' Participação do município no total do agrupamento
#'
#' Calcula o percentual que o município representa no total do agrupamento e
#' ano, por exemplo a participação no PIB da UF. Só faz sentido para
#' indicadores que podem ser somados (contagens, valores monetários,
#' área), e não para taxas ou proporções.
#'
#' @inheritParams compare_group
#' @param name Nome da nova coluna. Padrão: "<var>_share_<grupo>".
#'
#' @return A base com a participação, em %. Municípios com NA recebem NA; o
#'   total do grupo é calculado com os valores válidos.
#' @export
calculate_share <- function(data, var, group = NULL, by_year = TRUE, name = NULL) {
  .check_vars(data, var)
  groups <- .mape_groups(data, group, by_year)
  if (is.null(name)) name <- paste(var, "share", .group_label(group), sep = "_")
  sh <- function(x) {
    tot <- sum(x, na.rm = TRUE)
    if (tot == 0) return(rep(NA_real_, length(x)))
    100 * x / tot
  }
  .by_group(data, groups, var, sh, name)
}

# -----------------------------------------------------------------------------
# build_index
# -----------------------------------------------------------------------------

#' Construir um índice composto
#'
#' Combina vários indicadores em um índice. Cada indicador é normalizado
#' (min-max, de 0 a 1, ou z-score) dentro de cada ano e agrupamento, tem sua
#' direção ajustada e depois é combinado por média ponderada.
#'
#' @param data Base no formato município × ano.
#' @param vars Vetor com os indicadores.
#' @param weights Pesos de cada indicador (mesma ordem de `vars`). Padrão:
#'   pesos iguais. São reescalados para somar 1.
#' @param direction Vetor de 1 e -1 indicando se valores maiores do indicador
#'   aumentam (1) ou diminuem (-1) o índice. Por exemplo, em um índice de
#'   desenvolvimento, taxa de mortalidade infantil entra com -1. Padrão: todos 1.
#' @param method "minmax" (padrão) ou "zscore".
#' @param group Agrupamento para a normalização. NULL (padrão) normaliza em
#'   relação a todos os municípios do país em cada ano.
#' @param by_year Se TRUE (padrão), normaliza dentro de cada ano. Use FALSE para
#'   comparar o índice ao longo do tempo com uma escala comum a todos os anos.
#' @param min_vars Número mínimo de indicadores válidos para calcular o índice.
#'   Padrão: todos. Com valores menores, o índice é a média ponderada dos
#'   indicadores disponíveis (pesos reescalados).
#' @param name Nome da nova coluna. Padrão: "indice".
#' @param keep_components Se TRUE, mantém os indicadores normalizados.
#'
#' @examples
#' \dontrun{
#' build_index(bd, c("ideb_ai", "cobertura_esf", "mortalidade_infantil"),
#'             direction = c(1, 1, -1), name = "indice_social")
#' }
#' @export
build_index <- function(data, vars, weights = NULL, direction = NULL,
                        method = c("minmax", "zscore"), group = NULL, by_year = TRUE,
                        min_vars = length(vars), name = "indice", keep_components = FALSE) {
  method <- match.arg(method)
  .check_vars(data, vars)
  if (is.null(weights)) weights <- rep(1, length(vars))
  if (is.null(direction)) direction <- rep(1, length(vars))
  if (length(weights) != length(vars) || length(direction) != length(vars)) {
    stop("'weights' e 'direction' devem ter o mesmo tamanho de 'vars'.", call. = FALSE)
  }
  if (!all(direction %in% c(1, -1))) stop("'direction' s\u00f3 aceita 1 e -1.", call. = FALSE)
  groups <- .mape_groups(data, group, by_year)

  norm <- function(x) {
    if (sum(!is.na(x)) < 2) return(rep(NA_real_, length(x)))
    if (method == "minmax") {
      r <- range(x, na.rm = TRUE)
      if (r[1] == r[2]) return(rep(NA_real_, length(x)))
      (x - r[1]) / (r[2] - r[1])
    } else {
      s <- stats::sd(x, na.rm = TRUE)
      if (s == 0) return(rep(NA_real_, length(x)))
      (x - mean(x, na.rm = TRUE)) / s
    }
  }
  comp_names <- paste0(vars, "_norm")
  for (i in seq_along(vars)) {
    data <- .by_group(data, groups, vars[i], norm, comp_names[i])
    # inverte a direção; no min-max, 1 - x mantém a escala de 0 a 1
    if (direction[i] == -1) {
      data[[comp_names[i]]] <- if (method == "minmax") 1 - data[[comp_names[i]]] else -data[[comp_names[i]]]
    }
  }
  m <- as.matrix(data[comp_names])
  w <- matrix(weights, nrow = nrow(m), ncol = length(vars), byrow = TRUE)
  w[is.na(m)] <- 0
  soma_w <- rowSums(w)
  indice <- rowSums(m * w, na.rm = TRUE) / soma_w
  indice[rowSums(!is.na(m)) < min_vars | soma_w == 0] <- NA_real_
  data[[name]] <- indice
  if (!keep_components) data[comp_names] <- NULL
  data
}
