# =============================================================================
# Tempo
#
# Todas as funções buscam o valor do mesmo município pelo ANO (e não pela
# posição da linha). Se o painel tiver lacunas — por exemplo, um município sem
# linha em 2015 —, o lag de 2016 fica NA em vez de pegar o valor de 2014.
# =============================================================================

#' Valor defasado (lag)
#'
#' Retorna o valor do indicador no mesmo município `k` anos antes.
#'
#' @param data Base no formato município × ano.
#' @param var Um ou mais indicadores.
#' @param k Número de anos de defasagem (1, 2, 3, 10...). Aceita vários valores.
#' @param name Nomes das novas colunas (opcional, apenas para um `var` e um `k`).
#'   Padrão: "<var>_lag<k>".
#'
#' @examples
#' \dontrun{
#' calculate_lag(bd, "desmatamento", k = 1)        # desmatamento_lag1
#' calculate_lag(bd, "desmatamento", k = c(1, 2))  # desmatamento_lag1, desmatamento_lag2
#' }
#' @export
calculate_lag <- function(data, var, k = 1, name = NULL) {
  .shift(data, var, k, name, lead = FALSE)
}

#' Valor antecipado (lead)
#'
#' O inverso de [calculate_lag()]: retorna o valor do indicador no mesmo
#' município `k` anos depois. Útil para testar efeitos antecipatórios.
#'
#' @inheritParams calculate_lag
#' @param name Nomes das novas colunas (opcional). Padrão: "<var>_lead<k>".
#' @export
calculate_lead <- function(data, var, k = 1, name = NULL) {
  .shift(data, var, k, name, lead = TRUE)
}

.shift <- function(data, var, k, name, lead) {
  .check_panel(data)
  .check_vars(data, var, numeric = FALSE)
  if (!is.null(name) && length(name) != length(var) * length(k)) {
    stop("'name' deve ter um nome para cada combina\u00e7\u00e3o de var e k.", call. = FALSE)
  }
  i <- 0
  for (v in var) for (kk in k) {
    i <- i + 1
    nm <- if (!is.null(name)) name[i] else paste0(v, if (lead) "_lead" else "_lag", kk)
    data[[nm]] <- .value_at_year(data, v, if (lead) -kk else kk)
  }
  data
}

#' Taxa de crescimento
#'
#' Calcula a variação percentual do indicador em relação a `k` anos antes:
#' `(x_t - x_{t-k}) / x_{t-k} * 100`. Com `annualized = TRUE`, calcula a taxa
#' média anual composta no período: `((x_t / x_{t-k})^(1/k) - 1) * 100`.
#'
#' Quando o valor de base é zero, a taxa é indefinida e o resultado é NA.
#' Em contagens pequenas (homicídios em municípios pequenos, por exemplo),
#' prefira [calculate_diff()].
#'
#' @inheritParams calculate_lag
#' @param k Número de anos do intervalo.
#' @param annualized Se TRUE, calcula a taxa média anual composta.
#' @param name Nome da nova coluna. Padrão: "<var>_cresc<k>".
#' @export
calculate_growth <- function(data, var, k = 1, annualized = FALSE, name = NULL) {
  .check_panel(data)
  .check_vars(data, var)
  if (is.null(name)) name <- paste0(var, "_cresc", k, if (annualized) "_aa" else "")
  base <- .value_at_year(data, var, k)
  x <- data[[var]]
  res <- if (annualized) ((x / base)^(1 / k) - 1) * 100 else (x - base) / base * 100
  res[is.na(base) | base == 0 | !is.finite(res)] <- NA_real_
  data[[name]] <- res
  data
}

#' Diferença absoluta
#'
#' Calcula a diferença entre o valor do indicador e o valor `k` anos antes:
#' `x_t - x_{t-k}`. Complementa [calculate_growth()] quando a base é próxima
#' de zero.
#'
#' @inheritParams calculate_lag
#' @param k Número de anos do intervalo.
#' @param name Nome da nova coluna. Padrão: "<var>_dif<k>".
#' @export
calculate_diff <- function(data, var, k = 1, name = NULL) {
  .check_panel(data)
  .check_vars(data, var)
  if (is.null(name)) name <- paste0(var, "_dif", k)
  data[[name]] <- data[[var]] - .value_at_year(data, var, k)
  data
}

#' Média móvel
#'
#' Calcula a média do indicador nos últimos `window` anos, incluindo o ano
#' corrente (janela alinhada à direita: t, t-1, ..., t-window+1). Útil para
#' suavizar indicadores voláteis em municípios pequenos.
#'
#' @inheritParams calculate_lag
#' @param window Tamanho da janela, em anos.
#' @param min_obs Número mínimo de anos com dado válido na janela para que a
#'   média seja calculada. Padrão: igual a `window` (janela completa).
#' @param name Nome da nova coluna. Padrão: "<var>_mm<window>".
#' @export
calculate_moving_average <- function(data, var, window = 3, min_obs = window, name = NULL) {
  .check_panel(data)
  .check_vars(data, var)
  if (min_obs > window) stop("'min_obs' n\u00e3o pode ser maior que 'window'.", call. = FALSE)
  if (is.null(name)) name <- paste0(var, "_mm", window)
  m <- vapply(0:(window - 1), function(k) .value_at_year(data, var, k), numeric(nrow(data)))
  m <- matrix(m, nrow = nrow(data))
  n_validos <- rowSums(!is.na(m))
  res <- rowMeans(m, na.rm = TRUE)
  res[n_validos < min_obs] <- NA_real_
  data[[name]] <- res
  data
}

#' Valor acumulado
#'
#' Soma o indicador ao longo do tempo, dentro de cada município, a partir do
#' primeiro ano da base ou de `from`.
#'
#' @inheritParams calculate_lag
#' @param from Ano inicial do acúmulo (opcional). Anos anteriores recebem NA.
#' @param na_rm Se FALSE (padrão), um ano sem dado interrompe o acúmulo e os
#'   anos seguintes ficam NA. Se TRUE, anos sem dado depois da primeira
#'   observação contam como zero. Municípios sem nenhum dado ficam NA.
#' @param name Nome da nova coluna. Padrão: "<var>_acum".
#' @export
calculate_cumulative <- function(data, var, from = NULL, na_rm = FALSE, name = NULL) {
  .check_panel(data)
  .check_vars(data, var)
  if (is.null(name)) name <- paste0(var, "_acum")
  ordem <- order(data$id_municipio, data$ano)
  d <- data[ordem, , drop = FALSE]
  x <- d[[var]]
  if (!is.null(from)) x[d$ano < from] <- NA_real_
  acum <- stats::ave(x, d$id_municipio, FUN = function(v) {
    inicio <- which(!is.na(v))[1]
    out <- rep(NA_real_, length(v))
    if (is.na(inicio)) return(out)          # município sem nenhum dado
    trecho <- v[inicio:length(v)]
    if (na_rm) trecho[is.na(trecho)] <- 0   # só depois do primeiro valor observado
    out[inicio:length(v)] <- cumsum(trecho)
    out
  })
  data[[name]] <- NA_real_
  data[[name]][ordem] <- acum
  data
}

#' Interpolar valores entre anos observados
#'
#' Preenche os anos sem dado entre dois anos observados do mesmo município,
#' como entre censos. Não extrapola: anos antes da primeira ou depois da
#' última observação continuam NA (exceto no método "locf", que repete o
#' último valor para frente).
#'
#' Segue a convenção do projeto de não imputar silenciosamente: a variável
#' original é preservada, o resultado vai para uma nova coluna e uma coluna
#' indicadora marca quais valores foram imputados.
#'
#' O painel precisa ter uma linha para cada ano a ser preenchido.
#'
#' @inheritParams calculate_lag
#' @param method "linear" (padrão) ou "locf" (repete o último valor observado).
#' @param max_gap Maior intervalo, em anos, que pode ser preenchido. Lacunas
#'   maiores continuam NA. Padrão: sem limite.
#' @param name Nome da nova coluna. Padrão: "<var>_interpolado" (linear) ou
#'   "<var>_persistido" (locf).
#'
#' @return A base com a coluna preenchida e a coluna "<name>_imputado"
#'   (1 = valor imputado, 0 = observado, NA = sem valor).
#' @examples
#' \dontrun{
#' interpolate_mape(bd, "prop_evangelicos_pentecostais")   # entre 2000 e 2010
#' }
#' @export
interpolate_mape <- function(data, var, method = c("linear", "locf"), max_gap = Inf, name = NULL) {
  method <- match.arg(method)
  .check_panel(data)
  .check_vars(data, var)
  if (is.null(name)) name <- paste0(var, if (method == "linear") "_interpolado" else "_persistido")

  ordem <- order(data$id_municipio, data$ano)
  d <- data[ordem, , drop = FALSE]
  preenchido <- unsplit(lapply(split(d, d$id_municipio), function(g) {
    obs <- !is.na(g[[var]])
    if (sum(obs) == 0) return(g[[var]])
    anos_obs <- g$ano[obs]
    if (method == "linear") {
      if (sum(obs) < 2) return(g[[var]])
      val <- stats::approx(anos_obs, g[[var]][obs], xout = g$ano, rule = 1)$y
      anterior <- vapply(g$ano, function(a) suppressWarnings(max(anos_obs[anos_obs <= a])), numeric(1))
      seguinte <- vapply(g$ano, function(a) suppressWarnings(min(anos_obs[anos_obs >= a])), numeric(1))
      val[(seguinte - anterior) > max_gap] <- NA_real_
    } else {
      idx <- cumsum(obs)
      val <- ifelse(idx == 0, NA_real_, g[[var]][obs][pmax(idx, 1)])
      anterior <- vapply(g$ano, function(a) suppressWarnings(max(anos_obs[anos_obs <= a])), numeric(1))
      val[(g$ano - anterior) > max_gap] <- NA_real_
    }
    val[obs] <- g[[var]][obs]
    val
  }), d$id_municipio)

  data[[name]] <- NA_real_
  data[[name]][ordem] <- preenchido
  flag <- paste0(name, "_imputado")
  data[[flag]] <- ifelse(is.na(data[[name]]), NA_integer_, as.integer(is.na(data[[var]])))
  data
}

#' Associar o ano ao ciclo eleitoral municipal
#'
#' Acrescenta o mandato municipal correspondente a cada ano e indicadores do
#' calendário eleitoral. Os mandatos municipais começam em 1º de janeiro do ano
#' seguinte à eleição (eleições em 2000, 2004, ..., 2024; mandatos 2001–2004,
#' 2005–2008, ..., 2025–2028).
#'
#' @param data Base com a coluna `ano`.
#'
#' @return A base com:
#'   * `mandato`: período do mandato municipal (ex.: "2021-2024");
#'   * `ano_mandato`: ano dentro do mandato (1 a 4);
#'   * `ano_eleicao_municipal`: 1 nos anos de eleição municipal (2000, 2004, ...);
#'   * `ano_eleicao_geral`: 1 nos anos de eleição geral (2002, 2006, ...).
#' @export
add_electoral_cycle <- function(data) {
  if (!"ano" %in% names(data)) stop("A base precisa da coluna 'ano'.", call. = FALSE)
  ano <- data$ano
  ano_mandato <- ((ano - 2001) %% 4) + 1
  inicio <- ano - ano_mandato + 1
  data$mandato <- ifelse(is.na(ano), NA_character_, paste0(inicio, "-", inicio + 3))
  data$ano_mandato <- as.integer(ano_mandato)
  data$ano_eleicao_municipal <- as.integer(ano %% 4 == 0)
  data$ano_eleicao_geral <- as.integer(ano %% 4 == 2)
  data
}

#' Fixar o valor de um ano de referência
#'
#' Cria uma variável constante no tempo com o valor do indicador em um ano de
#' referência, repetida em todos os anos do município. Por exemplo, o IDHM de
#' 2010 como controle em todo o painel.
#'
#' @inheritParams calculate_lag
#' @param year Ano de referência.
#' @param name Nome da nova coluna. Padrão: "<var>_<year>".
#' @export
fix_baseline <- function(data, var, year, name = NULL) {
  .check_panel(data)
  .check_vars(data, var, numeric = FALSE)
  if (!year %in% data$ano) stop("O ano ", year, " n\u00e3o est\u00e1 na base.", call. = FALSE)
  if (is.null(name)) name <- paste0(var, "_", year)
  base <- data[data$ano == year, c("id_municipio", var)]
  data[[name]] <- base[[var]][match(data$id_municipio, base$id_municipio)]
  if (all(is.na(data[[name]]))) warning("A vari\u00e1vel '", var, "' \u00e9 toda NA em ", year, ".", call. = FALSE)
  data
}
