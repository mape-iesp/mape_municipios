# =============================================================================
# Explorar a base de dados
# =============================================================================

#' Buscar variáveis por palavra-chave
#'
#' Procura um termo no nome e na descrição das variáveis do dicionário. A busca
#' ignora acentos e maiúsculas, então `"homicidio"` encontra "Homicídios".
#' Vários termos podem ser combinados com `match = "all"` (todos) ou
#' `match = "any"` (qualquer um).
#'
#' @param term Um ou mais termos de busca.
#' @param dimension Restringe a busca a uma ou mais dimensões (opcional).
#' @param match "any" (padrão) ou "all", quando há mais de um termo.
#' @param dictionary Dicionário já carregado (opcional). Se NULL, usa
#'   [import_mape_dictionary()].
#'
#' @return Um data.frame com `variavel`, `dimensao`, `descricao` e
#'   `escopo_temporal` das variáveis encontradas.
#' @examples
#' \dontrun{
#' search_mape("homicídio")
#' search_mape(c("despesa", "saúde"), match = "all")
#' }
#' @export
search_mape <- function(term, dimension = NULL, match = c("any", "all"), dictionary = NULL) {
  match <- match.arg(match)
  dic <- .get_dictionary(dictionary)
  if (!is.null(dimension)) {
    .check_dimension(dimension)
    dic <- dic[dic$dimensao_slug %in% dimension, , drop = FALSE]
  }
  texto <- .norm(paste(dic$variavel, dic$descricao))
  hits <- vapply(.norm(term), function(t) grepl(t, texto, fixed = TRUE), logical(nrow(dic)))
  hits <- matrix(hits, nrow = nrow(dic))
  keep <- if (match == "any") rowSums(hits) > 0 else rowSums(hits) == ncol(hits)
  out <- dic[keep, c("variavel", "dimensao", "descricao", "escopo_temporal"), drop = FALSE]
  rownames(out) <- NULL
  if (nrow(out) == 0) message("Nenhuma vari\u00e1vel encontrada.")
  out
}

#' Listar as dimensões do mape_municipios
#'
#' @param dictionary Dicionário já carregado (opcional). Se informado, a
#'   listagem inclui o número de variáveis por dimensão.
#' @return Um data.frame com o nome curto (`slug`, usado nas funções de
#'   importação), o nome por extenso e, se possível, o número de variáveis.
#' @export
list_mape_dimensions <- function(dictionary = NULL) {
  dims <- .mape_dimensions()[c("slug", "nome")]
  dic <- tryCatch(.get_dictionary(dictionary), error = function(e) NULL)
  if (!is.null(dic)) {
    n <- table(factor(dic$dimensao_slug, levels = dims$slug))
    dims$n_variaveis <- as.integer(n)
  }
  dims
}

#' Listar as variáveis de uma ou mais dimensões
#'
#' @param dimension Uma ou mais dimensões. NULL = todas.
#' @param dictionary Dicionário já carregado (opcional).
#' @return Um data.frame com `variavel`, `dimensao`, `descricao`, `natureza` e
#'   `escopo_temporal`.
#' @export
list_mape_variables <- function(dimension = NULL, dictionary = NULL) {
  dic <- .get_dictionary(dictionary)
  if (!is.null(dimension)) {
    .check_dimension(dimension)
    dic <- dic[dic$dimensao_slug %in% dimension, , drop = FALSE]
  }
  out <- dic[c("variavel", "dimensao", "descricao", "natureza", "escopo_temporal")]
  rownames(out) <- NULL
  out
}

#' Ficha completa de uma variável
#'
#' Mostra todas as informações do dicionário sobre uma ou mais variáveis:
#' descrição, natureza, fonte, escopo temporal, transformações e observações
#' de validação. Se houver problemas documentados na validação, a função emite
#' um aviso, para que a variável não seja usada sem esse conhecimento.
#'
#' @param variable Nome de uma ou mais variáveis.
#' @param dictionary Dicionário já carregado (opcional).
#' @return Invisivelmente, as linhas do dicionário correspondentes. A ficha é
#'   impressa no console.
#' @export
info_mape <- function(variable, dictionary = NULL) {
  dic <- .get_dictionary(dictionary)
  variable <- tolower(variable)
  faltando <- setdiff(variable, dic$variavel)
  if (length(faltando) > 0) {
    stop("Vari\u00e1vel(is) n\u00e3o encontrada(s): ", paste(faltando, collapse = ", "),
         ". Use search_mape() para procurar.", call. = FALSE)
  }
  fichas <- dic[dic$variavel %in% variable, , drop = FALSE]
  rotulos <- c(dimensao = "Dimens\u00e3o", descricao = "Descri\u00e7\u00e3o", natureza = "Natureza",
               tipo = "Tipo", escopo_temporal = "Escopo temporal", objetivo = "Objetivo",
               transformacoes = "Transforma\u00e7\u00f5es", fonte_original = "Fonte original",
               fonte_extracao = "Fonte de extra\u00e7\u00e3o", link = "Link",
               validacao = "Valida\u00e7\u00e3o")
  for (i in seq_len(nrow(fichas))) {
    cat("\n== ", fichas$variavel[i], " ", strrep("=", 50), "\n", sep = "")
    for (col in names(rotulos)) {
      v <- fichas[[col]][i]
      if (!is.na(v) && nzchar(v)) cat(sprintf("%-18s %s\n", paste0(rotulos[[col]], ":"), v))
    }
    val <- fichas$validacao[i]
    if (!is.na(val) && grepl("NA|problema|erro|incompat|verificar|aten", val, ignore.case = TRUE)) {
      warning("A vari\u00e1vel '", fichas$variavel[i],
              "' tem observa\u00e7\u00f5es na valida\u00e7\u00e3o. Leia antes de usar.", call. = FALSE)
    }
  }
  invisible(fichas)
}

#' Cobertura das variáveis (percentual de dados válidos)
#'
#' Calcula o percentual de observações não ausentes por variável e ano e
#' sinaliza as combinações abaixo de um limiar (padrão: 50%). Opcionalmente,
#' desenha um mapa de calor ano × variável.
#'
#' @param data Base no formato município × ano.
#' @param vars Variáveis a avaliar. NULL = todas, exceto identificadores.
#' @param by "variavel_ano" (padrão), "variavel" ou "ano".
#' @param threshold Limiar de cobertura, em %.
#' @param plot Se TRUE, retorna um gráfico ggplot2 em vez da tabela
#'   (somente com `by = "variavel_ano"`).
#'
#' @return Um data.frame com `cobertura` (% de valores válidos), `n_validos`,
#'   `n_total` e `abaixo_limiar` (TRUE/FALSE), ou um objeto ggplot.
#' @examples
#' \dontrun{
#' coverage_mape(bd, c("pib", "taxa_homicidios"))
#' coverage_mape(bd, plot = TRUE)
#' }
#' @export
coverage_mape <- function(data, vars = NULL, by = c("variavel_ano", "variavel", "ano"),
                          threshold = 50, plot = FALSE) {
  by <- match.arg(by)
  ids <- c("id_municipio", "ano", "sigla_uf", "nome_municipio")
  if (is.null(vars)) vars <- setdiff(names(data), ids)
  .check_vars(data, vars, numeric = FALSE)
  if (by != "variavel" && !"ano" %in% names(data)) {
    stop("A base n\u00e3o tem a coluna 'ano'. Use by = \"variavel\".", call. = FALSE)
  }
  ano <- if ("ano" %in% names(data)) data$ano else rep(NA_integer_, nrow(data))
  long <- do.call(rbind, lapply(vars, function(v) {
    data.frame(variavel = v, ano = ano, valido = !is.na(data[[v]]), stringsAsFactors = FALSE)
  }))
  grupos <- switch(by, variavel_ano = c("variavel", "ano"), variavel = "variavel", ano = "ano")
  out <- long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) %>%
    dplyr::summarise(n_validos = sum(.data$valido), n_total = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(cobertura = round(100 * .data$n_validos / .data$n_total, 1),
                  abaixo_limiar = .data$cobertura < threshold)
  out <- as.data.frame(out)

  if (plot) {
    if (by != "variavel_ano") stop("O gr\u00e1fico exige by = \"variavel_ano\".", call. = FALSE)
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Instale o pacote 'ggplot2' para gerar o gr\u00e1fico.", call. = FALSE)
    }
    out$variavel <- factor(out$variavel, levels = rev(vars))
    return(
      ggplot2::ggplot(out, ggplot2::aes(x = .data$ano, y = .data$variavel, fill = .data$cobertura)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient(low = "#F4E3D7", high = "#1F4E4C", limits = c(0, 100),
                                     name = "% v\u00e1lido") +
        ggplot2::labs(x = "Ano", y = NULL,
                      title = "Cobertura das vari\u00e1veis",
                      subtitle = paste0("Limiar de refer\u00eancia: ", threshold, "%")) +
        ggplot2::theme_minimal()
    )
  }
  out
}

#' Citação do mape_municipios e das fontes originais
#'
#' Gera a citação do banco e lista as fontes originais das variáveis
#' efetivamente usadas, para incluir em artigos e materiais de replicação.
#'
#' @param data Base usada na análise (opcional). As variáveis são lidas dos
#'   nomes das colunas.
#' @param vars Alternativa a `data`: vetor com os nomes das variáveis usadas.
#' @param dictionary Dicionário já carregado (opcional).
#'
#' @return Invisivelmente, uma lista com `citacao` (texto) e `fontes`
#'   (data.frame com as fontes e as variáveis de cada uma). Ambos são impressos.
#' @export
cite_mape <- function(data = NULL, vars = NULL, dictionary = NULL) {
  versao <- tryCatch(mape_version(), error = function(e) NA_character_)
  citacao <- paste0(
    "MAPE-IESP/UERJ. mape_municipios: Mapeamento Abrangente dos Munic\u00edpios Brasileiros",
    if (!is.na(versao)) paste0(" (vers\u00e3o ", versao, ")") else "",
    ". Rio de Janeiro: IESP-UERJ, ", format(Sys.Date(), "%Y"), "."
  )
  # PROVISORIO: ajustar autoria, DOI e URL quando a base for publicada.
  cat("Cita\u00e7\u00e3o do banco:\n  ", citacao, "\n", sep = "")

  if (!is.null(data)) vars <- names(data)
  fontes <- NULL
  if (!is.null(vars)) {
    dic <- .get_dictionary(dictionary)
    usados <- dic[dic$variavel %in% setdiff(tolower(vars), c("id_municipio", "ano")) &
                    !dic$dimensao_slug %in% "identificacao", ]
    usados$fonte_original[is.na(usados$fonte_original)] <- "(fonte n\u00e3o informada)"
    fontes <- usados %>%
      dplyr::group_by(.data$fonte_original) %>%
      dplyr::summarise(variaveis = paste(sort(.data$variavel), collapse = ", "),
                       .groups = "drop") %>%
      as.data.frame()
    cat("\nFontes originais das vari\u00e1veis usadas:\n")
    for (i in seq_len(nrow(fontes))) {
      cat("  - ", fontes$fonte_original[i], ": ", fontes$variaveis[i], "\n", sep = "")
    }
  }
  invisible(list(citacao = citacao, fontes = fontes))
}
