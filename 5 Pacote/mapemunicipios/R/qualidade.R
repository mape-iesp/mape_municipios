# =============================================================================
# Qualidade e exportação
# =============================================================================

#' Checar a estrutura da base
#'
#' Verifica os problemas mais comuns em bases municipais em painel:
#' * `id_municipio` que não é texto ou não tem 7 dígitos;
#' * colunas `integer64` (vindas do BigQuery), que causam erros silenciosos
#'   em junções e em `ifelse()`/`fifelse()`;
#' * linhas duplicadas de município × ano;
#' * `ano` que não é inteiro;
#' * colunas 100% NA.
#'
#' @param data Base no formato município × ano.
#' @param fix Se TRUE, corrige o que pode ser corrigido automaticamente
#'   (converte `integer64` para numérico, `id_municipio` para texto e `ano`
#'   para inteiro) e devolve a base corrigida.
#'
#' @return Se `fix = FALSE`, invisivelmente, um data.frame com o resultado de
#'   cada checagem (`ok` = TRUE/FALSE e `detalhe`). Se `fix = TRUE`, a base
#'   corrigida. O resumo é impresso no console.
#' @export
check_mape <- function(data, fix = FALSE) {
  res <- list()
  add <- function(checagem, ok, detalhe = "") {
    res[[length(res) + 1]] <<- data.frame(checagem = checagem, ok = ok, detalhe = detalhe,
                                          stringsAsFactors = FALSE)
  }
  tem_id <- "id_municipio" %in% names(data)
  tem_ano <- "ano" %in% names(data)
  add("Coluna id_municipio presente", tem_id)
  add("Coluna ano presente", tem_ano)

  i64 <- names(data)[vapply(data, inherits, logical(1), what = "integer64")]
  add("Sem colunas integer64", length(i64) == 0, paste(i64, collapse = ", "))

  if (tem_id) {
    id <- data$id_municipio
    add("id_municipio \u00e9 texto", is.character(id), class(id)[1])
    n_bad <- sum(!grepl("^\\d{7}$", as.character(id)) & !is.na(id))
    add("id_municipio com 7 d\u00edgitos", n_bad == 0,
        if (n_bad > 0) paste(n_bad, "valores fora do padr\u00e3o") else "")
    add("id_municipio sem NA", !anyNA(id), paste(sum(is.na(id)), "NA"))
  }
  if (tem_ano) {
    add("ano \u00e9 inteiro", is.integer(data$ano), class(data$ano)[1])
  }
  if (tem_id && tem_ano) {
    dup <- sum(duplicated(data[c("id_municipio", "ano")]))
    add("Sem duplicatas munic\u00edpio \u00d7 ano", dup == 0, if (dup > 0) paste(dup, "linhas") else "")
  }
  todo_na <- names(data)[vapply(data, function(x) all(is.na(x)), logical(1))]
  add("Sem colunas 100% NA", length(todo_na) == 0, paste(todo_na, collapse = ", "))

  res <- do.call(rbind, res)
  for (i in seq_len(nrow(res))) {
    cat(if (res$ok[i]) "  [ok]    " else "  [ATEN\u00c7\u00c3O] ", res$checagem[i],
        if (!res$ok[i] && nzchar(res$detalhe[i])) paste0(" (", res$detalhe[i], ")") else "",
        "\n", sep = "")
  }
  if (fix) {
    data <- .fix_types(data)
    message("Tipos corrigidos. Duplicatas e colunas 100% NA n\u00e3o s\u00e3o alteradas automaticamente.")
    return(data)
  }
  invisible(res)
}

#' Sinalizar valores atípicos nas séries municipais
#'
#' Marca valores suspeitos dentro da série de cada município, que costumam ser
#' erros de registro. Os valores não são alterados, apenas sinalizados.
#'
#' Métodos:
#' * "mad" (padrão): o valor se afasta da mediana da série do município mais
#'   do que `threshold` desvios absolutos medianos;
#' * "jump": o valor é mais de `threshold` vezes maior (ou menor) que o do
#'   ano anterior.
#'
#' @param data Base no formato município × ano.
#' @param var Indicador a verificar.
#' @param method "mad" ou "jump".
#' @param threshold Limite de sinalização. Padrão: 5 para "mad" e 10 para "jump".
#' @param name Nome da nova coluna. Padrão: "<var>_atipico".
#'
#' @return A base com a coluna indicadora (1 = atípico, 0 = normal,
#'   NA = sem dado ou série curta demais para avaliar).
#' @export
flag_outliers <- function(data, var, method = c("mad", "jump"), threshold = NULL, name = NULL) {
  method <- match.arg(method)
  .check_panel(data)
  .check_vars(data, var)
  if (is.null(threshold)) threshold <- if (method == "mad") 5 else 10
  if (is.null(name)) name <- paste0(var, "_atipico")
  x <- data[[var]]

  if (method == "mad") {
    flag <- stats::ave(x, data$id_municipio, FUN = function(v) {
      if (sum(!is.na(v)) < 4) return(rep(NA_real_, length(v)))
      med <- stats::median(v, na.rm = TRUE)
      dev <- stats::mad(v, na.rm = TRUE)
      if (dev == 0) return(as.numeric(v != med))
      as.numeric(abs(v - med) / dev > threshold)
    })
  } else {
    ant <- .value_at_year(data, var, 1)
    razao <- x / ant
    flag <- ifelse(is.na(razao) | !is.finite(razao) | ant == 0, NA_real_,
                   as.numeric(razao > threshold | razao < 1 / threshold))
  }
  flag[is.na(x)] <- NA_real_
  data[[name]] <- as.integer(flag)
  n <- sum(data[[name]] == 1, na.rm = TRUE)
  if (n > 0) message(n, " valor(es) at\u00edpico(s) sinalizado(s) em '", var, "'.")
  data
}

#' Aplicar as descrições do dicionário como rótulos das variáveis
#'
#' Grava a descrição de cada variável no atributo `label`, que é reconhecido
#' pelo RStudio, pelo haven (Stata e SPSS) e por pacotes de tabelas como
#' gtsummary e modelsummary.
#'
#' @param data Base do mape_municipios.
#' @param dictionary Dicionário já carregado (opcional).
#' @return A base com os rótulos. Variáveis criadas pelo usuário ou pelas
#'   funções do pacote ficam sem rótulo.
#' @export
label_mape <- function(data, dictionary = NULL) {
  dic <- .get_dictionary(dictionary)
  for (v in intersect(names(data), dic$variavel)) {
    lab <- dic$descricao[match(v, dic$variavel)]
    if (!is.na(lab)) attr(data[[v]], "label") <- lab
  }
  data
}

#' Exportar a base
#'
#' Salva a base no formato indicado pela extensão do arquivo, preservando os
#' rótulos quando o formato permite.
#'
#' Formatos:
#' * `.csv`: UTF-8, separador vírgula;
#' * `.dta` (Stata) e `.sav` (SPSS): via haven, com rótulos. O Stata limita os
#'   nomes a 32 caracteres; nomes maiores geram erro;
#' * `.parquet`: via arrow;
#' * `.xlsx`: via openxlsx, com o padrão visual do projeto (Arial 10, cabeçalho
#'   verde-escuro, primeira linha congelada e filtro) e, opcionalmente, uma
#'   segunda aba com o dicionário das variáveis exportadas.
#' * `.rds`: formato nativo do R.
#'
#' @param data Base a exportar.
#' @param file Caminho do arquivo, com extensão.
#' @param dictionary Para `.xlsx`: se TRUE, inclui a aba "dicionario".
#' @param overwrite Se FALSE (padrão), não sobrescreve arquivos existentes.
#'
#' @return Invisivelmente, o caminho do arquivo.
#' @export
export_mape <- function(data, file, dictionary = TRUE, overwrite = FALSE) {
  if (file.exists(file) && !overwrite) {
    stop("O arquivo j\u00e1 existe. Use overwrite = TRUE para substituir.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(file))
  data <- .fix_types(data)
  precisa <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Instale o pacote '", pkg, "' para exportar em .", ext, ".", call. = FALSE)
    }
  }
  switch(ext,
    csv = utils::write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8", na = ""),
    rds = saveRDS(data, file),
    dta = {
      precisa("haven")
      longos <- names(data)[nchar(names(data)) > 32]
      if (length(longos) > 0) {
        stop("O Stata aceita nomes de at\u00e9 32 caracteres. Renomeie: ",
             paste(longos, collapse = ", "), call. = FALSE)
      }
      haven::write_dta(data, file)
    },
    sav = { precisa("haven"); haven::write_sav(data, file) },
    parquet = { precisa("arrow"); arrow::write_parquet(data, file) },
    xlsx = {
      precisa("openxlsx")
      wb <- openxlsx::createWorkbook()
      .xlsx_sheet(wb, "dados", data)
      if (isTRUE(dictionary)) {
        dic <- tryCatch(.get_dictionary(), error = function(e) NULL)
        if (!is.null(dic)) {
          dic <- dic[dic$variavel %in% names(data),
                     c("variavel", "dimensao", "descricao", "natureza", "escopo_temporal",
                       "fonte_original", "link")]
          .xlsx_sheet(wb, "dicionario", dic)
        }
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    },
    stop("Extens\u00e3o n\u00e3o suportada: .", ext,
         ". Use csv, dta, sav, parquet, xlsx ou rds.", call. = FALSE)
  )
  message("Base salva em ", file)
  invisible(file)
}

# Aba de Excel no padrão visual do projeto
.xlsx_sheet <- function(wb, sheet, df) {
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, df, withFilter = TRUE)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(fontName = "Arial", fontSize = 10),
                     rows = 1:(nrow(df) + 1), cols = seq_along(df), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet,
                     openxlsx::createStyle(fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
                                           fgFill = "#1F4E4C", textDecoration = "bold"),
                     rows = 1, cols = seq_along(df), gridExpand = TRUE)
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = seq_along(df), widths = "auto")
}
