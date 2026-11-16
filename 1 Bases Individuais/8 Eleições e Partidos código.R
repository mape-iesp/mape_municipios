# ══════════════════════════════════════════════════════════════════════════════
# 8 Eleições e Partidos — PIPELINE ÚNICO (do início até "5. Eleicoes_Municipais")
# ══════════════════════════════════════════════════════════════════════════════
#
# Este arquivo junta, NA ORDEM EM QUE PRECISAM RODAR, os scripts da dimensão
# "8 Eleições e Partidos". Os arquivos originais NÃO foram alterados; cada
# parte abaixo é uma cópia do original, e toda mudança feita aqui está marcada
# com o comentário [UNICO].
#
# REVISÃO [REV 2026-11] — correções feitas depois da comparação com a base original
# Toda mudança está marcada com [REV 2026-11]
# no próprio trecho. Resumo, todas na PARTE 5 (Eleições Municipais):
#   R1. Incumbência: candidato com dois registros (status 0 e 1) gerava
#       município-ano duplicado em 2004 -> 1 linha por pessoa (seção 2).
#   R2. Elegibilidade do prefeito comparava com 12 anos antes da eleição
#       (+8 sobre o ano do titular); o certo é o mandato anterior (+4).
#       Ganha a eleição de 2008, antes toda em NA (seção 2.1).
#   R3. Empate de votos no 1º/2º colocado: desempate pelo eleito (seção 6).
#   R4. Filtro "ORDIN" deixava passar "EXTRAORDINÁRIA": coligação podia vir
#       de eleição extraordinária (seção 7). Filtro aplicado DEPOIS da
#       leitura da base bruta (que continua sem tratamento).
#   R5. Gênero/raça: candidaturas deduplicadas e ligadas aos votos pela
#       PESSOA (título eleitoral), não pelo número de urna; número de urna
#       só como alternativa para votos sem título. Com testes que param o
#       script se o join duplicar votos (seção 9).
#   R6. Junção final: incumbência entra com left_join (antes full_join
#       criava o ano 2028 e municípios sem eleição em 2004) (seção 13).
#   R7. Antes de exportar: NEP infinito vira NA, eleitorado como inteiro e
#       teste de município-ano duplicado (seção 13).
#   R8. Retiradas linhas de teste soltas que quebravam a execução do zero.
# Depois de editar, reinicie o R antes de rodar: objetos antigos na memória
# podem mascarar erros (ex.: um objeto renomeado que continua existindo).
#
# Ordem de execução (cada parte aparece no "outline" do RStudio):
#   PARTE 0       Configuração: pacotes, projeto do Google Cloud, pastas, opções
#   PARTE 0_dados Bases de apoio: tabela de códigos TSE x IBGE (cod_mun_ibge_tse)
#   PARTE 3       Funções auxiliares ("3. Funcoes_Partidos_Indices.R")
#   PARTE 8       Eleitorado e diretórios partidários ("8 Eleições e Partidos código.R")
#                 -> eleitorado_municipio.rds / diretorios_partidarios.rds
#   PARTE 4       Eleições Gerais ("4. Eleicoes_Gerais.R")  -> eleicoes_gerais.xlsx/.RData
#   PARTE 5       Eleições Municipais ("5. Eleicoes_Municipais.R") -> eleicoes_municipais.xlsx/.RData
#   PARTE 6       Checagens do protocolo (duplicidade, classes, NA, painel anual)
#   ANEXO         Scripts antigos 0 / 1 / 2 (municipalityBR) — NÃO são necessários
#                 para as bases finais; só rodam se RODAR_LEGADO <- TRUE.
#
# Dependências entre as partes:
#   - A PARTE 3 tem de rodar antes das PARTES 4 e 5 (elas usam as funções).
#   - A PARTE 8 tem de rodar antes das PARTES 4 e 5 (elas leem os dois .rds).
#   - As PARTES 4 e 5 são independentes entre si (podem rodar em sessões
#     separadas, desde que as PARTES 0, 0_dados, 3 e 8 rodem antes).
#
# Sugestão: rode parte por parte (Ctrl+Alt+T no RStudio roda a seção atual).
# As consultas ao BigQuery das PARTES 4 e 5 são grandes e demoram.
#
# Bases brutas: toda extração do BigQuery/TSE é salva em
#   v2/1 Bases Individuais/8 Eleições e Partidos/0_dados/brutos/<parte>_<tabela>.rds
# antes de qualquer tratamento (já com o filtro de anos/colunas da consulta),
# e anotada em brutos/registro_extracoes.csv (fonte, filtro, data, nº de linhas).
# Com REUSAR_BRUTOS <- TRUE, rodar de novo lê esses arquivos e não refaz as
# consultas. Ver `coletar_bruto()` na PARTE 0.
#
# Bases de entrada que NENHUM script do projeto produz (vieram prontas):
#   - cod_mun_ibge_tse.csv            (a PARTE 0_dados recria pelo basedosdados se faltar)
#   - banco_diretorios_municipal2.xlsx (SGIP/TSE já tratado por partido-município-ano;
#                                       sem script de origem — ver PARTE 8)
#
# ADOTADO COMO CÓDIGO RAIZ DESTA DIMENSÃO em 13/11/2026 (validação): este
# arquivo é a revisão de Jorge (v2/1 Bases Individuais/8 Eleições e Partidos/
# Validação_Jorge/8_eleições e partidos.zip), que já junta e corrige os 8
# bugs acima encontrados ao comparar com a base original. Única mudança
# feita aqui: BILLING_ID preenchido com o projeto do Google Cloud do projeto
# ("dados-importacao"). Substitui o conjunto fragmentado anterior (código.R
# na raiz só gerando os .rds + source() de 1_script/3,4,5), que não rodava
# (1_script estava vazio) e tinha consultas mortas (candidatos/resultados
# nunca usados). A dimensão 8 agora é um único arquivo; "eleicoes_municipios.R"
# e "eleicoes.xlsx" (raiz da dimensão) são leftovers de uma versão anterior,
# não usados por este script — não apagados nesta validação.

# ==== PARTE 0 · Configuração ====

library(basedosdados)
library(tidyverse)   # dplyr, tidyr, stringr, purrr, readr, ggplot2...
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(openxlsx)
library(readxl)
library(rio)
library(electionsBR)
library(bit64)

options(scipen = 999)      # sem notação científica
options(timeout = 3600)    # [UNICO] downloads grandes do TSE (electionsBR) estouravam o limite padrão de 60 s

# Projeto do Google Cloud (BigQuery) de quem roda o script — definido UMA vez aqui.
BILLING_ID <- "dados-importacao"
set_billing_id(BILLING_ID)

# Chaves de execução ------------------------------------------------------------
REFAZER_ELEITORADO <- FALSE  # TRUE = refaz a consulta ao BigQuery do perfil do eleitorado
REFAZER_DIRETORIOS <- FALSE  # TRUE = recalcula diretorios_partidarios.rds a partir do .xlsx
FAZER_PAINEL_ANUAL <- TRUE   # PARTE 6: painel município x ano com anos entre eleições (protocolo 4.3)
ANO_FINAL_PAINEL   <- 2026
RODAR_LEGADO       <- FALSE  # ANEXO: scripts antigos 0/1/2 (não necessários)
REUSAR_BRUTOS      <- TRUE   # TRUE = se a base bruta já foi salva em 0_dados/brutos, lê do arquivo
                             #        em vez de consultar o BigQuery/TSE de novo;
                             # FALSE = sempre baixa de novo (e sobrescreve o arquivo salvo).

# Pastas — criadas logo no início (antes eram criadas só no final do script 4,
# e sem `recursive = TRUE`; aí o saveRDS() do script 8 falhava se a pasta
# 0_dados ainda não existisse).
dir_dim    <- here("1 Bases Individuais", "8 Eleições e Partidos")
dir_dados  <- file.path(dir_dim, "0_dados")
dir_script <- file.path(dir_dim, "1_script")
dir_diag   <- file.path(dir_dim, "diagnostico_NA")
dir_brutos <- file.path(dir_dados, "brutos")   # bases brutas (protocolo, Etapa 1)
for (d in c(dir_dados, dir_script, dir_diag, dir_brutos)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

cat("Raiz do projeto (here):", here(), "\n")
# CONFERIR: a raiz deve ser a pasta "mape_municiipios" (onde está o .Rproj).
# Se aparecer outra pasta, abra o projeto pelo arquivo mape_municiipios.Rproj.

# Funções utilitárias [UNICO] ----------------------------------------------------

# O BigQuery devolve colunas INT64 como integer64 (pacote bit64), o que causa
# erros silenciosos em joins e if_else() (protocolo, seção 4.2). Converte tudo
# para numeric logo depois de cada bd_collect().
corrigir_int64 <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::where(bit64::is.integer64), as.numeric))
}

# O electionsBR descompacta os arquivos do TSE numa pasta com o nome do ano
# DENTRO da pasta de trabalho, faz setwd() para ela e depois apaga. Dentro do
# OneDrive isso deixava as pastas 2000/, 2002/... vazias na raiz do projeto e,
# se der erro no meio, a sessão fica "presa" dentro da pasta do ano. Aqui ele
# roda dentro de uma pasta temporária e sempre volta para a pasta original.
elections_tse_tmp <- function(...) {
  wd_original <- getwd()
  on.exit(setwd(wd_original), add = TRUE)
  setwd(tempdir())
  electionsBR::elections_tse(...)
}

# Salvamento das bases brutas [UNICO] (protocolo, Etapa 1: "salve a base bruta,
# sem qualquer tratamento"). Cada coleta do BigQuery/TSE passa por aqui:
#   - salva o resultado da extração em 0_dados/brutos/<nome>.rds, logo depois
#     de baixar e ANTES de qualquer tratamento do script;
#   - registra em 0_dados/brutos/registro_extracoes.csv a fonte, o filtro
#     aplicado na extração, a data e o número de linhas/colunas (informações
#     pedidas na aba "Geral" da planilha METADADOS);
#   - se REUSAR_BRUTOS = TRUE e o arquivo já existir, lê dele e não baixa de novo.
# "Bruto" aqui = a extração como a consulta devolve: por causa do tamanho, as
# consultas já trazem só os anos e colunas usados (o filtro fica registrado).
# A única mudança é de TIPO (integer64 -> numeric), não de conteúdo.
#
# `baixar` deve ser uma FUNÇÃO sem argumentos (ex.: function() bd_collect(q)),
# para que a consulta só rode quando o arquivo salvo não for reaproveitado.
coletar_bruto <- function(nome, baixar, fonte, filtro = "") {
  arq <- file.path(dir_brutos, paste0(nome, ".rds"))
  if (REUSAR_BRUTOS && file.exists(arq)) {
    message("Lendo base bruta já salva: ", arq)
    return(readRDS(arq))
  }
  x <- corrigir_int64(baixar())
  saveRDS(x, arq)   # .rds comprimido (gzip)
  registro <- data.frame(
    arquivo = basename(arq), fonte = fonte, filtro_extracao = filtro,
    data_extracao = format(Sys.time(), "%Y-%m-%d %H:%M"),
    n_linhas = nrow(x), n_colunas = ncol(x),
    tamanho_mb = round(file.size(arq) / 1024^2, 1)
  )
  arq_reg <- file.path(dir_brutos, "registro_extracoes.csv")
  readr::write_csv(registro, arq_reg, append = file.exists(arq_reg))
  message("Base bruta salva: ", arq, " (", nrow(x), " linhas)")
  x
}

# ==== PARTE 0_dados · Bases de apoio ====
# Tabela de códigos TSE x IBGE. Nenhum script do projeto a produz; ela já
# estava pronta em "v2/cod_mun_ibge_tse.csv" (cópia idêntica em 0_dados).
# Se não existir em 0_dados, copia de v2/; se também não existir, recria a
# partir do diretório de municípios do basedosdados.

arq_cod_mun <- file.path(dir_dados, "cod_mun_ibge_tse.csv")

if (!file.exists(arq_cod_mun) && file.exists(here("cod_mun_ibge_tse.csv"))) {
  file.copy(here("cod_mun_ibge_tse.csv"), arq_cod_mun)
}

if (!file.exists(arq_cod_mun)) {
  # CONFERIR nomes de colunas em basedosdados.org (br_bd_diretorios_brasil.municipio).
  cod_mun_ibge_tse <- coletar_bruto(
    "0_diretorio_municipios",
    function() basedosdados::bdplyr("basedosdados.br_bd_diretorios_brasil.municipio") %>%
      dplyr::select(id_municipio, id_municipio_tse, sigla_uf, nome, capital_uf) %>%
      basedosdados::bd_collect(),
    fonte  = "basedosdados.br_bd_diretorios_brasil.municipio",
    filtro = "colunas: id_municipio, id_municipio_tse, sigla_uf, nome, capital_uf"
  ) %>%
    dplyr::transmute(
      codigo_tse     = as.numeric(id_municipio_tse),
      uf             = sigla_uf,
      nome_municipio = toupper(nome),
      capital        = as.integer(capital_uf),
      codigo_ibge    = as.numeric(id_municipio)
    )
  readr::write_csv(cod_mun_ibge_tse, arq_cod_mun)
}

cod_mun_ibge_tse <- rio::import(arq_cod_mun)
stopifnot(all(c("codigo_tse", "uf", "nome_municipio", "capital", "codigo_ibge") %in% names(cod_mun_ibge_tse)))
cat("cod_mun_ibge_tse:", nrow(cod_mun_ibge_tse), "municípios\n")


# ==== PARTE 3 · Funções auxiliares ("3. Funcoes_Partidos_Indices.R") ====
# Copiado do original; só foram retirados os library() (já carregados na PARTE 0).

# ══════════════════════════════════════════════════════════════════════════════
# 8 Eleições e Partidos — Funções auxiliares
# Harmonização de siglas partidárias, Número Efetivo de Partidos (NEP),
# volatilidade eleitoral (índice de Pedersen), índice de equidade
# racial/de gênero (DRI de Firpo, França, Pereira, Portella & Tavares) e
# separação de coligações em partidos (federações desmembradas).
# ══════════════════════════════════════════════════════════════════════════════
#
# Este arquivo é carregado (source) por "4. Eleicoes_Gerais.R" e
# "5. Eleicoes_Municipais.R". Não baixa nem processa nenhum dado por si só.


# ── 1. Harmonização de siglas partidárias ao longo do tempo ──────────────────
# Regra (validada com a coordenação da base em 03/11/2026):
#   - RENOMEAÇÃO (o mesmo partido trocou de nome): unificada em TODOS os anos,
#     sob o nome mais recente — ex.: PFL (até 2006) e DEM aparecem como "DEM".
#   - FUSÃO/INCORPORAÇÃO (partidos distintos que se juntaram): só vale a
#     partir da primeira eleição depois dela; antes disso cada partido mantém
#     a própria sigla — ex.: DEM e PSL ficam separados até 2020 e só a partir
#     de 2022 aparecem como "UNIAO".
#   - PSD extinto em 2003 (votos em 2000 e 2002) vira "PSD_ANTIGO", para não
#     se confundir com o PSD criado em 2011.
# Na volatilidade, porém, a eleição anterior a uma fusão é somada sob o
# partido resultante (ver `calcular_volatilidade()`), para que a fusão não
# seja contada como saída de partidos antigos + entrada de um novo.
#
# Siglas pequenas/extintas sem sucessor claro não entram nas tabelas e
# permanecem com sua própria sigla. Revisar sempre que houver nova
# renomeação/fusão.
#
# Fontes: cronologia de fusões/renomeações conferida na linha do tempo
# interativa do Nexo Jornal ("A genealogia e o perfil dos partidos
# brasileiros", atualizada em maio de 2026,
# https://www.nexojornal.com.br/especial/2024/09/25/politica-origem-partidos-brasil-genealogia);
# incorporação do PAN ao PTB (2007) conferida na Resolução TSE nº
# 22.519/2007 (https://www.tse.jus.br/legislacao/compilada/res/2007/resolucao-no-22-519-de-15-de-marco-de-2007).

# Renomeações — valem em todos os anos (sigla da época -> nome mais recente).
# Inclui variações de grafia da mesma sigla no TSE/basedosdados.
renomeacoes_partido <- tibble::tribble(
  ~sigla_partido,    ~sigla_partido_harm,
  "PFL",             "DEM",            # 2007
  "DEM",             "DEM",
  "PMDB",            "MDB",            # 2017
  "MDB",             "MDB",
  "PPB",             "PP",             # 2003
  "PROGRESSISTAS",   "PP",
  "PP",              "PP",
  "PT do B",         "AVANTE",         # 2017
  "PTdoB",           "AVANTE",
  "PT DO B",         "AVANTE",
  "AVANTE",          "AVANTE",
  "PPS",             "CIDADANIA",      # 2019
  "CIDADANIA",       "CIDADANIA",
  "PTN",             "PODE",           # 2017
  "PODEMOS",         "PODE",
  "PODE",            "PODE",
  "PSDC",            "DC",             # 2017
  "DC",              "DC",
  "PRB",             "REPUBLICANOS",   # 2019
  "REPUBLICANOS",    "REPUBLICANOS",
  "PRN",             "AGIR",           # PRN -> PTC (2000) -> AGIR (2022)
  "PTC",             "AGIR",
  "AGIR",            "AGIR",
  "PMN",             "MOBILIZA",       # 2023
  "MOBILIZA",        "MOBILIZA",
  # PMB (2015) renomeado Democrata em 2025 — SEM relação com o antigo
  # Democratas/DEM; são linhagens distintas apesar do nome parecido.
  "PMB",             "DEMOCRATA",
  "DEMOCRATA",       "DEMOCRATA",
  "PR",              "PL",             # 2019
  "PL",              "PL",
  "PEN",             "PATRIOTA",       # 2017
  "PATRIOTA",        "PATRIOTA",
  "SD",              "SOLIDARIEDADE",
  "SOLIDARIEDADE",   "SOLIDARIEDADE",
  "PC do B",         "PCdoB",
  "PC DO B",         "PCdoB",
  "PCDOB",           "PCdoB",
  "PCdoB",           "PCdoB",
  "UNIÃO",           "UNIAO",
  "UNIAO BRASIL",    "UNIAO",
  "UNIAO",           "UNIAO"
)

# Fusões/incorporações — valem a partir de `ano_inicio` (1ª eleição depois
# da fusão). As siglas já estão no nome pós-renomeação.
fusoes_partido <- tibble::tribble(
  ~sigla_partido, ~sigla_partido_harm, ~ano_inicio,
  "PSD_ANTIGO",   "PTB",               2004,  # PSD (1987) incorporado ao PTB em 2003
  "PST",          "PL",                2004,  # incorporados ao PL em 2003
  "PGT",          "PL",                2004,
  "PRONA",        "PL",                2008,  # PL + PRONA = PR (dez/2006), hoje PL
  "PAN",          "PTB",               2008,  # Res. TSE 22.519/2007
  "PRP",          "PATRIOTA",          2020,  # incorporado em 2019
  "PHS",          "PODE",              2020,  # incorporado em 2019
  "PPL",          "PCdoB",             2020,  # incorporado em 2019
  "DEM",          "UNIAO",             2022,  # DEM + PSL = União Brasil
  "PSL",          "UNIAO",             2022,
  "PATRIOTA",     "PRD",               2024,  # PATRIOTA + PTB = PRD (nov/2023)
  "PTB",          "PRD",               2024,
  "PSC",          "PODE",              2024,  # incorporado em 2023
  "PROS",         "SOLIDARIEDADE",     2024   # incorporado em 2023
)

#' Harmoniza siglas partidárias segundo a regra acima.
#' @param sigla vetor de siglas (da época ou já harmonizadas).
#' @param ano vetor (ou escalar) com o ano da eleição em que a sigla deve ser
#'   lida: renomeações valem sempre; fusões só quando `ano >= ano_inicio`.
#'   Chamar de novo com um ano posterior leva a sigla ao partido em que ela
#'   estava fundida naquele ano (usado na volatilidade).
harmonizar_sigla_partido <- function(sigla, ano) {
  sigla <- stringr::str_trim(sigla)
  ano   <- rep_len(ano, length(sigla))

  sigla <- dplyr::if_else(sigla %in% "PSD" & ano <= 2002, "PSD_ANTIGO", sigla)
  sigla <- dplyr::coalesce(
    renomeacoes_partido$sigla_partido_harm[match(sigla, renomeacoes_partido$sigla_partido)],
    sigla
  )
  # Fusões podem se encadear (ex.: PEN -> PATRIOTA -> PRD); repete até estabilizar.
  repeat {
    i <- match(sigla, fusoes_partido$sigla_partido)
    aplica <- !is.na(i) & ano >= fusoes_partido$ano_inicio[i]
    if (!any(aplica)) break
    sigla[aplica] <- fusoes_partido$sigla_partido_harm[i[aplica]]
  }
  sigla
}

#' Separa a composição de uma coligação (`DS_COMPOSICAO_COLIGACAO` do TSE) em
#' um vetor de siglas partidárias, uma por partido.
#'
#' Federações partidárias (a partir de 2022) NÃO são coligações: são uniões
#' de partidos que continuam existindo individualmente e elegendo seus
#' próprios vereadores. O TSE as grava no meio da coligação no formato
#' "Federação BRASIL DA ESPERANÇA - FE BRASIL(PT/PC do B/PV)" — o nome da
#' federação é descartado e cada partido entre parênteses vira um elemento
#' próprio (PT, PC do B e PV), para casar com a tabela de cadeiras por partido.
#' O split usa "/" com espaços opcionais dos dois lados (dentro da federação
#' não há espaço) sobre a string aparada, descartando elementos vazios — o
#' que também resolve coligações que começam/terminam com o separador
#' (ex.: "/ PP / PTB").
#'
#' @param coligacao vetor de strings de composição da coligação.
#' @return lista do mesmo tamanho de `coligacao`, cada elemento um vetor de
#'   siglas (vetor vazio quando a string é NA ou vazia).
separar_partidos_coligacao <- function(coligacao) {
  coligacao %>%
    stringr::str_replace_all(
      stringr::regex("Federa\\S*[^(/]*\\(([^)]*)\\)", ignore_case = TRUE), "\\1"
    ) %>%
    stringr::str_trim() %>%
    stringr::str_split("\\s*/\\s*") %>%
    purrr::map(~ .x[!is.na(.x) & .x != ""])
}

# ── 2. Número Efetivo de Partidos (NEP; Laakso & Taagepera, 1979) ────────────
#' @param df data.frame contendo a coluna de votos e as colunas de agrupamento
#'   em `chaves` (tipicamente ano, id_municipio, cargo — já filtrado para o
#'   1º turno antes de chamar esta função).
#' @param chaves vetor de nomes de colunas a agrupar (não deve incluir partido).
#' @param var_votos nome da coluna com os votos de cada partido no grupo.
calcular_nep <- function(df, chaves, var_votos = "votos") {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(chaves))) %>%
    dplyr::mutate(
      votos_totais_grupo = sum(.data[[var_votos]], na.rm = TRUE),
      prop = .data[[var_votos]] / votos_totais_grupo
    ) %>%
    dplyr::summarise(
      nep             = 1 / sum(prop^2, na.rm = TRUE),
      fracionalizacao = 1 - sum(prop^2, na.rm = TRUE),
      .groups = "drop"
    )
}

# ── 3. Volatilidade eleitoral (índice de Pedersen) ────────────────────────────
# Soma, para cada partido, o módulo da diferença entre sua participação de
# votos na eleição atual e na eleição anterior (do mesmo cargo e município),
# dividida por 2. Partidos presentes em só uma das duas eleições entram com
# participação zero na outra (daí o `full_join` abaixo), o que capta
# corretamente entradas e saídas de partidos — não só mudanças de votação de
# quem concorreu nas duas eleições.
#
# Fusões: antes da comparação, as siglas da eleição ANTERIOR são relidas com
# o ano da eleição ATUAL (`harmonizar_sigla_partido(sigla, ano_atual)`), o
# que soma os antecessores sob o partido resultante — ex.: DEM + PSL de 2018
# são comparados com UNIAO de 2022. As colunas de votos continuam com os
# nomes da época; o ajuste vale só para este cálculo.
#
#' @param votos_long data.frame com as colunas: id_municipio, cargo, ano,
#'   sigla_partido_harm, share (proporção de votos do partido naquele
#'   cargo/eleição/município, somando 1 dentro de cada id_municipio-cargo-ano).
calcular_volatilidade <- function(votos_long) {
  # Eleição anterior observada de cada município-cargo.
  pares_eleicao <- votos_long %>%
    dplyr::distinct(id_municipio, cargo, ano) %>%
    dplyr::arrange(id_municipio, cargo, ano) %>%
    dplyr::group_by(id_municipio, cargo) %>%
    dplyr::mutate(ano_anterior = dplyr::lag(ano)) %>%
    dplyr::ungroup()

  share_anterior <- votos_long %>%
    dplyr::rename(ano_anterior = ano) %>%
    dplyr::inner_join(pares_eleicao %>% dplyr::filter(!is.na(ano_anterior)),
                      by = c("id_municipio", "cargo", "ano_anterior")) %>%
    dplyr::mutate(sigla_partido_harm = harmonizar_sigla_partido(sigla_partido_harm, ano)) %>%
    dplyr::group_by(id_municipio, cargo, ano, sigla_partido_harm) %>%
    dplyr::summarise(share_anterior = sum(share, na.rm = TRUE), .groups = "drop")

  share_atual <- votos_long %>%
    dplyr::semi_join(pares_eleicao %>% dplyr::filter(!is.na(ano_anterior)),
                     by = c("id_municipio", "cargo", "ano")) %>%
    dplyr::group_by(id_municipio, cargo, ano, sigla_partido_harm) %>%
    dplyr::summarise(share = sum(share, na.rm = TRUE), .groups = "drop")

  volatilidade_por_grupo <- share_atual %>%
    dplyr::full_join(share_anterior, by = c("id_municipio", "cargo", "ano", "sigla_partido_harm")) %>%
    dplyr::group_by(id_municipio, cargo, ano) %>%
    dplyr::summarise(
      volatilidade = sum(abs(dplyr::coalesce(share, 0) - dplyr::coalesce(share_anterior, 0))) / 2,
      .groups = "drop"
    )

  # Na primeira eleição observada de cada município-cargo não há eleição
  # anterior de fato — a volatilidade ali não é informativa (seria sempre
  # ~100%) e por isso é marcada como NA, em vez de calculada contra "zero".
  pares_eleicao %>%
    dplyr::select(id_municipio, cargo, ano) %>%
    dplyr::left_join(volatilidade_por_grupo, by = c("id_municipio", "cargo", "ano"))
}

# ── 4. Índice de equidade racial/de gênero (DRI) ──────────────────────────────
# Descriptive Representation Index (DRI) de Firpo, França, Pereira, Portella &
# Tavares, "Descriptive Representation in Politics: A Measurement Proposal
# and Application for Brazil" (SSRN 4376179, versão de 02/03/2023), eq. (2)
# e Apêndice A, eq. (A.8) — fórmula CONFERIDA no artigo:
#
#   DRI(b, p) = (b - p) / sqrt( (b - p)^2 + b * (1 - b) )
#
# b = proporção do grupo entre os representantes (candidatos, eleitos ou
# votos); p = proporção do grupo na população de referência. Varia de -1
# (grupo ausente, b = 0) a +1 (só o grupo, b = 1); 0 = equilíbrio (b = p).
# É uma correção do Racial Balance Index de Ransom & Sutch (2001), que podia
# sair do intervalo [-1, 1] quando p < 0,21 ou p > 0,79. Conferido com os
# exemplos do artigo: DRI(0,1; 0,9) = -0,94; DRI(0,2; 0,3) = -0,24;
# DRI(0,3; 0,2) = 0,21. |DRI| ou DRI^2 dão a versão em [0, 1].
#
# Quando b = p (inclusive b = p = 0 ou b = p = 1, em que a fórmula vira 0/0)
# o índice é 0 por definição (equilíbrio perfeito).
#
#' @param prop_grupo_fenomeno proporção do grupo entre candidatos/eleitos/votos (b).
#' @param prop_grupo_referencia proporção do grupo na população de referência
#'   (p; eleitorado, para gênero; população do Censo IBGE, para raça).
calcular_indice_equidade <- function(prop_grupo_fenomeno, prop_grupo_referencia) {
  b <- prop_grupo_fenomeno
  p <- prop_grupo_referencia
  dplyr::if_else(
    b == p,
    0,
    (b - p) / sqrt((b - p)^2 + b * (1 - b))
  )
}

# ── 5. População de referência racial (Censo 2010 / Censo 2022) ──────────────
# Proporção de pessoas pretas e pardas na população de cada município, pelo
# censo mais adequado a cada ano eleitoral — em vez de fixar o Censo 2022
# para todos os anos. Usa a mesma janela de vigência de "2 População
# código.R": Censo 2010 -> anos 2006 a 2015; Censo 2022 -> anos 2016 em
# diante. Como o TSE só coleta raça de candidatos a partir de 2014, na
# prática: 2014 -> Censo 2010; 2016 a 2026 -> Censo 2022.
#
# A tabela `br_ibge_censo_2022.populacao_grupo_idade_sexo_raca` do
# basedosdados traz o Censo 2010 e o Censo 2022 (coluna `ano`) — por isso o
# agrupamento é por ano do censo (antes, sem `ano`, os dois censos eram
# somados como se fossem uma única população).
#
# Referência = população total (todas as idades). Firpo et al. usam a
# população de 18+ (PNAD Contínua) para o nível estadual; no nível municipal
# o artigo usa o Censo — os grupos de idade quinquenais do censo não
# permitem um corte exato em 18 anos.
ano_censo_referencia <- function(ano) {
  dplyr::case_when(
    ano >= 2006 & ano <= 2015 ~ 2010,
    ano >= 2016               ~ 2022,
    TRUE                      ~ NA_real_
  )
}

baixar_populacao_negra_censo <- function() {
  # [UNICO] base bruta salva em 0_dados/brutos (e integer64 -> numeric).
  populacao_raca <- coletar_bruto(
    "3_censo_populacao_grupo_idade_sexo_raca",
    function() basedosdados::bdplyr("basedosdados.br_ibge_censo_2022.populacao_grupo_idade_sexo_raca") %>%
      dplyr::select(ano, id_municipio, cor_raca, populacao) %>%
      basedosdados::bd_collect(),
    fonte  = "basedosdados.br_ibge_censo_2022.populacao_grupo_idade_sexo_raca",
    filtro = "colunas: ano, id_municipio, cor_raca, populacao (Censos 2010 e 2022)"
  )

  print(table(populacao_raca$ano))  # CONFERIR: esperado 2010 e 2022

  populacao_raca %>%
    dplyr::filter(!is.na(id_municipio)) %>%
    dplyr::mutate(id_municipio = as.character(id_municipio),
                  ano_censo    = as.numeric(ano)) %>%
    dplyr::group_by(ano_censo, id_municipio) %>%
    dplyr::summarise(
      populacao_total = sum(populacao, na.rm = TRUE),
      populacao_negra = sum(populacao[cor_raca %in% c("Preta", "Parda")], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(prop_populacao_negra = populacao_negra / populacao_total) %>%
    dplyr::select(ano_censo, id_municipio, prop_populacao_negra)
}


# ==== PARTE 8 · Eleitorado e diretórios partidários ("8 Eleições e Partidos código.R") ====
# Copiado do .REV. Mudanças [UNICO]:
#   - retiradas as consultas a `candidatos`, `resultados_candidato_municipio` e
#     `resultados_candidato` (linhas 99–157 do original): os objetos
#     `caracteristicas`/`resultados` não eram usados em lugar nenhum e só
#     custavam tempo de BigQuery;
#   - retirada a linha `partidos <- banco_diretorios_municipal2.xlsx` (erro:
#     objeto inexistente);
#   - retirados os source() do final (as funções e os scripts 4 e 5 estão
#     neste mesmo arquivo — e a pasta 1_script estava vazia, então o source()
#     falhava);
#   - cada base só é recalculada se o .rds não existir ou se REFAZER_* = TRUE.

# ---- 8.1 Características do eleitorado -> eleitorado_municipio.rds ----
arq_eleitorado <- file.path(dir_dados, "eleitorado_municipio.rds")

# O .rds pronto estava na raiz do projeto, não em 0_dados: copia se for o caso.
if (!REFAZER_ELEITORADO && !file.exists(arq_eleitorado) && file.exists(here("eleitorado_municipio.rds"))) {
  file.copy(here("eleitorado_municipio.rds"), arq_eleitorado)
}

if (REFAZER_ELEITORADO || !file.exists(arq_eleitorado)) {

## Características do eleitorado ### row 26 tab_geral

bd_eleitorado <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.perfil_eleitorado_municipio_zona") %>%
  dplyr::filter(
    ano %in% c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024, 2026)
  ) %>%
  dplyr::select(
    ano, sigla_uf, id_municipio,  id_municipio_tse,
    genero, estado_civil, instrucao, grupo_idade, eleitores
  )

caracteristicas_eleitorado <- coletar_bruto(
  "8_perfil_eleitorado_municipio_zona",
  function() basedosdados::bd_collect(bd_eleitorado),
  fonte  = "basedosdados.br_tse_eleicoes.perfil_eleitorado_municipio_zona",
  filtro = "anos 2000-2026 (pares); colunas: ano, sigla_uf, id_municipio, id_municipio_tse, genero, estado_civil, instrucao, grupo_idade, eleitores"
)
rm(bd_eleitorado); gc()

# `id_municipio` como character (consistente com o resto do painel) e sem
# linhas sem id_municipio (não dão para juntar a nada).
caracteristicas_eleitorado <- caracteristicas_eleitorado %>%
  dplyr::filter(!is.na(id_municipio)) %>%
  dplyr::mutate(id_municipio = as.character(id_municipio))

## Categorizar o dado

summary(caracteristicas_eleitorado)

table(caracteristicas_eleitorado$estado_civil)

table(caracteristicas_eleitorado$genero)

table(caracteristicas_eleitorado$instrucao)

table(caracteristicas_eleitorado$grupo_idade)


## Agrupar por município

chaves <- c("ano", "sigla_uf", "id_municipio", "id_municipio_tse")

## 1. Total de eleitores
eleitorado_total <- caracteristicas_eleitorado %>%
  group_by(across(all_of(chaves))) %>%
  summarise(total_eleitores = sum(eleitores, na.rm = TRUE), .groups = "drop")

## 2. Gênero (2 = masculino, 4 = feminino, 0 = não informado)
eleitorado_genero <- caracteristicas_eleitorado %>%
  mutate(genero = recode(genero,
                         "2" = "homens",
                         "4" = "mulheres",
                         "0" = "genero_ni")) %>%
  count(across(all_of(chaves)), genero, wt = eleitores, name = "n") %>%
  pivot_wider(names_from = genero, values_from = n,
              names_prefix = "eleitores_", values_fill = 0)

## 3. Instrução
eleitorado_instrucao <- caracteristicas_eleitorado %>%
  mutate(instrucao = recode(instrucao,
                            "0" = "instr_ni",
                            "1" = "analfabeto",
                            "2" = "le_escreve",
                            "3" = "fund_incompleto",
                            "4" = "fund_completo",
                            "5" = "medio_incompleto",
                            "6" = "medio_completo",
                            "7" = "sup_incompleto",
                            "8" = "sup_completo")) %>%
  count(across(all_of(chaves)), instrucao, wt = eleitores, name = "n") %>%
  pivot_wider(names_from = instrucao, values_from = n,
              names_prefix = "eleitores_", values_fill = 0)

## 4. Juntar e calcular proporções
eleitorado_municipio <- eleitorado_total %>%
  left_join(eleitorado_genero,    by = chaves) %>%
  left_join(eleitorado_instrucao, by = chaves) %>%
  mutate(across(starts_with("eleitores_"),
                ~ .x / total_eleitores,
                .names = "prop_{sub('eleitores_', '', .col)}"))


eleitorado_municipio <- eleitorado_municipio %>%
  filter(sigla_uf != "ZZ")

saveRDS(eleitorado_municipio, arq_eleitorado)
rm(caracteristicas_eleitorado, eleitorado_total, eleitorado_genero, eleitorado_instrucao); gc()

}  # fim do if (REFAZER_ELEITORADO ...)

eleitorado_municipio <- readRDS(arq_eleitorado)

# ---- 8.2 Diretórios partidários (SGIP/TSE) -> diretorios_partidarios.rds ----
# `banco_diretorios_municipal2.xlsx` já vem tratado (1 linha por partido x
# município x ano, com colunas derivadas como total_membros, total_mulheres,
# total_decisao, mulher_decisao, turnover...). O script que o gerou a partir
# dos arquivos brutos do SGIP NÃO está no projeto — registrar isso
# (protocolo: "sinalizar, não silenciar").
arq_diretorios <- file.path(dir_dados, "diretorios_partidarios.rds")

if (!REFAZER_DIRETORIOS && !file.exists(arq_diretorios) && file.exists(here("diretorios_partidarios.rds"))) {
  file.copy(here("diretorios_partidarios.rds"), arq_diretorios)
}

if (REFAZER_DIRETORIOS || !file.exists(arq_diretorios)) {

xlsx_diretorios <- c(file.path(dir_dados, "banco_diretorios_municipal2.xlsx"),
                     here("banco_diretorios_municipal2.xlsx"))
xlsx_diretorios <- xlsx_diretorios[file.exists(xlsx_diretorios)][1]
if (is.na(xlsx_diretorios)) stop("banco_diretorios_municipal2.xlsx não encontrado (nem em 0_dados nem na raiz).")

partidos <- openxlsx::read.xlsx(xlsx_diretorios)   # ~112 MB: demora
names(partidos)
summary(partidos)

table(partidos$NM_TIPO_ORGAO_PARTIDARIO)


# CD_MUNICIPIO vem como texto e com formatação inconsistente entre linhas
# do mesmo município/ano (ex.: "78" e "00078" para Ariquemes/RO em 2018) —
# convertido para numérico ANTES do agrupamento para não duplicar o
# município (duas linhas por ano x id_municipio no resultado final).
partidos <- partidos %>%
  dplyr::mutate(CD_MUNICIPIO = as.numeric(CD_MUNICIPIO))

diretorios <- partidos %>%
  group_by(ANO, CD_MUNICIPIO, SG_UF) %>%
  summarise(
    total_partidos                 = n_distinct(SG_PARTIDO),
    total_orgaos                   = n(),
    total_definitivo               = n_distinct(SG_PARTIDO[NM_TIPO_ORGAO_PARTIDARIO == "ORGAO DEFINITIVO"]),
    total_provisorio               = n_distinct(SG_PARTIDO[NM_TIPO_ORGAO_PARTIDARIO == "ORGAO PROVISORIO"]),
    total_interventora             = n_distinct(SG_PARTIDO[NM_TIPO_ORGAO_PARTIDARIO == "COMISSAO INTERVENTORA"]),
    total_membros                  = sum(total_membros,  na.rm = TRUE),
    total_membros_mulheres         = sum(total_mulheres, na.rm = TRUE),
    total_membros_decisao          = sum(total_decisao,  na.rm = TRUE),
    total_membros_decisao_mulheres = sum(mulher_decisao, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percentual_membros_mulheres         = if_else(total_membros > 0,
                                                  100 * total_membros_mulheres / total_membros, NA_real_),
    percentual_membros_decisao_mulheres = if_else(total_membros_decisao > 0,
                                                  100 * total_membros_decisao_mulheres / total_membros_decisao, NA_real_),
    percentual_membros_decisao          = if_else(total_membros > 0,
                                                  100 * total_membros_decisao / total_membros, NA_real_),
    percentual_definitivo               = 100 * total_definitivo   / total_partidos,
    percentual_provisorio               = 100 * total_provisorio   / total_partidos,
    percentual_interventora             = 100 * total_interventora / total_partidos
  )

summary(diretorios)

diretorios_partidarios <- diretorios %>%
  dplyr::ungroup() %>%
  dplyr::mutate(CD_MUNICIPIO = as.numeric(CD_MUNICIPIO)) %>%
  dplyr::left_join(
    cod_mun_ibge_tse %>% dplyr::select(codigo_tse, codigo_ibge),
    by = c("CD_MUNICIPIO" = "codigo_tse")
  ) %>%
  dplyr::rename(ano = ANO, id_municipio = codigo_ibge) %>%
  dplyr::mutate(id_municipio = as.character(id_municipio)) %>%
  dplyr::filter(!is.na(id_municipio)) %>%
  dplyr::select(-CD_MUNICIPIO, -SG_UF)

# CONFERIR: um mesmo partido pode ter órgão definitivo E provisório no mesmo
# município-ano, então percentual_definitivo + percentual_provisorio pode
# passar de 100.

saveRDS(diretorios_partidarios, arq_diretorios)
rm(partidos, diretorios); gc()

}  # fim do if (REFAZER_DIRETORIOS ...)

diretorios_partidarios <- readRDS(arq_diretorios)


# ==== PARTE 4 · Eleições Gerais ("4. Eleicoes_Gerais.R") ====
# Copiado do original. Mudanças [UNICO]: sem library()/set_billing_id()/source()
# (feitos na PARTE 0), integer64 corrigido após cada bd_collect(), e salvamento
# em .xlsx + .RData no final (protocolo, Etapa 4).

# ══════════════════════════════════════════════════════════════════════════════
# 8 Eleições e Partidos — Eleições Gerais
# Presidente, Governador, Senador, Deputado Federal e Deputado Estadual
# Eleições ordinárias: 2002, 2006, 2010, 2014, 2018, 2022, 2026
# ══════════════════════════════════════════════════════════════════════════════
#
# Constrói um painel id_municipio x ano com, para cada cargo (e turno, quando
# houver 2º turno):
#   - votos por partido (colunas largas, partido harmonizado ao longo do tempo)
#   - comparecimento, abstenção e votos válidos
#   - NEP e volatilidade (1º turno)
#   - total de candidatos e votos (de quem foi votado no município) por
#     gênero e raça + índice de equidade
# Não traz métricas de "eleitos": para estes cargos quem é eleito é
# decidido no país/estado, não no município (ver "5. Eleicoes_Municipais.R"
# para prefeito/vereador, onde isso faz sentido).
# e junta as características do eleitorado e os dirigentes partidários
# (SGIP/TSE) já construídos em "8 Eleições e Partidos código.R".
#
# Saída: "eleicoes_gerais.xlsx", na própria pasta desta dimensão.
#
# CONFERIR antes de rodar em produção: os nomes exatos dos valores de `cargo`
# e `tipo_eleicao` nas tabelas do basedosdados podem variar ao longo do tempo
# (ex.: caixa alta/baixa). Os `table()` abaixo existem justamente para isso —
# rode-os primeiro e ajuste `cargos_gerais`/`tipo_eleicao_valor` se necessário.
# O ano de 2026 pode ainda não estar disponível no basedosdados logo após a
# eleição (defasagem de ingestão) — o script já descarta anos sem dados.

anos_gerais        <- c(2002, 2006, 2010, 2014, 2018, 2022, 2026)
cargos_gerais      <- c("presidente", "governador", "senador",
                         "deputado federal", "deputado estadual")
tipo_eleicao_valor <- "eleicao ordinaria"

cargo_abbr_map <- c(
  "presidente"        = "pres",
  "governador"        = "gov",
  "senador"           = "sen",
  "deputado federal"  = "dep_fed",
  "deputado estadual" = "dep_est"
)

# ── 1. Votos por candidato e por município ────────────────────────────────────
bd_resultados <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.resultados_candidato_municipio") %>%
  dplyr::filter(ano %in% anos_gerais) %>%
  dplyr::select(ano, tipo_eleicao, sigla_uf, id_municipio, id_municipio_tse,
                cargo, turno, numero_candidato, numero_partido, sigla_partido,
                votos)

resultados_gerais <- coletar_bruto(
  "4_resultados_candidato_municipio_gerais",
  function() basedosdados::bd_collect(bd_resultados),
  fonte  = "basedosdados.br_tse_eleicoes.resultados_candidato_municipio",
  filtro = paste("anos", paste(anos_gerais, collapse = ", "),
                 "; colunas: ano, tipo_eleicao, sigla_uf, id_municipio, id_municipio_tse, cargo, turno, numero_candidato, numero_partido, sigla_partido, votos")
)
rm(bd_resultados); gc()

# `id_municipio` como character (consistente com o resto do painel) e sem
# linhas sem id_municipio (não dão para juntar a nada).
resultados_gerais <- resultados_gerais %>%
  dplyr::filter(!is.na(id_municipio)) %>%
  dplyr::mutate(id_municipio = as.character(id_municipio))

# CONFERIR rótulos antes de seguir:
table(resultados_gerais$cargo)
table(resultados_gerais$tipo_eleicao)
table(resultados_gerais$ano)

resultados_gerais <- resultados_gerais %>%
  mutate(cargo = tolower(cargo), tipo_eleicao = tolower(tipo_eleicao)) %>%
  filter(cargo %in% cargos_gerais,
         tipo_eleicao == tipo_eleicao_valor,
         sigla_uf != "ZZ") %>%               # ZZ = exterior, fora do escopo municipal
  mutate(
    cargo_abbr         = cargo_abbr_map[cargo],
    sigla_partido_harm = harmonizar_sigla_partido(sigla_partido, ano)
  )

# ── 2. Votos por partido/cargo/turno e colunas largas ────────────────────────
votos_partido_municipio <- resultados_gerais %>%
  group_by(ano, sigla_uf, id_municipio, cargo_abbr, turno, sigla_partido_harm) %>%
  summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop")

votos_largos_gerais <- votos_partido_municipio %>%
  mutate(nome_coluna = paste0("voto_", sigla_partido_harm, "_", cargo_abbr,
                               "_", turno, "_turno")) %>%
  select(ano, id_municipio, nome_coluna, votos) %>%
  pivot_wider(id_cols = c(ano, id_municipio), names_from = nome_coluna,
              values_from = votos, values_fill = 0)

# ── 3. NEP por cargo (1º turno) ───────────────────────────────────────────────
nep_gerais <- votos_partido_municipio %>%
  filter(turno == 1) %>%
  rename(cargo = cargo_abbr) %>%
  calcular_nep(chaves = c("ano", "id_municipio", "cargo"), var_votos = "votos") %>%
  pivot_wider(id_cols = c(ano, id_municipio), names_from = cargo,
              values_from = c(nep, fracionalizacao), names_glue = "{.value}_{cargo}")

# ── 4. Volatilidade por cargo (1º turno, eleições consecutivas) ──────────────
share_partido_gerais <- votos_partido_municipio %>%
  filter(turno == 1) %>%
  group_by(ano, id_municipio, cargo_abbr) %>%
  mutate(share = votos / sum(votos, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(cargo = cargo_abbr) %>%
  select(id_municipio, cargo, ano, sigla_partido_harm, share)

volatilidade_gerais <- share_partido_gerais %>%
  calcular_volatilidade() %>%
  rename(cargo_abbr = cargo) %>%
  pivot_wider(id_cols = c(ano, id_municipio), names_from = cargo_abbr,
              values_from = volatilidade, names_glue = "volatilidade_{cargo_abbr}")

# ── 5. Comparecimento, abstenção e votos válidos por cargo/turno ────────────
# Schema de `detalhes_votacao_municipio` conferido em basedosdados.org: ano,
# turno, id_eleicao, tipo_eleicao, data_eleicao, sigla_uf, id_municipio,
# id_municipio_tse, cargo, aptos, secoes, secoes_agregadas,
# aptos_totalizadas, secoes_totalizadas, comparecimento, abstencoes,
# votos_validos, votos_brancos, votos_nulos, votos_nominais, votos_legenda,
# proporcao_comparecimento, proporcao_votos_validos, proporcao_votos_brancos,
# proporcao_votos_nulos.
bd_votacao <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.detalhes_votacao_municipio") %>%
  dplyr::filter(ano %in% anos_gerais) %>%
  dplyr::select(ano, tipo_eleicao, id_municipio, cargo, turno, aptos,
                comparecimento, abstencoes, votos_validos,
                proporcao_comparecimento, proporcao_votos_brancos,
                proporcao_votos_nulos)

votacao_municipio_gerais <- coletar_bruto(
  "4_detalhes_votacao_municipio_gerais",
  function() basedosdados::bd_collect(bd_votacao),
  fonte  = "basedosdados.br_tse_eleicoes.detalhes_votacao_municipio",
  filtro = paste("anos", paste(anos_gerais, collapse = ", "),
                 "; colunas: ano, tipo_eleicao, id_municipio, cargo, turno, aptos, comparecimento, abstencoes, votos_validos, proporcao_comparecimento, proporcao_votos_brancos, proporcao_votos_nulos")
)
rm(bd_votacao); gc()

votacao_municipio_gerais <- votacao_municipio_gerais %>%
  dplyr::filter(!is.na(id_municipio)) %>%
  dplyr::mutate(id_municipio = as.character(id_municipio)) %>%
  mutate(cargo = tolower(cargo), tipo_eleicao = tolower(tipo_eleicao)) %>%
  filter(cargo %in% cargos_gerais, tipo_eleicao == tipo_eleicao_valor) %>%
  mutate(cargo_abbr = cargo_abbr_map[cargo])

participacao_gerais <- votacao_municipio_gerais %>%
  group_by(ano, id_municipio, cargo_abbr, turno) %>%
  summarise(
    aptos                     = mean(aptos, na.rm = TRUE),
    comparecimento            = mean(comparecimento, na.rm = TRUE),
    abstencao                 = mean(abstencoes, na.rm = TRUE),
    votos_validos             = mean(votos_validos, na.rm = TRUE),
    proporcao_comparecimento  = mean(proporcao_comparecimento, na.rm = TRUE),
    proporcao_votos_brancos   = mean(proporcao_votos_brancos, na.rm = TRUE),
    proporcao_votos_nulos     = mean(proporcao_votos_nulos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # `proporcao_comparecimento` vem do basedosdados em escala percentual
  # (0-100), não em fração (0-1) — por isso a abstenção é `100 -`, não `1 -`.
  mutate(proporcao_abstencao = 100 - proporcao_comparecimento)

participacao_largo_gerais <- participacao_gerais %>%
  pivot_wider(
    id_cols = c(ano, id_municipio),
    names_from = c(cargo_abbr, turno),
    values_from = c(aptos, comparecimento, proporcao_comparecimento,
                     abstencao, proporcao_abstencao, votos_validos,
                     proporcao_votos_brancos, proporcao_votos_nulos),
    names_glue = "{.value}_{cargo_abbr}_{turno}_turno"
  )

# ── 6. Características dos candidatos (gênero e raça) ────────────────────────
bd_cand <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.candidatos") %>%
  dplyr::filter(ano %in% anos_gerais) %>%
  dplyr::select(ano, tipo_eleicao, sigla_uf, cargo, numero,
                numero_partido, sigla_partido, genero, raca, situacao)

candidatos_gerais <- coletar_bruto(
  "4_candidatos_gerais",
  function() basedosdados::bd_collect(bd_cand),
  fonte  = "basedosdados.br_tse_eleicoes.candidatos",
  filtro = paste("anos", paste(anos_gerais, collapse = ", "),
                 "; colunas: ano, tipo_eleicao, sigla_uf, cargo, numero, numero_partido, sigla_partido, genero, raca, situacao")
)
rm(bd_cand); gc()

# A tabela `candidatos` identifica o candidato pela coluna `numero`, enquanto
# `resultados_candidato_municipio` usa `numero_candidato` — renomeia aqui
# para os dois lados terem a mesma chave de junção.
candidatos_gerais <- candidatos_gerais %>%
  dplyr::rename(numero_candidato = numero)

# `candidatos` registra candidaturas, não candidatos: quando a candidatura
# original é indeferida/cancelada e substituída, tanto a candidatura
# rejeitada quanto a substituta aparecem com o mesmo número de urna — o que
# duplicava (many-to-many) a junção com `resultados_gerais` abaixo. Mantém
# só uma linha por número de urna, priorizando a candidatura deferida.
candidatos_gerais <- candidatos_gerais %>%
  dplyr::mutate(situacao = tolower(situacao)) %>%
  dplyr::group_by(ano, tipo_eleicao, sigla_uf, cargo, numero_candidato,
                   numero_partido, sigla_partido) %>%
  dplyr::arrange(situacao %in% c("indeferido", "indeferido com recurso",
                                  "impugnado", "cancelado", "cancelado com recurso"),
                  .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

table(candidatos_gerais$genero)  # CONFERIR rótulos (ex.: "feminino"/"masculino")
table(candidatos_gerais$raca)    # CONFERIR rótulos (TSE só coleta raça a partir de 2014)

candidatos_gerais <- candidatos_gerais %>%
  mutate(
    cargo         = tolower(cargo),
    tipo_eleicao  = tolower(tipo_eleicao),
    genero        = tolower(genero),
    raca_lower    = tolower(raca),
    grupo_racial  = case_when(
      raca_lower %in% c("preta", "parda")                       ~ "negra",
      raca_lower %in% c("branca", "amarela", "indigena", "indígena") ~ "nao_negra",
      TRUE                                                       ~ NA_character_
    )
  ) %>%
  filter(cargo %in% cargos_gerais, tipo_eleicao == tipo_eleicao_valor) %>%
  mutate(cargo_abbr = cargo_abbr_map[cargo]) %>%
  select(ano, tipo_eleicao, sigla_uf, cargo, cargo_abbr, numero_candidato,
         numero_partido, sigla_partido, genero, grupo_racial)

# Candidatos de Presidente, Governador, Senador, Dep. Federal e Dep. Estadual
# não são específicos de um município (concorrem em todo o estado/país); por
# isso o cruzamento com os votos por município abaixo NÃO usa id_municipio
# como chave — cada candidato é "espalhado" para todos os municípios onde
# recebeu votos.
#
# Candidatura a presidente é nacional: `candidatos.sigla_uf` vem sempre NA
# para `cargo == "presidente"` (não há recorte estadual), enquanto em
# `resultados_gerais` o `sigla_uf` é o estado real do município — juntar
# por `sigla_uf` nunca casava para presidente, deixando genero/grupo_racial
# sempre NA (e, por propagação de NA em `n_distinct()`/filtros lógicos,
# produzindo uma "candidata"/"voto de mulher" fantasma em cada grupo). Por
# isso presidente é juntado separadamente, sem `sigla_uf` como chave.
base_candidatos_presidente <- resultados_gerais %>%
  filter(cargo == "presidente") %>%
  left_join(
    candidatos_gerais %>% filter(cargo == "presidente") %>% select(-sigla_uf),
    by = c("ano", "tipo_eleicao", "cargo", "cargo_abbr",
           "numero_candidato", "numero_partido", "sigla_partido")
  )

base_candidatos_outros_cargos <- resultados_gerais %>%
  filter(cargo != "presidente") %>%
  left_join(
    candidatos_gerais %>% filter(cargo != "presidente"),
    by = c("ano", "tipo_eleicao", "sigla_uf", "cargo", "cargo_abbr",
           "numero_candidato", "numero_partido", "sigla_partido")
  )

base_candidatos_gerais <- dplyr::bind_rows(
  base_candidatos_presidente,
  base_candidatos_outros_cargos
)

# ── 7. Votos e candidatos por gênero e raça (1º turno) ───────────────────────
# Não calcula métricas de "eleitos": para todos os cargos desta dimensão
# (presidente, governador, senador, dep. federal, dep. estadual) quem é
# eleito é decidido no país/estado, não no município — não há um "eleito"
# por município a reportar aqui (isso só se aplica a prefeito/vereador, em
# "5. Eleicoes_Municipais.R").
#
# `resultados_candidato_municipio` traz, para presidente/governador/senador/
# dep. federal/dep. estadual, TODOS os candidatos registrados no
# país/estado em cada município — inclusive os que não receberam nenhum
# voto ali (comum para dep. federal/estadual, que têm centenas de
# candidatos por estado). O filtro `votos > 0` abaixo restringe
# `total_candidatos`/`candidatos_mulheres`/`candidatos_negros` a quem de
# fato foi votado naquele município.
# `numero_candidato[genero == "feminino"]` devolveria NA (não omitido) para
# linhas em que `genero` é NA, e `n_distinct()`/`sum(..., na.rm=TRUE)` conta
# esse NA como um valor a mais — produzindo uma "candidata"/"voto de
# mulher" fantasma sempre que algum candidato do grupo tem gênero/raça
# ausente. `%in%` evita isso (NA nunca está "em" um conjunto, vira FALSE).
desigualdade_gerais <- base_candidatos_gerais %>%
  filter(turno == 1, votos > 0) %>%
  group_by(ano, id_municipio, cargo_abbr) %>%
  summarise(
    total_candidatos        = n_distinct(numero_candidato),
    candidatos_mulheres     = n_distinct(numero_candidato[genero %in% "feminino"]),
    candidatos_negros       = n_distinct(numero_candidato[grupo_racial %in% "negra"]),
    votos_totais            = sum(votos, na.rm = TRUE),
    votos_mulheres          = sum(votos[genero %in% "feminino"], na.rm = TRUE),
    votos_negros            = sum(votos[grupo_racial %in% "negra"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_candidatos_mulheres = candidatos_mulheres / total_candidatos,
    prop_candidatos_negros   = candidatos_negros   / total_candidatos,
    prop_votos_mulheres      = votos_mulheres      / votos_totais,
    prop_votos_negros        = votos_negros        / votos_totais,
    # TSE só coleta raça a partir de 2014 — antes disso `grupo_racial` é
    # sempre NA, o que (mesmo com o `%in%` acima) daria 0 candidatos/votos
    # negros em vez de "sem dado". Zera para NA explicitamente.
    candidatos_negros        = dplyr::if_else(ano < 2014, NA_integer_, candidatos_negros),
    prop_candidatos_negros   = dplyr::if_else(ano < 2014, NA_real_,    prop_candidatos_negros),
    votos_negros              = dplyr::if_else(ano < 2014, NA_real_,    votos_negros),
    prop_votos_negros        = dplyr::if_else(ano < 2014, NA_real_,    prop_votos_negros)
  )

# ── 8. População de referência para o índice de equidade racial ─────────────
# Censo 2010 para 2014 e Censo 2022 de 2018 em diante — ver
# `baixar_populacao_negra_censo()` em "3. Funcoes_Partidos_Indices.R".
# [UNICO] baixa uma vez só e reaproveita na PARTE 5.
if (!exists("populacao_negra_municipio")) populacao_negra_municipio <- baixar_populacao_negra_censo()

# ── 9. Características do eleitorado e dirigentes partidários (SGIP/TSE) ────
# Gerados em "8 Eleições e Partidos código.R" (seção final) e salvos como .rds
# para reaproveitamento sem repetir as consultas ao BigQuery.
eleitorado_municipio <- readRDS(here("1 Bases Individuais", "8 Eleições e Partidos",
                                      "0_dados", "eleitorado_municipio.rds"))
diretorios_partidarios <- readRDS(here("1 Bases Individuais", "8 Eleições e Partidos",
                                        "0_dados", "diretorios_partidarios.rds"))

# ── 10. Índice de equidade (gênero: referência = eleitorado; raça: Censo) ───
desigualdade_gerais <- desigualdade_gerais %>%
  left_join(
    eleitorado_municipio %>% select(ano, id_municipio, prop_mulheres),
    by = c("ano", "id_municipio")
  ) %>%
  mutate(ano_censo = ano_censo_referencia(ano)) %>%
  left_join(populacao_negra_municipio, by = c("ano_censo", "id_municipio")) %>%
  mutate(
    # Índice = DRI de Firpo et al. (ver `calcular_indice_equidade()`).
    # Índice baseado na composição dos candidatos votados no município (ver
    # filtro `votos > 0` na seção 7) — mede diversidade de quem concorreu.
    indice_equidade_genero_candidatos = calcular_indice_equidade(prop_candidatos_mulheres, prop_mulheres),
    indice_equidade_racial_candidatos = calcular_indice_equidade(prop_candidatos_negros, prop_populacao_negra),
    # Índice baseado nos votos recebidos no município — mede diversidade de
    # quem foi efetivamente escolhido pelo eleitorado local, e não só de
    # quem estava na cédula.
    indice_equidade_genero_votos      = calcular_indice_equidade(prop_votos_mulheres, prop_mulheres),
    indice_equidade_racial_votos      = calcular_indice_equidade(prop_votos_negros, prop_populacao_negra)
  ) %>%
  select(-prop_mulheres, -prop_populacao_negra, -ano_censo)

colunas_valor_desigualdade <- setdiff(names(desigualdade_gerais), c("ano", "id_municipio", "cargo_abbr"))

desigualdade_largo_gerais <- desigualdade_gerais %>%
  pivot_wider(
    id_cols = c(ano, id_municipio),
    names_from = cargo_abbr,
    values_from = dplyr::all_of(colunas_valor_desigualdade),
    names_glue = "{.value}_{cargo_abbr}"
  )

# ── 11. Junção final e exportação ─────────────────────────────────────────────
eleicoes_gerais <- votos_largos_gerais %>%
  full_join(nep_gerais,                 by = c("ano", "id_municipio")) %>%
  full_join(volatilidade_gerais,        by = c("ano", "id_municipio")) %>%
  full_join(participacao_largo_gerais,  by = c("ano", "id_municipio")) %>%
  full_join(desigualdade_largo_gerais,  by = c("ano", "id_municipio")) %>%
  left_join(eleitorado_municipio,       by = c("ano", "id_municipio")) %>%
  left_join(diretorios_partidarios,     by = c("ano", "id_municipio"))

summary(eleicoes_gerais)

# [UNICO] `ano` como integer e `id_municipio` como character (protocolo, Etapa 4).
eleicoes_gerais <- eleicoes_gerais %>%
  mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio))

write.xlsx(eleicoes_gerais, file = file.path(dir_dim, "eleicoes_gerais.xlsx"))
save(eleicoes_gerais, file = file.path(dir_dim, "eleicoes_gerais.RData"))

# Libera memória antes da PARTE 5 (os nomes abaixo são reaproveitados lá).
rm(resultados_gerais, votos_partido_municipio, votacao_municipio_gerais,
   candidatos_gerais, base_candidatos_presidente, base_candidatos_outros_cargos,
   base_candidatos_gerais, desigualdade_gerais); gc()


# ==== PARTE 5 · Eleições Municipais ("5. Eleicoes_Municipais.R") ====
# Copiado do original (não do .REV, que tem linhas de teste). Mudanças [UNICO]:
# sem library()/set_billing_id()/source(); integer64 corrigido; electionsBR
# rodando em pasta temporária; filtro de cargo/tipo de eleição tolerante a
# maiúsculas; salvamento em .xlsx + .RData no final.

# ══════════════════════════════════════════════════════════════════════════════
# 8 Eleições e Partidos — Eleições Municipais (Prefeito e Vereador)
# Eleições ordinárias: 2000, 2004, 2008, 2012, 2016, 2020, 2024
# ══════════════════════════════════════════════════════════════════════════════
#
# Constrói um painel id_municipio x ano com:
#   - votos por partido/cargo/turno (colunas largas, partido harmonizado)
#   - comparecimento, abstenção e votos válidos por cargo/turno
#   - margem do 1º para o 2º colocado na Prefeitura (1º e 2º turno)
#   - partido e coligação do prefeito eleito
#   - cadeiras do partido do prefeito e da coligação do prefeito na Câmara,
#     partido com mais cadeiras e se é o mesmo do prefeito
#   - NEP e volatilidade (Prefeitura e Câmara, 1º turno)
#   - total de candidatos/eleitos e votos por gênero e raça + índice de
#     equidade (Prefeitura e Câmara)
#   - incumbência: se o titular (quem venceu a eleição de 4 anos antes, no
#     mesmo município) concorreu e se venceu de novo; se ele tinha
#     possibilidade de reeleição (não estava no 2º mandato consecutivo) e,
#     dado isso, se concorreu — tudo pela Prefeitura; para a Câmara, total de
#     vereadores titulares que concorreram de novo e total que se reelegeram
# e junta as características do eleitorado e os dirigentes partidários
# (SGIP/TSE) já construídos em "8 Eleições e Partidos código.R".
#
# Saída: "eleicoes_municipais.xlsx", na própria pasta desta dimensão.
#
# Só eleições ORDINÁRIAS entram no painel (suplementares, extraordinárias e
# renovações ficam de fora). Única exceção: Macapá/AP em 2020, cuja eleição
# ordinária foi adiada para 06/12 e 20/12/2020 (apagão no Amapá, em meio à
# pandemia) — o basedosdados a rotula como "eleicoes municipais 2020 - ap"
# em vez de "eleicao ordinaria" (CONFERIDO: é o único rótulo assim em
# 2000–2024; os demais municípios do AP vêm como "eleicao ordinaria"). No
# arquivo do TSE lido pelo electionsBR ela já vem como "Eleição Ordinária".
#
# CONFERIR antes de rodar em produção: nomes exatos dos valores de `cargo`/
# `tipo_eleicao`/`resultado` podem variar entre vintages do basedosdados —
# os `table()` abaixo existem para isso.

anos_municipais    <- c(2000, 2004, 2008, 2012, 2016, 2020, 2024)
cargos_municipais  <- c("prefeito", "vereador")
tipo_eleicao_valor <- c("eleicao ordinaria",
                        "eleicoes municipais 2020 - ap")  # Macapá 2020 (ver nota acima)

# ── 1. Votos por candidato e por município ────────────────────────────────────
# `titulo_eleitoral_candidato` é a chave de PESSOA que o próprio basedosdados
# usa para identificar o candidato entre eleições diferentes (ver "Nível de
# observação" da tabela em basedosdados.org) — por isso é ela, e não
# `numero_candidato` (que muda de eleição para eleição), que identifica o
# MESMO candidato ao longo do tempo na seção 2 (incumbência), abaixo.
bd_resultados <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.resultados_candidato") %>%
  dplyr::filter(ano %in% anos_municipais) %>%
  dplyr::select(ano, tipo_eleicao, sigla_uf, id_municipio, id_municipio_tse,
                cargo, turno, numero_candidato, numero_partido, sigla_partido,
                titulo_eleitoral_candidato, resultado, votos)

resultados_municipais <- coletar_bruto(
  "5_resultados_candidato_municipais",
  function() basedosdados::bd_collect(bd_resultados),
  fonte  = "basedosdados.br_tse_eleicoes.resultados_candidato",
  filtro = paste("anos", paste(anos_municipais, collapse = ", "),
                 "; colunas: ano, tipo_eleicao, sigla_uf, id_municipio, id_municipio_tse, cargo, turno, numero_candidato, numero_partido, sigla_partido, titulo_eleitoral_candidato, resultado, votos")
)
rm(bd_resultados); gc()

# `id_municipio` como character (consistente com o resto do painel) e sem
# linhas sem id_municipio (não dão para juntar a nada).
resultados_municipais <- resultados_municipais %>%
  dplyr::filter(!is.na(id_municipio)) %>%
  dplyr::mutate(id_municipio = as.character(id_municipio))

# CONFERIR rótulos antes de seguir:
table(resultados_municipais$cargo)
table(resultados_municipais$tipo_eleicao)
table(resultados_municipais$resultado)

resultados_municipais <- resultados_municipais %>%
  mutate(cargo = tolower(cargo), tipo_eleicao = tolower(tipo_eleicao)) %>%
  filter(cargo %in% cargos_municipais, tipo_eleicao %in% tipo_eleicao_valor) %>%
  mutate(
    sigla_partido_harm = harmonizar_sigla_partido(sigla_partido, ano),
    # Comparação por igualdade com valores fixos ("eleito por qp ", com
    # espaço no final) subcontava eleitos a partir de 2012, quando o TSE
    # passou a gravar "eleito por qp" sem o espaço (CONFERIDO via
    # table(resultado) por ano: também existem "eleito por media",
    # "registro negado antes/apos a eleicao", "renuncia/falecimento/
    # cassacao", "indeferido com recurso" e `resultado` às vezes NA em
    # 2008). `str_detect(..., "^eleito")` cobre todas as variantes de
    # "eleito..." independentemente de sufixo/espaçamento.
    status_eleicao = case_when(
      stringr::str_detect(stringr::str_trim(tolower(resultado)), "^eleito") ~ 1,
      !is.na(resultado)                                                     ~ 0,
      TRUE                                                                  ~ NA_real_
    )
  )

# `status_eleicao` por LINHA (turno) não identifica corretamente o vencedor
# do Executivo quando há 2º turno: o candidato eleito aparece com `resultado`
# "2º turno" (status_eleicao = 0) na linha do 1º turno, e só "eleito" na
# linha do 2º turno. Isso fazia `desigualdade_municipais` (seção 9), que só
# olha o 1º turno para não contar o mesmo candidato duas vezes, mostrar
# total_eleitos = 0 para Prefeito em todo município com 2º turno — por isso
# `status_eleicao` é reescrito aqui para o resultado FINAL do candidato (o
# maior valor entre os turnos que disputou), antes de qualquer outro uso.
# Vereador nunca tem 2º turno, então não muda nada para ele.
status_eleicao_final <- resultados_municipais %>%
  group_by(ano, id_municipio, cargo, numero_candidato, numero_partido, sigla_partido) %>%
  summarise(
    status_eleicao = dplyr::if_else(all(is.na(status_eleicao)), NA_real_,
                                     max(status_eleicao, na.rm = TRUE)),
    .groups = "drop"
  )

resultados_municipais <- resultados_municipais %>%
  select(-status_eleicao) %>%
  left_join(status_eleicao_final,
            by = c("ano", "id_municipio", "cargo", "numero_candidato", "numero_partido", "sigla_partido"))

# ── 2. Incumbência — Prefeito e Vereadores ────────────────────────────────────
# "Titular" = quem venceu aquele cargo na eleição de 4 anos antes, no mesmo
# município (identificado por `titulo_eleitoral_candidato`, não por
# `numero_candidato`, que pode mudar de uma eleição para outra). Depende de
# `titulo_eleitoral_candidato` estar preenchido e ser estável para a mesma
# pessoa entre eleições — CONFERIR se há muitos NA nessa coluna (nesse caso
# o candidato simplesmente não é reconhecido como titular/incumbente, sem
# erro, mas subcontando incumbência).
vencedores_titulo <- resultados_municipais %>%
  filter(turno == 1, status_eleicao %in% 1, !is.na(titulo_eleitoral_candidato)) %>%
  distinct(ano, id_municipio, cargo, titulo_eleitoral_candidato)

# Quem concorreu em cada eleição (1 linha por candidato — usa o 1º turno,
# presente para todo candidato, mesmo quem não avançou a um 2º turno).
# [REV 2026-11] R1: antes era distinct(..., status_eleicao). Quando a mesma pessoa
# aparece com dois registros no mesmo município (ex.: dois números/partidos),
# um com status 0 e outro com 1, ela virava duas linhas, e a incumbência
# duplicava o município-ano (5 casos em 2004, que só diferiam em
# prefeito_incumbente_venceu). Agora: 1 linha por pessoa, e ela conta como
# eleita se venceu em qualquer dos registros (max).
candidatos_titulo <- resultados_municipais %>%
  filter(turno == 1, !is.na(titulo_eleitoral_candidato)) %>%
  group_by(ano, id_municipio, cargo, titulo_eleitoral_candidato) %>%
  summarise(status_eleicao = if (all(is.na(status_eleicao))) NA_real_ else max(status_eleicao, na.rm = TRUE),
            .groups = "drop")

# --- 2.1 Prefeito -------------------------------------------------------------
titular_prefeito <- vencedores_titulo %>%
  filter(cargo == "prefeito") %>%
  transmute(ano_titular = ano, id_municipio, titulo_titular = titulo_eleitoral_candidato)

# Elegibilidade do titular para a reeleição: pela Constituição, o chefe do
# Executivo só pode cumprir 2 mandatos CONSECUTIVOS. Na eleição E, o titular
# é quem venceu em E-4; ele NÃO é elegível se também venceu em E-8, ou seja,
# no mandato imediatamente anterior ao seu (titular - 4 anos).
#   Ex.: eleição 2016 -> titular venceu em 2012 -> compara com o vencedor de 2008.
# [REV 2026-11] R2: antes o código somava +8 ao ano do TITULAR (não ao da eleição),
# comparando com E-12 (no exemplo, 2004): quem venceu em 2008 e 2012 saía
# como elegível em 2016, e quem voltou depois de um intervalo saía como
# inelegível. Com +4, o vencedor de 2008 recebe o rótulo 2012 e é comparado
# com o titular de 2012.
# Cobertura com dados desde 2000: o titular de 2000 precisaria de 1996 (fora
# da base) -> NA, o que afeta só a eleição de 2004. De 2008 em diante
# (titular 2004 x vencedor 2000) tudo é calculável.
vencedor_mandato_anterior <- titular_prefeito %>%
  transmute(ano_titular = ano_titular + 4, id_municipio, titulo_mandato_anterior = titulo_titular)

elegibilidade_titular_prefeito <- titular_prefeito %>%
  left_join(vencedor_mandato_anterior, by = c("ano_titular", "id_municipio")) %>%
  mutate(
    elegivel_reeleicao = dplyr::case_when(
      ano_titular < 2004                          ~ NA_real_,  # titular de 2000: sem dado de 1996
      titulo_titular == titulo_mandato_anterior   ~ 0,         # já estava no 2º mandato seguido
      TRUE                                         ~ 1
    )
  ) %>%
  select(ano_titular, id_municipio, titulo_titular, elegivel_reeleicao)

candidatos_prefeito_titulo <- candidatos_titulo %>%
  filter(cargo == "prefeito") %>%
  select(ano, id_municipio, titulo_eleitoral_candidato, status_eleicao)

incumbencia_prefeito <- elegibilidade_titular_prefeito %>%
  transmute(ano = ano_titular + 4, id_municipio, titulo_titular, elegivel_reeleicao) %>%
  left_join(
    candidatos_prefeito_titulo %>%
      rename(titulo_titular = titulo_eleitoral_candidato, status_titular = status_eleicao),
    by = c("ano", "id_municipio", "titulo_titular")
  ) %>%
  mutate(
    # 1 = o titular concorreu de novo; 0 = não concorreu. Sempre determinado
    # quando existe titular (não depende de elegibilidade).
    prefeito_incumbente_candidato = as.numeric(!is.na(status_titular)),
    # 1 = titular concorreu e venceu; 0 = titular não concorreu OU concorreu
    # e perdeu (as duas coisas contam como "não venceu").
    prefeito_incumbente_venceu = dplyr::if_else(
      is.na(status_titular), 0, as.numeric(status_titular %in% 1)
    ),
    # Só definido (1/0) quando o titular tinha possibilidade de reeleição;
    # NA quando não era elegível ou a elegibilidade é desconhecida.
    prefeito_incumbente_elegivel_disputou = dplyr::case_when(
      is.na(elegivel_reeleicao) | elegivel_reeleicao == 0 ~ NA_real_,
      TRUE ~ as.numeric(!is.na(status_titular))
    )
  ) %>%
  select(ano, id_municipio, prefeito_incumbente_candidato, prefeito_incumbente_venceu,
         prefeito_incumbente_elegivel_disputou)

# --- 2.2 Vereadores (totais por município — não há limite de reeleição) ─────
titular_vereadores <- vencedores_titulo %>%
  filter(cargo == "vereador") %>%
  transmute(ano = ano + 4, id_municipio, titulo_titular = titulo_eleitoral_candidato)

candidatos_vereador_titulo <- candidatos_titulo %>%
  filter(cargo == "vereador") %>%
  select(ano, id_municipio, titulo_eleitoral_candidato, status_eleicao)

incumbencia_vereadores <- titular_vereadores %>%
  left_join(
    candidatos_vereador_titulo,
    by = c("ano", "id_municipio", "titulo_titular" = "titulo_eleitoral_candidato")
  ) %>%
  group_by(ano, id_municipio) %>%
  summarise(
    vereadores_incumbentes_candidatos = sum(!is.na(status_eleicao)),
    vereadores_incumbentes_eleitos    = sum(status_eleicao %in% 1, na.rm = TRUE),
    .groups = "drop"
  )

# ── 3. Votos por partido/cargo/turno e colunas largas ────────────────────────
votos_partido_municipio <- resultados_municipais %>%
  group_by(ano, sigla_uf, id_municipio, cargo, turno, sigla_partido_harm) %>%
  summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop")

votos_largos_municipais <- votos_partido_municipio %>%
  mutate(nome_coluna = paste0("voto_", sigla_partido_harm, "_", cargo,
                               "_", turno, "_turno")) %>%
  select(ano, id_municipio, nome_coluna, votos) %>%
  pivot_wider(id_cols = c(ano, id_municipio), names_from = nome_coluna,
              values_from = votos, values_fill = 0)

# ── 4. NEP e volatilidade por cargo (1º turno) ────────────────────────────────
nep_municipais <- votos_partido_municipio %>%
  filter(turno == 1) %>%
  calcular_nep(chaves = c("ano", "id_municipio", "cargo"), var_votos = "votos") %>%
  pivot_wider(id_cols = c(ano, id_municipio), names_from = cargo,
              values_from = c(nep, fracionalizacao), names_glue = "{.value}_{cargo}")

share_partido_municipais <- votos_partido_municipio %>%
  filter(turno == 1) %>%
  group_by(ano, id_municipio, cargo) %>%
  mutate(share = votos / sum(votos, na.rm = TRUE)) %>%
  ungroup() %>%
  select(id_municipio, cargo, ano, sigla_partido_harm, share)

volatilidade_municipais <- share_partido_municipais %>%
  calcular_volatilidade() %>%
  pivot_wider(id_cols = c(ano, id_municipio), names_from = cargo,
              values_from = volatilidade, names_glue = "volatilidade_{cargo}")

# ── 5. Comparecimento, abstenção e votos válidos por cargo/turno ────────────
# Schema de `detalhes_votacao_municipio` conferido em basedosdados.org: ano,
# turno, id_eleicao, tipo_eleicao, data_eleicao, sigla_uf, id_municipio,
# id_municipio_tse, cargo, aptos, secoes, secoes_agregadas,
# aptos_totalizadas, secoes_totalizadas, comparecimento, abstencoes,
# votos_validos, votos_brancos, votos_nulos, votos_nominais, votos_legenda,
# proporcao_comparecimento, proporcao_votos_validos, proporcao_votos_brancos,
# proporcao_votos_nulos.
bd_votacao <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.detalhes_votacao_municipio") %>%
  dplyr::filter(ano %in% anos_municipais) %>%
  dplyr::select(ano, tipo_eleicao, id_municipio, cargo, turno, aptos,
                comparecimento, abstencoes, votos_validos,
                proporcao_comparecimento, proporcao_votos_brancos,
                proporcao_votos_nulos)

votacao_municipio_municipais <- coletar_bruto(
  "5_detalhes_votacao_municipio_municipais",
  function() basedosdados::bd_collect(bd_votacao),
  fonte  = "basedosdados.br_tse_eleicoes.detalhes_votacao_municipio",
  filtro = paste("anos", paste(anos_municipais, collapse = ", "),
                 "; colunas: ano, tipo_eleicao, id_municipio, cargo, turno, aptos, comparecimento, abstencoes, votos_validos, proporcao_comparecimento, proporcao_votos_brancos, proporcao_votos_nulos")
)
rm(bd_votacao); gc()

votacao_municipio_municipais <- votacao_municipio_municipais %>%
  dplyr::filter(!is.na(id_municipio)) %>%
  dplyr::mutate(id_municipio = as.character(id_municipio)) %>%
  mutate(cargo = tolower(cargo), tipo_eleicao = tolower(tipo_eleicao)) %>%
  filter(cargo %in% cargos_municipais, tipo_eleicao %in% tipo_eleicao_valor)


# Fonte única de votos válidos por cargo/turno/município (também reaproveitada
# na seção 6, para a margem do prefeito) — vem direto de
# `detalhes_votacao_municipio`, mais confiável do que somar os votos
# nominais por candidato (que pode diferir ligeiramente por conta de votos
# de legenda/brancos/nulos conforme o cargo e o ano).
votos_validos_municipais <- votacao_municipio_municipais %>%
  group_by(ano, id_municipio, cargo, turno) %>%
  summarise(votos_validos = mean(votos_validos, na.rm = TRUE), .groups = "drop")

participacao_municipais <- votacao_municipio_municipais %>%
  group_by(ano, id_municipio, cargo, turno) %>%
  summarise(
    aptos                    = mean(aptos, na.rm = TRUE),
    comparecimento           = mean(comparecimento, na.rm = TRUE),
    abstencao                = mean(abstencoes, na.rm = TRUE),
    votos_validos            = mean(votos_validos, na.rm = TRUE),
    proporcao_comparecimento = mean(proporcao_comparecimento, na.rm = TRUE),
    proporcao_votos_brancos  = mean(proporcao_votos_brancos, na.rm = TRUE),
    proporcao_votos_nulos    = mean(proporcao_votos_nulos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # `proporcao_comparecimento` vem do basedosdados em escala percentual
  # (0-100), não em fração (0-1) — por isso a abstenção é `100 -`, não `1 -`.
  mutate(proporcao_abstencao = 100 - proporcao_comparecimento)

participacao_largo_municipais <- participacao_municipais %>%
  pivot_wider(
    id_cols = c(ano, id_municipio),
    names_from = c(cargo, turno),
    values_from = c(aptos, comparecimento, proporcao_comparecimento,
                     abstencao, proporcao_abstencao, votos_validos,
                     proporcao_votos_brancos, proporcao_votos_nulos),
    names_glue = "{.value}_{cargo}_{turno}_turno"
  )

# ── 6. Margem do 1º para o 2º colocado na Prefeitura (1º e 2º turno) ────────
# [REV 2026-11] R3: em empate de votos, a ordem do 1º/2º colocado era arbitrária
# (24 casos, sobretudo em 2004). O critério legal de desempate é o candidato
# mais velho; como quem venceu o empate é o eleito, ordenar pelo eleito dá
# o mesmo resultado sem precisar da data de nascimento.
top2_prefeito <- resultados_municipais %>%
  filter(cargo == "prefeito") %>%
  arrange(ano, id_municipio, turno, desc(votos), desc(status_eleicao %in% 1)) %>%
  group_by(ano, id_municipio, turno) %>%
  mutate(posicao = row_number()) %>%
  filter(posicao <= 2) %>%
  ungroup()

margem_prefeito <- top2_prefeito %>%
  select(ano, id_municipio, turno, posicao, sigla_partido_harm, votos) %>%
  pivot_wider(
    id_cols = c(ano, id_municipio, turno),
    names_from = posicao,
    values_from = c(sigla_partido_harm, votos),
    names_glue = "{.value}_posicao{posicao}"
  ) %>%
  rename(
    partido_1_colocado = sigla_partido_harm_posicao1,
    partido_2_colocado = sigla_partido_harm_posicao2,
    votos_1_colocado   = votos_posicao1,
    votos_2_colocado   = votos_posicao2
  ) %>%
  left_join(
    votos_validos_municipais %>% filter(cargo == "prefeito") %>%
      select(ano, id_municipio, turno, votos_validos_prefeito = votos_validos),
    by = c("ano", "id_municipio", "turno")
  ) %>%
  mutate(
    margem_votos = votos_1_colocado - votos_2_colocado,
    margem_pct   = margem_votos / votos_validos_prefeito,
    # CONFERIR: em casos raríssimos (observado em 2 de ~39 mil linhas,
    # ambos em 2024) `votos_validos_prefeito` (de
    # `detalhes_votacao_municipio`) sai menor que os próprios votos do 1º
    # colocado (de `resultados_candidato`) — aparente inconsistência entre
    # essas duas tabelas do TSE para esses município-anos específicos, não
    # um erro de cálculo aqui. Um `margem_pct` acima de 1 (100%) é
    # logicamente impossível, então vira NA em vez de um valor inválido.
    margem_pct   = dplyr::if_else(margem_pct > 1, NA_real_, margem_pct)
  )

margem_prefeito_largo <- margem_prefeito %>%
  # `votos_validos_prefeito` não entra aqui: a mesma informação já está em
  # `participacao_largo_municipais` (como "votos_validos_prefeito_{turno}_turno"),
  # e mantê-la aqui também duplicava a coluna (sufixo .x/.y) na junção final.
  pivot_wider(
    id_cols = c(ano, id_municipio),
    names_from = turno,
    values_from = c(partido_1_colocado, partido_2_colocado, votos_1_colocado,
                     votos_2_colocado, margem_votos, margem_pct),
    names_glue = "{.value}_{turno}_turno"
  )

# Prefeito eleito: no último turno disputado em cada município (2º turno,
# quando houve; senão, o 1º turno), o candidato com resultado "eleito" —
# desempatando/na falta dele pelo mais votado. O número do candidato é
# guardado para buscar a coligação DESTE MESMO candidato na seção 7 (antes
# o vencedor era identificado de forma independente nas duas fontes, e em
# 2000 o filtro "DEFERIDO" do electionsBR descartava prefeitos eleitos
# "SUB JUDICE", trazendo a coligação do 2º colocado).
prefeito_eleito_partido <- resultados_municipais %>%
  filter(cargo == "prefeito") %>%
  group_by(ano, id_municipio) %>%
  filter(turno == max(turno)) %>%
  arrange(desc(status_eleicao %in% 1), desc(votos), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(ano, id_municipio, numero_prefeito_eleito = numero_candidato,
         partido_prefeito_eleito = sigla_partido_harm)

# ── 7. Coligação do prefeito eleito (via electionsBR — ver nota abaixo) ─────
# A composição da coligação não foi localizada nas tabelas do basedosdados
# usadas acima; por isso é obtida diretamente do TSE via electionsBR, nos
# mesmos moldes da função `baixar_eleicoes_governadores` já existente em
# "0. Funcoes.Rmd". CONFERIR se o basedosdados passou a expor uma coluna de
# coligação em `resultados_candidato`/`candidatos` — se sim, este bloco pode
# ser substituído por uma consulta ao BigQuery, mais rápida.
# Devolve a coligação de CADA candidato a prefeito (ano x município x número
# do candidato); a do eleito é escolhida depois, pelo número identificado
# acima em `prefeito_eleito_partido`. Não filtra por situação da
# candidatura (o filtro "DEFERIDO"/"APTO" descartava eleitos sub judice e,
# em 2024, o campo vem "#NE" para todos).

extrair_coligacao_prefeito_eleito <- function(ano) {
  # [UNICO] baixa/descompacta numa pasta temporária (fora do OneDrive) e
  # sempre volta para a pasta do projeto, mesmo se der erro.
  # [UNICO] base bruta salva por ano, só com as linhas de Prefeito (o arquivo
  # completo também traz vereador e é muito maior); todas as colunas mantidas.
  x <- coletar_bruto(
    paste0("5_tse_votacao_candidato_munzona_prefeito_", ano),
    function() elections_tse_tmp(ano, "vote_mun_zone", readme_pdf = FALSE) %>%
      dplyr::filter(toupper(DS_CARGO) == "PREFEITO"),
    fonte  = paste0("TSE via electionsBR: votacao_candidato_munzona_", ano,
                    " (cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona)"),
    filtro = "só DS_CARGO = Prefeito; todas as colunas"
  )

  # Só a eleição ordinária (Macapá 2020, adiada, já vem como "Eleição
  # Ordinária" no arquivo do TSE).
  x %>%
    # [UNICO] comparação sem depender de maiúsculas/acentos ("Prefeito"/"PREFEITO",
    # "Eleição Ordinária"/"ELEICAO ORDINARIA") — ver teste de 2008 no .REV.
    # [REV 2026-11] R4: "EXTRAORDINÁRIA" contém "ORDIN", então o filtro antigo deixava
    # passar eleições extraordinárias/suplementares (que o TSE grava no
    # arquivo do ano do mandato, ex.: extraordinária de 2018 no arquivo de
    # 2016). Com isso, 167 candidatos de 2016 tinham duas coligações e o
    # slice(1) abaixo escolhia uma ao acaso. Depois da correção: 0 casos.
    # O filtro fica AQUI, e não na função de download, para que a base bruta
    # continue salva sem tratamento (e porque, com REUSAR_BRUTOS = TRUE, a
    # função de download nem roda).
    dplyr::filter(toupper(DS_CARGO) == "PREFEITO",
                  stringr::str_detect(toupper(NM_TIPO_ELEICAO), "ORDIN"),
                  !stringr::str_detect(toupper(NM_TIPO_ELEICAO), "EXTRA|SUPLEM")) %>%
    dplyr::distinct(ANO_ELEICAO, CD_MUNICIPIO, NR_CANDIDATO, DS_COMPOSICAO_COLIGACAO) %>%
    dplyr::group_by(ANO_ELEICAO, CD_MUNICIPIO, NR_CANDIDATO) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      ano = ANO_ELEICAO,
      codigo_tse = as.numeric(CD_MUNICIPIO),
      numero_prefeito_eleito = as.numeric(NR_CANDIDATO),
      # Códigos de "não informado" do TSE ("#NULO#", "#NE#") viram NA, para
      # não serem lidos como uma coligação conhecida sem cadeiras.
      coligacao_prefeito_eleito = dplyr::if_else(
        stringr::str_detect(DS_COMPOSICAO_COLIGACAO, "^#"), NA_character_, DS_COMPOSICAO_COLIGACAO
      )
    )
}

coligacao_prefeito_eleito_tse <- purrr::map_dfr(anos_municipais, extrair_coligacao_prefeito_eleito)

# [UNICO] `cod_mun_ibge_tse` já foi lido na PARTE 0_dados.

coligacao_prefeito_eleito <- coligacao_prefeito_eleito_tse %>%
  left_join(cod_mun_ibge_tse %>% select(codigo_tse, codigo_ibge), by = "codigo_tse") %>%
  rename(id_municipio = codigo_ibge) %>%
  mutate(id_municipio = as.character(id_municipio)) %>%
  select(ano, id_municipio, numero_prefeito_eleito, coligacao_prefeito_eleito) %>%
  inner_join(prefeito_eleito_partido %>%
               mutate(numero_prefeito_eleito = as.numeric(numero_prefeito_eleito)) %>%
               select(ano, id_municipio, numero_prefeito_eleito),
             by = c("ano", "id_municipio", "numero_prefeito_eleito")) %>%
  select(ano, id_municipio, coligacao_prefeito_eleito)

# ── 8. Cadeiras na Câmara de Vereadores ───────────────────────────────────────
cadeiras_partido <- resultados_municipais %>%
  filter(cargo == "vereador", status_eleicao == 1) %>%
  count(ano, id_municipio, sigla_partido_harm, name = "cadeiras")

partido_mais_cadeiras <- cadeiras_partido %>%
  group_by(ano, id_municipio) %>%
  slice_max(cadeiras, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ano, id_municipio, partido_mais_cadeiras = sigla_partido_harm,
         cadeiras_partido_mais_cadeiras = cadeiras)

cadeiras_partido_prefeito <- cadeiras_partido %>%
  rename(partido_prefeito_eleito = sigla_partido_harm,
         cadeiras_partido_prefeito = cadeiras)

coligacao_longa <- coligacao_prefeito_eleito %>%
  filter(!is.na(coligacao_prefeito_eleito), coligacao_prefeito_eleito != "") %>%
  # Federações são desmembradas nos partidos que as compõem, contados
  # individualmente (ver `separar_partidos_coligacao()` em
  # "3. Funcoes_Partidos_Indices.R"); o split antigo por " / " gerava
  # "Federação …(PT/PC do B/PV)" como um único "partido" em 2024 e quebrava
  # coligações que começam com "/ ", subcontando `cadeiras_coligacao_prefeito`.
  mutate(sigla_partido_coligado = separar_partidos_coligacao(coligacao_prefeito_eleito)) %>%
  tidyr::unnest(sigla_partido_coligado) %>%
  mutate(sigla_partido_coligado_harm = harmonizar_sigla_partido(sigla_partido_coligado, ano)) %>%
  # O mesmo partido pode aparecer duas vezes (sigla antiga e nova da mesma
  # linhagem) — sem `distinct`, suas cadeiras seriam somadas em dobro.
  distinct(ano, id_municipio, sigla_partido_coligado_harm)

cadeiras_coligacao_prefeito <- coligacao_longa %>%
  left_join(cadeiras_partido,
            by = c("ano", "id_municipio", "sigla_partido_coligado_harm" = "sigla_partido_harm")) %>%
  mutate(cadeiras = dplyr::coalesce(cadeiras, 0L)) %>%
  group_by(ano, id_municipio) %>%
  summarise(cadeiras_coligacao_prefeito = sum(cadeiras, na.rm = TRUE), .groups = "drop")

camara_resumo <- prefeito_eleito_partido %>%
  select(-numero_prefeito_eleito) %>%
  left_join(coligacao_prefeito_eleito,       by = c("ano", "id_municipio")) %>%
  left_join(partido_mais_cadeiras,           by = c("ano", "id_municipio")) %>%
  left_join(cadeiras_partido_prefeito,       by = c("ano", "id_municipio", "partido_prefeito_eleito")) %>%
  left_join(cadeiras_coligacao_prefeito,     by = c("ano", "id_municipio")) %>%
  mutate(
    cadeiras_partido_prefeito    = dplyr::coalesce(cadeiras_partido_prefeito, 0L),
    # `cadeiras_coligacao_prefeito` só vira 0 quando a coligação É
    # conhecida mas nenhum partido dela elegeu vereador; quando a
    # coligação em si é desconhecida (`coligacao_prefeito_eleito` NA —
    # ver nota em `extrair_coligacao_prefeito_eleito()`), o coalesce
    # ingênuo transformaria "não sei" em "zero cadeiras", o que é diferente.
    cadeiras_coligacao_prefeito  = dplyr::if_else(
      is.na(coligacao_prefeito_eleito), NA_integer_,
      dplyr::coalesce(cadeiras_coligacao_prefeito, 0L)
    ),
    mesmo_partido_prefeito_mais_cadeiras = as.integer(partido_mais_cadeiras == partido_prefeito_eleito)
  )

# ── 9. Características dos candidatos (gênero e raça) ─────────────────────────
bd_cand <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.candidatos") %>%
  dplyr::filter(ano %in% anos_municipais) %>%
  dplyr::select(ano, tipo_eleicao, sigla_uf, id_municipio, id_municipio_tse,
                cargo, numero, numero_partido, sigla_partido,
                genero, raca, situacao, titulo_eleitoral) # [REV 2026-11] R5: título = chave de pessoa

candidatos_municipais <- coletar_bruto(
  "5_candidatos_municipais",
  function() basedosdados::bd_collect(bd_cand),
  fonte  = "basedosdados.br_tse_eleicoes.candidatos",
  filtro = paste("anos", paste(anos_municipais, collapse = ", "),
                 "; colunas: ano, tipo_eleicao, sigla_uf, id_municipio, id_municipio_tse, cargo, numero, numero_partido, sigla_partido, genero, raca, situacao")
)
rm(bd_cand); gc()

candidatos_municipais <- candidatos_municipais %>%
  dplyr::mutate(id_municipio = as.character(id_municipio))

# A tabela `candidatos` identifica o candidato pela coluna `numero`, enquanto
# `resultados_candidato` usa `numero_candidato` — renomeia aqui para os dois
# lados terem a mesma chave de junção.
candidatos_municipais <- candidatos_municipais %>%
  dplyr::rename(numero_candidato = numero)

# `candidatos` registra candidaturas, não candidatos: quando a candidatura
# original é indeferida/cancelada/renunciada e substituída, as duas aparecem
# com o mesmo número de urna — e podem ser PESSOAS DIFERENTES, com gênero e
# raça diferentes.
# [REV 2026-11] R5: antes deduplicava por número de urna e só rebaixava indeferidos e
# cancelados; em renúncia/falecimento etc. o slice(1) escolhia uma pessoa ao
# acaso, o que mudava a contagem de mulheres/negros em ±1 (~1% das linhas).
# Agora deduplica por PESSOA (título eleitoral): o slice(1) só escolhe entre
# registros da mesma pessoa, que têm o mesmo gênero e a mesma raça.
# CONFERIDO: 0 títulos ausentes e 0 títulos repetidos por eleição-município.
candidatos_municipais <- candidatos_municipais %>%
  dplyr::mutate(situacao = tolower(situacao)) %>%
  dplyr::filter(!is.na(titulo_eleitoral)) %>%
  dplyr::group_by(ano, tipo_eleicao, id_municipio, cargo, titulo_eleitoral) %>%
  dplyr::arrange(!situacao %in% c("deferido", "apto"), .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

table(candidatos_municipais$genero)  # CONFERIR rótulos
table(candidatos_municipais$raca)    # CONFERIR rótulos (TSE só coleta raça a partir de 2014)

candidatos_municipais <- candidatos_municipais %>%
  mutate(
    cargo        = tolower(cargo),
    tipo_eleicao = tolower(tipo_eleicao),
    genero       = tolower(genero),
    raca_lower   = tolower(raca),
    grupo_racial = case_when(
      raca_lower %in% c("preta", "parda")                       ~ "negra",
      raca_lower %in% c("branca", "amarela", "indigena", "indígena") ~ "nao_negra",
      TRUE                                                       ~ NA_character_
    )
  ) %>%
  filter(cargo %in% cargos_municipais, tipo_eleicao %in% tipo_eleicao_valor) %>%
  select(ano, tipo_eleicao, sigla_uf, id_municipio, id_municipio_tse, cargo,
         numero_candidato, numero_partido, sigla_partido, genero, grupo_racial, titulo_eleitoral)

# [REV 2026-11] R5: junção dos votos com gênero/raça.
# Antes juntava pelo número de urna (+ partido etc.). Com a deduplicação
# acima feita por pessoa, um mesmo número pode ter duas pessoas, e o join
# por número duplicava votos (votos_totais maior que a soma dos partidos em
# ~1.100 municípios para prefeito e ~4.600 para vereador).
# Agora:
#   1) junta pela PESSOA (título eleitoral), com na_matches = "never" (NA
#      não casa com NA) e relationship = "many-to-one" (o R para com erro
#      se algum voto casar com mais de um candidato);
#   2) só para os votos sem título em `resultados_municipais` (5.692 linhas
#      na última rodada), usa o número de urna como alternativa.
cand_sel <- candidatos_municipais %>%
  select(ano, tipo_eleicao, id_municipio, cargo, numero_candidato,
         titulo_eleitoral, genero, grupo_racial)

base_candidatos_municipais <- resultados_municipais %>%
  left_join(
    cand_sel %>%
      select(ano, tipo_eleicao, id_municipio, cargo,
             titulo_eleitoral_candidato = titulo_eleitoral, genero, grupo_racial),
    by = c("ano", "tipo_eleicao", "id_municipio", "cargo", "titulo_eleitoral_candidato"),
    na_matches = "never", relationship = "many-to-one"
  ) %>%
  left_join(
    cand_sel %>%
      distinct(ano, tipo_eleicao, id_municipio, cargo, numero_candidato, .keep_all = TRUE) %>%
      select(ano, tipo_eleicao, id_municipio, cargo, numero_candidato,
             genero_num = genero, grupo_racial_num = grupo_racial),
    by = c("ano", "tipo_eleicao", "id_municipio", "cargo", "numero_candidato"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    genero       = dplyr::coalesce(genero, genero_num),
    grupo_racial = dplyr::coalesce(grupo_racial, grupo_racial_num)
  ) %>%
  select(-genero_num, -grupo_racial_num)

# [REV 2026-11] R5: o join só acrescenta colunas — não pode criar linhas nem votos.
stopifnot(nrow(base_candidatos_municipais) == nrow(resultados_municipais))
stopifnot(isTRUE(all.equal(sum(base_candidatos_municipais$votos, na.rm = TRUE),
                           sum(resultados_municipais$votos, na.rm = TRUE))))

# CONFERIR: parcela dos votos que ficou sem gênero (deve ser pequena).
base_candidatos_municipais %>%
  group_by(ano) %>%
  summarise(votos_sem_genero = sum(votos[is.na(genero)], na.rm = TRUE) / sum(votos, na.rm = TRUE))
rm(cand_sel)

# `numero_candidato[genero == "feminino"]` devolveria NA (não omitido) para
# linhas em que `genero`/`grupo_racial`/`status_eleicao` é NA, e
# `n_distinct()` conta esse NA como um valor a mais — produzindo uma
# "candidata eleita"/"voto de mulher" fantasma sempre que algum candidato
# do grupo tem gênero/raça/status ausente (ex.: `total_eleitos_prefeito`
# saindo 2 em vez de 1). `%in%` evita isso (NA nunca está "em" um
# conjunto, vira FALSE).
desigualdade_municipais <- base_candidatos_municipais %>%
  filter(turno == 1) %>%
  group_by(ano, id_municipio, cargo) %>%
  summarise(
    total_candidatos     = n_distinct(numero_candidato),
    candidatos_mulheres  = n_distinct(numero_candidato[genero %in% "feminino"]),
    candidatos_negros    = n_distinct(numero_candidato[grupo_racial %in% "negra"]),
    total_eleitos        = n_distinct(numero_candidato[status_eleicao %in% 1]),
    eleitos_mulheres     = n_distinct(numero_candidato[status_eleicao %in% 1 & genero %in% "feminino"]),
    eleitos_negros       = n_distinct(numero_candidato[status_eleicao %in% 1 & grupo_racial %in% "negra"]),
    votos_totais         = sum(votos, na.rm = TRUE),
    votos_mulheres       = sum(votos[genero %in% "feminino"], na.rm = TRUE),
    votos_negros         = sum(votos[grupo_racial %in% "negra"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_candidatos_mulheres = candidatos_mulheres / total_candidatos,
    prop_candidatos_negros   = candidatos_negros   / total_candidatos,
    prop_eleitos_mulheres    = eleitos_mulheres    / total_eleitos,
    prop_eleitos_negros      = eleitos_negros      / total_eleitos,
    prop_votos_mulheres      = votos_mulheres      / votos_totais,
    prop_votos_negros        = votos_negros        / votos_totais,
    razao_eleitos_candidatos_mulheres = prop_eleitos_mulheres / prop_candidatos_mulheres,
    razao_eleitos_candidatos_negros   = prop_eleitos_negros   / prop_candidatos_negros,
    # TSE só coleta raça a partir de 2014 — antes disso `grupo_racial` é
    # sempre NA, o que (mesmo com o `%in%` acima) daria 0 em vez de "sem
    # dado" para as colunas de raça. Zera para NA explicitamente.
    candidatos_negros   = dplyr::if_else(ano < 2014, NA_integer_, candidatos_negros),
    eleitos_negros       = dplyr::if_else(ano < 2014, NA_integer_, eleitos_negros),
    votos_negros          = dplyr::if_else(ano < 2014, NA_real_,    votos_negros),
    prop_candidatos_negros = dplyr::if_else(ano < 2014, NA_real_,  prop_candidatos_negros),
    prop_eleitos_negros   = dplyr::if_else(ano < 2014, NA_real_,   prop_eleitos_negros),
    prop_votos_negros     = dplyr::if_else(ano < 2014, NA_real_,   prop_votos_negros),
    razao_eleitos_candidatos_negros = dplyr::if_else(ano < 2014, NA_real_, razao_eleitos_candidatos_negros)
  )

# ── 10. População de referência para o índice de equidade racial ────────────
# Censo 2010 para 2014 e Censo 2022 de 2016 em diante — ver
# `baixar_populacao_negra_censo()` em "3. Funcoes_Partidos_Indices.R".
if (!exists("populacao_negra_municipio")) populacao_negra_municipio <- baixar_populacao_negra_censo()

# ── 11. Características do eleitorado e dirigentes partidários (SGIP/TSE) ───
eleitorado_municipio <- readRDS(here("1 Bases Individuais", "8 Eleições e Partidos",
                                      "0_dados", "eleitorado_municipio.rds"))
diretorios_partidarios <- readRDS(here("1 Bases Individuais", "8 Eleições e Partidos",
                                        "0_dados", "diretorios_partidarios.rds"))

# ── 12. Índice de equidade (gênero: referência = eleitorado; raça: Censo) ───
desigualdade_municipais <- desigualdade_municipais %>%
  left_join(
    eleitorado_municipio %>% select(ano, id_municipio, prop_mulheres),
    by = c("ano", "id_municipio")
  ) %>%
  mutate(ano_censo = ano_censo_referencia(ano)) %>%
  left_join(populacao_negra_municipio, by = c("ano_censo", "id_municipio")) %>%
  mutate(
    # Índice = DRI de Firpo et al. (ver `calcular_indice_equidade()`).
    indice_equidade_genero_candidatos = calcular_indice_equidade(prop_candidatos_mulheres, prop_mulheres),
    indice_equidade_genero_eleitos    = calcular_indice_equidade(prop_eleitos_mulheres, prop_mulheres),
    indice_equidade_racial_candidatos = calcular_indice_equidade(prop_candidatos_negros, prop_populacao_negra),
    indice_equidade_racial_eleitos    = calcular_indice_equidade(prop_eleitos_negros, prop_populacao_negra),
    # Índice baseado nos votos recebidos no município (e não só em quem
    # concorreu ou em quem foi eleito) — ver "4. Eleicoes_Gerais.R" para a
    # mesma lógica aplicada aos cargos gerais.
    indice_equidade_genero_votos      = calcular_indice_equidade(prop_votos_mulheres, prop_mulheres),
    indice_equidade_racial_votos      = calcular_indice_equidade(prop_votos_negros, prop_populacao_negra)
  ) %>%
  select(-prop_mulheres, -prop_populacao_negra, -ano_censo)

colunas_valor_desigualdade <- setdiff(names(desigualdade_municipais), c("ano", "id_municipio", "cargo"))

desigualdade_largo_municipais <- desigualdade_municipais %>%
  pivot_wider(
    id_cols = c(ano, id_municipio),
    names_from = cargo,
    values_from = dplyr::all_of(colunas_valor_desigualdade),
    names_glue = "{.value}_{cargo}"
  )

# ── 13. Junção final e exportação ─────────────────────────────────────────────
# [REV 2026-11] R6: incumbência entra com left_join. As tabelas de incumbência têm
# ano = ano do titular + 4; com full_join, os eleitos de 2024 viravam
# linhas de 2028 (5.566) e os eleitos de 2000 em municípios sem eleição em
# 2004 viravam linhas novas (2). Com left_join só entram município-anos
# que tiveram eleição.
eleicoes_municipais <- votos_largos_municipais %>%
  full_join(nep_municipais,                   by = c("ano", "id_municipio")) %>%
  full_join(volatilidade_municipais,          by = c("ano", "id_municipio")) %>%
  full_join(participacao_largo_municipais,    by = c("ano", "id_municipio")) %>%
  full_join(margem_prefeito_largo,            by = c("ano", "id_municipio")) %>%
  full_join(camara_resumo,                    by = c("ano", "id_municipio")) %>%
  left_join(incumbencia_prefeito,             by = c("ano", "id_municipio")) %>%
  left_join(incumbencia_vereadores,           by = c("ano", "id_municipio")) %>%
  full_join(desigualdade_largo_municipais,    by = c("ano", "id_municipio")) %>%
  left_join(eleitorado_municipio,             by = c("ano", "id_municipio")) %>%
  left_join(diretorios_partidarios,           by = c("ano", "id_municipio"))

summary(eleicoes_municipais)

eleicoes_municipais <- eleicoes_municipais %>%
  mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio))

# [REV 2026-11] R7: ajustes e checagem antes de exportar.
#   - NEP infinito (município sem votos válidos no cargo, ex.: 5203500 em
#     2012) vira NA, como na base original;
#   - contagens de eleitorado como inteiro (vinham como decimal);
#   - o script PARA se houver município-ano duplicado (antes a PARTE 6 só
#     imprimia a contagem).
eleicoes_municipais <- eleicoes_municipais %>%
  mutate(
    across(starts_with("nep_"), ~ ifelse(is.infinite(.x), NA_real_, .x)),
    across(c(total_eleitores, starts_with("eleitores_")), as.integer)
  )
stopifnot(!anyDuplicated(eleicoes_municipais[c("ano", "id_municipio")]))
stopifnot(all(eleicoes_municipais$ano %in% anos_municipais))


write.xlsx(eleicoes_municipais, file = file.path(dir_dim, "eleicoes_municipais.xlsx"))
save(eleicoes_municipais, file = file.path(dir_dim, "eleicoes_municipais.RData"))

rm(resultados_municipais, votacao_municipio_municipais, candidatos_municipais,
   base_candidatos_municipais, coligacao_prefeito_eleito_tse); gc()


# ==== PARTE 6 · Checagens do protocolo [UNICO] ====
# O protocolo (Etapas 2 e 4) pede, antes de dar a base como pronta:
#   - id_municipio character com 7 dígitos; ano integer (não integer64);
#   - nenhuma duplicidade de município-ano;
#   - diagnóstico de NA (ano x variável), limiar de 50% e variáveis 100% NA
#     sinalizadas;
#   - painel construído a partir de um esqueleto município x ano, com os anos
#     entre eleições recebendo o valor do último pleito (seção 4.3).
# Os scripts 4 e 5 originais não fazem nenhuma dessas etapas; elas estão aqui.

checar_base <- function(df, nome) {
  cat("\n==========", nome, "==========\n")
  cat("class(ano):", class(df$ano), "| class(id_municipio):", class(df$id_municipio), "\n")
  cat("id_municipio com 7 dígitos:", all(nchar(df$id_municipio) == 7, na.rm = TRUE), "\n")
  dup <- df %>% count(ano, id_municipio) %>% filter(n > 1)
  cat("Linhas município-ano duplicadas:", nrow(dup), "\n")
  print(table(df$ano))

  na_long <- df %>%
    group_by(ano) %>%
    summarise(across(everything(), ~ mean(is.na(.x))), .groups = "drop") %>%
    pivot_longer(-ano, names_to = "variavel", values_to = "prop_na")

  vars_100_na <- na_long %>% group_by(variavel) %>% filter(all(prop_na == 1)) %>% distinct(variavel)
  cat("Variáveis 100% NA:", nrow(vars_100_na), "\n")
  if (nrow(vars_100_na) > 0) print(vars_100_na$variavel)

  # Escopo temporal válido (anos com >= 50% de cobertura) — vai para o dicionário.
  escopo <- na_long %>%
    filter(prop_na <= 0.5) %>%
    group_by(variavel) %>%
    summarise(ano_ini = min(ano), ano_fim = max(ano), n_anos_validos = n(), .groups = "drop")
  write.xlsx(list(na_por_ano = na_long, escopo_temporal = escopo, vars_100_na = vars_100_na),
             file = file.path(dir_diag, paste0("diagnostico_NA_", nome, ".xlsx")))

  # Heatmap (sem as colunas voto_*, que são milhares e preenchidas com 0).
  na_plot <- na_long %>% filter(!stringr::str_starts(variavel, "voto_"))
  g <- ggplot(na_plot, aes(x = factor(ano), y = variavel, fill = prop_na)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "firebrick", limits = c(0, 1), name = "% NA") +
    labs(x = "ano", y = NULL, title = paste("Cobertura —", nome)) +
    theme_minimal(base_size = 7)
  ggsave(file.path(dir_diag, paste0("heatmap_NA_", nome, ".png")), g,
         width = 10, height = max(6, n_distinct(na_plot$variavel) * 0.12), limitsize = FALSE)
  invisible(dup)
}

# Painel anual: esqueleto município x ano e, em cada ano, os valores da
# eleição mais recente (ex.: 2001–2003 recebem 2000). As colunas dos
# diretórios partidários são anuais na fonte, então são re-juntadas pelo ano
# real em vez de repetidas da eleição.
expandir_painel_anual <- function(df, anos_eleicao, ano_final) {
  cols_dir <- setdiff(names(diretorios_partidarios), c("ano", "id_municipio"))
  esqueleto <- tidyr::crossing(id_municipio = unique(df$id_municipio),
                               ano = seq(min(anos_eleicao), ano_final))
  esqueleto %>%
    mutate(ano_eleicao_referencia = anos_eleicao[findInterval(ano, anos_eleicao)]) %>%
    left_join(df %>% select(-any_of(cols_dir)) %>% rename(ano_eleicao_referencia = ano),
              by = c("id_municipio", "ano_eleicao_referencia")) %>%
    left_join(diretorios_partidarios %>% mutate(ano = as.integer(ano)),
              by = c("ano", "id_municipio")) %>%
    mutate(ano = as.integer(ano))
}

if (exists("eleicoes_gerais"))     checar_base(eleicoes_gerais, "eleicoes_gerais")
if (exists("eleicoes_municipais")) checar_base(eleicoes_municipais, "eleicoes_municipais")

# ── Validação padronizada (2 Validação/8 Eleições e Partidos/) ──────────────
# checar_base() acima é o diagnóstico interno da dimensão (grava em
# 1 Bases Individuais/.../diagnostico_NA/); validar_dimensao() gera, além
# disso, os artefatos padrão do projeto (2 Validação/).
if (exists("eleicoes_gerais"))     validar_dimensao(eleicoes_gerais, n_dim = 8, sufixo = "gerais")
if (exists("eleicoes_municipais")) validar_dimensao(eleicoes_municipais, n_dim = 8, sufixo = "municipais")

if (FAZER_PAINEL_ANUAL) {
  if (exists("eleicoes_gerais")) {
    anos_g <- sort(unique(eleicoes_gerais$ano))
    eleicoes_gerais_painel_anual <- expandir_painel_anual(eleicoes_gerais, anos_g, ANO_FINAL_PAINEL)
    save(eleicoes_gerais_painel_anual, file = file.path(dir_dim, "eleicoes_gerais_painel_anual.RData"))
  }
  if (exists("eleicoes_municipais")) {
    anos_m <- sort(unique(eleicoes_municipais$ano))
    eleicoes_municipais_painel_anual <- expandir_painel_anual(eleicoes_municipais, anos_m, ANO_FINAL_PAINEL)
    save(eleicoes_municipais_painel_anual, file = file.path(dir_dim, "eleicoes_municipais_painel_anual.RData"))
  }
  # Só .RData: em .xlsx esses painéis ficam muito pesados (milhares de colunas
  # x ~140 mil linhas). Se precisar de .xlsx, use write.xlsx() aqui.
}

# ==== ANEXO · Scripts antigos 0 / 1 / 2 (municipalityBR — OPCIONAL) ====
# Estes três Rmd (Tomás Paixão, nov–dez/2024) são de uma rodada anterior do
# projeto ("municipalityBR"). NENHUMA base produzida por eles é lida pelos
# scripts 3, 4, 5 ou 8: as bases finais vêm inteiras do basedosdados. O único
# reaproveitamento é de ideia: o script 5 extrai a coligação do prefeito pelo
# electionsBR "nos moldes" de baixar_eleicoes_governadores(), mas com função
# própria.
#
# O que só existe aqui (e não nas bases finais): o painel prefeito x
# governador com `alinhado_partido_governador` e `alinhado_coalizao_governador`.
# Se essa variável for necessária, rode com RODAR_LEGADO <- TRUE. Este bloco
# não foi copiado para o código raiz (RODAR_LEGADO fica FALSE e a função
# correspondente não existe aqui) — ver o .REV de Jorge se precisar dele.
