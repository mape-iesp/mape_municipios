# =============================================================================
# mape_municipios — Etapa 3: Junção e Transformação
#
# Junta os bancos finais das 17 dimensões (pasta "1 Bases Individuais",
# Etapas 1-4 do Protocolo antigo, já executadas dimensão por dimensão) em um
# único painel município (id_municipio) x ano.
#
# Identificação e População são sempre carregadas (são a base do painel) e
# NÃO recebem etiqueta de dimensão — não fazem sentido como "dimensão de
# política pública" filtrável. As demais 15 dimensões têm cada uma de suas
# variáveis etiquetadas com o atributo `dimensao` (ver `juntar_dimensao()`),
# e o crosswalk variável -> dimensão é salvo em `dicionario_dimensoes`: é
# essa etiqueta que o pacote mape_municipios (Etapa 5 do projeto, pasta
# "5 Pacote") vai usar para filtrar colunas em import_mape_dimension(),
# list_mape_variables(), info_mape() etc.
#
# Convenções seguidas (Protocolo, Etapa 2.2 e 3):
#   - id_municipio como character (7 dígitos IBGE), ano como integer, SEMPRE
#     conferidos antes de qualquer join — ver `carregar_base()`.
#   - Painel primeiro, join depois: o esqueleto município x ano é construído
#     a partir do intervalo de anos realmente presente nas bases (calculado
#     em runtime, nunca hardcoded), para não cortar silenciosamente
#     dimensões com cobertura mais ampla ou mais estreita que Identificação
#     (2000-2025) ou População (1991-2025).
#   - Flagging over silencing: colisões de nome de coluna entre dimensões são
#     resolvidas descartando a versão mais nova (com uma mensagem no
#     console dizendo o quê e de onde), nunca sobrescrevendo em silêncio.
#
# IMPORTANTE — nenhum valor monetário é deflacionado aqui (nem em nenhuma
# outra etapa do banco): as variáveis em R$ permanecem nominais, no valor do
# ano de referência. Isso é uma decisão deliberada do projeto, não um
# esquecimento — não "corrigir" isso adicionando ipca()/deflateBR aqui.
# =============================================================================

library(tidyverse)
library(here)
library(readxl)

PASTA_DIMENSOES <- here("1 Bases Individuais")

# ── 1. Configuração: as 14 dimensões temáticas "simples" ────────────────────
# ("Eleições e Partidos" fica fora desta tabela porque sua base final está
# dividida em dois arquivos — gerais e municipais — que precisam ser unidos
# entre si antes de entrar no painel; ver seção 4.)
#
# slug   : etiqueta ASCII da dimensão, usada pelo pacote (sem acentos —
#          CRAN exige código em ASCII; ver nota equivalente em typology_mape()
#          nos requisitos do pacote).
# tipo   : "xlsx" ou "rdata" — formato do arquivo final já salvo na Etapa 4
#          antiga, dentro da pasta da própria dimensão.
# objeto : nome do objeto R salvo no .RData (ignorado quando tipo == "xlsx").
# chave  : colunas de junção. A maioria é município x ano; "historia" é
#          cross-sectional (uma linha por município — ano de fundação, AMC,
#          capacidade estatal em 1920) e entra só por id_municipio, repetindo
#          o mesmo valor em todos os anos do município (mesmo espírito de
#          fix_baseline(), já fixando um valor constante no tempo).
dimensoes <- tribble(
  ~slug,                     ~nome,                                   ~pasta,                                     ~arquivo,                        ~tipo,   ~objeto,                 ~chave,
  "assistencia_dh",          "Assistência Social e Direitos Humanos", "3 Assistência Social e Direitos Humanos", "assistencia_social_DH.RData",    "rdata", "assistencia_social_dh", c("id_municipio", "ano"),
  "corrupcao_transparencia", "Corrupção e Transparência",             "4 Corrupção e Transparência",             "corrupcao.RData",                "rdata", "corrupcao",             c("id_municipio", "ano"),
  "historia",                "Dados Históricos",                     "5 Dados Históricos",                      "historia_brasileira.xlsx",       "xlsx",  NA_character_,           c("id_municipio"),
  "economia",                "Economia",                             "6 Economia",                               "economia_brasileira.xlsx",       "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "educacao",                "Educação",                             "7 Educação",                               "educacao.RData",                 "rdata", "educacao",              c("id_municipio", "ano"),
  "energia_internet",        "Energia e Internet",                   "9 Energia e Internet",                     "energia_internet.xlsx",          "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "financas",                "Finanças",                             "10 Finanças",                              "financas_brasileiras.xlsx",      "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "habitacao_zoneamento",    "Habitação e Zoneamento",               "11 Habitação e Zoneamento",                "habitacao_zoneamento.xlsx",      "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "meio_ambiente",           "Meio-Ambiente",                        "12 Meio-Ambiente",                         "meio_ambiente.xlsx",             "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "rh",                      "Recursos Humanos",                     "13 Recursos Humanos",                      "recursos_humanos.xlsx",          "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "saude",                   "Saúde",                                "14 Saúde",                                 "saude_brasileira.xlsx",          "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "seguranca",               "Segurança",                            "15 Segurança",                             "seguranca_brasileira.xlsx",      "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "sociedade_participacao",  "Sociedade e Participação",             "16 Sociedade e Participação",              "sociedade_participacao.xlsx",    "xlsx",  NA_character_,           c("id_municipio", "ano"),
  "transportes_mobilidade",  "Transportes e Mobilidade",             "17 Mobilidade",                            "mobilidade_brasileira.RData",    "rdata", "mobilidade_brasileira", c("id_municipio", "ano")
)

# Observação sobre "12 Meio-Ambiente": a pasta tem tanto meio_ambiente.xlsx
# quanto meio_ambiente.RData, mas o .RData não é gerado por nenhum save() em
# "12 Meio-Ambiente código.R" (conferido no código-fonte) — é provavelmente
# resíduo de uma versão anterior. Por segurança, carregamos o .xlsx, que é
# exatamente o que o script atual exporta por último.

# ── 2. Funções genéricas de carregamento e junção ───────────────────────────

#' Carrega a base final de uma dimensão, padronizando id_municipio
#' (character) e ano (integer) antes de qualquer join (Protocolo, 4.2).
carregar_base <- function(pasta, arquivo, tipo, objeto = NA_character_) {
  caminho <- file.path(PASTA_DIMENSOES, pasta, arquivo)
  stopifnot("Arquivo não encontrado" = file.exists(caminho))

  base <- if (tipo == "xlsx") {
    readxl::read_xlsx(caminho, guess_max = 200000)  # > 5.571 municípios x 35 anos
  } else if (tipo == "rdata") {
    e <- new.env()
    load(caminho, envir = e)
    stopifnot("Objeto não encontrado no .RData" = exists(objeto, envir = e))
    get(objeto, envir = e)
  } else {
    stop("Tipo de arquivo desconhecido: ", tipo)
  }

  if ("id_municipio" %in% names(base)) base$id_municipio <- as.character(base$id_municipio)
  if ("ano"          %in% names(base)) base$ano          <- as.integer(base$ano)
  base
}

#' Junta `base` ao `painel` pelas colunas `chave`, descarta colunas que já
#' existem no painel (preserva a versão já carregada, avisando no console —
#' flagging, não silencing) e etiqueta as colunas novas com a dimensão de
#' origem. Acumula o crosswalk variável -> dimensão em `dicionario_dimensoes`
#' (variável global, atualizada por <<- deliberadamente: mantém o loop da
#' seção 5 simples, sem precisar carregar um Reduce() para dois resultados).
juntar_dimensao <- function(painel, base, chave, slug, nome_dimensao) {
  n_dup <- base %>%
    group_by(across(all_of(chave))) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1) %>%
    nrow()
  if (n_dup > 0) {
    stop(
      "[", nome_dimensao, "] ", n_dup, " combinação(ões) de ", paste(chave, collapse = "+"),
      " duplicada(s) na base final — corrija a Etapa 1-4 dessa dimensão antes de juntar."
    )
  }

  colunas_novas   <- setdiff(names(base), chave)
  colunas_colisao <- intersect(colunas_novas, names(painel))

  if (length(colunas_colisao) > 0) {
    message(
      "[", nome_dimensao, "] ", length(colunas_colisao),
      " coluna(s) descartada(s) por já existir no painel (mantida a versão ",
      "já carregada): ", paste(colunas_colisao, collapse = ", ")
    )
    base <- base %>% select(-all_of(colunas_colisao))
  }

  colunas_mantidas <- setdiff(names(base), chave)

  relacionamento <- if (length(chave) == 1) "many-to-one" else "one-to-one"
  painel <- painel %>% left_join(base, by = chave, relationship = relacionamento)

  for (v in colunas_mantidas) attr(painel[[v]], "dimensao") <- slug

  dicionario_dimensoes <<- bind_rows(
    dicionario_dimensoes,
    tibble(variavel = colunas_mantidas, dimensao = slug, dimensao_nome = nome_dimensao)
  )

  painel
}

# ── 3. Identificação e População (sempre carregadas, nunca etiquetadas) ────
diretorios <- carregar_base("1 Identificação", "diretorios_brasileiros.xlsx", "xlsx")
populacao  <- carregar_base("2 População",      "populacao_brasileira.xlsx",   "xlsx")

# id_municipio_nome/sigla_uf_nome de População duplicam, com outro nome,
# informação que Identificação já traz (nome/nome_uf) — descartados aqui
# para o painel final não acabar com dois nomes diferentes para a mesma
# coisa. As colisões de nome EXATO entre as demais dimensões (ex.: toda
# dimensão que também carrega "sigla_uf") são tratadas de forma genérica em
# `juntar_dimensao()`, seção 6 — mas Identificação e População são juntadas
# direto ao painel (abaixo, fora de `juntar_dimensao()`), então a mesma
# colisão exata entre as duas precisa ser resolvida aqui à mão.
# [CORREÇÃO] população também trai sigla_uf (não só sigla_uf_nome) — sem
# descartar, o left_join direto abaixo (sem lógica de colisão) duplicava a
# coluna em sigla_uf.x/sigla_uf.y em vez de manter uma só versão (detectado
# pela checagem final da seção 7, "toda variável deve estar no dicionário ou
# ser de Identificação/População").
populacao <- populacao %>% select(-id_municipio_nome, -sigla_uf_nome, -sigla_uf)

municipios <- diretorios %>% distinct(id_municipio)
stopifnot("Esperado 5.571 municípios em Identificação" = nrow(municipios) == 5571)

# ── 4. Carregamento das 15 dimensões temáticas ──────────────────────────────

bases_dimensoes <- pmap(dimensoes, function(slug, nome, pasta, arquivo, tipo, objeto, chave) {
  list(slug = slug, nome = nome, chave = chave,
       base = carregar_base(pasta, arquivo, tipo, objeto))
})
names(bases_dimensoes) <- dimensoes$slug

# Ajustes específicos de duas dimensões, antes da junção genérica (seção 6):
#   - "16 Sociedade e Participação": a coluna de município chama-se
#     cod_municipio (não id_municipio, como em todas as outras dimensões), e
#     sua "ano_censo" tem um significado diferente do "ano_censo" de
#     População (ano de referência do IVS/IDHM/Gini, não do Censo
#     demográfico) — sem o rename, colidiria em silêncio com o de
#     População dentro de `juntar_dimensao()`.
#   - "15 Segurança": sigla_uf_join/municipio_join são chaves auxiliares de
#     matching interno do próprio script da dimensão (não indicadores) —
#     descartadas antes de entrar no painel.
bases_dimensoes[["sociedade_participacao"]]$base <- bases_dimensoes[["sociedade_participacao"]]$base %>%
  rename(id_municipio = cod_municipio, ano_censo_ivs_idhm_gini = ano_censo) %>%
  mutate(id_municipio = as.character(id_municipio))

#   - "15 Segurança" também tem linhas com id_municipio = NA: nomes de
#     município do MDJSP que não bateram com o diretório do IBGE mesmo após
#     normalização de texto (727 linhas na fonte bruta, 13 a 38 por ano —
#     sinalizado no próprio script da dimensão, seção 11, não corrigido
#     manualmente). group_by(id_municipio, ano) do dplyr agrupa todo NA junto,
#     então esses município-ano "sem município" apareciam como 26
#     combinações duplicadas na checagem de `juntar_dimensao()` — não é
#     duplicidade real (nenhuma delas pode casar com um município de verdade
#     no painel, que não tem linha de id_municipio = NA) e sim o mesmo caso já
#     tratado para o Disque-100 (dimensão 3): registro sem município
#     identificável fica fora do painel, perda medida e reportada, não
#     escondida — mesmo princípio de "flagging over silencing".
bases_dimensoes[["seguranca"]]$base <- bases_dimensoes[["seguranca"]]$base %>%
  select(-any_of(c("sigla_uf_join", "municipio_join"))) %>%
  filter(!is.na(id_municipio))

# "Eleições e Partidos": a base final está dividida em eleições gerais e
# municipais. Usamos as versões "_painel_anual" (não as originais
# eleicoes_gerais.RData / eleicoes_municipais.RData), porque só elas estão no
# grão município x ano — o ano entre dois pleitos recebe o resultado do
# pleito de referência, persistido (ver `expandir_painel_anual()` em
# "8 Eleições e Partidos código.R", seção 14; convenção de dado com
# periodicidade irregular do Protocolo, 4.3). As originais têm uma linha só
# nos anos de eleição.
eleicoes_gerais_painel <- carregar_base(
  "8 Eleições e Partidos", "eleicoes_gerais_painel_anual.RData", "rdata",
  "eleicoes_gerais_painel_anual"
) %>%
  rename(ano_eleicao_referencia_gerais = ano_eleicao_referencia)

eleicoes_municipais_painel <- carregar_base(
  "8 Eleições e Partidos", "eleicoes_municipais_painel_anual.RData", "rdata",
  "eleicoes_municipais_painel_anual"
) %>%
  rename(ano_eleicao_referencia_municipais = ano_eleicao_referencia)

# As colunas de diretórios partidários (SGIP) entram nas duas bases, re-
# juntadas pelo ano real dentro de `expandir_painel_anual()` — são idênticas
# nos dois lados. O full_join abaixo as duplica com sufixo _gerais/
# _municipais em vez de perder uma cópia; redundante, mas não há perda de
# informação (flagging over silencing).
eleicao_partidos <- full_join(
  eleicoes_gerais_painel, eleicoes_municipais_painel,
  by = c("id_municipio", "ano"), suffix = c("_gerais", "_municipais")
)

bases_dimensoes[["eleicao_partidos"]] <- list(
  slug = "eleicao_partidos", nome = "Eleições e Partidos",
  chave = c("id_municipio", "ano"), base = eleicao_partidos
)

stopifnot("Esperadas 15 dimensões temáticas" = length(bases_dimensoes) == 15)

# ── 5. Esqueleto do painel (município x ano) ────────────────────────────────
# O intervalo de anos é calculado a partir do que as bases realmente têm —
# nunca hardcoded — para não cortar silenciosamente dimensões com cobertura
# mais ampla ou mais estreita que Identificação (2000-2025) ou População
# (1991-2025). "historia" não tem coluna `ano` (é cross-sectional) e por
# isso não entra nesse cálculo.
bases_com_ano <- c(list(diretorios = diretorios, populacao = populacao),
                    map(bases_dimensoes, "base")) %>%
  keep(~ "ano" %in% names(.x))

ano_min <- bases_com_ano %>% map_dbl(~ min(.x$ano, na.rm = TRUE)) %>% min()
ano_max <- bases_com_ano %>% map_dbl(~ max(.x$ano, na.rm = TRUE)) %>% max()

cat("Intervalo de anos do painel final:", ano_min, "-", ano_max, "\n")

painel <- municipios %>% crossing(ano = ano_min:ano_max)

# Identificação e População não passam por `juntar_dimensao()`: são a base
# do painel, não "dimensões de política" filtráveis, e não têm colisão de
# nome entre si (já tratada na seção 3).
painel <- painel %>%
  left_join(diretorios, by = c("id_municipio", "ano"), relationship = "one-to-one") %>%
  left_join(populacao,  by = c("id_municipio", "ano"), relationship = "one-to-one")

# ── 6. Junção + etiquetagem das 15 dimensões temáticas ──────────────────────
dicionario_dimensoes <- tibble(variavel = character(), dimensao = character(), dimensao_nome = character())

for (d in bases_dimensoes) {
  painel <- juntar_dimensao(painel, d$base, d$chave, d$slug, d$nome)
}

# ── 7. Checagens finais ──────────────────────────────────────────────────────
stopifnot("Município-ano duplicado no painel final" = !anyDuplicated(painel[c("id_municipio", "ano")]))
stopifnot("Painel não é um retângulo município x ano completo" =
            nrow(painel) == nrow(municipios) * length(ano_min:ano_max))
.nao_contabilizadas <- setdiff(names(painel), c("id_municipio", "ano", names(diretorios), names(populacao),
                                                  dicionario_dimensoes$variavel))
if (length(.nao_contabilizadas) > 0) {
  cat("DIAGNOSTICO - colunas nao contabilizadas:", paste(.nao_contabilizadas, collapse = ", "), "\n")
}
stopifnot("Toda variável do painel (fora id_municipio/ano) deve estar OU no dicionário de dimensões OU ser de Identificação/População" =
            length(.nao_contabilizadas) == 0)

cat("Painel final:", nrow(painel), "linhas x", ncol(painel), "colunas\n")
print(count(dicionario_dimensoes, dimensao_nome, name = "n_variaveis"))

# ── 8. Exportação (uso interno — a Etapa 4 faz a exportação multi-formato) ──
dir.create(here("3 Junção e Transformação"), showWarnings = FALSE)

save(
  painel, dicionario_dimensoes,
  file = here("3 Junção e Transformação", "painel_mape_municipios.RData")
)

write_csv(
  dicionario_dimensoes,
  here("3 Junção e Transformação", "dicionario_dimensoes.csv")
)
