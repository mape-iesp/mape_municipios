# ══════════════════════════════════════════════════════════════════════════
# PAINEL SEGURANÇA + MORTALIDADE MUNICIPAL
# ══════════════════════════════════════════════════════════════════════════
# Etapas:
#   1. Setup
#   2. Importação — Segurança Pública (MDJSP)
#   3. Padronização e agregação — base `seguranca`
#   4. Crosswalk de município (id_municipio/nome/UF — via basedosdados, tabela
#      de referência apenas, não os dados de mortalidade)
#   5. Importação e agregação — Mortalidade (SIM via `microdatasus`, 2000-2023)
#   6. Diagnóstico visual (opcional)
#   7. Preparação para join (chaves normalizadas — seguranca x painel_final)
#   8. Crosswalk de id_municipio (seguranca -> id_municipio oficial)
#   9. Full join das duas bases (mortalidade + segurança MDJSP)
#  10. Preenchimento de id_municipio via crosswalk
#  11. Checagem de não-correspondências
#  12. VALIDAÇÃO — NAs por variável e ano
#  13. VALIDAÇÃO — separar "fora do range temporal" de "erro de join"
#  14. MUNIC — Guarda Municipal e Conselho de Segurança (existência ano a ano)
#  15. Exportação
# ══════════════════════════════════════════════════════════════════════════


# ── 1. Setup ─────────────────────────────────────────────────────────────
library(readxl)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(tidyverse)
library(openxlsx)
library(writexl)
library(basedosdados)   # usado só para o crosswalk de município (etapa 4) —
# a MORTALIDADE em si não vem mais daqui (etapa 5)
library(microdatasus)   # SIM direto do DataSUS, sem o teto de 2019 do basedosdados
library(data.table)
library(stringr)
library(stringi)
library(purrr)

# ── Caminho-base da pasta de Segurança (usado em toda a exportação e cache) ─
pasta_seguranca <- here("1 Bases Individuais", "15 Segurança")
dir.create(pasta_seguranca, showWarnings = FALSE, recursive = TRUE)

# ── Controle de cache do download do SIM (ver etapa 5) ───────────────────
# Por padrão NÃO grava nada em disco: cada ano é baixado, agregado e mantido
# só em memória (o objeto agregado é pequeno — poucas linhas por município).
# Se quiser resiliência para uma rodada nacional de 24 anos (útil caso a
# conexão caia no meio do caminho), mude para TRUE — o cache então vai para
# dentro da própria pasta de Segurança, nunca em outro lugar do computador.
salvar_cache_sim <- FALSE
pasta_cache_sim  <- file.path(pasta_seguranca, "cache_sim_microdatasus")
if (salvar_cache_sim) dir.create(pasta_cache_sim, showWarnings = FALSE)


# ── 2. Importação — Segurança Pública (MDJSP) ───────────────────────────
arquivos <- list.files(
  path       = file.path(pasta_seguranca, "MDJSP"),
  pattern    = "\\.xlsx$",
  full.names = TRUE
)

dados <- map_dfr(arquivos, function(arquivo) {
  ano <- str_extract(basename(arquivo), "\\d{4}")
  read.xlsx(arquivo) %>%
    mutate(ano = as.integer(ano))
})


# ── 3. Padronização e agregação — base `seguranca` ──────────────────────

# 3.1 Padroniza texto
dados <- dados %>%
  mutate(
    ano         = as.integer(ano),
    municipio   = str_to_upper(str_trim(municipio)),
    uf          = str_to_upper(str_trim(uf)),
    evento      = str_trim(evento),
    abrangencia = str_trim(abrangencia)
  )

# 3.2 Coluna de valor unificada
#   total_peso   → Apreensão de Cocaína / Maconha (kg)
#   total_vitima → crimes com vítimas pessoais
#   total        → ocorrências / objetos / operações
eventos_peso <- c("Apreensão de Cocaína", "Apreensão de Maconha")

dados <- dados %>%
  mutate(
    valor = case_when(
      evento %in% eventos_peso                ~ total_peso,
      !is.na(total_vitima) & total_vitima > 0  ~ as.numeric(total_vitima),
      TRUE                                     ~ as.numeric(total)
    )
  )

# 3.3 Função auxiliar: limpar nome de evento para virar nome de coluna
limpar_nome <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("ç",       "c") %>%
    str_replace_all("ã|â|á|à", "a") %>%
    str_replace_all("ê|é|è",   "e") %>%
    str_replace_all("í|î|ì",   "i") %>%
    str_replace_all("ó|ô|õ|ò", "o") %>%
    str_replace_all("ú|û|ù",   "u") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove("_$")
}

# 3.4 Sufixo por abrangência
sufixo_abrangencia <- c(
  "Estadual"                   = "estadual",
  "Polícia Federal"            = "pf",
  "Polícia Rodoviária Federal" = "prf"
)

# 3.5 Agrega por uf-município-ano-evento-abrangência
painel <- dados %>%
  group_by(uf, municipio, ano, evento, abrangencia) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

# 3.6 Pivota

# 3.6a Com abrangência (ex.: homicidio_doloso_estadual)
painel_wide_abrang <- painel %>%
  mutate(col = paste0(limpar_nome(evento), "_", sufixo_abrangencia[abrangencia])) %>%
  select(uf, municipio, ano, col, valor) %>%
  pivot_wider(names_from = col, values_from = valor, values_fill = 0)

# 3.6b Total geral por evento (soma todas as abrangências)
painel_wide_geral <- painel %>%
  group_by(uf, municipio, ano, evento) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  mutate(col = paste0(limpar_nome(evento), "_total")) %>%
  select(uf, municipio, ano, col, valor) %>%
  pivot_wider(names_from = col, values_from = valor, values_fill = 0)

# 3.7 Junta tudo
seguranca <- painel_wide_geral %>%
  left_join(painel_wide_abrang, by = c("uf", "municipio", "ano"))

# 3.8 Remove município "NÃO INFORMADO" e seleciona só as colunas de interesse
seguranca <- seguranca %>%
  filter(municipio != "NÃO INFORMADO") %>%
  select(
    uf, municipio, ano,
    feminicidio_total, homicidio_doloso_total, lesao_corporal_seguida_de_morte_total,
    mandado_de_prisao_cumprido_total,
    morte_no_transito_ou_em_decorrencia_dele_exceto_homicidio_doloso_total,
    mortes_a_esclarecer_sem_indicio_de_crime_total, mortes_no_transito_total,
    roubo_seguido_de_morte_latrocinio_total, suicidio_total,
    tentativa_de_feminicidio_total, tentativa_de_homicidio_total
  )

# 3.9 Checagem rápida
glimpse(seguranca)
summary(seguranca)


# ── 4. Crosswalk de município (id_municipio 7 dígitos + nome + UF) ──────
# O `microdatasus` não traduz CODMUNRES (6 dígitos) para o padrão IBGE de
# 7 dígitos nem devolve nomes/UF por extenso. Em vez de reimplementar essa
# tabela de referência, usamos o basedosdados só para ISSO — uma tabela
# de diretório pequena e estável, sem o problema de cobertura temporal que
# a base de mortalidade tinha.
set_billing_id("dados-importacao") ## Substituir pelo seu projeto

query_crosswalk <- "
SELECT
    municipio.id_municipio AS id_municipio,
    municipio.nome         AS id_municipio_nome,
    municipio.sigla_uf     AS sigla_uf,
    uf.nome                AS sigla_uf_nome
FROM `basedosdados.br_bd_diretorios_brasil.municipio` AS municipio
LEFT JOIN `basedosdados.br_bd_diretorios_brasil.uf` AS uf
    ON municipio.sigla_uf = uf.sigla
"

crosswalk_municipio <- read_sql(query_crosswalk, billing_project_id = get_billing_id())

# id_municipio SEMPRE character — nunca deixar como integer64 (aprendizado
# recorrente do projeto: BigQuery devolve INT64/integer64 e isso quebra
# join silenciosamente)
crosswalk_municipio <- crosswalk_municipio %>%
  mutate(
    id_municipio   = as.character(id_municipio),
    id_municipio_6 = str_sub(id_municipio, 1, 6)
  )

class(crosswalk_municipio$id_municipio)  # confere: "character"

# Checa unicidade do id_municipio_6 (deve ser 1 para 1 — 6 dígitos + dígito
# verificador é uma bijeção dentro do universo de municípios brasileiros)
crosswalk_municipio %>% count(id_municipio_6) %>% filter(n > 1)


# ── 5. Importação e agregação — Mortalidade (SIM via `microdatasus`) ────
# Cobre 2000-2023 (o basedosdados parava em 2019). Baixa, classifica e
# agrega ANO A ANO — nunca todo o Brasil/todos os anos de uma vez — porque
# o volume de óbitos individuais para o país inteiro não cabe
# confortavelmemte em memória. O agregado por ano/município é pequeno e
# fica em uma lista até o fim do loop.

# 5.1 Classificação de causa (mesma regra usada com o basedosdados)
# Homicídio: CID-10 X85-X99, Y00-Y05 (agressões) e R96 (morte súbita,
# usada como proxy de causa violenta indeterminada em alguns registros)
regex_homicidio <- "^(X8[5-9]|X9[0-9]|Y0[0-5]|R96)"

# 5.2 Função: baixa, processa e agrega UM ano
processar_ano_sim <- function(ano_alvo, uf_alvo = "all") {
  
  arquivo_cache <- file.path(pasta_cache_sim, paste0("sim_agregado_", ano_alvo, ".rds"))
  if (salvar_cache_sim && file.exists(arquivo_cache)) {
    message("Ano ", ano_alvo, " já em cache (", pasta_cache_sim, "), pulando download.")
    return(readRDS(arquivo_cache))
  }
  
  message("Baixando SIM-DO ", ano_alvo, " (uf = ", paste(uf_alvo, collapse = ","), ")...")
  
  sim_raw <- fetch_datasus(
    year_start          = ano_alvo,
    year_end            = ano_alvo,
    uf                  = uf_alvo,
    information_system  = "SIM-DO",
    vars                = c("CODMUNRES", "DTOBITO", "CAUSABAS", "SEXO", "RACACOR", "LOCOCOR"),
    stop_on_error       = FALSE,
    quiet               = TRUE
  )
  
  sim <- process_sim(sim_raw)
  rm(sim_raw); gc()
  
  setDT(sim)
  
  # ── AQUI ESTAVA O BUG ────────────────────────────────────────────────
  # Cria a coluna 'ano' na própria tabela ANTES de agrupar por ela.
  # Nome do parâmetro (ano_alvo) é DIFERENTE do nome da coluna (ano) —
  # de propósito, para o data.table nunca precisar decidir entre
  # "coluna ainda inexistente" e "variável externa".
  sim[, ano := ano_alvo]
  
  sim[, cod_mun_6 := str_pad(as.character(CODMUNRES), width = 6, side = "left", pad = "0")]
  sim[, homicidio := as.integer(str_detect(as.character(CAUSABAS), regex_homicidio))]
  
  sim[, sexo_padrao  := str_to_title(str_squish(as.character(SEXO)))]
  sim[, raca_padrao  := str_to_title(str_squish(as.character(RACACOR)))]
  sim[, local_padrao := str_to_lower(str_squish(as.character(LOCOCOR)))]
  
  sim[, `:=`(
    is_feminino  = as.integer(sexo_padrao == "Feminino"),
    is_masculino = as.integer(sexo_padrao == "Masculino"),
    is_domicilio = as.integer(str_detect(local_padrao, "domic")),
    is_branca    = as.integer(raca_padrao == "Branca"),
    is_preta     = as.integer(raca_padrao == "Preta"),
    is_parda     = as.integer(raca_padrao == "Parda"),
    is_amarela   = as.integer(raca_padrao == "Amarela"),
    is_indigena  = as.integer(str_detect(raca_padrao, "Ind[ií]gena"))
  )]
  
  sim[, `:=`(
    ob_total                     = 1L,
    ob_feminino                  = is_feminino,
    ob_masculino                 = is_masculino,
    ob_branca                    = is_branca,
    ob_preta                     = is_preta,
    ob_parda                     = is_parda,
    ob_amarela                   = is_amarela,
    ob_indigena                  = is_indigena,
    ob_preta_feminino            = is_preta    * is_feminino,
    ob_parda_feminino            = is_parda    * is_feminino,
    ob_branca_feminino           = is_branca   * is_feminino,
    ob_amarela_feminino          = is_amarela  * is_feminino,
    ob_indigena_feminino         = is_indigena * is_feminino,
    ob_homicidio                 = homicidio,
    ob_homicidio_feminino        = homicidio * is_feminino,
    ob_homicidio_masculino       = homicidio * is_masculino,
    ob_homicidio_branca          = homicidio * is_branca,
    ob_homicidio_preta           = homicidio * is_preta,
    ob_homicidio_parda           = homicidio * is_parda,
    ob_homicidio_amarela         = homicidio * is_amarela,
    ob_homicidio_indigena        = homicidio * is_indigena,
    ob_homicidio_preta_feminino    = homicidio * is_preta    * is_feminino,
    ob_homicidio_parda_feminino    = homicidio * is_parda    * is_feminino,
    ob_homicidio_branca_feminino   = homicidio * is_branca   * is_feminino,
    ob_homicidio_amarela_feminino  = homicidio * is_amarela  * is_feminino,
    ob_homicidio_indigena_feminino = homicidio * is_indigena * is_feminino,
    ob_homicidio_feminino_domicilio          = homicidio * is_feminino * is_domicilio,
    ob_homicidio_branca_feminino_domicilio   = homicidio * is_branca   * is_feminino * is_domicilio,
    ob_homicidio_preta_feminino_domicilio    = homicidio * is_preta    * is_feminino * is_domicilio,
    ob_homicidio_parda_feminino_domicilio    = homicidio * is_parda    * is_feminino * is_domicilio,
    ob_homicidio_amarela_feminino_domicilio  = homicidio * is_amarela  * is_feminino * is_domicilio,
    ob_homicidio_indigena_feminino_domicilio = homicidio * is_indigena * is_feminino * is_domicilio
  )]
  
  colunas_ob <- grep("^ob_", names(sim), value = TRUE)
  
  # Agora 'ano' e 'cod_mun_6' são colunas de verdade dentro de 'sim' —
  # sem ambiguidade nenhuma no by
  agregado <- sim[
    , lapply(.SD, sum, na.rm = TRUE),
    by = .(ano, id_municipio_6 = cod_mun_6),
    .SDcols = colunas_ob
  ]
  
  if (salvar_cache_sim) saveRDS(agregado, arquivo_cache)
  
  rm(sim); gc()
  agregado
}

teste_rj_2000 <- processar_ano_sim(ano_alvo = 2000, uf_alvo = "RJ")

nrow(teste_rj_2000)
summary(teste_rj_2000)

# Confere se as categorias vieram como esperado antes de rodar o Brasil
# inteiro — pegue uma amostra ANTES da agregação pra inspecionar:
sim_raw_teste <- fetch_datasus(
  year_start = 2000, year_end = 2000, uf = "RJ",
  information_system = "SIM-DO",
  vars = c("CODMUNRES", "CAUSABAS", "SEXO", "RACACOR", "LOCOCOR"),
  stop_on_error = FALSE, quiet = TRUE
)
sim_teste <- process_sim(sim_raw_teste)

table(sim_teste$SEXO)
table(sim_teste$RACACOR)
table(sim_teste$LOCOCOR)


# 5.3 Loop ano a ano (2000-2023)
salvar_cache_sim <- TRUE   # só durante a rodada nacional
dir.create(pasta_cache_sim, showWarnings = FALSE)

anos <- 2000:2023
lista_agregados <- map(anos, processar_ano_sim, .progress = TRUE)
sim_agregado <- rbindlist(lista_agregados)

summary(sim_agregado)


# depois de conferir que sim_agregado está correto e já exportado:
# unlink(pasta_cache_sim, recursive = TRUE)

# 5.4 Checagens antes de seguir
class(sim_agregado$id_municipio_6)  # deve ser "character"
class(sim_agregado$ano)             # deve ser "integer"/"numeric"

sim_agregado %>%
  mutate(n_digitos = nchar(id_municipio_6)) %>%
  count(n_digitos)   # deve dar 100% em 6 dígitos; se não, revisar str_pad acima

# 5.5 Junta nomes/UF/id_municipio de 7 dígitos via crosswalk
painel_final <- sim_agregado %>%
  left_join(
    crosswalk_municipio %>% select(id_municipio, id_municipio_nome, sigla_uf, sigla_uf_nome, id_municipio_6),
    by = "id_municipio_6"
  ) %>%
  select(-id_municipio_6) %>%
  relocate(ano, sigla_uf, sigla_uf_nome, id_municipio, id_municipio_nome)

class(painel_final$id_municipio)  # confere: "character"

summary(painel_final)


# ── 6. Diagnóstico visual (opcional) ─────────────────────────────────────
painel_final %>%
  ggplot(aes(x = ob_homicidio, y = ob_total)) +
  geom_point() +
  geom_smooth(method = "lm")


# ── 7. Preparação para join — chaves normalizadas ────────────────────────
# Nomes de município quase nunca batem 100% entre bases diferentes
# (acentuação, maiúsculas, hífens, espaços duplos). A chave normalizada
# (uf + nome sem acento/maiúsculo/pontuação) resolve a maior parte disso.
normalizar_nome <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%   # remove acentos
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%  # remove hífen, apóstrofo, ponto etc.
    str_squish()                             # colapsa espaços múltiplos
}

seguranca <- seguranca %>%
  mutate(
    sigla_uf_join  = str_to_upper(str_squish(uf)),
    municipio_join = normalizar_nome(municipio)
  )

painel_final <- painel_final %>%
  mutate(
    sigla_uf_join  = str_to_upper(str_squish(sigla_uf)),
    municipio_join = normalizar_nome(id_municipio_nome)
  )


# ── 8. Crosswalk de id_municipio (chave normalizada -> id_municipio) ────
# Dicionário chave -> id_municipio, tirado do painel_final (fonte oficial
# de códigos IBGE), usado depois para preencher id_municipio nas linhas
# que vieram só da base `seguranca`.
crosswalk_join <- painel_final %>%
  distinct(sigla_uf_join, municipio_join, id_municipio, id_municipio_nome)

# Checa se alguma chave tem MAIS DE UM id_municipio associado (ambiguidade)
# — se retornar linhas, há nomes duplicados/ambíguos que precisam ser
# resolvidos antes de seguir.
crosswalk_join %>%
  count(sigla_uf_join, municipio_join) %>%
  filter(n > 1)


# ── 9. Full join das duas bases ──────────────────────────────────────────
base_final <- painel_final %>%
  full_join(
    seguranca %>% select(-uf, -municipio),
    by = c("sigla_uf_join", "municipio_join", "ano")
  )


# ── 10. Preenchimento de id_municipio via crosswalk ──────────────────────
# No full_join, id_municipio só vem preenchido nas linhas onde a chave
# bateu nas duas bases. Linhas vindas só de `seguranca` ficam com
# id_municipio = NA. Este passo completa esses casos via crosswalk, sem
# sobrescrever os que já vieram certos do join principal (coalesce).
base_final <- base_final %>%
  left_join(
    crosswalk_join %>% rename(
      id_municipio_cw      = id_municipio,
      id_municipio_nome_cw = id_municipio_nome
    ),
    by = c("sigla_uf_join", "municipio_join")
  ) %>%
  mutate(
    id_municipio      = coalesce(id_municipio, id_municipio_cw),
    id_municipio_nome = coalesce(id_municipio_nome, id_municipio_nome_cw)
  ) %>%
  select(-id_municipio_cw, -id_municipio_nome_cw)


# ── 11. Checagem de não-correspondências ─────────────────────────────────
# Municípios de `seguranca` cujo nome não existe (nem após normalização)
# em `painel_final` — candidatos a correção manual (grafia divergente,
# mudança de nome oficial etc.).
sem_id <- base_final %>% filter(is.na(id_municipio))
sem_id %>% distinct(sigla_uf_join, municipio_join)

summary(base_final)


# ══════════════════════════════════════════════════════════════════════════
# ── 12. VALIDAÇÃO — NAs por variável e ano ───────────────────────────────
# ══════════════════════════════════════════════════════════════════════════

# 12.1 Conta NAs por variável, dentro de cada ano
na_por_ano <- base_final %>%
  group_by(ano) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  ungroup()

# 12.2 Formato longo: ano | variavel | n_na
na_long <- na_por_ano %>%
  pivot_longer(-ano, names_to = "variavel", values_to = "n_na")

# 12.3 % de NA sobre o total de linhas daquele ano
n_linhas_ano <- base_final %>% count(ano, name = "n_total")

na_long <- na_long %>%
  left_join(n_linhas_ano, by = "ano") %>%
  mutate(pct_na = round(100 * n_na / n_total, 1))

# 12.4 Ranking geral: variáveis com mais NA no total (todos os anos)
na_long %>%
  group_by(variavel) %>%
  summarise(total_na = sum(n_na)) %>%
  arrange(desc(total_na))

# 12.5 Ranking por ano: top 5 variáveis com mais NA em cada ano
na_long %>%
  arrange(ano, desc(n_na)) %>%
  group_by(ano) %>%
  slice_max(n_na, n = 5)

# 12.6 Tabela larga (heatmap): ano nas linhas, variável nas colunas, % NA
na_wide_pct <- na_long %>%
  select(ano, variavel, pct_na) %>%
  pivot_wider(names_from = variavel, values_from = pct_na)

if (interactive()) View(na_wide_pct)   # ou write.csv se quiser abrir no Excel

# 12.7 Heatmap visual (opcional)
ggplot(na_long, aes(x = variavel, y = factor(ano), fill = pct_na)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = "% NA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  labs(x = NULL, y = "Ano", title = "Percentual de NA por variável e ano")


# ══════════════════════════════════════════════════════════════════════════
# ── 13. VALIDAÇÃO — range temporal x erro de join real ───────────────────
# ══════════════════════════════════════════════════════════════════════════
# Nem todo NA é erro de join: parte é só cobertura temporal diferente entre
# as bases (ex.: `seguranca` cobre 2015-2025, `painel_final` cobre 2000-2023).
# Este bloco separa os dois casos.

# 13.1 Resumo por ano e grupo de variável (mortalidade x segurança)
resumo_grupo <- na_long %>%
  mutate(
    grupo = case_when(
      variavel %in% c(
        "feminicidio_total", "homicidio_doloso_total",
        "lesao_corporal_seguida_de_morte_total",
        "mandado_de_prisao_cumprido_total",
        "morte_no_transito_ou_em_decorrencia_dele_exceto_homicidio_doloso_total",
        "mortes_a_esclarecer_sem_indicio_de_crime_total",
        "mortes_no_transito_total",
        "roubo_seguido_de_morte_latrocinio_total",
        "suicidio_total", "tentativa_de_feminicidio_total",
        "tentativa_de_homicidio_total"
      ) ~ "seguranca",
      str_starts(variavel, "ob_") ~ "mortalidade",
      TRUE ~ "outro"
    )
  ) %>%
  filter(grupo != "outro") %>%
  group_by(ano, grupo) %>%
  summarise(pct_na_medio = round(mean(pct_na), 1), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = pct_na_medio) %>%
  arrange(ano)

print(resumo_grupo, n = 30)

# 13.2 Range real de cada base (na fonte original, não no base_final)
range_seguranca   <- seguranca %>% summarise(min = min(ano), max = max(ano))
range_painel_mort <- painel_final %>%
  filter(!is.na(ob_total)) %>%
  summarise(min = min(ano), max = max(ano))

range_seguranca
range_painel_mort

# 13.3 Dentro do overlap das duas bases, quais linhas ainda ficaram sem
# `seguranca`? Aqui sim é problema de nome/uf não batendo — não é range.
overlap_anos <- intersect(
  seq(range_seguranca$min, range_seguranca$max),
  seq(range_painel_mort$min, range_painel_mort$max)
)

suspeitos_join <- base_final %>%
  filter(ano %in% overlap_anos, is.na(homicidio_doloso_total)) %>%
  distinct(sigla_uf_join, municipio_join, ano)

nrow(suspeitos_join)

seguranca_brasileira <- base_final


# ══════════════════════════════════════════════════════════════════════════
# ── 14. MUNIC — Guarda Municipal e Conselho de Segurança ─────────────────
# ══════════════════════════════════════════════════════════════════════════

# 14.0 Caminhos
caminho_munic <- file.path(pasta_seguranca, "Base_MUNIC_2023.xlsx")

# 14.1 Importação das abas de interesse
# Cada aba usa nomes ligeiramente diferentes para as colunas de identificação
# do município (CodMun/Cod Munic, Uf/UF, Desc Mun/Mun). Padronizamos abaixo.
abas <- c(
  assistencia_social = 3,
  politica_mulheres   = 6,
  seguranca_publica   = 7,
  direitos_humanos    = 8
)

ler_aba <- function(indice_aba) {
  read_excel(caminho_munic, sheet = indice_aba) |>
    rename_with(~"cod_mun",   any_of(c("CodMun", "Cod Munic"))) |>
    rename_with(~"uf",        any_of(c("Uf", "UF"))) |>
    rename_with(~"cod_uf",    any_of(c("Cod Uf", "Cod UF"))) |>
    rename_with(~"municipio", any_of(c("Desc Mun", "Mun"))) |>
    rename_with(~"pop_mun",   any_of("PopMun")) |>
    rename_with(~"faixa_pop", any_of("Faixa_pop")) |>
    rename_with(~"regiao",    any_of("Regiao")) |>
    mutate(cod_mun = as.character(cod_mun))
}

bases <- map(abas, ler_aba)

# 14.2 Indicadores: existência + ano de criação/fundação + status (ativo/inativo)
# Para cada conselho/plano/fundo relevante, extraímos as três variáveis
# (quando existentes na pesquisa): existência, ano de criação e situação atual.

indicadores_assistencia <- bases$assistencia_social |>
  select(cod_mun, uf, municipio, pop_mun, faixa_pop, regiao,
         mass_plano_existe          = MASS11,
         mass_plano_ano_atualizacao = MASS11a,
         mass_conselho_existe       = MASS16,
         mass_conselho_ano_criacao  = MASS161b,
         mass_conselho_status       = MASS161c,
         mass_fundo_existe          = MASS22)

indicadores_mulheres <- bases$politica_mulheres |>
  select(cod_mun,
         mppm_organismo_existe     = MPPM01,
         mppm_plano_existe         = MPPM11,
         mppm_plano_ano_criacao    = MPPM11A,
         mppm_conselho_existe      = MPPM13,
         mppm_conselho_ano_criacao = MPPM131b,
         mppm_conselho_status      = MPPM131c)

indicadores_seguranca <- bases$seguranca_publica |>
  select(cod_mun,
         mseg_conselho_municipal_existe      = MSEG07,
         mseg_conselho_municipal_ano_criacao = MSEG071b,
         mseg_conselho_municipal_status      = MSEG071c,
         mseg_conselho_comunitario_existe    = MSEG13,
         mseg_fundo_existe                   = MSEG14,
         mseg_plano_existe                   = MSEG15,
         mseg_guarda_municipal_existe        = MSEG27,
         mseg_guarda_municipal_ano_criacao   = MSEG271b)

indicadores_dh <- bases$direitos_humanos |>
  select(cod_mun,
         mdhu_plano_existe                            = MDHU11,
         mdhu_plano_ano_criacao                        = MDHU112b,
         mdhu_conselho_dh_existe                       = MDHU12,
         mdhu_conselho_dh_ano_criacao                  = MDHU121b,
         mdhu_conselho_dh_status                       = MDHU121c,
         mdhu_conselho_crianca_existe                  = MDHU18,
         mdhu_conselho_crianca_ano_criacao             = MDHU181b,
         mdhu_conselho_crianca_status                  = MDHU181c,
         mdhu_conselho_idoso_existe                    = MDHU24,
         mdhu_conselho_idoso_ano_criacao               = MDHU241b,
         mdhu_conselho_idoso_status                    = MDHU241c,
         mdhu_conselho_pcd_existe                      = MDHU30,
         mdhu_conselho_pcd_ano_criacao                 = MDHU301b,
         mdhu_conselho_pcd_status                      = MDHU301c,
         mdhu_conselho_lgbt_existe                     = MDHU36,
         mdhu_conselho_lgbt_ano_criacao                = MDHU361b,
         mdhu_conselho_lgbt_status                     = MDHU361c,
         mdhu_conselho_igualdade_racial_existe         = MDHU42,
         mdhu_conselho_igualdade_racial_ano_criacao    = MDHU421b,
         mdhu_conselho_igualdade_racial_status         = MDHU421c,
         mdhu_conselho_povos_tradicionais_existe       = MDHU48,
         mdhu_conselho_povos_tradicionais_ano_criacao  = MDHU481b,
         mdhu_conselho_povos_tradicionais_status       = MDHU481c,
         mdhu_conselho_tutelar_existe                  = MDHU61,
         mdhu_conselho_tutelar_ano_criacao             = MDHU611b)

# 14.3 Junta tudo em um único banco, ao nível do município
painel_politicas <- indicadores_assistencia |>
  left_join(indicadores_mulheres,  by = "cod_mun") |>
  left_join(indicadores_seguranca, by = "cod_mun") |>
  left_join(indicadores_dh,        by = "cod_mun")

names(painel_politicas)

conselho_guarda <- painel_politicas %>%
  select(cod_mun, uf, mseg_conselho_municipal_existe, mseg_conselho_municipal_ano_criacao, mseg_conselho_municipal_status,
         mseg_conselho_comunitario_existe, mseg_fundo_existe, mseg_plano_existe,
         mseg_guarda_municipal_existe, mseg_guarda_municipal_ano_criacao)

table(conselho_guarda$mseg_guarda_municipal_ano_criacao)
table(conselho_guarda$mseg_conselho_municipal_status)

# 14.4 Limpa as colunas de ano de criação
# "-" e "Não informou" viram NA; o resto vira numérico
conselho_guarda <- conselho_guarda %>%
  mutate(
    mseg_guarda_municipal_ano_criacao = mseg_guarda_municipal_ano_criacao %>%
      na_if("-") %>%
      na_if("Não informou") %>%
      as.numeric(),
    mseg_conselho_municipal_ano_criacao = mseg_conselho_municipal_ano_criacao %>%
      na_if("-") %>%
      na_if("Não informou") %>%
      as.numeric()
  )

# 14.5 Cria o esqueleto do painel ano x município (2000-2023)
municipios <- conselho_guarda %>%
  distinct(cod_mun, uf)

painel_base <- municipios %>%
  crossing(ano = 2000:2023)

nrow(painel_base)               # deve ser n_municipios * 24
n_distinct(municipios$cod_mun)  # confere n de municípios únicos

# 14.6 Junta as informações de criação/status (fixas por município)
painel_base <- painel_base %>%
  left_join(
    conselho_guarda %>%
      select(cod_mun,
             mseg_guarda_municipal_ano_criacao,
             mseg_conselho_municipal_ano_criacao,
             mseg_conselho_municipal_status),
    by = "cod_mun"
  )

# 14.7 existe_guarda_municipal
# 1 a partir do ano de criação (inclusive) até o fim da série;
# se não tem ano de criação (NA), é 0 em todos os anos
painel_base <- painel_base %>%
  mutate(
    existe_guarda_municipal = case_when(
      is.na(mseg_guarda_municipal_ano_criacao) ~ 0L,
      ano >= mseg_guarda_municipal_ano_criacao  ~ 1L,
      TRUE                                       ~ 0L
    )
  )

# 14.8 existe_conselho_seguranca
# Mesma lógica do ano de criação, MAS só conta como "existe" se o status
# atual for "Ativo". Se está "Inativo", "Foi criado mas não foi instalado"
# ou "Não informou", tratamos como 0 em toda a série — porque não temos
# a data em que deixou de estar ativo.
painel_base <- painel_base %>%
  mutate(
    existe_conselho_seguranca = case_when(
      is.na(mseg_conselho_municipal_ano_criacao) ~ 0L,
      mseg_conselho_municipal_status != "Ativo"   ~ 0L,
      ano >= mseg_conselho_municipal_ano_criacao  ~ 1L,
      TRUE                                         ~ 0L
    )
  )

# 14.9 Checagens
painel_base %>%
  distinct(cod_mun, mseg_conselho_municipal_status) %>%
  count(mseg_conselho_municipal_status)

painel_base %>% filter(existe_guarda_municipal == 1) %>% distinct(cod_mun) %>% nrow()
painel_base %>% filter(existe_conselho_seguranca == 1) %>% distinct(cod_mun) %>% nrow()

painel_base %>%
  filter(mseg_guarda_municipal_ano_criacao == 1892) %>%
  count(cod_mun, existe_guarda_municipal)

# 14.10 Junta ao painel principal
names(painel_base)
names(seguranca_brasileira)

seguranca_brasileira <- left_join(
  seguranca_brasileira, painel_base,
  by = c("ano", "sigla_uf" = "uf", "id_municipio" = "cod_mun")
)

str(seguranca_brasileira)
summary(seguranca_brasileira)

# ── 15. Diagnóstico visual (opcional) ─────────────────────────────────────
seguranca_brasileira %>%
  ggplot(aes(x = ob_homicidio, y = homicidio_doloso_total)) +
  geom_point() +
  geom_smooth(method = "lm")

str(seguranca_brasileira)


# ── 15. Exportação ────────────────────────────────────────────────────────
write_xlsx(
  seguranca_brasileira,
  path = file.path(pasta_seguranca, "seguranca_brasileira.xlsx")
)

# ── Validação padronizada (2 Validação/15 Segurança/) ────────────────────────
validar_dimensao(seguranca_brasileira, n_dim = 15)