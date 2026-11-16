# ══════════════════════════════════════════════════════════════════════════════
# 7 Educação
#
# Junta as bases já tratadas e validadas desta dimensão num painel município-ano.
# Substitui o antigo educacao.R (script da v1, com caminhos "municipalityBR\...",
# que só juntava um Ideb desatualizado com instituicoes.xlsx e nunca tratou a
# Sinopse da educação básica).
#
# Bases que entram:
#   • Censo da Educação Superior — Inep/Base dos Dados, 2009-2024 (painel
#     1995-2025 já construído e validado pelo responsável: ver
#     "Censo Educação Superior/validacao_censo_superior.R" e
#     "Censo Educação Superior/diagnostico/validacao_resultado.csv", 28/28
#     checagens aprovadas em 06/11/2026). Fonte usada aqui: o .RData já
#     tratado (censo_educacao_superior_mun_ano.RData), não o .xlsx — o .xlsx
#     tem ~1 GB de XML internamente e é muito mais lento para ler sem motivo,
#     já que o .RData é o mesmo objeto. 236 variáveis.
#   • Ideb — Inep/Base dos Dados, 2005-2025. Entre os vários arquivos de Ideb
#     na pasta (1_script/ideb.R, ideb_saeb.R, ideb_saeb_v2.R,
#     ideb_atualização.R; "1 Bases de dados limpos/ideb_atualizacao_
#     20261104_233637/ideb_atualizado.RData"), usamos a cópia mais recente na
#     raiz da pasta da dimensão: "7 Educação/ideb_atualizado.RData" — mesmo
#     conteúdo da cópia de 04/11 (116.991 linhas, 86 colunas, idênтico),
#     mas é o arquivo mais novo da pasta (prioridade pedida: sempre o mais
#     recente). Os demais NÃO foram apagados — quem for revisar a dimensão
#     deveria decidir se algum ainda serve pra algo e, se não, removê-los.
#   • Sinopse da Educação Básica (sinopse_resumo) — Inep/Base dos Dados,
#     2007-2024. Fonte: "1 Bases de dados limpos/sinopse_resumo.RData"
#     (100.230 linhas, 5.570 municípios, 92 variáveis de matrícula por etapa/
#     modalidade de ensino, raça/cor e sexo). Não entrava antes (não havia
#     dado tratado na pasta); passou a existir e entra nesta validação.
#
# Decisões:
#   • Censo da Educação Superior já é um painel completo (município x ano,
#     1995-2025, 5.571 municípios) — ele é o esqueleto da dimensão. Ideb e
#     Sinopse entram com left_join sobre esse esqueleto (nenhum dos dois tem
#     município fora do universo do Censo Superior — conferido na seção 4).
#   • Ideb em anos pares: o Ideb só é calculado em anos ímpares (tudo NA nos
#     pares, nas 12 colunas ideb_<etapa>_<rede>_obs). Para cada uma dessas 12
#     colunas, criamos uma coluna irmã ideb_ponderado_<etapa>_<rede> com: nos
#     anos ímpares, o mesmo valor de ideb_<etapa>_<rede>_obs; nos anos pares,
#     a média do Ideb do ano anterior e do ano seguinte (ex.: 2006 = média de
#     2005 e 2007) — pedido explícito do usuário em 13/11/2026. As colunas
#     ideb_<etapa>_<rede>_obs ORIGINAIS continuam no banco, sem alteração
#     (NA nos anos pares); ideb_ponderado_* é sempre uma coluna NOVA.
#   • Nenhuma variável foi descartada por excesso de NA ou redundância —
#     pedido explícito do usuário: errar para o lado de incluir mais
#     variáveis, não menos ("peque mais pelo excesso do que pela falta").
#   • Exportação via writexl::write_xlsx(): ver nota equivalente em
#     "4 Corrupção e Transparência código.R" sobre a referência interna
#     quebrada que o openxlsx (4.2.8.1) grava com write.xlsx().
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(tidyverse)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(writexl)

pasta_dimensao <- here("1 Bases Individuais", "7 Educação")


# ── 2. Ler as bases já tratadas ───────────────────────────────────────────────
e_censo <- new.env()
load(file.path(pasta_dimensao, "Censo Educação Superior", "censo_educacao_superior_mun_ano.RData"),
     envir = e_censo)
censo_superior <- e_censo[["censo_superior"]]

e_ideb <- new.env()
load(file.path(pasta_dimensao, "ideb_atualizado.RData"), envir = e_ideb)
ideb <- e_ideb[["ideb"]]

e_sinopse <- new.env()
load(file.path(pasta_dimensao, "1 Bases de dados limpos", "sinopse_resumo.RData"), envir = e_sinopse)
sinopse <- e_sinopse[["sinopse_resumo"]]

stopifnot(
  is.character(censo_superior$id_municipio), is.integer(censo_superior$ano),
  is.character(ideb$id_municipio),           is.integer(ideb$ano),
  is.character(sinopse$id_municipio),        is.integer(sinopse$ano)
)

range(censo_superior$ano)                # esperado: 1995-2025
range(ideb$ano)                          # esperado: 2005-2025
range(sinopse$ano)                       # esperado: 2007-2024
n_distinct(censo_superior$id_municipio)  # esperado: 5571
n_distinct(ideb$id_municipio)            # esperado: 5571
n_distinct(sinopse$id_municipio)         # esperado: 5570

anyDuplicated(ideb[c("id_municipio", "ano")])     # esperado: 0
anyDuplicated(sinopse[c("id_municipio", "ano")])  # esperado: 0

# Todo município do Ideb/Sinopse precisa estar no universo do Censo Superior
# (que vira o esqueleto da dimensão no join por left_join, seção 4)
setdiff(ideb$id_municipio, censo_superior$id_municipio)     # esperado: character(0)
setdiff(sinopse$id_municipio, censo_superior$id_municipio)  # esperado: character(0)


# ── 3. Ideb: ponderar os anos pares pela média dos ímpares vizinhos ───────────
# Pedido do usuário (13/11/2026): "o IDEB de 2006 é a média do ideb 2005 e
# 2007". Isso só funciona porque o painel do Ideb é uma sequência contínua de
# anos (2005-2025) sem buraco, uma linha por município-ano — checado acima.
colunas_ideb_obs <- grep("^ideb_.*_obs$", names(ideb), value = TRUE)
length(colunas_ideb_obs)  # esperado: 12 (3 etapas x 4 redes)

nomes_ponderado <- paste0("ideb_ponderado_", sub("^ideb_", "", sub("_obs$", "", colunas_ideb_obs)))

ideb <- ideb %>% arrange(id_municipio, ano) %>% group_by(id_municipio)

for (i in seq_along(colunas_ideb_obs)) {
  col_obs  <- colunas_ideb_obs[i]
  col_pond <- nomes_ponderado[i]
  ideb <- ideb %>%
    mutate(!!col_pond := if_else(
      ano %% 2L == 0L,
      (lag(.data[[col_obs]]) + lead(.data[[col_obs]])) / 2,
      .data[[col_obs]]
    ))
}
ideb <- ideb %>% ungroup()

# Checagem: coluna ponderada deve ser idêntica à original nos anos ímpares, e
# só diferir (ou ser NA quando um dos dois vizinhos falta) nos anos pares.
ideb %>%
  filter(ano %% 2L == 1L) %>%
  summarise(diferenca_em_anos_impares = sum(
    .data[[colunas_ideb_obs[1]]] != .data[[nomes_ponderado[1]]], na.rm = TRUE
  ))  # esperado: 0

ideb %>%
  filter(ano == 2006) %>%
  select(id_municipio, all_of(colunas_ideb_obs[1]), all_of(nomes_ponderado[1])) %>%
  head(5)


# ── 4. Juntar (Censo Superior é o esqueleto; Ideb e Sinopse entram por left_join) ──
# O Ideb/Sinopse trazem id_municipio_nome/sigla_uf também; descartados para
# não duplicar coluna, mantendo a versão do Censo da Educação Superior.
ideb_sem_id    <- ideb    %>% select(-any_of(c("id_municipio_nome", "sigla_uf")))
sinopse_sem_id <- sinopse %>% select(-any_of(c("id_municipio_nome", "sigla_uf")))

educacao <- censo_superior %>%
  left_join(ideb_sem_id,    by = c("id_municipio", "ano")) %>%
  left_join(sinopse_sem_id, by = c("id_municipio", "ano")) %>%
  relocate(ano, id_municipio, id_municipio_nome, sigla_uf) %>%
  arrange(id_municipio, ano)


# ── 5. Checagens ──────────────────────────────────────────────────────────────
nrow(educacao) == nrow(censo_superior)              # esperado: TRUE (Ideb/Sinopse cabem dentro do Censo Superior)
anyDuplicated(educacao[c("id_municipio", "ano")])    # esperado: 0
n_distinct(educacao$id_municipio)                    # esperado: 5571
all(nchar(educacao$id_municipio) == 7)               # esperado: TRUE
ncol(educacao)                                        # 236 (censo) + 84 (ideb, sem id/uf) + 12 (ponderado) + 92 (sinopse, sem id/uf)

# Nada se perdeu no join (soma de não-NA antes/depois bate)
sum(!is.na(ideb_sem_id[colunas_ideb_obs])) == sum(!is.na(educacao[colunas_ideb_obs]))
sum(!is.na(sinopse_sem_id$total_matriculas)) == sum(!is.na(educacao$total_matriculas))

# Município-ano sem nenhum dado de Ideb (fora do intervalo 2005-2025, ou
# sem informação disponível naquele ano)
educacao %>%
  filter(is.na(ideb_fundamental_iniciais_1_5_estadual_obs) & ano >= 2005) %>%
  summarise(n = n())

# Município-ano sem dado de Sinopse (fora do intervalo 2007-2024)
educacao %>%
  filter(is.na(total_matriculas) & ano >= 2007 & ano <= 2024) %>%
  summarise(n = n())

summary(educacao[c("ano", "id_municipio")])


# ── 6. Exportar ───────────────────────────────────────────────────────────────
save(educacao, file = file.path(pasta_dimensao, "educacao.RData"))

write_xlsx(
  educacao,
  path = file.path(pasta_dimensao, "educacao.xlsx")
)

# ── 7. Validação padronizada (2 Validação/7 Educação/) ──────────────────────
validar_dimensao(educacao, n_dim = 7)
