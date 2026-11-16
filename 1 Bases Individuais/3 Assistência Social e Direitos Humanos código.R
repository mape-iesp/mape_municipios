# ══════════════════════════════════════════════════════════════════════════════
# 3 Assistência Social e Direitos Humanos
#
# Seções 1-8: Cadastro Único (CadÚnico) — base município-ano (2015–2023) com as
# famílias com cadastro atualizado e a Taxa de Atualização Cadastral.
# Fonte: MDS/SAGI, indicador IN004 (https://wiki-sagi.mds.gov.br/home/DS/Cad/I/IN004).
# Os brutos estão em CadUnico/0_dados/raw, um .txt por ano, com séries mensais e
# código de município de 6 dígitos.
#
# Decisões (CadÚnico):
#   • O valor de cada ano é o retrato de dezembro.
#   • A série termina em 2023 porque o arquivo de 2024 vai só até novembro.
#   • Nada é imputado: município-ano sem dado fica NA.
#
# Seções 9-13: unifica CadÚnico com as outras bases já prontas desta
# dimensão — Disque-100 (já padronizado), Censo SUAS 2023 e, a partir da
# validação de 13/11/2026, Bolsa Família e BPC via ipeadatar — e exporta o
# painel da dimensão (assistencia_social_DH.xlsx), substituindo o
# assistencia_social_DH.R/.xlsx antigos (script da v1, com caminhos
# "municipalityBR\...", que liam um cadunico.csv que não existe mais).
#
# Bases que entram nas seções 9-14:
#   • Disque-100 (Denúncias de violações de Direitos Humanos) — Ministério dos
#     Direitos Humanos, 2011-2025 (Disque 100/disque100.xlsx, regenerado nesta
#     validação por Disque 100/gerar_disque100.R — ver seção 9: a versão
#     anterior era de 12/11/2024, só ia até 2023 e tinha 5 categorias; a atual
#     tem 13, incluindo intolerância religiosa). Painel não-balanceado (só
#     município-ano com denúncia).
#   • Censo SUAS 2023 (Gestão municipal, unidades da rede socioassistencial e
#     público atendido) — MDS, retrato de um único ano (2023). Bruto: "Censo
#     SUAS/2023/cf26a1e2-867d-4050-a9ea-41d50bfd861e.xlsx", sem id_municipio,
#     só "UF - Nome"; o município é recuperado por nome (ver seção 11).
#   • Bolsa Família (PBF) e Benefício de Prestação Continuada (BPC) — MDS, via
#     pacote ipeadatar (IPEADATA), 2004-2025 (PBF) e 2004-2013/2016 (BPC,
#     séries "INATIVA" no IPEADATA). Ver seção 14: o CadÚnico (IN004) tem as
#     métricas de CADASTRO (famílias atualizadas, taxa de atualização), não
#     os benefícios pagos; procurado no IPEADATA sob "cadastro", "cadúnico",
#     "cadastro único", "SAGI", "IN004", "registro único", "famílias
#     cadastradas" — nenhuma dessas métricas existe lá. PBF e BPC são os
#     programas de benefício correspondentes, não o CadÚnico propriamente.
#
# Base que NÃO entra (dado bruto de 2024 ainda não processado para município):
#   • Censo SUAS 2024 (CRAS/CREAS/Centro POP/Centro de Convivência/Unidade de
#     Acolhimento) — "Censo SUAS/2024/", microdados por unidade, sem
#     agregação a município ainda. Pendente.
#
# Decisões (unificação):
#   • Painel = Diretórios (5.571 municípios) x 2004:2025 (cobre o PBF, que é
#     a série mais longa das cinco fontes). Cada fonte preenche só o próprio
#     intervalo: Censo SUAS 2023 só o ano 2023; CadÚnico só 2015-2023; BPC só
#     até 2013/2016 (séries descontinuadas pelo IPEADATA).
#   • Censo SUAS: "População" não entra (já coberta pela dimensão 2
#     População); as demais colunas ganham o prefixo "suas_".
#   • PBF/BPC: valores monetários (VAL_PBF, BPCVAL_idos, BPCVAL_defi) vêm do
#     IPEADATA em R$ mil NOMINAIS do ano de cada observação; convertidos aqui
#     só para R$ (x 1.000, mudança de unidade) — SEM deflacionar, por
#     instrução da coordenação (mesmo padrão de montante_fiscalizado,
#     dimensão 4).
#   • Exportação via writexl::write_xlsx() em vez de openxlsx::write.xlsx():
#     ver nota equivalente em "4 Corrupção e Transparência código.R" sobre a
#     referência interna quebrada que o openxlsx (4.2.8.1) grava com
#     write.xlsx().
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(tidyverse)
library(openxlsx)
library(here)
library(stringi)
library(writexl)
library(ipeadatar)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))

pasta_dimensao <- here("1 Bases Individuais", "3 Assistência Social e Direitos Humanos")


# ── 2. Ler os brutos ──────────────────────────────────────────────────────────
arquivos <- list.files(
  file.path(pasta_dimensao, "CadUnico", "0_dados", "raw"),
  pattern = "\\.txt$",
  full.names = TRUE
)

cadunico_bruto <- arquivos %>%
  map(~ read_csv(.x, show_col_types = FALSE)) %>%
  bind_rows()

table(substr(cadunico_bruto$anomes_s, 1, 4)) # conferir os anos lidos


# ── 3. Ficar com o retrato de dezembro ────────────────────────────────────────
cadunico <- cadunico_bruto %>%
  filter(str_detect(anomes_s, "12$")) %>%
  mutate(ano = as.integer(substr(anomes_s, 1, 4))) %>%
  select(-anomes_s)

table(cadunico$ano) # esperado: 2015 a 2023


# ── 4. Passar o código do município de 6 para 7 dígitos ───────────────────────
diretorios <- read.xlsx(
  here("1 Bases Individuais", "1 Identificação", "diretorios_brasileiros.xlsx")
) %>%
  distinct(id_municipio, id_municipio_6) %>%
  mutate(
    id_municipio   = as.character(id_municipio),
    id_municipio_6 = as.character(id_municipio_6)
  )

cadunico <- cadunico %>%
  mutate(id_municipio_6 = as.character(codigo_ibge)) %>%
  left_join(diretorios, by = "id_municipio_6") %>%
  select(-codigo_ibge, -id_municipio_6)

sum(is.na(cadunico$id_municipio)) # esperado: 0 códigos sem correspondência


# ── 5. Padronizar nomes e tipos ───────────────────────────────────────────────
# As taxas estão em percentual, então o sufixo _d do bruto vira _pct, como no ETL.
cadunico <- cadunico %>%
  rename(
    cadun_taxa_atualizacao_cadastral_pct = cadun_taxa_atualizacao_cadastral_d,
    cadun_taxa_atualizacao_cadastral_rfpc_ate_meio_sm_pct = cadun_taxa_atualizacao_cadastral_rfpc_ate_meio_sm_d
  ) %>%
  mutate(across(ends_with("_i"), as.integer))


# ── 6. Montar o painel município-ano e juntar os dados ────────────────────────
painel <- diretorios %>%
  distinct(id_municipio) %>%
  crossing(ano = 2015:2023)

cadunico_final <- painel %>%
  left_join(cadunico, by = c("id_municipio", "ano")) %>%
  arrange(id_municipio, ano)


# ── 7. Checagens ──────────────────────────────────────────────────────────────
n_distinct(cadunico_final$id_municipio)  # esperado: 5571
nrow(cadunico_final)                     # esperado: 5571 x 9 = 50139
anyDuplicated(select(cadunico_final, id_municipio, ano)) # esperado: 0

# Município sem nenhum dado na fonte
cadunico_final %>%
  filter(is.na(cadun_qtd_familias_atualizadas_i)) %>%
  distinct(id_municipio)

# Cobertura: as faixas de renda não existem na fonte em todos os anos
cobertura <- cadunico_final %>%
  pivot_longer(starts_with("cadun_"), names_to = "variavel", values_to = "valor") %>%
  group_by(ano, variavel) %>%
  summarise(pct_na = mean(is.na(valor)) * 100, .groups = "drop")

heatmap_na <- ggplot(cobertura, aes(factor(ano), variavel, fill = pct_na)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "firebrick", limits = c(0, 100)) +
  labs(x = "Ano", y = NULL, fill = "% NA", title = "CadÚnico: dados faltantes por ano e variável")

ggsave(
  file.path(pasta_dimensao, "cadunico_heatmap_na.png"),
  heatmap_na, width = 10, height = 5, dpi = 150
)

# Taxas acima de 100% vêm da própria fonte e são mantidas
cadunico_final %>%
  filter(cadun_taxa_atualizacao_cadastral_rfpc_ate_meio_sm_pct > 100) %>%
  count(ano)


# ── 8. Exportar ───────────────────────────────────────────────────────────────
save(cadunico_final, file = file.path(pasta_dimensao, "cadunico.RData"))

write.xlsx(
  cadunico_final,
  file      = file.path(pasta_dimensao, "cadunico.xlsx"),
  rowNames  = FALSE,
  overwrite = TRUE
)


# ── 9. Ler e tratar o Disque-100 ───────────────────────────────────────────────
# CORREÇÃO (sinalizado pelo usuário em 13/11/2026): o disque100.xlsx lido aqui
# antes era de 12/11/2024 e só cobria 2011-2023, com 5 categorias — não batia
# com o que "Disque 100/gerar_disque100.R" já faz. Esse script lê as bases
# brutas disque100-*.csv (não há arquivo de 2010 na pasta, só a partir de
# 2011, apesar do nome sugerir "2010 até 2025") e gera um banco com 12 grupos
# vulneráveis + intolerância religiosa, no grão ano x UF x nome do município
# (não id_municipio). gerar_disque100.R foi rodado de novo nesta validação
# (usa o cache de cada ano bruto já processado, Disque 100/_cache_disque100)
# e a leitura abaixo foi reescrita para a nova estrutura.
disque100_raw <- read.xlsx(file.path(pasta_dimensao, "Disque 100", "disque100.xlsx")) %>%
  as_tibble()

names(disque100_raw) <- names(disque100_raw) %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\\.", "_") %>%
  str_replace_all("\\+", "") %>%
  tolower()

# O código do município da vítima vem com 6 ou 7 dígitos (6 nas bases mais
# antigas); padronizado para 7 via Diretórios, igual ao CadÚnico (seção 4).
# Linha sem código (município "Não Informado" ou código fora dos Diretórios)
# não pode ser atribuída a nenhum município e fica fora do painel — a perda
# é medida e reportada abaixo, não escondida (flagging over silencing).
disque100_raw <- disque100_raw %>%
  mutate(
    cod_6 = if_else(nchar(cod_ibge_municipio_vitima) == 6, cod_ibge_municipio_vitima, NA_character_),
    cod_7 = if_else(nchar(cod_ibge_municipio_vitima) == 7, cod_ibge_municipio_vitima, NA_character_)
  ) %>%
  left_join(diretorios %>% rename(cod_7_de_6 = id_municipio), by = c("cod_6" = "id_municipio_6")) %>%
  mutate(id_municipio = coalesce(cod_7, cod_7_de_6))

n_linhas_sem_municipio <- sum(is.na(disque100_raw$id_municipio))
pct_denuncias_sem_municipio <- sum(disque100_raw$total_denuncias[is.na(disque100_raw$id_municipio)]) /
  sum(disque100_raw$total_denuncias) * 100
cat("Disque-100: linhas sem município identificável:", n_linhas_sem_municipio, "de", nrow(disque100_raw), "\n")
cat("Disque-100: % de denúncias perdidas (sem município):", round(pct_denuncias_sem_municipio, 2), "\n")

disque100 <- disque100_raw %>%
  filter(!is.na(id_municipio)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(id_municipio, ano) %>%
  summarise(across(starts_with("total_denuncias"), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename(
    total_violacoes                            = total_denuncias,
    total_violacoes_cidadao_familia_comunidade = total_denuncias_cidadao_familia_ou_comunidade,
    total_violacoes_crianca_adolescente         = total_denuncias_crianca_e_adolescente,
    total_violacoes_igualdade_racial            = total_denuncias_igualdade_racial,
    total_violacoes_lgbtq                       = total_denuncias_lgbtqia,
    total_violacoes_mulher                      = total_denuncias_mulher,
    total_violacoes_grupo_nao_informado         = total_denuncias_nao_informado,
    total_violacoes_outros                      = total_denuncias_outros,
    total_violacoes_pcd                         = total_denuncias_pessoa_com_deficiencia,
    total_violacoes_pessoa_restricao_liberdade  = total_denuncias_pessoa_em_restricao_de_liberdade,
    total_violacoes_pessoa_idosa                = total_denuncias_pessoa_idosa,
    total_violacoes_pessoa_vulneravel           = total_denuncias_pessoa_socialmente_vulneravel,
    total_violacoes_populacao_rua               = total_denuncias_populacao_em_situacao_de_rua,
    total_violacoes_direitos_humanos            = total_denuncias_violacao_de_direitos_humanos,
    total_violacoes_religiao                    = total_denuncias_intolerancia_religiosa
  )

n_distinct(disque100$id_municipio)                  # esperado: cerca de 5.570
range(disque100$ano)                                # esperado: 2011-2025
anyDuplicated(disque100[c("id_municipio", "ano")])  # esperado: 0


# ── 10. Ler o Censo SUAS 2023 (bruto, nível município) ────────────────────────
# Os nomes de coluna do bruto (acentuados, com ".") são trocados por nomes
# ASCII na hora da leitura, para não depender de identificador acentuado
# solto no meio do código (risco de problema de codificação ao abrir o
# script em outra máquina/editor).
censo_suas_bruto <- read.xlsx(
  file.path(pasta_dimensao, "Censo SUAS", "2023", "cf26a1e2-867d-4050-a9ea-41d50bfd861e.xlsx")
) %>%
  as_tibble() %>%
  rename(
    municipio_raw                         = 2,   # "Município" ("UF  - Nome")
    pessoas_cadunico                      = 4,   # "Pessoas no Cadúnico"
    pessoas_pbf                           = 5,   # "Pessoas no PBF"
    pessoas_bpc                           = 6,   # "Pessoas com BPC"
    pessoas_familia_ate_meio_salario      = 7,   # "Pessoas em família até meio salário"
    gestao_municipal                      = 8,
    fundo_municipal                       = 9,
    conselho_municipal                    = 10,
    unidades_cras                         = 11,
    unidades_posto_cadastramento          = 12,
    unidades_centro_convivencia           = 13,
    unidades_creas                        = 14,
    unidades_centro_pop                   = 15,
    unidades_centro_dia                   = 16,
    unidades_familia_acolhedora           = 17,
    unidades_unidade_acolhimento          = 18
  )

nrow(censo_suas_bruto)  # esperado: 5570 (todos os municípios de 2023 exceto os criados depois)


# ── 11. Recuperar id_municipio do Censo SUAS por nome + UF ────────────────────
# O bruto só tem "Município" no formato "UF  - Nome" (sem id_municipio). A
# normalização abaixo (minúsculas, sem acento, hífen tratado como espaço)
# resolve a maior parte dos casos; os 10 que sobram são nomes que mudaram ou
# têm grafia diferente entre o Censo SUAS e os Diretórios (checado à mão
# contra a lista de municípios do IBGE).
normalizar_nome <- function(x) {
  x <- str_replace_all(x, "-", " ")
  x <- str_squish(x)
  x <- tolower(x)
  stri_trans_general(x, "Latin-ASCII")
}

# Pares "Município (Censo SUAS)" -> "nome (Diretórios)" para os municípios que
# não casam só com a normalização (grafia diferente ou nome antigo).
crosswalk_nomes_suas <- tribble(
  ~uf_token, ~nome_suas,                   ~nome_diretorios,
  "BA",      "Muquém do São Francisco",    "Muquém de São Francisco",
  "BA",      "Santa Terezinha",            "Santa Teresinha",
  "CE",      "Itapajé",                    "Itapagé",
  "MT",      "Poxoréu",                    "Poxoréo",
  "PA",      "Santa Izabel do Pará",       "Santa Isabel do Pará",
  "PE",      "Iguaracy",                   "Iguaraci",
  "RN",      "Augusto Severo",             "Campo Grande",
  "RN",      "Januário Cicco",             "Boa Saúde",
  "SP",      "Florínea",                   "Florínia",
  "SP",      "São Luiz do Paraitinga",     "São Luís do Paraitinga"
) %>%
  mutate(chave_suas = paste(normalizar_nome(nome_suas), uf_token))

diretorios_nomes <- read.xlsx(
  here("1 Bases Individuais", "1 Identificação", "diretorios_brasileiros.xlsx")
) %>%
  as_tibble() %>%
  distinct(id_municipio, nome, sigla_uf) %>%
  mutate(
    id_municipio = as.character(id_municipio),
    chave        = paste(normalizar_nome(nome), sigla_uf)
  )

censo_suas <- censo_suas_bruto %>%
  mutate(
    uf_token   = str_trim(str_extract(municipio_raw, "^[A-Z]{2}")),
    nome_token = str_trim(str_remove(municipio_raw, "^[A-Z]{2}\\s*-\\s*")),
    chave_norm = paste(normalizar_nome(nome_token), uf_token)
  ) %>%
  left_join(crosswalk_nomes_suas, by = c("uf_token", "nome_token" = "nome_suas")) %>%
  mutate(
    chave = if_else(!is.na(nome_diretorios),
                     paste(normalizar_nome(nome_diretorios), uf_token),
                     chave_norm)
  ) %>%
  left_join(diretorios_nomes %>% select(id_municipio, chave), by = "chave")

# Deve sobrar só Boa Esperança do Norte (MT, instalado em 2025 — não existia
# no Censo SUAS 2023)
sum(is.na(censo_suas$id_municipio))  # esperado: 0 (ninguém do Censo SUAS ficou sem id)
setdiff(diretorios_nomes$id_municipio, censo_suas$id_municipio)  # esperado: 1 município (Boa Esperança do Norte)


# ── 12. Padronizar colunas do Censo SUAS ──────────────────────────────────────
# "População" do bruto não entra: já é coberta pela dimensão 2 População.
censo_suas <- censo_suas %>%
  transmute(
    id_municipio                          = id_municipio,
    ano                                   = 2023L,
    suas_pessoas_cadunico                 = pessoas_cadunico,
    suas_pessoas_pbf                      = pessoas_pbf,
    suas_pessoas_bpc                      = pessoas_bpc,
    suas_pessoas_familia_ate_meio_salario = pessoas_familia_ate_meio_salario,
    suas_gestao_municipal                 = gestao_municipal,
    suas_fundo_municipal                  = fundo_municipal,
    suas_conselho_municipal               = conselho_municipal,
    suas_unidades_cras                    = unidades_cras,
    suas_unidades_posto_cadastramento     = unidades_posto_cadastramento,
    suas_unidades_centro_convivencia      = unidades_centro_convivencia,
    suas_unidades_creas                   = unidades_creas,
    suas_unidades_centro_pop              = unidades_centro_pop,
    suas_unidades_centro_dia              = unidades_centro_dia,
    suas_unidades_familia_acolhedora      = unidades_familia_acolhedora,
    suas_unidades_unidade_acolhimento     = unidades_unidade_acolhimento
  )

anyDuplicated(censo_suas$id_municipio)  # esperado: 0


# ── 14. Bolsa Família e BPC via ipeadatar (IPEADATA) ──────────────────────────
pbf_familias      <- ipeadata("FAM_PBF")    %>% filter(uname == "Municipality")
pbf_valor         <- ipeadata("VAL_PBF")    %>% filter(uname == "Municipality")
bpc_idosos_n      <- ipeadata("BPC_idos")   %>% filter(uname == "Municipality")
bpc_deficientes_n <- ipeadata("BPC_defi")   %>% filter(uname == "Municipality")
bpc_idosos_r      <- ipeadata("BPCVAL_idos") %>% filter(uname == "Municipality")
bpc_deficientes_r <- ipeadata("BPCVAL_defi") %>% filter(uname == "Municipality")

range(pbf_familias$date)      # esperado: 2004-2025
range(bpc_idosos_n$date)      # esperado: 2004-2013 (série "INATIVA" no IPEADATA)
range(bpc_deficientes_n$date) # esperado: 2004-2016 ("INATIVA")

# Cada série do ipeadatar vem numa tabela separada (code, date, value, uname,
# tcode); esta função já extrai id_municipio/ano e aplica o fator de unidade
# (x 1.000 só para os valores monetários, que vêm em R$ mil — não é deflação).
preparar_serie_ipea <- function(df, nome_var, fator = 1) {
  df %>%
    transmute(
      id_municipio = as.character(tcode),
      ano          = as.integer(format(date, "%Y")),
      "{nome_var}" := value * fator
    )
}

pbf <- preparar_serie_ipea(pbf_familias, "pbf_familias_beneficiadas_dezembro_i") %>%
  full_join(
    preparar_serie_ipea(pbf_valor, "pbf_valor_dezembro_r", fator = 1000),
    by = c("id_municipio", "ano")
  )

bpc <- preparar_serie_ipea(bpc_idosos_n, "bpc_idosos_beneficios_dezembro_i") %>%
  full_join(preparar_serie_ipea(bpc_deficientes_n, "bpc_deficientes_beneficios_dezembro_i"), by = c("id_municipio", "ano")) %>%
  full_join(preparar_serie_ipea(bpc_idosos_r, "bpc_idosos_valor_dezembro_r", fator = 1000), by = c("id_municipio", "ano")) %>%
  full_join(preparar_serie_ipea(bpc_deficientes_r, "bpc_deficientes_valor_dezembro_r", fator = 1000), by = c("id_municipio", "ano"))

anyDuplicated(pbf[c("id_municipio", "ano")])  # esperado: 0
anyDuplicated(bpc[c("id_municipio", "ano")])  # esperado: 0


# ── 15. Painel da dimensão (2004-2025) e exportação ───────────────────────────
anos_dimensao <- 2004:2025

esqueleto_dimensao <- diretorios %>%
  distinct(id_municipio) %>%
  crossing(ano = anos_dimensao)

assistencia_social_dh <- esqueleto_dimensao %>%
  left_join(cadunico_final, by = c("id_municipio", "ano")) %>%
  left_join(disque100,      by = c("id_municipio", "ano")) %>%
  left_join(censo_suas,     by = c("id_municipio", "ano")) %>%
  left_join(pbf,            by = c("id_municipio", "ano")) %>%
  left_join(bpc,            by = c("id_municipio", "ano")) %>%
  arrange(id_municipio, ano)

# Checagens
n_distinct(assistencia_social_dh$id_municipio)                              # esperado: 5571
nrow(assistencia_social_dh) == n_distinct(diretorios$id_municipio) * length(anos_dimensao)  # esperado: TRUE
anyDuplicated(assistencia_social_dh[c("id_municipio", "ano")])              # esperado: 0
all(nchar(assistencia_social_dh$id_municipio) == 7)                         # esperado: TRUE

# Município-ano sem nenhuma das cinco fontes
assistencia_social_dh %>%
  filter(is.na(cadun_qtd_familias_atualizadas_i) &
           is.na(total_violacoes) &
           is.na(suas_unidades_cras) &
           is.na(pbf_familias_beneficiadas_dezembro_i) &
           is.na(bpc_idosos_beneficios_dezembro_i)) %>%
  summarise(n = n())

save(assistencia_social_dh, file = file.path(pasta_dimensao, "assistencia_social_DH.RData"))

write_xlsx(
  assistencia_social_dh,
  path = file.path(pasta_dimensao, "assistencia_social_DH.xlsx")
)

# ── 16. Validação padronizada (2 Validação/3 Assistência Social e Direitos Humanos/)
validar_dimensao(assistencia_social_dh, n_dim = 3)
