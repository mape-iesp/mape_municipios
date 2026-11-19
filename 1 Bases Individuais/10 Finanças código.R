# ══════════════════════════════════════════════════════════════════════════
# mape_municipios — Dimensão Finanças
# Pipeline completo: Emendas parlamentares (CGU/Portal da Transparência),
# Tesouro (Ranking de Qualidade da Informação Contábil e Fiscal), FIRJAN
# (IFGF), SICONFI (receitas e despesas) — e, ao final, a análise descritiva
# (gráficos e mapas).
# Grão final: id_municipio (IBGE, 7 dígitos) x ano
# ══════════════════════════════════════════════════════════════════════════
#
# UNIFICADO em 13/11/2026 (validação): este arquivo juntou, nesta ordem, três
# scripts que antes eram separados — "10 Finanças código.R" (Tesouro, FIRJAN,
# SICONFI), "10 Emendas parlamentares.R" (raiz de "1 Bases Individuais" e sua
# cópia idêntica em "10 Finanças/Emendas parlamentares/") e "10 Finanças
# análise gráficos.R" — a pedido explícito da validação. Os dois últimos
# foram apagados depois da junção (o conteúdo deles está todo aqui). Nenhuma
# lógica foi alterada; mudanças estão marcadas com [UNICO] no próprio trecho:
#   - pacotes, `set_billing_id()` e `PASTA_FINANCAS` ficam uma única vez, no
#     topo (os três scripts repetiam isso);
#   - a base de emendas (`emendas_parlamentares`) entra na Etapa 7
#     (Integração final), por id_municipio x ano, como o próprio script de
#     Emendas já previa ("Próximos passos", ao final da sua Etapa 9: "na
#     Etapa 3, juntar esta base às outras de Finanças... e rodar de novo o
#     diagnóstico de NA");
#   - a Etapa 9 do script de Emendas (que montava a aba "Geral" e o
#     dicionário de variáveis SÓ para Emendas, como se fosse uma dimensão
#     própria) não entrou aqui: com a junção, a atualização de metadados
#     passa a ser feita uma vez só para toda a dimensão Finanças (Etapa 7 do
#     procedimento padrão desta validação), não mais em duplicidade.
# Os arquivos que o script de Emendas já exportava sozinho
# (10 Finanças/Emendas parlamentares/emendas_parlamentares.xlsx/.RData,
# heatmap_na_emendas_parlamentares.png, metadados_emendas_parlamentares.xlsx)
# continuam sendo gerados por este script (Etapa 2 abaixo) — não foram
# apagados, e ficam como registro do pipeline de Emendas isolado antes da
# junção; o arquivo final da dimensão é financas_brasileiras.xlsx (Etapa 8).

# ── 1. Pacotes ────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(openxlsx)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(readxl)
library(ineq)
library(sf)
library(geobr)
library(patchwork)

set_billing_id("dados-importacao")  ## Substituir pelo projeto de billing do usuário

# Caminho da pasta de Finanças, reutilizado em várias extrações abaixo
PASTA_FINANCAS <- here("1 Bases Individuais", "10 Finanças")


# ══════════════════════════════════════════════════════════════════════════
# 2. EMENDAS PARLAMENTARES
# ══════════════════════════════════════════════════════════════════════════
# Fonte: CGU, Portal da Transparência (coleta direta, arquivo único de emendas)
# Escopo: 2014-2025 | Responsáveis: Carlos e Guilherme | Validação: Priscila
# Valores em reais correntes de cada ano (sem deflação)
# Última atualização: 09/11/2026
#
# Copiado de "10 Emendas parlamentares.R" (script apagado depois da junção).
# [UNICO] pasta_dimensao aponta para PASTA_FINANCAS (já definida acima, em vez
# de redefinida aqui); pacotes e set_billing_id também já feitos acima.
#
# Etapas (preservadas do script original):
#   2.1. Coleta da base bruta
#   2.2. Diretório de municípios
#   2.3. Arquivo principal: emendas pelo município do localizador
#   2.4. Arquivo de favorecidos: valor recebido pelo município do favorecido
#   2.5. Painel município-ano, zeros e NA
#   2.6. Diagnóstico de NA
#   2.7. Checagens e exportação da base (isolada, de Emendas)
# A Etapa 9 original (metadados "Geral"/dicionário só de Emendas, como se
# fosse uma dimensão própria) não entra aqui — ver nota no cabeçalho do
# arquivo: a atualização de metadados agora é feita uma vez para toda a
# dimensão Finanças.

pasta_dimensao <- PASTA_FINANCAS  # [UNICO] antes: here("v2", "1 Bases Individuais", "10 Finanças")
pasta_emendas  <- file.path(pasta_dimensao, "Emendas parlamentares") # arquivos desta base

anos <- 2014:2025

# Base final da dimensão (Etapa 4), onde estas variáveis vão ficar depois da
# Etapa 3. É o nome que vai na coluna "Nome do arquivo" dos metadados
arquivo_dimensao <- "financas_brasileiras.xlsx"

# "Código da Emenda" -> "codigo_da_emenda"
limpar_nome <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_remove_all("^_|_$")
}

# "São José d'Oeste" -> "SAO JOSE D OESTE" (para casar nomes de município)
normalizar_nome <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish()
}

# "1.234,56" -> 1234.56
valor_br <- function(x) {
  parse_number(x, locale = locale(decimal_mark = ",", grouping_mark = "."))
}


# ── 2.1 Coleta da base bruta ──────────────────────────────────────────────
# O .zip do Portal traz três arquivos: o principal (emendas por localidade), o
# de favorecidos (cada pagamento, com o município de quem recebeu) e o de
# convênios (não usado). A bruta nunca é sobrescrita: o download só acontece
# se o .zip ainda não existir. Se o R não conseguir baixar, baixe pelo
# navegador (opção "arquivo único") e salve o .zip na pasta Emendas parlamentares.
link_portal <- "https://portaldatransparencia.gov.br/download-de-dados/emendas-parlamentares"
arquivo_bruto <- file.path(pasta_emendas, "emendas_parlamentares_bruta.zip")

if (!file.exists(arquivo_bruto)) {
  options(timeout = 1800)
  download.file(paste0(link_portal, "/UNICO"), arquivo_bruto, mode = "wb",
                headers = c("User-Agent" = "Mozilla/5.0"))
}

data_coleta <- as.Date(file.info(arquivo_bruto)$mtime)

unzip(arquivo_bruto, list = TRUE)

# Os .csv são extraídos na pasta temporária do próprio R (apagada quando o R
# fecha), sem criar pasta nova
pasta_temp <- tempdir()
unzip(arquivo_bruto, exdir = pasta_temp, overwrite = TRUE,
      files = c("EmendasParlamentares.csv", "EmendasParlamentares_PorFavorecido.csv"))

# Lê um .csv do Portal: separador ";", latin1 e tudo como texto, para não
# perder os zeros à esquerda dos códigos
ler_csv_portal <- function(arquivo) {
  read_delim(file.path(pasta_temp, arquivo), delim = ";",
             locale = locale(encoding = "latin1"),
             col_types = cols(.default = "c"), trim_ws = TRUE) %>%
    rename_with(limpar_nome)
}


# ── 2.2 Diretório de municípios ───────────────────────────────────────────
query <- "
SELECT
    dados.id_municipio AS id_municipio,
    dados.nome AS id_municipio_nome,
    dados.sigla_uf AS sigla_uf
FROM `basedosdados.br_bd_diretorios_brasil.municipio` AS dados
"

municipios <- read_sql(query, billing_project_id = get_billing_id()) %>%
  mutate(id_municipio = as.character(id_municipio)) # nunca deixar como integer64

class(municipios$id_municipio) # confere: "character"
nrow(municipios)               # esperado: 5571

# Boa Esperança do Norte (MT) só foi instalado em 2025: antes disso, NA
municipios_novos <- tibble(id_municipio = "5101837", ano_instalacao = 2025)


# ── 2.3 Arquivo principal: emendas pelo município do localizador ─────────
# A localidade deste arquivo é o localizador do orçamento, definido na Lei
# Orçamentária. Só entram emendas com localizador municipal (código IBGE de 7
# dígitos): quando ele é um estado, "MÚLTIPLO", nacional ou uma região, o
# município de destino não está no arquivo. O ano é o ano da emenda.
# "Restos a pagar inscritos" não é usado: o campo é inconsistente na fonte.
emendas <- ler_csv_portal("EmendasParlamentares.csv") %>%
  select(ano = ano_da_emenda,
         id_emenda = codigo_da_emenda,
         tipo_emenda = tipo_de_emenda,
         id_municipio = codigo_municipio_ibge,
         valor_empenhado,
         valor_liquidado,
         valor_pago,
         valor_resto_pagar_cancelado = valor_restos_a_pagar_cancelados,
         valor_resto_pagar_pagos = valor_restos_a_pagar_pagos) %>%
  mutate(ano = as.integer(ano),
         across(starts_with("valor_"), valor_br),
         municipal = str_detect(coalesce(id_municipio, ""), "^[0-9]{7}$"),
         sem_codigo = is.na(id_emenda) | id_emenda %in% c("", "Sem informação"),
         tipo = str_to_lower(tipo_emenda),
         tipo = case_when(str_detect(tipo, "individual") ~ "individual", # inclui as emendas Pix
                          str_detect(tipo, "bancada") ~ "bancada",
                          str_detect(tipo, "comiss") ~ "comissao",
                          str_detect(tipo, "relator") ~ "relator",
                          TRUE ~ "outros"))

table(emendas$ano)                       # cobertura da fonte
table(emendas$tipo_emenda, emendas$tipo) # cada tipo original na categoria certa

cobertura_principal <- range(emendas$ano)

emendas <- emendas %>% filter(ano %in% anos)

setdiff(anos, emendas$ano) # anos do escopo ausentes na fonte (esperado: nenhum)

# Ano em que cada tipo aparece na fonte, em qualquer destino. Em 2014 e 2015 o
# arquivo só traz emendas individuais: bancada, comissão e relator ficam NA
# nesses anos, e não zero (protocolo, seção 4.3)
inicio_tipo <- emendas %>%
  group_by(tipo) %>%
  summarise(inicio = min(ano))

inicio_tipo

# Parte do valor empenhado que fica fora por não ter localizador municipal
perc_sem_localizador <- sum(emendas$valor_empenhado[!emendas$municipal], na.rm = TRUE) /
  sum(emendas$valor_empenhado, na.rm = TRUE) * 100

perc_sem_localizador

# Códigos da CGU que não existem no diretório (esperado: 0 linhas)
codigos_sem_diretorio <- emendas %>%
  filter(municipal, !id_municipio %in% municipios$id_municipio) %>%
  count(id_municipio)

codigos_sem_diretorio

# Soma por município e ano. Emendas sem código (comuns em 2014, 2016 e 2017)
# contam uma por linha, porque na fonte cada linha é uma emenda
emendas_municipio <- emendas %>%
  filter(municipal) %>%
  group_by(id_municipio, ano) %>%
  summarise(quantidade_emendas = n_distinct(id_emenda[!sem_codigo]) + sum(sem_codigo),
            across(starts_with("valor_"), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(valor_pago_total = valor_pago + valor_resto_pagar_pagos)

# Valores por tipo: primeiro agrega numa tabela longa, depois transforma os
# tipos em colunas (protocolo: separar agregação de reshape)
tipos <- c("individual", "bancada", "comissao", "relator")

emendas_tipo <- emendas %>%
  filter(municipal) %>%
  group_by(id_municipio, ano, tipo) %>%
  summarise(valor_empenhado = sum(valor_empenhado, na.rm = TRUE),
            valor_pago_total = sum(valor_pago, na.rm = TRUE) + sum(valor_resto_pagar_pagos, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = tipo, values_from = c(valor_empenhado, valor_pago_total),
              values_fill = 0)

emendas_municipio <- emendas_municipio %>%
  left_join(emendas_tipo, by = c("id_municipio", "ano"))

# A soma dos tipos tem que bater com o total (esperado: praticamente 0)
max(abs(emendas_municipio$valor_empenhado -
          rowSums(select(emendas_municipio, starts_with("valor_empenhado_")))))


# ── 2.4 Arquivo de favorecidos: valor recebido pelo município do favorecido ─
# Cada linha é um pagamento, com o município de quem recebeu (o favorecido) e
# o mês do pagamento. O ano aqui é o ano do pagamento.
favorecidos <- ler_csv_portal("EmendasParlamentares_PorFavorecido.csv") %>%
  select(ano_mes,
         id_favorecido = codigo_do_favorecido,
         natureza_juridica,
         sigla_uf = uf_favorecido,
         municipio = municipio_favorecido,
         valor_recebido) %>%
  mutate(ano_mes = str_remove_all(ano_mes, "\\D"),
         ano = as.integer(str_sub(ano_mes, 1, 4)),
         valor_recebido = valor_br(valor_recebido),
         sigla_uf = str_to_upper(str_trim(sigla_uf)))

table(favorecidos$ano) # cobertura da fonte: começa em junho de 2014

cobertura_favorecidos <- range(favorecidos$ano_mes[nchar(favorecidos$ano_mes) == 6])

favorecidos <- favorecidos %>% filter(ano %in% anos)

# Intermediários financeiros: bancos e agentes de pagamento sediados em
# Brasília, que repassam o dinheiro a outros entes (sobretudo saúde fundo a
# fundo, via Banco do Brasil). Como o destino final não aparece no arquivo,
# eles saem do valor recebido. A identificação é pelo código, e não pelo nome,
# para evitar falsos positivos: raiz do CNPJ do Banco do Brasil e da Caixa.
favorecidos <- favorecidos %>%
  mutate(intermediario = case_when(
    str_detect(id_favorecido, "^00000000[0-9]{6}$") ~ "Banco do Brasil",
    str_detect(id_favorecido, "^00360305[0-9]{6}$") ~ "Caixa Econômica Federal",
    id_favorecido == "RB0000104" ~ "Caixa (agente operador do PBF)",
    id_favorecido == "RB0038166" ~ "Folha de pagamento",
    TRUE ~ NA_character_))

# Parte do valor pago a intermediários. Em 2025 ela quase some: o Portal
# passou a registrar o fundo municipal como favorecido (quebra de série)
perc_intermediarios <- favorecidos %>%
  filter(ano >= 2015) %>%
  group_by(periodo = if_else(ano >= 2025, "2025", "2015-2024")) %>%
  summarise(perc = sum(valor_recebido[!is.na(intermediario)], na.rm = TRUE) /
              sum(valor_recebido, na.rm = TRUE) * 100)

perc_intermediarios

# O arquivo não tem código IBGE: casamos por UF + nome padronizado. Alguns
# municípios aparecem no Portal com o nome antigo; a tabela abaixo corrige
# esses casos (se surgirem outros, acrescente aqui)
correcoes_nomes <- tribble(
  ~sigla_uf, ~nome_padronizado,       ~id_municipio_correcao,
  "SP",      "EMBU",                  "3515004", # Embu das Artes
  "RS",      "SANTANA DO LIVRAMENTO", "4317103", # Sant'Ana do Livramento
  "PA",      "ELDORADO DOS CARAJAS",  "1502954", # Eldorado do Carajás
  "RN",      "PRESIDENTE JUSCELINO",  "2410306", # Serra Caiada
  "RN",      "AUGUSTO SEVERO",        "2401305", # Campo Grande
  "MG",      "BRASOPOLIS",            "3108909", # Brazópolis
  "RN",      "JANUARIO CICCO",        "2405306", # Boa Saúde
  "PB",      "SERIDO",                "2515401", # São Vicente do Seridó
  "RJ",      "TRAJANO DE MORAIS",     "3305901"  # Trajano de Moraes
)

diretorio_nomes <- municipios %>%
  transmute(id_municipio, sigla_uf, nome_padronizado = normalizar_nome(id_municipio_nome))

# UF + nome precisa ser único no diretório, senão o join duplica pagamentos
diretorio_nomes %>% count(sigla_uf, nome_padronizado) %>% filter(n > 1) # esperado: 0 linhas

favorecidos <- favorecidos %>%
  mutate(nome_padronizado = normalizar_nome(municipio)) %>%
  left_join(diretorio_nomes, by = c("sigla_uf", "nome_padronizado")) %>%
  left_join(correcoes_nomes, by = c("sigla_uf", "nome_padronizado")) %>%
  mutate(id_municipio = coalesce(id_municipio, id_municipio_correcao)) %>%
  select(-id_municipio_correcao)

# Pagamentos sem município identificado (esperado: registros sem município
# informado, como "Sem informação" e "Inválido", e poucos reais com UF trocada;
# se aparecer um nome de município de verdade, acrescente em correcoes_nomes)
favorecidos %>%
  filter(is.na(intermediario), is.na(id_municipio)) %>%
  count(sigla_uf, municipio, wt = valor_recebido, sort = TRUE)

perc_sem_municipio <- favorecidos %>%
  summarise(sum(valor_recebido[is.na(intermediario) & is.na(id_municipio)], na.rm = TRUE) /
              sum(valor_recebido, na.rm = TRUE) * 100) %>%
  pull()

# Administração municipal: natureza jurídica com "Município" ou "Municipal"
# (prefeitura, fundos, autarquias, fundações). Em Brasília não há prefeitura:
# lá, o governo do Distrito Federal conta como administração local
favorecidos <- favorecidos %>%
  mutate(natureza = normalizar_nome(coalesce(natureza_juridica, "")),
         adm_municipal = str_detect(natureza, "MUNICIP") |
           (coalesce(id_municipio, "") == "5300108" & str_detect(natureza, "DISTRITO FEDERAL")))

# Soma por município e ano do pagamento, já sem os intermediários
recebido_municipio <- favorecidos %>%
  filter(is.na(intermediario), !is.na(id_municipio)) %>%
  group_by(id_municipio, ano) %>%
  summarise(valor_recebido_total = sum(valor_recebido, na.rm = TRUE),
            valor_recebido_adm_municipal = sum(valor_recebido[adm_municipal], na.rm = TRUE),
            .groups = "drop")


# ── 2.5 Painel município-ano, zeros e NA ──────────────────────────────────
# Painel primeiro, join depois (protocolo, seção 4.2)
painel <- crossing(id_municipio = municipios$id_municipio, ano = anos)

nrow(painel) # esperado: 5571 municípios x 12 anos = 66852

# Classes das chaves antes do join (esperado: "character" e "integer")
class(painel$id_municipio)
class(emendas_municipio$id_municipio)
class(recebido_municipio$id_municipio)
class(painel$ano)
class(emendas_municipio$ano)
class(recebido_municipio$ano)

emendas_parlamentares <- painel %>%
  left_join(municipios, by = "id_municipio") %>%
  left_join(emendas_municipio, by = c("id_municipio", "ano")) %>%
  left_join(recebido_municipio, by = c("id_municipio", "ano"))

# Município-ano sem emenda ou sem pagamento = 0: a fonte reúne todas as
# emendas, então a ausência é um zero verdadeiro. Ficam NA:
#   - valor recebido em 2014 (o arquivo de favorecidos começa em junho de 2014);
#   - bancada, comissão e relator antes de aparecerem na fonte (2016);
#   - Boa Esperança do Norte (MT) antes de ser instalado (2025).
# Não há LOCF: emendas são fluxos anuais, o valor de um ano não vale para outro.
emendas_parlamentares <- emendas_parlamentares %>%
  mutate(across(c(quantidade_emendas, starts_with("valor_")), ~ replace_na(.x, 0)),
         recebeu_emenda = if_else(quantidade_emendas > 0, 1L, 0L),
         across(c(valor_recebido_total, valor_recebido_adm_municipal),
                ~ if_else(ano < 2015, NA, .x)))

for (i in seq_len(nrow(inicio_tipo))) {
  colunas <- paste0(c("valor_empenhado_", "valor_pago_total_"), inicio_tipo$tipo[i])
  emendas_parlamentares <- emendas_parlamentares %>%
    mutate(across(any_of(colunas), ~ if_else(ano < inicio_tipo$inicio[i], NA, .x)))
}

emendas_parlamentares <- emendas_parlamentares %>%
  left_join(municipios_novos, by = "id_municipio") %>%
  mutate(across(c(recebeu_emenda, quantidade_emendas, starts_with("valor_")),
                ~ if_else(!is.na(ano_instalacao) & ano < ano_instalacao, NA, .x))) %>%
  select(-ano_instalacao)

# Valores em reais correntes de cada ano (sem deflação), arredondados para
# centavos para não deixar resíduos das somas, e ordem final das colunas
emendas_parlamentares <- emendas_parlamentares %>%
  mutate(across(starts_with("valor_"), ~ round(.x, 2))) %>%
  select(id_municipio, ano, id_municipio_nome, sigla_uf,
         recebeu_emenda, quantidade_emendas,
         valor_empenhado, valor_liquidado, valor_pago,
         valor_resto_pagar_cancelado, valor_resto_pagar_pagos, valor_pago_total,
         all_of(paste0("valor_empenhado_", tipos)),
         all_of(paste0("valor_pago_total_", tipos)),
         valor_recebido_total, valor_recebido_adm_municipal) %>%
  arrange(id_municipio, ano)

# Conferência rápida por ano (R$ bilhões correntes)
emendas_parlamentares %>%
  group_by(ano) %>%
  summarise(municipios_com_emenda = sum(recebeu_emenda, na.rm = TRUE),
            empenhado_bi = sum(valor_empenhado, na.rm = TRUE) / 1e9,
            pago_total_bi = sum(valor_pago_total, na.rm = TRUE) / 1e9,
            recebido_total_bi = sum(valor_recebido_total, na.rm = TRUE) / 1e9)


# ── 2.6 Diagnóstico de NA (isolado, de Emendas) ───────────────────────────
# Percentual de NA por variável e ano. O escopo temporal válido de cada
# variável são os anos com no máximo 50% de NA (protocolo, seção 4.4)
na_long_emendas <- emendas_parlamentares %>%
  group_by(ano) %>%
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) %>%
  pivot_longer(-ano, names_to = "variavel", values_to = "pct_na")

grafico_na_emendas <- ggplot(na_long_emendas, aes(x = factor(ano), y = fct_rev(fct_inorder(variavel)), fill = pct_na)) +
  geom_tile(color = "grey80") +
  geom_text(aes(label = round(pct_na)), size = 2.5) +
  scale_fill_gradient(low = "white", high = "red", limits = c(0, 100), name = "% NA") +
  theme_minimal() +
  labs(x = "Ano", y = NULL, title = "Percentual de NA por variável e ano: emendas parlamentares")

grafico_na_emendas

ggsave(file.path(pasta_emendas, "heatmap_na_emendas_parlamentares.png"),
       grafico_na_emendas, width = 10, height = 7, dpi = 300, bg = "white")

escopo_emendas <- na_long_emendas %>%
  filter(pct_na <= 50) %>%
  group_by(variavel) %>%
  summarise(escopo_temporal = if_else(n() == max(ano) - min(ano) + 1,
                                      paste(min(ano), "–", max(ano)),
                                      paste(sort(ano), collapse = ", "))) %>%
  add_row(variavel = "ano", escopo_temporal = paste(min(anos), "–", max(anos)))

escopo_emendas

# Variáveis 100% NA: nunca apagar, registrar nas Observações (esperado: nenhuma)
variaveis_100_na_emendas <- na_long_emendas %>%
  group_by(variavel) %>%
  summarise(todo_na = all(pct_na == 100)) %>%
  filter(todo_na) %>%
  pull(variavel)

variaveis_100_na_emendas


# ── 2.7 Checagens e exportação da base (isolada, de Emendas) ─────────────
# Checklist do protocolo antes de salvar (seção 6): o script para se algo falhar
stopifnot(
  is.character(emendas_parlamentares$id_municipio),
  all(nchar(emendas_parlamentares$id_municipio) == 7),
  is.integer(emendas_parlamentares$ano),
  nrow(distinct(emendas_parlamentares, id_municipio, ano)) == nrow(emendas_parlamentares),
  nrow(emendas_parlamentares) == nrow(municipios) * length(anos)
)

str(emendas_parlamentares)

save(emendas_parlamentares, file = file.path(pasta_emendas, "emendas_parlamentares.RData"))

write.xlsx(
  emendas_parlamentares,
  file      = file.path(pasta_emendas, "emendas_parlamentares.xlsx"),
  rowNames  = FALSE,
  overwrite = TRUE
)

# [UNICO] a partir daqui, `emendas_parlamentares` é reaproveitada na Etapa 7
# (Integração final) desta dimensão — ver nota no cabeçalho do arquivo.


# ══════════════════════════════════════════════════════════════════════════
# 3. TESOURO — Ranking da Qualidade da Informação Contábil e Fiscal
# ══════════════════════════════════════════════════════════════════════════

tesouro <- readr::read_csv2(
  file.path(PASTA_FINANCAS,
            "Ranking da Qualidade da Informação Contábil e Fiscal no Siconfi",
            "dados_ranking.csv")
) %>%
  select(exercicio, Ente, nome, sigla,
         DI, DII, DIII, DIV, acertos_total, nota_ranking, class_ranking) %>%
  rename(
    ano                       = exercicio,
    id_municipio              = Ente,
    entrega_prazo_retificacao = DI,
    elementos_contabeis       = DII,
    fiscal                    = DIII,
    contabil_fiscal           = DIV,
    nota_ranking_tesouro      = nota_ranking,
    classe_ranking_tesouro    = class_ranking
  ) %>%
  mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio))


# ══════════════════════════════════════════════════════════════════════════
# 4. FIRJAN — Índice FIRJAN de Gestão Fiscal (IFGF), 5 dimensões
# ══════════════════════════════════════════════════════════════════════════

firjan_path <- file.path(PASTA_FINANCAS, "FIRJAN",
                         "Evolucao_por_Indicador_2013_a_2024_IFGF_2025.xlsx")

# Empilha uma aba (ano em colunas -> ano em linhas) e converte para numérico.
# "Dados Indisponíveis" (texto presente nas 5 abas) vira NA automaticamente
# via as.numeric — comportamento esperado da fonte, não erro de leitura.
reshape_ifgf <- function(sheet, var_name) {
  openxlsx::read.xlsx(firjan_path, sheet = sheet) %>%
    select(Código, UF, Município, matches("^IFGF")) %>%  # descarta coluna-fantasma (NA final em "Gastos com Pessoal")
    pivot_longer(matches("^IFGF"), names_to = "ano", names_pattern = "(\\d{4})$",
                 values_to = var_name) %>%
    mutate(
      ano = as.integer(ano),
      "{var_name}" := suppressWarnings(as.numeric(.data[[var_name]]))
    )
}

firjan_long <- reshape_ifgf("IFGF Geral", "ifgf_geral") %>%
  left_join(reshape_ifgf("IFGF Autonomia", "ifgf_autonomia"),
            by = c("Código", "UF", "Município", "ano")) %>%
  left_join(reshape_ifgf("IFGF Gastos com Pessoal", "ifgf_gastos_pessoal"),
            by = c("Código", "UF", "Município", "ano")) %>%
  left_join(reshape_ifgf("IFGF Liquidez", "ifgf_liquidez"),
            by = c("Código", "UF", "Município", "ano")) %>%
  left_join(reshape_ifgf("IFGF Investimentos", "ifgf_investimentos"),
            by = c("Código", "UF", "Município", "ano")) %>%
  rename(id_municipio_6 = Código) %>%
  mutate(id_municipio_6 = as.character(id_municipio_6))

# FIRJAN identifica município por código IBGE de 6 dígitos (sem dígito
# verificador) — de-para para o id_municipio de 7 dígitos usado no restante
# do painel, via o diretório oficial de municípios (ano de referência 2024).
identificacao <- openxlsx::read.xlsx(
  here("1 Bases Individuais", "1 Identificação", "diretorios_brasileiros.xlsx")
) %>%
  filter(ano == 2024) %>%
  select(id_municipio, id_municipio_6)

firjan <- firjan_long %>%
  left_join(identificacao, by = "id_municipio_6") %>%
  select(-Município, -UF, -id_municipio_6)


# ══════════════════════════════════════════════════════════════════════════
# 5. SICONFI — RECEITAS ORÇAMENTÁRIAS
# ══════════════════════════════════════════════════════════════════════════
#
# Decisões de modelagem (ver histórico de diagnóstico para o racional completo):
#  - Escopo restrito a ano >= 2013: antes disso (FINBRA) não há id_conta_bd
#    nem estágio estruturado, o que impossibilita reconstruir o total sem
#    risco de dupla contagem.
#  - Classificação por TEXTO exato de `conta`, não por `id_conta_bd`: o
#    código só é harmonizado pelo Base dos Dados em 2018-2021; fora dessa
#    janela vem vazio/NA mesmo quando a conta é claramente identificável
#    pelo nome. O texto de cada conta é estável dentro de cada "era" de
#    portaria, variando apenas entre eras (cada variável listada abaixo
#    cobre todas as variantes observadas).
#  - Estágio usado: "Receitas Brutas Realizadas" (não "Receitas Realizadas",
#    que é um rótulo exclusivo de 2013, inexistente nos anos seguintes). A
#    receita líquida é derivada subtraindo as deduções manualmente.
#  - Linhas intraorçamentárias (transferências internas entre órgãos do
#    mesmo município) excluídas: a partir de 2019 passaram a duplicar
#    "Receitas/Transferências Correntes" sob o mesmo texto de conta, com
#    portaria/id_conta_bd iniciando em "7" em vez de "1".
#  - IVVC (imposto extinto em 1996) e "Adicional ISS - Fundo Municipal de
#    Combate à Pobreza" (classificado pela fonte como DEDUÇÃO, não receita)
#    ficam de fora por não constarem no dicionário de contas abaixo.

raw_receitas_path <- file.path(PASTA_FINANCAS, "SICONFI", "receitas_orcamentarias_raw.rds")
FORCAR_ATUALIZACAO_RECEITAS <- FALSE  # TRUE só para forçar nova extração do BigQuery

if (file.exists(raw_receitas_path) && !FORCAR_ATUALIZACAO_RECEITAS) {
  receitas_orcamentarias <- readRDS(raw_receitas_path)
} else {
  query_receitas <- "
  SELECT
      dados.ano as ano, dados.sigla_uf AS sigla_uf, dados.id_municipio AS id_municipio,
      dados.estagio as estagio, dados.portaria as portaria, dados.conta as conta,
      dados.estagio_bd as estagio_bd, dados.id_conta_bd as id_conta_bd,
      dados.conta_bd as conta_bd, dados.valor as valor
  FROM `basedosdados.br_me_siconfi.municipio_receitas_orcamentarias` AS dados
  "
  receitas_orcamentarias <- read_sql(query_receitas, billing_project_id = get_billing_id())
  dir.create(dirname(raw_receitas_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(receitas_orcamentarias, raw_receitas_path)
}

receitas_orcamentarias <- receitas_orcamentarias %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano)) %>%
  filter(ano >= 2013)

ESTAGIO_RECEITA_BRUTO <- "Receitas Brutas Realizadas"

# Dicionário por texto exato de conta — cada variável pode ter múltiplas
# variantes textuais ao longo dos anos, conforme a portaria vigente.
contas_receita <- tribble(
  ~variavel,                  ~conta,
  "receita_total",            "Rec Orçamentária",
  "receita_total",            "Total Receitas",
  "receita_total",            "RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)",
  "receita_corrente",         "Rec Correntes",
  "receita_corrente",         "Receitas Correntes",
  "transferencias_correntes", "Rec Transf Correntes",
  "transferencias_correntes", "Transferências Correntes",
  "iptu",                     "IPTU",
  "iptu",                     "Imposto sobre a Propriedade Predial e Territorial Urbana",
  "iptu",                     "Imposto sobre a Propriedade Predial e Territorial Urbana - IPTU",
  "iss",                      "ISS",
  "iss",                      "ISSQN",
  "iss",                      "Imposto sobre Serviços de Qualquer Natureza",
  "iss",                      "Imposto sobre Serviços de Qualquer Natureza - ISSQN",
  "itbi",                     "ITBI",
  "itbi",                     'Imposto sobre Transmissão "Inter Vivos" de Bens Imóveis e de Direitos Reais sobre Imóveis - ITBI',
  "itbi",                     "Imposto sobre Transmissão -Inter Vivos- de Bens Imóveis e de Direitos Reais sobre Imóveis",
  "itbi",                     'Impostos sobre Transmissão "Inter Vivos" de Bens Imóveis e de Direitos Reais sobre Imóveis'
)

# Intraorçamentária: id_conta_bd ou portaria iniciando em "7" em vez de "1"
marcar_intra <- function(df) {
  df %>% mutate(eh_intra = coalesce(str_detect(id_conta_bd, "^1\\.7\\."), FALSE) |
                  coalesce(str_starts(portaria, "7\\."), FALSE))
}

receitas_extraidas <- receitas_orcamentarias %>%
  filter(estagio == ESTAGIO_RECEITA_BRUTO) %>%
  marcar_intra() %>%
  filter(!eh_intra) %>%
  inner_join(contas_receita, by = "conta") %>%
  select(id_municipio, ano, variavel, valor)

dup <- receitas_extraidas %>% count(id_municipio, ano, variavel) %>% filter(n > 1)
if (nrow(dup) > 0) warning(nrow(dup), " duplicatas em receitas_extraidas — investigar antes de prosseguir")

receitas_wide <- receitas_extraidas %>%
  pivot_wider(names_from = variavel, values_from = valor)

vars_faltantes <- setdiff(unique(contas_receita$variavel), names(receitas_wide))
if (length(vars_faltantes) > 0) {
  receitas_wide <- receitas_wide %>%
    bind_cols(vars_faltantes %>% set_names() %>% map_dfc(~ NA_real_))
}

# Deduções da receita bruta. "Deduções da Receita" (rótulo usado só em 2013)
# e "Outras Deduções da Receita" (2014+) são tratadas como equivalentes.
# FUNDEB não existe como estágio próprio em 2013 (primeiro ano do SICONFI) —
# NA nesse ano é estrutural, não dado faltante.
deducoes <- receitas_orcamentarias %>%
  filter(estagio %in% c("Deduções - FUNDEB", "Deduções - Transferências Constitucionais",
                        "Deduções da Receita", "Outras Deduções da Receita"),
         conta %in% contas_receita$conta[contas_receita$variavel == "receita_total"]) %>%
  marcar_intra() %>%
  filter(!eh_intra) %>%
  mutate(estagio_slug = case_when(
    estagio == "Deduções - FUNDEB" ~ "deducao_fundeb",
    estagio == "Deduções - Transferências Constitucionais" ~ "deducao_transf_constitucional",
    estagio %in% c("Deduções da Receita", "Outras Deduções da Receita") ~ "deducao_outras"
  )) %>%
  select(id_municipio, ano, estagio_slug, valor)

dup_ded <- deducoes %>% count(id_municipio, ano, estagio_slug) %>% filter(n > 1)
if (nrow(dup_ded) > 0) warning(nrow(dup_ded), " duplicatas em deducoes — investigar")

deducoes_wide <- deducoes %>%
  group_by(id_municipio, ano, estagio_slug) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = estagio_slug, values_from = valor)

LIMIAR_RECEITA_MINIMA <- 10000  # R$10 mil/ano — abaixo disso, denominador tratado
# como inválido (erro de lançamento na fonte, ex.
# receita_total = R$1; sem o piso a divisão gera
# proporções astronômicas / Inf)

receitas_painel <- receitas_wide %>%
  left_join(deducoes_wide, by = c("id_municipio", "ano")) %>%
  mutate(
    across(starts_with("deducao_"), ~ coalesce(., 0)),  # ausência de dedução = 0, não NA propagante
    receita_total_liquida = receita_total - deducao_fundeb - deducao_transf_constitucional - deducao_outras,
    impostos_proprios      = iptu + itbi + iss,           # soma das folhas específicas; NÃO usa o nó
    # agregador "Impostos", que inclui o IVVC
    receita_propria        = receita_corrente - transferencias_correntes,

    pct_impostos_proprios_total = if_else(receita_total_liquida > LIMIAR_RECEITA_MINIMA,
                                          impostos_proprios / receita_total_liquida, NA_real_),
    pct_receita_propria_total   = if_else(receita_total_liquida > LIMIAR_RECEITA_MINIMA,
                                          receita_propria   / receita_total_liquida, NA_real_),
    pct_iptu_total               = if_else(receita_total_liquida > LIMIAR_RECEITA_MINIMA,
                                           iptu              / receita_total_liquida, NA_real_),
    pct_iptu_receita_propria     = if_else(receita_propria    > LIMIAR_RECEITA_MINIMA,
                                           iptu              / receita_propria,       NA_real_),
    pct_iptu_impostos_proprios   = if_else(impostos_proprios  > LIMIAR_RECEITA_MINIMA,
                                           iptu              / impostos_proprios,     NA_real_),
    pct_fundeb_total              = if_else(receita_total     > LIMIAR_RECEITA_MINIMA,
                                            deducao_fundeb    / receita_total,          NA_real_)
  )


# ══════════════════════════════════════════════════════════════════════════
# 6. SICONFI — DESPESAS POR FUNÇÃO
# ══════════════════════════════════════════════════════════════════════════
#
# Decisões de modelagem:
#  - `conta`/`conta_bd` descrevem SUBFUNÇÃO, não função — a mesma subfunção
#    (ex. "Administração Geral") se repete sob quase todas as 28 funções,
#    por ser uma categoria administrativa transversal. O código de função
#    vem de `portaria` (padrão FUNÇÃO.SUBFUNÇÃO; sem ponto = linha de TOTAL
#    da função).
#  - Classificação usa a linha de TOTAL por função (portaria sem ponto), não
#    a soma das subfunções: a categoria residual "FUxx - Demais Subfunções"
#    não carrega código de função detectável e ficaria de fora de uma soma
#    por subfunção (gap de até 90% em funções como Organização Agrária),
#    mas já está corretamente embutida na linha de total.
#  - despesa_total: 3 variantes textuais sem sobreposição temporal —
#    "Despesas (Exceto Intra-Orçamentárias)" / "Despesas (Exceto
#    Intraorçamentárias)" em 2013-2016, "Despesas Exceto Intraorçamentárias"
#    em 2017-2025.
#  - NA de uma função vira 0 somente quando o total do MESMO estágio está
#    presente (classificação por função é obrigatória e exaustiva por lei —
#    ausência de linha = execução zero). Quando o total também é NA, o
#    município não reportou naquele ano/estágio e a célula permanece NA.

raw_despesas_path <- file.path(PASTA_FINANCAS, "SICONFI", "despesas_funcao_raw.rds")
FORCAR_ATUALIZACAO_DESPESAS <- FALSE

if (file.exists(raw_despesas_path) && !FORCAR_ATUALIZACAO_DESPESAS) {
  despesas <- readRDS(raw_despesas_path)
} else {
  despesas <- basedosdados::bdplyr("basedosdados.br_me_siconfi.municipio_despesas_funcao") %>%
    dplyr::filter(ano >= 2013) %>%
    dplyr::select(ano, sigla_uf, id_municipio, estagio, portaria, conta,
                  estagio_bd, id_conta_bd, conta_bd, valor) %>%
    basedosdados::bd_collect()
  dir.create(dirname(raw_despesas_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(despesas, raw_despesas_path)
}

despesas_gerais <- despesas %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano)) %>%
  filter(estagio %in% c("Despesas Empenhadas", "Despesas Liquidadas", "Despesas Pagas")) %>%
  mutate(
    tem_subfuncao = str_detect(portaria, "\\."),
    codigo_funcao = str_extract(portaria, "^\\d+") %>% str_pad(2, pad = "0")
  )

# Código -> nome de função, conforme Portaria MOG nº 42/1999
dicionario_funcao <- tribble(
  ~codigo_funcao, ~funcao,
  "01","Legislativa", "02","Judiciária", "03","Essencial à Justiça", "04","Administração",
  "05","Defesa Nacional", "06","Segurança Pública", "07","Relações Exteriores",
  "08","Assistência Social", "09","Previdência Social", "10","Saúde", "11","Trabalho",
  "12","Educação", "13","Cultura", "14","Direitos da Cidadania", "15","Urbanismo",
  "16","Habitação", "17","Saneamento", "18","Gestão Ambiental", "19","Ciência e Tecnologia",
  "20","Agricultura", "21","Organização Agrária", "22","Indústria", "23","Comércio e Serviços",
  "24","Comunicações", "25","Energia", "26","Transporte", "27","Desporto e Lazer",
  "28","Encargos Especiais"
) %>%
  mutate(funcao_slug = funcao %>% stringi::stri_trans_general("Latin-ASCII") %>%
           str_to_lower() %>% str_replace_all("[^a-z0-9]+", "_") %>% str_replace_all("^_|_$", ""))

estagio_slug <- c("Despesas Empenhadas" = "empenhadas",
                  "Despesas Liquidadas" = "liquidadas",
                  "Despesas Pagas"      = "pagas")

despesas_classificadas <- despesas_gerais %>%
  filter(!tem_subfuncao) %>%
  inner_join(dicionario_funcao, by = "codigo_funcao") %>%  # intra + "Demais Subfunções" (código NA) caem fora aqui
  select(id_municipio, ano, estagio, funcao_slug, valor)

despesas_funcao_wide <- despesas_classificadas %>%
  mutate(estagio_slug = estagio_slug[estagio],
         coluna = paste0("despesas_", estagio_slug, "_", funcao_slug)) %>%
  group_by(id_municipio, ano, coluna) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = coluna, values_from = valor)

despesas_total_wide <- despesas_gerais %>%
  filter(conta %in% c("Despesas Exceto Intraorçamentárias",
                      "Despesas (Exceto Intra-Orçamentárias)",
                      "Despesas (Exceto Intraorçamentárias)")) %>%
  {
    dup <- count(., id_municipio, ano, estagio) %>% filter(n > 1)
    if (nrow(dup) > 0) warning(nrow(dup), " duplicatas entre variantes de despesas_total — investigar")
    .
  } %>%
  mutate(estagio_slug = estagio_slug[estagio],
         coluna = paste0("despesas_", estagio_slug, "_total")) %>%
  group_by(id_municipio, ano, coluna) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = coluna, values_from = valor)

despesas_painel <- despesas_funcao_wide %>%
  full_join(despesas_total_wide, by = c("id_municipio", "ano"))

colunas_funcao_pagas      <- grep("^despesas_pagas_",      names(despesas_painel), value = TRUE) %>% setdiff("despesas_pagas_total")
colunas_funcao_liquidadas <- grep("^despesas_liquidadas_", names(despesas_painel), value = TRUE) %>% setdiff("despesas_liquidadas_total")
colunas_funcao_empenhadas <- grep("^despesas_empenhadas_", names(despesas_painel), value = TRUE) %>% setdiff("despesas_empenhadas_total")

despesas_painel <- despesas_painel %>%
  mutate(
    across(all_of(colunas_funcao_pagas),      ~ if_else(!is.na(despesas_pagas_total)      & is.na(.), 0, .)),
    across(all_of(colunas_funcao_liquidadas), ~ if_else(!is.na(despesas_liquidadas_total) & is.na(.), 0, .)),
    across(all_of(colunas_funcao_empenhadas), ~ if_else(!is.na(despesas_empenhadas_total) & is.na(.), 0, .))
  )

LIMIAR_DESPESA_MINIMA <- 10000  # mesmo piso de plausibilidade usado em receitas

calcular_pct_despesas <- function(df, colunas_funcao, col_total) {
  df %>%
    mutate(across(
      all_of(colunas_funcao),
      ~ if_else(.data[[col_total]] > LIMIAR_DESPESA_MINIMA, . / .data[[col_total]], NA_real_),
      .names = "pct_{.col}"
    ))
}

despesas_painel <- despesas_painel %>%
  calcular_pct_despesas(colunas_funcao_pagas,      "despesas_pagas_total") %>%
  calcular_pct_despesas(colunas_funcao_liquidadas, "despesas_liquidadas_total") %>%
  calcular_pct_despesas(colunas_funcao_empenhadas, "despesas_empenhadas_total")


# ══════════════════════════════════════════════════════════════════════════
# 6.5. CONVÊNIOS DA UNIÃO — Portal da Transparência (CGU)
# ══════════════════════════════════════════════════════════════════════════
# Pipeline próprio (convenios_uniao/convenios_uniao.R), com seu próprio
# billing project de coleta — por isso não é refeito aqui, só carregado do
# .RData já exportado. Integrado à dimensão em 19/11/2026 (antes só tinha o
# pipeline isolado, sem entrar em financas_brasileiras.xlsx — ver
# mape_municipios METADADOS.xlsx, aba "Geral"). Escopo 1997-2025, mais amplo
# que as demais fontes desta dimensão (que começam em 2013): os anos
# 1997-2012 entram no painel só com as colunas de convênios preenchidas, as
# demais ficam NA (fora do escopo de cada fonte, mesmo padrão já usado para
# FIRJAN/Tesouro/Emendas acima).
load(file.path(PASTA_FINANCAS, "convenios_uniao", "convenios_uniao.RData"))
convenios_uniao_final <- convenios_uniao_final %>%
  mutate(id_municipio = as.character(id_municipio), ano = as.integer(ano))


# ══════════════════════════════════════════════════════════════════════════
# 7. INTEGRAÇÃO FINAL — emendas + convênios + receitas + FIRJAN + Tesouro +
#    despesas
# ══════════════════════════════════════════════════════════════════════════
# [UNICO] `emendas_parlamentares` (Etapa 2) entra aqui por id_municipio x ano,
# como o próprio script de Emendas já previa em suas "Próximas etapas".

emendas_para_juntar <- emendas_parlamentares %>%
  select(-id_municipio_nome, -sigla_uf)  # já vêm de `firjan`/`municipios` via as outras bases

financas_brasileiras <- full_join(receitas_painel, firjan, by = c("ano", "id_municipio")) %>%
  left_join(tesouro, by = c("ano", "id_municipio")) %>%
  full_join(despesas_painel, by = c("ano", "id_municipio")) %>%
  full_join(emendas_para_juntar, by = c("ano", "id_municipio")) %>%
  full_join(convenios_uniao_final, by = c("ano", "id_municipio"))

# Checagem de granularidade: cada id_municipio-ano deve ser único no painel final
n_dup_final <- financas_brasileiras %>% count(id_municipio, ano) %>% filter(n > 1) %>% nrow()
if (n_dup_final > 0) warning(n_dup_final, " município-ano duplicados no painel final — investigar antes de exportar")

names(financas_brasileiras)
dim(financas_brasileiras)

summary(financas_brasileiras)

# ══════════════════════════════════════════════════════════════════════════
# 8. EXPORTAÇÃO
# ══════════════════════════════════════════════════════════════════════════

dir.create(PASTA_FINANCAS, showWarnings = FALSE, recursive = TRUE)

write.xlsx(
  financas_brasileiras,
  file      = file.path(PASTA_FINANCAS, "financas_brasileiras.xlsx"),
  rowNames  = FALSE,
  overwrite = TRUE
)

# ── Validação padronizada (2 Validação/10 Finanças/) ─────────────────────────
validar_dimensao(financas_brasileiras, n_dim = 10)


# ══════════════════════════════════════════════════════════════════════════
# 9. ANÁLISE DESCRITIVA (gráficos e mapas)
# ══════════════════════════════════════════════════════════════════════════
# Copiado de "10 Finanças análise gráficos.R" (script apagado depois da
# junção). Distribuição temporal (Brasil, região, UF) e espacial (mapas
# municipais) da capacidade fiscal (IPTU, IFGF) e da composição das despesas
# por função. Lê de volta o arquivo exportado na Etapa 8 (como o script
# original já fazia, de forma independente da Etapa 7 em memória) — assim o
# que está no gráfico é sempre o que está no arquivo final, não um objeto
# intermediário que podia ter sido alterado entre as etapas.
# [UNICO] PASTA_FINANCAS e os pacotes já vêm das Etapas 1 e 7/8; esta seção
# usa a mesma PASTA_FINANCAS, sem redefini-la.

PASTA_GRAFICOS <- file.path(PASTA_FINANCAS, "graficos")
walk(file.path(PASTA_GRAFICOS, c("temporal_brasil", "temporal_regiao", "temporal_uf", "mapas")),
     dir.create, showWarnings = FALSE, recursive = TRUE)

ANOS_MAPA   <- c(2013, 2017, 2021, 2024)
GERAR_MAPAS <- TRUE  # os 32 mapas municipais levam ~20 min; FALSE para rodar só o resto


# ── 9.1 Paleta e tema ─────────────────────────────────────────────────────
# Sequencial azul (magnitude/média) e laranja (desigualdade/Gini); categórica
# em ordem fixa para as 5 regiões.
AZUL    <- "#2a78d6"
LARANJA <- "#eb6834"
RAMPA_AZUL    <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
RAMPA_LARANJA <- c("#fde3d6", "#f9b99a", "#f28c62", "#eb6834", "#c94f1f", "#9a3a14", "#6b260b")
CORES_REGIAO  <- c("Norte" = "#2a78d6", "Nordeste" = "#eb6834", "Sudeste" = "#1baf7a",
                   "Sul" = "#eda100", "Centro-Oeste" = "#e87ba4")
COR_SEM_DADOS <- "#e4e3df"

tema_base <- theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ebeae6", linewidth = 0.3),
    axis.text          = element_text(color = "#6b6a63"),
    strip.text         = element_text(face = "bold", hjust = 0),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(color = "#6b6a63"),
    plot.caption       = element_text(color = "#8a8980", hjust = 0),
    legend.position    = "bottom"
  )
theme_set(tema_base)


# ── 9.2 Variáveis e rótulos ───────────────────────────────────────────────
rotulos_funcao <- c(
  legislativa = "Legislativa", judiciaria = "Judiciária",
  essencial_a_justica = "Essencial à Justiça", administracao = "Administração",
  defesa_nacional = "Defesa Nacional", seguranca_publica = "Segurança Pública",
  relacoes_exteriores = "Relações Exteriores", assistencia_social = "Assistência Social",
  previdencia_social = "Previdência Social", saude = "Saúde", trabalho = "Trabalho",
  educacao = "Educação", cultura = "Cultura", direitos_da_cidadania = "Direitos da Cidadania",
  urbanismo = "Urbanismo", habitacao = "Habitação", saneamento = "Saneamento",
  gestao_ambiental = "Gestão Ambiental", ciencia_e_tecnologia = "Ciência e Tecnologia",
  agricultura = "Agricultura", organizacao_agraria = "Organização Agrária",
  industria = "Indústria", comercio_e_servicos = "Comércio e Serviços",
  comunicacoes = "Comunicações", energia = "Energia", transporte = "Transporte",
  desporto_e_lazer = "Desporto e Lazer", encargos_especiais = "Encargos Especiais"
)

dicionario_vars <- bind_rows(
  tibble(
    variavel = c("pct_iptu_total", "pct_iptu_receita_propria", "ifgf_geral", "ifgf_autonomia"),
    rotulo   = c("IPTU / receita total", "IPTU / receita própria",
                 "IFGF Geral", "IFGF Autonomia"),
    bloco    = "fiscal",
    formato  = c("pct", "pct", "indice", "indice")
  ),
  tibble(
    variavel = paste0("pct_despesas_liquidadas_", names(rotulos_funcao)),
    rotulo   = unname(rotulos_funcao),
    bloco    = "despesas",
    formato  = "pct"
  )
) %>%
  mutate(rotulo = factor(rotulo, levels = rotulo))

# Grupos para os gráficos temporais com eixo y fixo dentro de cada figura:
# variáveis de magnitude parecida no mesmo painel, para que a comparação entre
# elas seja visual. Despesas agrupadas pela participação média 2013–2025.
grupos_despesa <- list(
  "1_principais"    = c("educacao", "saude", "administracao", "urbanismo"),
  "2_intermediarias" = c("assistencia_social", "legislativa", "previdencia_social", "transporte",
                         "encargos_especiais", "agricultura", "saneamento", "cultura"),
  "3_pequenas"      = c("desporto_e_lazer", "gestao_ambiental", "energia", "seguranca_publica",
                        "comercio_e_servicos", "judiciaria", "habitacao", "trabalho"),
  "4_residuais"     = c("industria", "essencial_a_justica", "direitos_da_cidadania", "comunicacoes",
                        "ciencia_e_tecnologia", "defesa_nacional", "organizacao_agraria",
                        "relacoes_exteriores")
)
titulos_grupo <- c(
  iptu = "IPTU", ifgf = "IFGF",
  despesas_1_principais     = "Despesas por função — principais (≥ 8% em média)",
  despesas_2_intermediarias = "Despesas por função — intermediárias (1–4% em média)",
  despesas_3_pequenas       = "Despesas por função — pequenas (0,1–1% em média)",
  despesas_4_residuais      = "Despesas por função — residuais (< 0,1% em média)"
)

grupo_por_slug <- setNames(rep(names(grupos_despesa), lengths(grupos_despesa)),
                           unlist(grupos_despesa))

dicionario_vars <- dicionario_vars %>%
  mutate(
    slug  = str_remove(variavel, "^pct_despesas_liquidadas_"),
    grupo = case_when(
      str_detect(variavel, "iptu") ~ "iptu",
      str_detect(variavel, "ifgf") ~ "ifgf",
      TRUE ~ paste0("despesas_", grupo_por_slug[slug])
    )
  ) %>%
  # Ordem dos painéis = ordem de magnitude dentro do grupo
  arrange(match(slug, unlist(grupos_despesa))) %>%
  mutate(rotulo = fct_inorder(as.character(rotulo))) %>%
  select(-slug)

VARS <- dicionario_vars$variavel

formatar <- function(formato) {
  if (formato == "pct") scales::label_percent(accuracy = 0.1) else scales::label_number(accuracy = 0.01)
}

# UF a partir dos 2 primeiros dígitos do código IBGE (a coluna `sigla` vem do
# Tesouro e é NA para município-ano fora do ranking)
ufs <- tribble(
  ~cod_uf, ~uf,  ~regiao,
  "11", "RO", "Norte", "12", "AC", "Norte", "13", "AM", "Norte", "14", "RR", "Norte",
  "15", "PA", "Norte", "16", "AP", "Norte", "17", "TO", "Norte",
  "21", "MA", "Nordeste", "22", "PI", "Nordeste", "23", "CE", "Nordeste",
  "24", "RN", "Nordeste", "25", "PB", "Nordeste", "26", "PE", "Nordeste",
  "27", "AL", "Nordeste", "28", "SE", "Nordeste", "29", "BA", "Nordeste",
  "31", "MG", "Sudeste", "32", "ES", "Sudeste", "33", "RJ", "Sudeste", "35", "SP", "Sudeste",
  "41", "PR", "Sul", "42", "SC", "Sul", "43", "RS", "Sul",
  "50", "MS", "Centro-Oeste", "51", "MT", "Centro-Oeste", "52", "GO", "Centro-Oeste",
  "53", "DF", "Centro-Oeste"
) %>%
  mutate(regiao = factor(regiao, levels = names(CORES_REGIAO)),
         uf     = factor(uf, levels = uf))


# ── 9.3 Dados ─────────────────────────────────────────────────────────────
financas <- read_excel(file.path(PASTA_FINANCAS, "financas_brasileiras.xlsx"),
                       guess_max = 100000) %>%
  select(id_municipio, ano, all_of(VARS)) %>%
  mutate(id_municipio = as.character(id_municipio),
         ano          = as.integer(ano),
         cod_uf       = str_sub(id_municipio, 1, 2)) %>%
  left_join(ufs, by = "cod_uf")

# Todas as variáveis são razões (participações em [0, 1]) ou índices em [0, 1].
# Valores fora desse intervalo (ex.: IPTU negativo por estorno/dedução maior que
# a arrecadação, ou receita própria menor que o IPTU) são inconsistências de
# fonte: viram NA para não distorcer média e Gini (que exige valores >= 0).
fora_intervalo <- financas %>%
  summarise(across(all_of(VARS), ~ sum(. < 0 | . > 1, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "n_fora_0_1") %>%
  filter(n_fora_0_1 > 0)
print(fora_intervalo)

financas <- financas %>%
  mutate(across(all_of(VARS), ~ if_else(. < 0 | . > 1, NA_real_, .)))

financas_long <- financas %>%
  pivot_longer(all_of(VARS), names_to = "variavel", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  left_join(dicionario_vars, by = "variavel")


# ── 9.4 Estatísticas: média e Gini entre municípios ───────────────────────
# Média simples (cada município pesa 1) e Gini da distribuição entre
# municípios. Gini exige pelo menos 2 valores e soma > 0.
gini_seguro <- function(x) if (length(x) < 2 || sum(x) == 0) NA_real_ else ineq::Gini(x)

resumir <- function(df, ...) {
  df %>%
    group_by(variavel, rotulo, bloco, grupo, formato, ano, ...) %>%
    summarise(
      n     = n(),
      media = mean(valor),
      p25   = quantile(valor, 0.25),
      p75   = quantile(valor, 0.75),
      gini  = gini_seguro(valor),
      .groups = "drop"
    )
}

stats_brasil <- resumir(financas_long)
stats_regiao <- resumir(financas_long, regiao)
stats_uf     <- resumir(financas_long, regiao, uf)

write.xlsx(
  list(brasil = stats_brasil, regiao = stats_regiao, uf = stats_uf,
       valores_fora_0_1 = fora_intervalo),
  file.path(PASTA_GRAFICOS, "estatisticas_media_gini.xlsx"),
  overwrite = TRUE
)


# ── 9.5 Evolução temporal — Brasil ────────────────────────────────────────
NOTA_FONTE <- "Fonte: SICONFI/Tesouro Nacional e FIRJAN (IFGF). Média simples entre municípios; Gini da distribuição entre municípios."

salvar <- function(plot, nome, pasta, largura = 11, altura = 7) {
  ggsave(file.path(PASTA_GRAFICOS, pasta, paste0(nome, ".png")), plot,
         width = largura, height = altura, dpi = 200, bg = "white")
}

# Uma figura por grupo de variáveis, com eixo y FIXO entre os painéis para que
# os níveis sejam comparáveis. Médias partem de zero; o Gini usa a faixa
# observada no grupo (fixa entre painéis), para não achatar a tendência.
rotulo_eixo <- function(d, metrica) {
  if (metrica != "gini" && all(d$formato == "pct")) scales::label_percent() else scales::label_number(accuracy = 0.01)
}
dimensoes_grupo <- function(n_vars) {
  if (n_vars <= 2) list(ncol = 2, largura = 9,  altura = 4.5)
  else if (n_vars <= 4) list(ncol = 4, largura = 13, altura = 4.5)
  else list(ncol = 4, largura = 13, altura = 7.5)
}

grafico_brasil <- function(grupo_sel, metrica) {
  d   <- filter(stats_brasil, grupo == grupo_sel)
  dim <- dimensoes_grupo(n_distinct(d$variavel))
  cor <- if (metrica == "media") AZUL else LARANJA

  p <- ggplot(d, aes(ano, .data[[metrica]]))
  if (metrica == "media") {
    p <- p + geom_ribbon(aes(ymin = p25, ymax = p75), fill = AZUL, alpha = 0.15) +
      expand_limits(y = 0)
  }
  p <- p +
    geom_line(color = cor, linewidth = 0.8) +
    geom_point(color = cor, size = 1.2) +
    facet_wrap(~ rotulo, ncol = dim$ncol) +
    scale_x_continuous(breaks = seq(2013, 2025, 4), limits = c(2013, 2025)) +
    scale_y_continuous(labels = rotulo_eixo(d, metrica)) +
    labs(
      title = paste(titulos_grupo[[grupo_sel]],
                    if (metrica == "media") "— média entre municípios" else "— desigualdade entre municípios (Gini)"),
      subtitle = if (metrica == "media") "Linha: média · Faixa: intervalo interquartil (p25–p75) · Eixo y comum aos painéis"
                 else "0 = todos os municípios iguais · 1 = concentração máxima · Eixo y comum aos painéis",
      x = NULL, y = NULL, caption = NOTA_FONTE
    )

  salvar(p, paste0(grupo_sel, "_", metrica), "temporal_brasil", dim$largura, dim$altura)
}

GRUPOS <- unique(dicionario_vars$grupo)
walk(GRUPOS, \(g) walk(c("media", "gini"), \(m) grafico_brasil(g, m)))


# ── 9.6 Evolução temporal — regiões ───────────────────────────────────────
grafico_regiao <- function(grupo_sel, metrica) {
  d   <- filter(stats_regiao, grupo == grupo_sel)
  dim <- dimensoes_grupo(n_distinct(d$variavel))

  p <- ggplot(d, aes(ano, .data[[metrica]], color = regiao)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1) +
    facet_wrap(~ rotulo, ncol = dim$ncol) +
    scale_color_manual(values = CORES_REGIAO, name = NULL) +
    scale_x_continuous(breaks = seq(2013, 2025, 4), limits = c(2013, 2025)) +
    scale_y_continuous(labels = rotulo_eixo(d, metrica)) +
    labs(title = paste(titulos_grupo[[grupo_sel]],
                       if (metrica == "media") "— média por região" else "— Gini por região"),
         subtitle = "Eixo y comum aos painéis",
         x = NULL, y = NULL, caption = NOTA_FONTE)
  if (metrica == "media") p <- p + expand_limits(y = 0)

  salvar(p, paste0(grupo_sel, "_", metrica, "_regiao"), "temporal_regiao",
         dim$largura, dim$altura + 0.5)
}

walk(GRUPOS, \(g) walk(c("media", "gini"), \(m) grafico_regiao(g, m)))


# ── 9.7 Evolução temporal — UF (heatmaps UF x ano) ────────────────────────
# Um arquivo por variável: média (azul) e Gini (laranja) lado a lado, UFs
# agrupadas por região.
heatmap_uf <- function(var) {
  info <- filter(dicionario_vars, variavel == var)
  d    <- filter(stats_uf, variavel == var)

  base_heat <- function(metrica, rampa, nome_legenda, rotulos) {
    ggplot(d, aes(ano, fct_rev(uf), fill = .data[[metrica]])) +
      geom_tile(color = "white", linewidth = 0.6) +
      facet_grid(regiao ~ ., scales = "free_y", space = "free_y", switch = "y") +
      scale_fill_gradientn(colors = rampa, labels = rotulos, name = nome_legenda,
                           na.value = COR_SEM_DADOS) +
      scale_x_continuous(breaks = 2013:2025, expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      theme(panel.grid = element_blank(),
            strip.placement = "outside",
            strip.text.y.left = element_text(angle = 0, face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = 0.5),
            legend.key.width = unit(1.6, "cm"))
  }

  (base_heat("media", RAMPA_AZUL, "Média", formatar(info$formato)) +
     base_heat("gini", RAMPA_LARANJA, "Gini", scales::label_number(accuracy = 0.01))) +
    plot_annotation(
      title    = paste0(info$rotulo, " — média e Gini por UF"),
      subtitle = var,
      caption  = NOTA_FONTE
    )
}

walk(VARS, \(v) salvar(heatmap_uf(v), v, "temporal_uf", 12, 8))


# ── 9.8 Mapas municipais (2013, 2017, 2021, 2024) ─────────────────────────
# Malhas do geobr em cache local para não baixar a cada execução.
cache_geo <- file.path(PASTA_GRAFICOS, "malhas_geobr.rds")
if (!file.exists(cache_geo)) {
  malhas <- list(
    municipios = read_municipality(year = 2022, showProgress = FALSE),
    estados    = read_state(year = 2020, showProgress = FALSE)
  )
  saveRDS(malhas, cache_geo)
}
malhas <- readRDS(cache_geo)
municipios_sf <- malhas$municipios %>%
  transmute(id_municipio = as.character(code_muni))
estados_sf <- malhas$estados

# Classes por quantis, comuns aos 4 anos, para que as cores sejam comparáveis
# no tempo. Se houver muitos zeros (funções raras como Defesa Nacional), zero
# vira uma classe própria e os quantis são calculados sobre os positivos.
classificar <- function(x, formato, n_classes = 6) {
  fmt <- formatar(formato)
  tem_zero <- mean(x == 0, na.rm = TRUE) > 0.05
  base <- if (tem_zero) x[!is.na(x) & x > 0] else x[!is.na(x)]
  quebras <- unique(quantile(base, probs = seq(0, 1, length.out = n_classes + 1), names = FALSE))
  quebras[1] <- if (tem_zero) 0 else quebras[1]
  rotulos <- make.unique(paste(fmt(head(quebras, -1)), "–", fmt(quebras[-1])), sep = " ")
  classes <- cut(x, quebras, labels = rotulos, include.lowest = !tem_zero)
  if (tem_zero) {
    classes <- fct_expand(classes, "0") %>% fct_relevel("0")
    classes[!is.na(x) & x == 0] <- "0"
  }
  classes
}

mapa_variavel <- function(var) {
  info <- filter(dicionario_vars, variavel == var)
  d <- financas %>%
    filter(ano %in% ANOS_MAPA) %>%
    select(id_municipio, ano, valor = all_of(var)) %>%
    mutate(classe = classificar(valor, info$formato))

  # Grade completa município x ano: municípios sem dado aparecem em cinza e
  # anos sem nenhuma informação (ex.: IPTU em 2013) aparecem como painel cinza
  dados_mapa <- municipios_sf %>%
    cross_join(tibble(ano = ANOS_MAPA)) %>%
    left_join(d, by = c("id_municipio", "ano"))

  n_classes <- nlevels(d$classe)
  cores <- colorRampPalette(RAMPA_AZUL)(n_classes)
  anos_sem_dado <- d %>% group_by(ano) %>% summarise(ok = any(!is.na(valor))) %>% filter(!ok) %>% pull(ano)
  subtitulo <- "Classes por quantis, comuns aos quatro anos"
  if (length(anos_sem_dado) > 0) subtitulo <- paste0(subtitulo, " · Sem dados em ", paste(anos_sem_dado, collapse = ", "))

  ggplot(dados_mapa) +
    geom_sf(aes(fill = classe), color = NA) +
    geom_sf(data = estados_sf, fill = NA, color = "white", linewidth = 0.25) +
    facet_wrap(~ ano, ncol = 2) +
    scale_fill_manual(values = cores, na.value = COR_SEM_DADOS, drop = FALSE,
                      name = NULL, na.translate = TRUE,
                      labels = \(x) if_else(is.na(x), "Sem dados", x)) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(color = NA))) +
    labs(title = info$rotulo, subtitle = paste0(var, " · ", subtitulo),
         caption = "Fonte: SICONFI/Tesouro Nacional e FIRJAN (IFGF). Malha municipal: IBGE via geobr.") +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "#6b6a63"),
          plot.caption = element_text(color = "#8a8980", hjust = 0),
          strip.text = element_text(face = "bold", size = 11),
          legend.position = "bottom",
          plot.margin = margin(10, 10, 10, 10))
}

if (GERAR_MAPAS) {
  walk(VARS, \(v) {
    message("Mapa: ", v)
    salvar(mapa_variavel(v), v, "mapas", 10, 10.5)
  })
}


# ══════════════════════════════════════════════════════════════════════════
# 10. Principal despesa: função com maior participação em cada município-ano
# ══════════════════════════════════════════════════════════════════════════
# Educação, Saúde e Administração cobrem praticamente todos os casos; as
# demais funções são agrupadas em "Outras".
CORES_PRINCIPAL <- c("Educação" = "#2a78d6", "Saúde" = "#eb6834",
                     "Administração" = "#1baf7a", "Outras" = "#a3a29a")

principal <- financas_long %>%
  filter(bloco == "despesas") %>%
  group_by(id_municipio, ano, regiao, uf) %>%
  slice_max(valor, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(principal = fct_other(as.character(rotulo), keep = names(CORES_PRINCIPAL)[1:3],
                               other_level = "Outras") %>%
           fct_relevel(names(CORES_PRINCIPAL)))

contar_principal <- function(df, ...) {
  df %>%
    count(ano, ..., principal, .drop = FALSE) %>%
    group_by(ano, ...) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    filter(!is.nan(pct))  # .drop = FALSE cria combinações região x UF inexistentes
}
principal_brasil <- contar_principal(principal)
principal_regiao <- contar_principal(principal, regiao)
principal_uf     <- contar_principal(principal, regiao, uf)

# Composição média do orçamento: 4 maiores funções + demais
composicao <- stats_brasil %>%
  filter(bloco == "despesas") %>%
  mutate(funcao = fct_other(as.character(rotulo),
                            keep = c("Educação", "Saúde", "Administração", "Urbanismo"),
                            other_level = "Demais funções") %>%
           fct_relevel("Educação", "Saúde", "Administração", "Urbanismo")) %>%
  group_by(ano, funcao) %>%
  summarise(media = sum(media), .groups = "drop")

print(principal_brasil %>% select(-n) %>% pivot_wider(names_from = principal, values_from = pct))
print(composicao %>% pivot_wider(names_from = funcao, values_from = media))

write.xlsx(
  list(brasil = principal_brasil, regiao = principal_regiao, uf = principal_uf,
       composicao_media = composicao),
  file.path(PASTA_GRAFICOS, "principal_despesa.xlsx"),
  overwrite = TRUE
)

# 10.1 Brasil: % de municípios por principal despesa + composição média
p_principal <- ggplot(principal_brasil, aes(ano, pct, color = principal)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_color_manual(values = CORES_PRINCIPAL, name = NULL, drop = FALSE) +
  scale_x_continuous(breaks = seq(2013, 2025, 2)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(title = "Principal despesa do município",
       subtitle = "% dos municípios em que cada função tem a maior participação nas despesas liquidadas",
       x = NULL, y = NULL)

p_composicao <- ggplot(composicao, aes(ano, media, fill = funcao)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.4) +
  scale_fill_manual(values = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#d6d5cf"), name = NULL) +
  scale_x_continuous(breaks = seq(2013, 2025, 2)) +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  labs(title = "Composição média das despesas",
       subtitle = "Participação média de cada função no total liquidado do município",
       x = NULL, y = NULL)

salvar((p_principal | p_composicao) + plot_annotation(caption = NOTA_FONTE),
       "principal_despesa", "temporal_brasil", 14, 5.5)

# 10.2 Regiões
salvar(
  ggplot(principal_regiao, aes(ano, pct, color = principal)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1) +
    facet_wrap(~ regiao, ncol = 5) +
    scale_color_manual(values = CORES_PRINCIPAL, name = NULL, drop = FALSE) +
    scale_x_continuous(breaks = seq(2013, 2025, 4), limits = c(2013, 2025)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(title = "Principal despesa do município — por região",
         subtitle = "% dos municípios em que cada função tem a maior participação · Eixo y comum aos painéis",
         x = NULL, y = NULL, caption = NOTA_FONTE),
  "principal_despesa_regiao", "temporal_regiao", 14, 4.8
)

# 10.3 UF: % de municípios em que Saúde (e não Educação) é a principal despesa
salvar(
  principal_uf %>%
    filter(principal == "Saúde") %>%
    ggplot(aes(ano, fct_rev(uf), fill = pct)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = scales::percent(pct, accuracy = 1),
                  color = pct > 0.5), size = 2.6, show.legend = FALSE) +
    facet_grid(regiao ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_gradientn(colors = RAMPA_LARANJA, labels = scales::label_percent(),
                         limits = c(0, 1), name = NULL) +
    scale_color_manual(values = c(`FALSE` = "#3a3935", `TRUE` = "white")) +
    scale_x_continuous(breaks = 2013:2025, expand = c(0, 0)) +
    labs(title = "% dos municípios em que Saúde é a principal despesa, por UF",
         subtitle = "Nos demais, a principal despesa é quase sempre Educação",
         x = NULL, y = NULL, caption = NOTA_FONTE) +
    theme(panel.grid = element_blank(),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, face = "bold"),
          legend.key.width = unit(1.6, "cm")),
  "principal_despesa_saude_uf", "temporal_uf", 10, 9
)

# 10.4 Mapa municipal da principal despesa
salvar(
  municipios_sf %>%
    cross_join(tibble(ano = ANOS_MAPA)) %>%
    left_join(select(principal, id_municipio, ano, principal), by = c("id_municipio", "ano")) %>%
    ggplot() +
    geom_sf(aes(fill = principal), color = NA) +
    geom_sf(data = estados_sf, fill = NA, color = "white", linewidth = 0.25) +
    facet_wrap(~ ano, ncol = 2) +
    scale_fill_manual(values = CORES_PRINCIPAL, na.value = COR_SEM_DADOS, name = NULL,
                      labels = \(x) if_else(is.na(x), "Sem dados", x)) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = "Principal despesa do município",
         subtitle = "Função com maior participação nas despesas liquidadas",
         caption = "Fonte: SICONFI/Tesouro Nacional. Malha municipal: IBGE via geobr.") +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "#6b6a63"),
          plot.caption = element_text(color = "#8a8980", hjust = 0),
          strip.text = element_text(face = "bold", size = 11),
          legend.position = "bottom",
          plot.margin = margin(10, 10, 10, 10)),
  "principal_despesa", "mapas", 10, 10.5
)
