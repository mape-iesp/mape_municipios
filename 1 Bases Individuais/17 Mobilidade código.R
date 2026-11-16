# ══════════════════════════════════════════════════════════════════════════════
# 17 Mobilidade e Transportes
#
# UNIFICADO em 13/11/2026 (validação): este arquivo já promovia para a raiz da
# dimensão o painel município-ano construído e validado em "mobilidados/
# 17_mobilidade_agregacao.R" (Izabela; validação: Frederico). Por instrução
# desta validação, juntou-se a ele:
#   - Tarifa Zero (Daniel Santini, OLD/Banco_Santini-2026.xlsx): cidades com
#     passe livre universal, vigentes e encerradas. Único arquivo relacionado
#     a tarifa zero no projeto, sem script de tratamento até agora.
#   - Senatran (frota de veículos por município e tipo, via basedosdados):
#     não existia nenhuma menção a isso no código antes desta validação.
#   - Projeto Acesso a Oportunidades (IPEA, indicadores de acessibilidade
#     urbana por grid H3, só para 2019): idem.
# Nenhuma transformação da base de mobilidade em si (mobilidade_brasileira) foi
# alterada — ela continua vindo pronta de "mobilidados/17_mobilidade_agregacao.R"
# e não é refeita aqui.
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Pacotes ────────────────────────────────────────────────────────────────
library(basedosdados)
library(tidyverse)
library(here)

source(here::here("1 Bases Individuais", "_funcoes_comuns.R"))
library(readxl)
library(writexl)
library(lubridate)

set_billing_id("dados-importacao") ## Substituir pelo seu projeto

pasta_dimensao <- here("1 Bases Individuais", "17 Mobilidade")

# Período final do painel desta dimensão: a base de mobilidade cobre só até
# 2021, mas a Tarifa Zero (pedido da validação) precisa valer até 2025 e o
# Senatran tem dados mais recentes — o painel final é estendido até 2025;
# fora do alcance de cada fonte, a variável fica NA (exceto tarifa zero, que
# é 0 por padrão quando o município nunca aderiu — ver seção 3).
ANO_FINAL_PAINEL <- 2025


# ── 2. Ler a base de mobilidade já tratada e validada ─────────────────────────
e_mob <- new.env()
load(file.path(pasta_dimensao, "mobilidados", "mobilidade_brasileira.RData"), envir = e_mob)
mobilidade_brasileira <- e_mob[["mobilidade_brasileira"]]

# Checagens (confirma que é o painel completo, sem refazer a junção)
stopifnot(
  is.character(mobilidade_brasileira$id_municipio),
  is.integer(mobilidade_brasileira$ano),
  !anyDuplicated(mobilidade_brasileira[c("id_municipio", "ano")]),
  all(nchar(mobilidade_brasileira$id_municipio) == 7)
)
n_distinct(mobilidade_brasileira$id_municipio)  # esperado: 5571
range(mobilidade_brasileira$ano)                # esperado: 2000-2021


# ══════════════════════════════════════════════════════════════════════════════
# 3. TARIFA ZERO (Daniel Santini — políticas de passe livre universal)
# ══════════════════════════════════════════════════════════════════════════════
# Fonte: Santini, Daniel, "Brazilian municipalities with full Fare-Free Public
# Transport policies", Harvard Dataverse (https://doi.org/10.7910/DVN/Z927PD),
# atualizado em abril/2026. Coletado manualmente (sem API); arquivo salvo em
# "OLD/Banco_Santini-2026.xlsx" (pasta de material descartado da v1 — o
# arquivo em si é atual, só a pasta é antiga).
#
# Regra (pedido da validação): se o município adotou tarifa zero em Y e nunca
# encerrou, vale 1 de Y até o fim do painel (2025); se encerrou, vale 1 só
# entre o início e o fim da política; se nunca adotou, vale 0 em todos os anos
# (zero estrutural, não NA — a base de Santini é um mapeamento abrangente de
# todas as cidades com essa política, então ausência = não adotou).

arquivo_santini <- file.path(pasta_dimensao, "OLD", "Banco_Santini-2026.xlsx")

# Início/Fim (e Inicio, em "vigentes") vêm em 3 formatos diferentes na
# planilha, mesmo dentro da mesma coluna: ano puro ("1995", "2020.0"), serial
# de data do Excel como texto ("45658.0", porque a célula tem data completa
# em alguns municípios) ou data no formato DD/MM/AAAA ("29/12/2023"). Lido
# como texto (col_types = "text") para não perder nenhum dos 3 formatos na
# adivinhação de tipo do readxl, e convertido para ano aqui.
converter_ano <- function(x) {
  x <- str_trim(x)
  x_num <- suppressWarnings(as.numeric(x))
  case_when(
    str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ year(dmy(x)),
    !is.na(x_num) & x_num > 3000               ~ year(as.Date(x_num, origin = "1899-12-30")),
    !is.na(x_num)                               ~ as.integer(x_num),
    TRUE                                         ~ NA_integer_
  )
}

# 3.1 Vigentes: 1 intervalo [Inicio, ANO_FINAL_PAINEL] por município
tarifa_vigentes <- read_excel(arquivo_santini, sheet = "vigentes", col_types = "text") %>%
  transmute(
    id_municipio = as.character(as.numeric(id_municipio)),
    ano_inicio   = converter_ano(Inicio),
    ano_fim      = ANO_FINAL_PAINEL
  )

# 3.2 Encerradas
tarifa_encerradas <- read_excel(arquivo_santini, sheet = "encerradas", col_types = "text") %>%
  transmute(
    id_municipio = as.character(as.numeric(`Código IBGE`)),
    ano_inicio   = converter_ano(Início),
    ano_fim      = converter_ano(Fim)
  )

# 3.3 Empilha os dois (um município pode ter mais de 1 intervalo — ex.: Palmas/TO,
# que aparece 2 vezes em "encerradas", com políticas em períodos diferentes) e
# expande cada intervalo nos anos cobertos.
intervalos_tarifa_zero <- bind_rows(tarifa_vigentes, tarifa_encerradas) %>%
  filter(!is.na(ano_inicio), !is.na(ano_fim), ano_inicio <= ano_fim)

anos_tarifa_zero <- intervalos_tarifa_zero %>%
  rowwise() %>%
  mutate(ano = list(seq(ano_inicio, min(ano_fim, ANO_FINAL_PAINEL)))) %>%
  ungroup() %>%
  select(id_municipio, ano) %>%
  unnest(ano) %>%
  distinct(id_municipio, ano)

# 3.4 Painel completo (todos os municípios do painel de mobilidade x todos os
# anos) com tarifa_zero_universal = 1 nos anos cobertos, 0 nos demais (zero
# estrutural, nunca NA).
municipios_painel <- unique(mobilidade_brasileira$id_municipio)
anos_painel <- 2000:ANO_FINAL_PAINEL

tarifa_zero <- crossing(id_municipio = municipios_painel, ano = anos_painel) %>%
  left_join(anos_tarifa_zero %>% mutate(tarifa_zero_universal_flag = 1L),
            by = c("id_municipio", "ano")) %>%
  mutate(tarifa_zero_universal = coalesce(tarifa_zero_universal_flag, 0L)) %>%
  select(id_municipio, ano, tarifa_zero_universal)

stopifnot(!anyDuplicated(tarifa_zero[c("id_municipio", "ano")]))
sum(tarifa_zero$tarifa_zero_universal)  # total de município-anos com a política


# ══════════════════════════════════════════════════════════════════════════════
# 4. SENATRAN — Frota de veículos por município e tipo
# ══════════════════════════════════════════════════════════════════════════════
# basedosdados.br_senatran_estatisticas.municipio_tipo: dados MENSAIS (frota é
# um estoque, não um fluxo) e a tabela inteira é grande (~1,5 GB — já
# filtrada por mes = 12, ainda assim ~2,68 milhões de linhas, uma por
# município x ano x tipo de veículo). [UNICO] a 1ª versão baixava essas 2,68
# milhões de linhas e agregava total/por tipo em R (group_by + pivot_wider),
# o que ficou rodando por mais de 20 minutos sem terminar — o mesmo problema
# já corrigido no Acesso a Oportunidades (seção 5). Corrigido do mesmo jeito:
# a soma total e a soma por tipo (via SUM(CASE WHEN ...)) são feitas no
# próprio BigQuery; só a tabela já no formato final (município x ano, uma
# coluna por tipo) é baixada — ~128 mil linhas em vez de 2,68 milhões.
# Usa o mês de dezembro de cada ano como posição anual (mesmo critério do
# IBGE para "frota em 31/12"); o ano mais recente (ainda incompleto na
# fonte) fica sem dezembro e, por isso, NA.
# "" (tipo_veiculo vazio, na fonte) é o único valor fora da lista abaixo — mantido
# como "nao_especificado" para que a soma dos frota_<tipo> sempre bata com frota_total.
tipos_veiculo <- c(
  "", "automovel", "bonde", "caminhao", "caminhao trator", "caminhonete", "camioneta",
  "chassi plataforma", "ciclomotor", "micro-onibus", "motocicleta", "motoneta",
  "onibus", "outros", "quadriciclo", "reboque", "semi-reboque", "side-car",
  "trator esteira", "trator rodas", "triciclo", "utilitario"
)
slug_tipo_veiculo <- function(x) {
  if_else(x == "", "nao_especificado",
          x %>% str_replace_all("-", " ") %>% str_replace_all("[^a-z0-9 ]", "") %>%
            str_squish() %>% str_replace_all(" ", "_"))
}
colunas_tipo_sql <- paste0(
  "  SUM(CASE WHEN tipo_veiculo = '", tipos_veiculo, "' THEN quantidade ELSE 0 END) AS frota_",
  slug_tipo_veiculo(tipos_veiculo)
)

query_senatran <- paste0(
  "SELECT ano, id_municipio,\n",
  "  SUM(quantidade) AS frota_total,\n",
  paste(colunas_tipo_sql, collapse = ",\n"),
  "\nFROM `basedosdados.br_senatran_estatisticas.municipio_tipo`\n",
  "WHERE mes = 12\n",
  "GROUP BY ano, id_municipio"
)
message("[", Sys.time(), "] Senatran: iniciando consulta (SUM/CASE WHEN no servidor)...")
senatran <- read_sql(query_senatran, billing_project_id = get_billing_id())
message("[", Sys.time(), "] Senatran: consulta terminou, ", nrow(senatran), " linhas x ", ncol(senatran), " colunas.")

senatran <- senatran %>%
  mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio))

table(senatran$ano)  # confere a cobertura por ano (dezembro)

stopifnot(!anyDuplicated(senatran[c("ano", "id_municipio")]))
range(senatran$ano)
message("[", Sys.time(), "] Senatran: seção 4 concluída.")


# ══════════════════════════════════════════════════════════════════════════════
# 5. PROJETO ACESSO A OPORTUNIDADES (IPEA) — acessibilidade urbana, só 2019
# ══════════════════════════════════════════════════════════════════════════════
# basedosdados.br_ipea_acesso_oportunidades.indicadores_2019: grão é o
# hexágono H3 (resolução 9) x modo de transporte (tp/bicicleta/caminhada) x
# horário (pico/fora-pico) — não é município. [UNICO] a tabela tem um
# hexágono por linha para as maiores cidades do Brasil (muito mais linhas do
# que municípios x modo x horário); baixar tudo e agregar em R (como na
# 1ª versão) demorou mais de 20 minutos sem terminar. Agregado agora com
# AVG()/GROUP BY já na consulta SQL — o BigQuery faz a média dos hexágonos
# de cada município no servidor, e só a tabela já pequena (município x modo
# de transporte) é baixada. Por município, por modo de transporte (3 colunas
# por indicador); horário de pico e fora-pico são agregados juntos (média),
# para não multiplicar por 6 o número de colunas — se precisar separar por
# horário no futuro, refazer a partir daqui (fica registrado no dicionário).

colunas_indicador <- c(
  "CMATT15", "CMATQ15", "CMATD15", "CMAST15", "CMASB15", "CMASM15", "CMASA15", "CMAET15", "CMAEI15", "CMAEF15", "CMAEM15",
  "CMATT30", "CMATQ30", "CMATD30", "CMAST30", "CMASB30", "CMASM30", "CMASA30", "CMAET30", "CMAEI30", "CMAEF30", "CMAEM30",
  "CMATT45", "CMATQ45", "CMATD45", "CMAST45", "CMASB45", "CMASM45", "CMASA45", "CMAET45", "CMAEI45", "CMAEF45", "CMAEM45",
  "CMATT60", "CMATQ60", "CMATD60", "CMAST60", "CMASB60", "CMASM60", "CMASA60", "CMAET60", "CMAEI60", "CMAEF60", "CMAEM60",
  "CMATT90", "CMATQ90", "CMATD90", "CMAST90", "CMASB90", "CMASM90", "CMASA90", "CMAET90", "CMAEI90", "CMAEF90", "CMAEM90",
  "CMATT120", "CMATQ120", "CMATD120", "CMAST120", "CMASB120", "CMASM120", "CMASA120", "CMAET120", "CMAEI120", "CMAEF120", "CMAEM120",
  "TMIST", "TMISB", "TMISM", "TMISA", "TMIET", "TMIEI", "TMIEF", "TMIEM"
)

query_acesso <- paste0(
  "SELECT id_municipio, modo_transporte,\n",
  paste0("  AVG(", colunas_indicador, ") AS ", colunas_indicador, collapse = ",\n"),
  "\nFROM `basedosdados.br_ipea_acesso_oportunidades.indicadores_2019`\n",
  "GROUP BY id_municipio, modo_transporte"
)
message("[", Sys.time(), "] Acesso a Oportunidades: iniciando consulta (AVG/GROUP BY no servidor)...")
acesso_municipio <- read_sql(query_acesso, billing_project_id = get_billing_id())
message("[", Sys.time(), "] Acesso a Oportunidades: consulta terminou, ", nrow(acesso_municipio), " linhas x ", ncol(acesso_municipio), " colunas.")

acesso_municipio <- acesso_municipio %>%
  mutate(id_municipio = as.character(id_municipio))

message("[", Sys.time(), "] Acesso a Oportunidades: pivotando...")
acesso_oportunidades <- acesso_municipio %>%
  pivot_wider(
    id_cols = id_municipio,
    names_from = modo_transporte,
    values_from = all_of(colunas_indicador),
    names_glue = "acesso_{.value}_{modo_transporte}"
  ) %>%
  mutate(ano = 2019L)

stopifnot(!anyDuplicated(acesso_oportunidades$id_municipio))
dim(acesso_oportunidades)
message("[", Sys.time(), "] Acesso a Oportunidades: seção 5 concluída.")


# ══════════════════════════════════════════════════════════════════════════════
# 6. INTEGRAÇÃO FINAL — painel primeiro, join depois
# ══════════════════════════════════════════════════════════════════════════════

message("[", Sys.time(), "] Integração final: montando esqueleto do painel...")
painel_mobilidade <- crossing(id_municipio = municipios_painel, ano = anos_painel)

# [UNICO] reatribuído ao mesmo nome "mobilidade_brasileira" (não
# "mobilidade_transportes"): mantém o objeto/arquivo com o nome já usado por
# este e por outros scripts do projeto que possam carregar este .RData.
mobilidade_brasileira <- painel_mobilidade %>%
  left_join(mobilidade_brasileira,  by = c("id_municipio", "ano")) %>%
  left_join(tarifa_zero,            by = c("id_municipio", "ano")) %>%
  left_join(senatran,               by = c("id_municipio", "ano")) %>%
  left_join(acesso_oportunidades,   by = c("id_municipio", "ano"))
message("[", Sys.time(), "] Integração final: joins concluídos.")

n_dup <- mobilidade_brasileira %>% count(id_municipio, ano) %>% filter(n > 1) %>% nrow()
if (n_dup > 0) warning(n_dup, " município-ano duplicados no painel final — investigar antes de exportar")

dim(mobilidade_brasileira)
range(mobilidade_brasileira$ano)
message("[", Sys.time(), "] Integração final: checagens concluídas, começando exportação...")


# ══════════════════════════════════════════════════════════════════════════════
# 7. EXPORTAÇÃO
# ══════════════════════════════════════════════════════════════════════════════

save(mobilidade_brasileira, file = file.path(pasta_dimensao, "mobilidade_brasileira.RData"))
message("[", Sys.time(), "] Exportação: .RData salvo.")

write_xlsx(
  mobilidade_brasileira,
  path = file.path(pasta_dimensao, "mobilidade_brasileira.xlsx")
)
message("[", Sys.time(), "] Exportação: .xlsx salvo. Script concluído.")

# ── Validação padronizada (2 Validação/17 Mobilidade/) ───────────────────────
validar_dimensao(mobilidade_brasileira, n_dim = 17)
