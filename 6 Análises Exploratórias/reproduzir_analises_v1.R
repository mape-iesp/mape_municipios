# =============================================================================
# 6 Análises Exploratórias — Reproduz os exemplos de uso de
# "Banco de dados Informações Municipais v1.docx" (seção "Usos potenciais")
# contra o painel final atual (4 Base Final/mape_municipios.parquet).
#
# Ver README.md desta pasta para instruções de execução. Os nomes de
# variável abaixo são os do painel atual — podem não ser idênticos aos
# citados no v1.docx (ex. "indice_risco_inundacoes_enxurradas" corresponde
# hoje a adaptabrasil_risco_inundacao_valor; ver dicionario_dimensoes.csv
# em "4 Base Final/" para conferir qualquer variável).
#
# Reproduz:
#   1. Relação entre IVS e risco de inundação/enxurrada (Figura 2 do v1)
#   2. Tipologia 2x2 de vulnerabilidade social x risco de enchentes (Figura 3)
#   3. Mapa temático da tipologia para o Rio de Janeiro (Figura 4)
#   4. Evolução temporal da Taxa de Mortalidade Ajustada por CSAP (Figura 5)
#   5. Modelo de regressão em painel para a Taxa de Mortalidade CSAP (Tabela 2)
# =============================================================================

pacotes <- c("arrow", "dplyr", "ggplot2", "geobr", "sf", "plm", "broom", "here")
for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
library(dplyr)
library(ggplot2)
library(here)

dir_saida <- here("6 Análises Exploratórias", "saida")
dir.create(dir_saida, showWarnings = FALSE)

# ── 0. Carregar o painel final ───────────────────────────────────────────────
# Use o parquet (mais rápido e mais leve que o .csv/.RData equivalentes).
painel <- arrow::read_parquet(here("4 Base Final", "mape_municipios.parquet"))

# ── 1. Relação entre vulnerabilidade social e risco ambiental ───────────────
# v1.docx, Figura 2: "Relação entre variáveis ivs e
# indice_risco_inundacoes_enxurradas". No painel atual, a variável de risco
# equivalente é adaptabrasil_risco_inundacao_valor (AdaptaBrasil/MCTI,
# dimensão Meio-Ambiente; ver "1 Bases Individuais/12 Meio-Ambiente código.R").
risco_social_ambiental <- painel %>%
  filter(!is.na(ivs_media), !is.na(adaptabrasil_risco_inundacao_valor)) %>%
  distinct(id_municipio, ivs_media, adaptabrasil_risco_inundacao_valor)

cat("Figura 2 — municípios com IVS e risco de inundação observados:",
    nrow(risco_social_ambiental), "\n")
cat("Correlação (Pearson):",
    cor(risco_social_ambiental$ivs_media, risco_social_ambiental$adaptabrasil_risco_inundacao_valor,
        use = "complete.obs"), "\n")

g1 <- ggplot(risco_social_ambiental, aes(x = ivs_media, y = adaptabrasil_risco_inundacao_valor)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "IVS (vulnerabilidade social)", y = "Risco de inundação/enxurrada (AdaptaBrasil)",
       title = "Relação entre vulnerabilidade social e risco ambiental") +
  theme_minimal()
ggsave(file.path(dir_saida, "figura_2_ivs_x_risco_inundacao.png"), g1, width = 7, height = 5)

# ── 2. Tipologia 2x2: vulnerabilidade social x risco de enchentes ───────────
# v1.docx, Figura 3: quatro grupos a partir da mediana de cada variável.
# (O pacote mapemunicipios terá typology_mape() para isso — ver
# 5 Pacote/mapemunicipios/R/comparacao.R; reproduzido aqui sem depender do
# pacote, que ainda não está publicado.)
mediana_ivs    <- median(risco_social_ambiental$ivs_media, na.rm = TRUE)
mediana_risco  <- median(risco_social_ambiental$adaptabrasil_risco_inundacao_valor, na.rm = TRUE)

tipologia <- risco_social_ambiental %>%
  mutate(
    tipo = case_when(
      ivs_media >= mediana_ivs  & adaptabrasil_risco_inundacao_valor >= mediana_risco ~ "Alta vulnerabilidade social + alto risco",
      ivs_media >= mediana_ivs  & adaptabrasil_risco_inundacao_valor <  mediana_risco ~ "Alta vulnerabilidade social + baixo risco",
      ivs_media <  mediana_ivs  & adaptabrasil_risco_inundacao_valor >= mediana_risco ~ "Baixa vulnerabilidade social + alto risco",
      TRUE                                                                            ~ "Baixa vulnerabilidade social + baixo risco"
    )
  )

cat("\nFigura 3 — municípios por tipo:\n")
print(table(tipologia$tipo))
write.csv(tipologia, file.path(dir_saida, "figura_3_tipologia_riscos.csv"), row.names = FALSE)

# ── 3. Mapa temático da tipologia (Rio de Janeiro) ───────────────────────────
# v1.docx, Figura 4: mesma tipologia aplicada ao mapa do RJ, via geobr.
rj_municipios <- geobr::read_municipality(code_muni = "RJ", year = 2022, showProgress = FALSE)

rj_tipologia <- rj_municipios %>%
  mutate(id_municipio = as.character(code_muni)) %>%
  left_join(tipologia, by = "id_municipio")

g3 <- ggplot(rj_tipologia) +
  geom_sf(aes(fill = tipo), color = "white", linewidth = 0.1) +
  scale_fill_brewer(palette = "Set1", na.value = "grey80", name = NULL) +
  labs(title = "Tipologia de risco social x ambiental — municípios do RJ") +
  theme_void()
ggsave(file.path(dir_saida, "figura_4_mapa_rj_tipologia.png"), g3, width = 7, height = 7)

# ── 4. Evolução temporal da Taxa de Mortalidade Ajustada por CSAP ───────────
# v1.docx, Figura 5: tx_mort_csap_aj_cens ao longo do tempo (Saúde).
evolucao_csap <- painel %>%
  filter(!is.na(tx_mort_csap_aj_cens)) %>%
  group_by(ano) %>%
  summarise(
    media_nacional = mean(tx_mort_csap_aj_cens, na.rm = TRUE),
    n_municipios   = n(),
    .groups = "drop"
  )

cat("\nFigura 5 — anos com tx_mort_csap_aj_cens observada:", nrow(evolucao_csap), "\n")

g4 <- ggplot(evolucao_csap, aes(x = ano, y = media_nacional)) +
  geom_line() +
  geom_point() +
  labs(x = "Ano", y = "Taxa de Mortalidade CSAP (média nacional, por 100 mil hab.)",
       title = "Evolução da Mortalidade Ajustada por Condições Sensíveis à Atenção Primária") +
  theme_minimal()
ggsave(file.path(dir_saida, "figura_5_evolucao_csap.png"), g4, width = 7, height = 5)

# ── 5. Modelo de regressão em painel para a Taxa de Mortalidade CSAP ────────
# v1.docx, Tabela 2: tx_mort_csap_aj_cens explicada por PIB per capita (log),
# gasto do Bolsa Família per capita, IVS, cobertura da Estratégia de Saúde da
# Família e risco de inundação/enxurrada (dimensões Economia, Assistência
# Social, Sociedade e Participação, Saúde e Meio-Ambiente).
base_modelo <- painel %>%
  mutate(
    pib_per_capita = pib / populacao_estimativa,
    pbf_per_capita = pbf_valor_dezembro_r / populacao_estimativa
  ) %>%
  filter(
    !is.na(tx_mort_csap_aj_cens), !is.na(pib_per_capita), pib_per_capita > 0,
    !is.na(pbf_per_capita), !is.na(ivs_media), !is.na(cob_esf),
    !is.na(adaptabrasil_risco_inundacao_valor)
  ) %>%
  mutate(log_pib_per_capita = log(pib_per_capita))

cat("\nTabela 2 — observações município-ano no modelo:", nrow(base_modelo), "\n")

if (requireNamespace("plm", quietly = TRUE) && nrow(base_modelo) > 0) {
  painel_plm <- plm::pdata.frame(base_modelo, index = c("id_municipio", "ano"))

  # model = "pooling" (não "within"/efeitos fixos): ivs_media e
  # adaptabrasil_risco_inundacao_valor são quase invariantes no tempo dentro
  # de cada município (ver "16 Sociedade e Participação código.R" e
  # "12 Meio-Ambiente código.R" — o IVS muda só a cada Censo, e o risco da
  # AdaptaBrasil é imputado repetindo o mesmo valor por até 10 anos a partir
  # do ano de referência). Num modelo de efeitos fixos essas duas variáveis
  # são absorvidas pelo efeito de município e o plm as descarta (confirmado
  # testando com model = "within" primeiro). "pooling" é o que permite
  # manter as 5 variáveis do exemplo do v1.docx — se quiser efeitos fixos
  # de município mesmo assim, troque para "within" e espere perder essas
  # duas colunas do resultado.
  modelo_csap <- plm::plm(
    tx_mort_csap_aj_cens ~ log_pib_per_capita + pbf_per_capita + ivs_media +
      cob_esf + adaptabrasil_risco_inundacao_valor,
    data  = painel_plm,
    model = "pooling"
  )

  print(summary(modelo_csap))
  sink(file.path(dir_saida, "tabela_2_modelo_csap.txt"))
  print(summary(modelo_csap))
  sink()
} else {
  warning("plm não disponível ou base do modelo vazia — Tabela 2 não reproduzida.")
}

cat("\nConcluído. Saídas em:", dir_saida, "\n")
