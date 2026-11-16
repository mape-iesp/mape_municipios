# mapemunicipios

Pacote R para acessar e analisar o **mape_municipios**, banco de dados em painel
(município × ano) sobre os municípios brasileiros, organizado em dimensões
temáticas e documentado em um dicionário de variáveis.

> **Versão provisória.** A fonte dos arquivos ainda está sendo definida. Por
> enquanto, aponte o pacote para uma pasta local ou URL:
>
> ```r
> options(mape.source = "G:/Drives compartilhados/mape_municipios/pacote")
> ```
>
> A pasta deve conter um arquivo por dimensão (`identificacao.parquet`,
> `populacao.parquet`, `saude.parquet`...; `.rds` também funciona),
> `dicionario.xlsx` e, opcionalmente, `VERSION`.

## Instalação

```r
# remotes::install_github("ORG/mapemunicipios")
```

## Exemplo

```r
library(mapemunicipios)

search_mape("homicídio")
info_mape("taxa_homicidios")

bd <- import_mape_dimension(c("economia", "meio_ambiente"), years = 2005:2022)

bd <- bd |>
  deflate_monetary(ref = "12/2025") |>
  calculate_population_weighting("pib_real", "populacao_estimativa") |>
  calculate_growth("desmatamento", k = 1) |>
  add_electoral_cycle() |>
  typology_mape("pib_real_pc", "desmatamento", group = "sigla_uf", stat = "median")

coverage_mape(bd, plot = TRUE)
cite_mape(bd)
```

## Funções

| Grupo | Funções |
|---|---|
| Explorar | `search_mape`, `list_mape_dimensions`, `list_mape_variables`, `info_mape`, `coverage_mape`, `cite_mape` |
| Carregar | `import_mape`, `import_mape_dimension`, `import_mape_variables`, `import_mape_dictionary`, `import_mape_dictionary_dimension`, `mape_source`, `mape_version` |
| Cálculos | `deflate_monetary`, `calculate_population_weighting`, `calculate_population_race_weighting`, `calculate_area_weighting` |
| Tempo | `calculate_lag`, `calculate_lead`, `calculate_growth`, `calculate_diff`, `calculate_moving_average`, `calculate_cumulative`, `interpolate_mape`, `add_electoral_cycle`, `fix_baseline` |
| Comparação | `compare_group`, `compare_mean`, `compare_median`, `ranking_mape`, `typology_mape`, `standardize_mape`, `calculate_quantile`, `calculate_share`, `build_index` |
| Qualidade e exportação | `check_mape`, `flag_outliers`, `label_mape`, `export_mape` |

## Convenções

* `id_municipio` é sempre texto (7 dígitos) e `ano` é sempre inteiro;
  colunas `integer64` vindas do BigQuery são convertidas na importação.
* Operações temporais buscam o valor pelo **ano**, não pela posição da linha:
  lacunas no painel geram NA em vez de valores deslocados.
* Comparações são feitas dentro de cada ano e agrupamento territorial
  (`group = NULL` compara com o Brasil).
* As funções não sobrescrevem variáveis: criam colunas novas com nomes
  padronizados (`<variavel>_<operacao>_<referencia>_<grupo>`).
* Valores imputados nunca substituem os originais e são sempre sinalizados
  (`*_interpolado`, `*_persistido`, `*_imputado`).
