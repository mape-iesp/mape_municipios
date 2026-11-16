# 6 Análises Exploratórias

Pasta reservada para análises exploratórias a partir do painel final (`4 Base Final/mape_municipios.parquet`).

## `reproduzir_analises_v1.R`

Reproduz, contra o painel final **atual**, os exemplos de uso descritos na seção "Usos potenciais" de `Banco de dados Informações Municipais v1.docx` (documento interno, não versionado neste repositório — ver o Drive compartilhado do projeto):

1. **Relação entre indicadores** — correlação entre vulnerabilidade social (`ivs_media`) e risco de inundação/enxurrada (`adaptabrasil_risco_inundacao_valor`).
2. **Tipologia 2×2** — quatro grupos de municípios a partir da mediana dessas duas variáveis.
3. **Mapa temático** — a mesma tipologia aplicada ao mapa dos municípios do Rio de Janeiro, via `geobr`.
4. **Evolução temporal** — média nacional, ano a ano, da Taxa de Mortalidade Ajustada por Condições Sensíveis à Atenção Primária (`tx_mort_csap_aj_cens`).
5. **Modelo em painel** — regressão de efeitos fixos (`plm`) da mesma taxa de mortalidade sobre PIB per capita, gasto per capita do Bolsa Família, IVS, cobertura da Estratégia de Saúde da Família e risco de inundação.

### Como rodar

1. Abra o projeto pela raiz do repositório (`mape_municipios Construção.Rproj`), para que os caminhos via `here::here()` resolvam corretamente.
2. Confirme que `4 Base Final/mape_municipios.parquet` existe localmente (ele é grande e fica fora do Git LFS por padrão em alguns clones — baixe do Drive compartilhado ou rode "3 Junção e Transformação" + "4 Base Final" se precisar gerá-lo de novo).
3. Rode o script:
   ```r
   source("6 Análises Exploratórias/reproduzir_analises_v1.R")
   ```
   Na primeira vez, ele instala os pacotes que faltarem (`arrow`, `geobr`, `sf`, `plm`, `broom`, `ggplot2`, `here`).
4. As figuras e a tabela do modelo saem em `6 Análises Exploratórias/saida/` (pasta criada automaticamente, não versionada — ver `.gitignore`).

### Se os nomes de variável mudarem

Os nomes usados no script (`ivs_media`, `tx_mort_csap_aj_cens` etc.) são os do painel atual, conferidos em `4 Base Final/dicionario_dimensoes.csv` e em `mape_municipios METADADOS.xlsx`. Depois de rodar "3 Junção e Transformação"/"4 Base Final" de novo (por exemplo, após uma nova validação de dimensão), confira se os mesmos nomes continuam existindo antes de rodar este script — e ajuste aqui se alguma variável for renomeada.
