# mape_municipios

**mape_municipios** é um banco de dados em painel (município × ano) sobre os municípios brasileiros, construído pelo [MAPE](https://mape.org.br/), do IESP-UERJ. Ele consolida informações de fontes oficiais e acadêmicas — órgãos governamentais (IBGE, TSE, INEP, Ministério da Saúde, CGU, entre outros), organizações privadas e bases produzidas para artigos científicos — num formato padronizado e pronto para análise, cobrindo:

- **17 dimensões** de política pública: Identificação, População, Assistência Social e Direitos Humanos, Corrupção e Transparência, Dados Históricos, Economia, Educação, Eleições e Partidos, Energia e Internet, Finanças, Habitação e Zoneamento, Meio-Ambiente, Recursos Humanos, Saúde, Segurança, Sociedade e Participação, e Mobilidade e Transportes;
- **2.634 variáveis** no painel final consolidado;
- **200.556 observações** município-ano, cobrindo o período de **1991 a 2026**.

O objetivo é oferecer dados de maneira padronizada e qualificada para pesquisadores/as, gestores/as e sociedade civil, facilitando o cruzamento de informações entre áreas como saúde, educação, assistência social, meio ambiente, segurança e finanças municipais, e permitindo investigar a relação entre indicadores municipais e políticas públicas ao longo do tempo.

A estruturação da base segue um processo documentado de coleta, tratamento e padronização — direta (download de bases oficiais) ou indireta (via pacotes de R como `basedosdados`, `censobr` e `electionsBR`, ou trabalhos acadêmicos já sistematizados) — garantindo transparência e replicabilidade. Ver [Protocolo de Construção - mape_municipios (Guia para Bolsistas).docx](Protocolo%20de%20Construção%20-%20mape_municipios%20(Guia%20para%20Bolsistas).docx) para o passo a passo seguido por quem incorpora uma nova pesquisa à base.

## Estrutura do repositório

| Pasta | Conteúdo |
|---|---|
| [`1 Bases Individuais/`](1%20Bases%20Individuais/) | Um `código.R` por dimensão (coleta + tratamento das pesquisas que a compõem) e o export final de cada uma (ex. `saude_brasileira.xlsx`). |
| [`2 Validação/`](2%20Validação/) | Para cada dimensão: resumo de dados faltantes por variável (`*_resumo_na.csv`) e heatmap de cobertura (`*_heatmap_na.png`), gerados automaticamente por `validar_dimensao()` (em `1 Bases Individuais/_funcoes_comuns.R`). |
| [`3 Junção e Transformação/`](3%20Junção%20e%20Transformação/) | Código que junta as 17 dimensões num único painel município × ano, com o crosswalk variável → dimensão (`dicionario_dimensoes.csv`). |
| [`4 Base Final/`](4%20Base%20Final/) | Exportação do painel final consolidado em múltiplos formatos (parquet, RData, csv). |
| [`5 Pacote/`](5%20Pacote/) | Pacote R `mapemunicipios` (em desenvolvimento) para importar, explorar e transformar a base. |
| [`6 Análises Exploratórias/`](6%20Análises%20Exploratórias/) | Reservada para análises exploratórias a partir da base final. |
| [`mape_municipios METADADOS.xlsx`](mape_municipios%20METADADOS.xlsx) | Dicionário de dados: uma aba por dimensão com a descrição, natureza, fonte, escopo temporal e transformações de cada variável, mais uma aba "Geral" com o controle de todas as pesquisas que entram na base. |

Dados brutos baixados de cada fonte e alguns formatos muito pesados do painel final (`.dta`) não são versionados aqui — ficam no Drive compartilhado do projeto. Ver [`5 Pacote/NOTA_DECISAO_FONTE_DADOS.md`](5%20Pacote/NOTA_DECISAO_FONTE_DADOS.md) sobre a fonte de dados que o pacote R vai usar.
