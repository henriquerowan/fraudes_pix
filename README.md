<<<<<<< HEAD
# PIX e fraudes — pipeline analítico

Projeto acadêmico de análise exploratória das bases públicas de transações PIX, contestações e pagamentos municipais. O pipeline lê os CSVs brutos, diagnostica os dados, agrega indicadores e produz tabelas, gráficos e relatório metodológico. Os arquivos brutos não são alterados.

## Execução

Execute a partir da raiz do projeto:

```bash
Rscript 00_pipeline_fraude_pix.R
```

Dependências R:

```r
install.packages(c("readr", "dplyr", "tidyr", "ggplot2"))
```

A configuração central está em [`R/config.R`](R/config.R). Ela define o locale brasileiro, códigos de ausência, tipos das colunas e caminhos dos arquivos.

## Entradas

Os arquivos brutos esperados na raiz são:

- `estatisticas_fraudes_pix.csv`: contestações, contestações aceitas, valores e indicadores relacionados ao MED.
- `estatisticas_transacoes_pix.csv`: transações nacionais por mês, perfil PF/PJ, região, idade, forma de iniciação, natureza e finalidade.
- `transacoes_pix_municipio.csv`: valores, quantidades e pessoas pagadoras/recebedoras por município, estado e região.

O campo `AnoMes` é validado no formato `YYYYMM`. Meses inválidos ou ausentes são registrados nos diagnósticos e não entram nas agregações temporais.

## Etapas do pipeline

1. Leitura das três bases com tipos e locale definidos em `R/config.R`.
2. Diagnóstico de esquema, dimensões, ausências, problemas de parsing e cobertura temporal.
3. Padronização de `AnoMes` e validação de datas, valores e quantidades.
4. Agregação mensal nacional de contestações e transações.
5. Agregação municipal de valor, quantidade e participação dos pagamentos.
6. Testes exploratórios de correlação, tendência e meses atípicos pelo intervalo interquartil.
7. Geração dos gráficos em PNG e PDF, em formato paisagem.
8. Geração do relatório metodológico em Markdown.

Os gráficos temporais consideram dados até julho de 2026. O corte é aplicado à visualização e não modifica os CSVs brutos.

## Gráficos produzidos

- **G1:** evolução relativa das contestações e transações, em índice base 100.
- **G2:** variações mensais e observações que excedem 1,5 intervalo interquartil, com rótulos dos meses atípicos.
- **G3:** evolução da quantidade de transações, valor transacionado e ticket médio.
- **G4:** participação percentual do valor pago por região do país.
- **G5:** dez cidades com maior volume de pagamentos e participação no total.
- **G6:** Pareto dos municípios pagadores, destacando a participação dos 20% maiores.
- **G7:** corte acumulativo dos municípios necessários para alcançar 80% do valor pago, com linhas tracejadas e ponto de corte.
- **G8.1:** Itajubá/MG — valor por pagador e recebedor PF/PJ.
- **G8.2:** Itajubá/MG — quantidade de PIX por pagador e recebedor PF/PJ.
- **G8.3:** Itajubá/MG — quantidade de pessoas por pagador e recebedor PF/PJ.
- **G9.1:** crescimento relativo do valor pago: Itajubá, Minas Gerais e Brasil.
- **G9.2:** crescimento relativo da quantidade de pessoas: Itajubá, Minas Gerais e Brasil.

Nos gráficos de Itajubá, o eixo temporal é reduzido para melhorar a leitura. Os rótulos de crescimento mostram o último mês em relação ao primeiro mês da série.

## Saídas

- `outputs/pipeline/diagnostico/`: esquema, ausências, parsing, dimensões, cobertura, validações e arquivos gráficos.
- `outputs/pipeline/resultados/`: painéis mensais, agregações municipais, perfis categóricos, hipóteses e resumo.
- `outputs/graficos/`: PNGs em 300 dpi e PDFs em paisagem.
- `docs/relatorio_pipeline_fraude_pix.md`: relatório metodológico e resumo dos resultados.

## Limitações

- Contestação ou contestação aceita não equivale a fraude comprovada.
- As bases não possuem uma chave transacional comum; a integração entre contestações e transações é apenas mensal.
- Não é possível atribuir contestações nacionais a municípios, regiões ou perfis individuais.
- Correlações e tendências são descritivas e não demonstram causalidade.
- Ausências são preservadas nos diagnósticos e não são imputadas como zero.
- A base municipal pode ter cobertura temporal diferente da base nacional.
- Municípios sem código IBGE são excluídos das agregações municipais.

## Scripts históricos

Os scripts em `scripts/` documentam etapas exploratórias anteriores do projeto. O pipeline integrado e reproduzível é `00_pipeline_fraude_pix.R`.
=======
# pix_fraudes
>>>>>>> 059f4d31ad391a8928d6e362025595133e5eff83
