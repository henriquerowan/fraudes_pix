# =============================================================================
# SCRIPT 02 — ANÁLISE EXPLORATÓRIA INICIAL DAS BASES DO PIX
# =============================================================================
# Descrição:
#   Integra as tabelas mensais produzidas pelo script 01 para criar um painel
#   nacional que permite comparar estatísticas de contestações do PIX com
#   estatísticas gerais de transações. Também documenta a cobertura temporal
#   de cada uma das três bases analisadas.
#
# Objetivo:
#   - Comparar os meses disponíveis simultaneamente nas bases de contestações
#     e de transações nacionais;
#   - calcular indicadores descritivos por mês;
#   - registrar em quais meses cada base possui dados, facilitando a avaliação
#     de diferenças de cobertura temporal.
#
# Pré-requisito:
#   Executar primeiro o script 01:
#     Rscript scripts/01_importacao_diagnostico.R
#
# Execução:
#   Executar a partir da raiz do projeto:
#     Rscript scripts/02_analise_exploratoria.R
#
# Entradas:
#   - outputs/tabelas/fraudes_mensal.csv
#   - outputs/tabelas/transacoes_mensal.csv
#   - outputs/tabelas/municipios_mensal.csv
#   - R/config.R, que fornece as funções de caminho e as configurações comuns.
#
# Saídas:
#   - outputs/tabelas/painel_fraude_transacoes_mensal.csv:
#       painel nacional com os meses comuns às bases de contestações e
#       transações, acrescido de indicadores calculados.
#   - outputs/diagnosticos/cobertura_temporal.csv:
#       tabela indicando, por mês, se há dados em cada uma das três bases.
#
# Limitações metodológicas:
#   - A junção entre contestações e transações é feita somente por AnoMes.
#     As bases usadas aqui não oferecem uma chave comum que permita associar
#     contestações a município, região ou modalidade de transação.
#   - A variável taxa_pix_contestados_por_milhao é uma razão descritiva entre
#     a quantidade de PIX contestados e a quantidade total de transações.
#     Ela não deve ser interpretada como taxa de fraude comprovada.
#   - O valor médio é calculado como valor total dividido pela quantidade total;
#     se a quantidade total for zero ou ausente, o resultado pode ser Inf/NaN
#     ou NA, conforme os dados.
#   - O inner_join conserva somente os meses presentes nas duas tabelas
#     nacionais. A tabela de cobertura temporal é criada separadamente para
#     tornar visíveis as diferenças de disponibilidade entre as três bases.
# =============================================================================

# Carrega os pacotes usados neste script.
# suppressPackageStartupMessages() oculta apenas as mensagens de inicialização
# dos pacotes; erros e avisos relevantes continuam podendo ser exibidos.
suppressPackageStartupMessages({
  library(readr)  # Leitura e gravação de arquivos CSV.
  library(dplyr)  # Junções, seleção, criação e agregação de variáveis.
  library(tidyr)  # Transformação da tabela de cobertura para formato largo.
  library(ggplot2) # Pacote utilizado nas etapas analíticas do projeto; não há
                   # geração de gráficos neste script.
})

# Carrega a configuração compartilhada do projeto.
# O arquivo define, entre outros itens, diretorio_saida(), que monta caminhos
# dentro da estrutura de resultados. Por isso, execute o script a partir da
# raiz do projeto, onde a pasta R/ está disponível.
source("R/config.R")

# Função auxiliar para ler arquivos gerados pelo pipeline.
# Os argumentos (...) permitem passar separadamente a pasta e o nome do arquivo.
# show_col_types = FALSE evita que readr imprima os tipos inferidos no terminal.
ler_saida <- function(...) {
  read_csv(diretorio_saida(...), show_col_types = FALSE)
}

# Lê as três tabelas mensais produzidas pelo script 01.
# Cada tabela já é uma saída agregada; este script não relê nem reprocessa
# diretamente os arquivos brutos.
fraudes <- ler_saida("outputs", "tabelas", "fraudes_mensal.csv")
transacoes <- ler_saida("outputs", "tabelas", "transacoes_mensal.csv")
municipios <- ler_saida("outputs", "tabelas", "municipios_mensal.csv")

# -----------------------------------------------------------------------------
# 1. PAINEL NACIONAL DE CONTESTAÇÕES E TRANSAÇÕES
# -----------------------------------------------------------------------------

# Combina a tabela mensal de contestações com a tabela mensal de transações.
# inner_join(..., by = "AnoMes") mantém apenas os meses existentes nas duas
# tabelas. A análise, portanto, cobre o período comum, não necessariamente
# todo o período disponível em cada base separadamente.
#
# Indicadores calculados:
# - taxa_pix_contestados_por_milhao:
#     quantidade de PIX contestados por milhão de transações, calculada como
#     QtdePixcontestados / quantidade_total * 1.000.000.
# - valor_medio_transacao:
#     valor total transacionado dividido pela quantidade total de transações.
#
# Interpretação importante:
#   São indicadores agregados e descritivos. A razão de contestações por milhão
#   não mede a proporção de fraudes comprovadas, pois uma contestação não é,
#   por si só, confirmação de fraude.
painel_fraude_transacoes <- fraudes %>%
  inner_join(transacoes, by = "AnoMes") %>%
  mutate(
    taxa_pix_contestados_por_milhao =
      1e6 * QtdePixcontestados / quantidade_total,
    valor_medio_transacao =
      valor_total / quantidade_total
  )

# Grava o painel mensal para as próximas etapas do projeto.
# O script 03 constrói análises adicionais a partir das tabelas mensais e o
# script 04 utiliza as saídas exploratórias para gerar os gráficos finais.
write_csv(
  painel_fraude_transacoes,
  diretorio_saida(
    "outputs", "tabelas", "painel_fraude_transacoes_mensal.csv"
  )
)

# -----------------------------------------------------------------------------
# 2. DIAGNÓSTICO DE COBERTURA TEMPORAL
# -----------------------------------------------------------------------------

# Reúne os meses encontrados em cada uma das três bases mensais.
# transmute() cria uma tabela simples com o nome da base e a coluna AnoMes.
# bind_rows() empilha as tabelas; distinct() elimina combinações repetidas de
# base e mês, caso existam duplicatas nas entradas.
cobertura_temporal <- bind_rows(
  transacoes %>%
    transmute(base = "transacoes", AnoMes),
  municipios %>%
    transmute(base = "municipios", AnoMes),
  fraudes %>%
    transmute(base = "fraudes", AnoMes)
) %>%
  distinct() %>%
  # A presença de uma linha significa que a base possui registros para o mês.
  mutate(disponivel = TRUE) %>%
  # Converte a tabela para uma linha por mês e uma coluna por base.
  # values_fill = FALSE preenche com FALSE os meses em que uma base não aparece.
  pivot_wider(
    names_from = base,
    values_from = disponivel,
    values_fill = FALSE
  )

# Salva a cobertura temporal no diretório de diagnósticos.
# Essa tabela permite verificar diferenças entre a disponibilidade temporal
# das bases sem restringir a comparação ao período comum do painel.
write_csv(
  cobertura_temporal,
  diretorio_saida(
    "outputs", "diagnosticos", "cobertura_temporal.csv"
  )
)

# Mensagem de conclusão: informa quantos meses foram mantidos no painel comum.
# nrow() conta as linhas da tabela resultante, normalmente uma por mês comum.
message(
  "Painel mensal criado com ",
  nrow(painel_fraude_transacoes),
  " meses em comum."
)
