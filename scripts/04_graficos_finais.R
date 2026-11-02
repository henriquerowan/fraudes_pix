# =============================================================================
# SCRIPT 04 — GERAÇÃO DOS GRÁFICOS FINAIS DA ANÁLISE DO PIX
# =============================================================================
# Descrição:
#   Produz três gráficos finais para apresentação acadêmica a partir das tabelas
#   agregadas e indicadores calculados pelo script 03. Os gráficos comparam a
#   evolução relativa das contestações e das transações, destacam variações
#   mensais atípicas e apresentam a concentração do valor de PIX pagos nos
#   dez municípios com maior valor agregado no período analisado.
#
# Objetivos:
#   1. Comparar índices de evolução de contestações e transações (base 100);
#   2. visualizar variações percentuais mensais e meses atípicos pelo critério
#      do intervalo interquartil (IQR);
#   3. mostrar os dez municípios com maior valor agregado de PIX pagos e sua
#      participação percentual no total municipal considerado.
#
# Pré-requisitos:
#   Executar os scripts 01, 02 e 03, na ordem prevista pelo projeto.
#   O script 03 deve ter produzido as tabelas em outputs/exploracao/.
#
# Execução:
#   Executar a partir da raiz do projeto:
#     Rscript scripts/04_graficos_finais.R
#
# Entradas:
#   - outputs/exploracao/painel_nacional_indicadores.csv
#   - outputs/exploracao/meses_atipicos_variacoes.csv
#   - outputs/exploracao/municipios_periodo.csv
#   - outputs/exploracao/concentracao_municipal.csv
#   - R/config.R, que define a função de construção dos caminhos de saída.
#
# Saídas (diretório outputs/graficos/):
#   - grafico_01_evolucao_comparada.png e .pdf
#   - grafico_02_divergencias_mensais.png e .pdf
#   - grafico_03_concentracao_municipal.png e .pdf
#
# Observações metodológicas:
#   - Os índices de base 100 mostram evolução relativa em relação ao primeiro
#     mês comum; não comparam diretamente magnitudes absolutas.
#   - Contestações e valores contestados aceitos não equivalem a fraudes
#     comprovadas.
#   - Os meses atípicos são sinalizados por uma regra estatística (IQR), sem
#     atribuição automática de causa.
#   - A análise municipal exclui registros sem código Municipio_Ibge conforme
#     a agregação produzida pelo script 03.
#   - O script valida algumas condições esperadas antes de salvar os gráficos.
# =============================================================================

# Carrega os pacotes necessários para ler as tabelas, transformá-las e produzir
# os gráficos. As mensagens rotineiras de inicialização ficam ocultas.
suppressPackageStartupMessages({
  library(readr)  # Leitura dos CSVs de resultados.
  library(dplyr)  # Ordenação, seleção, filtragem e criação de variáveis.
  library(tidyr)  # Conversão de tabelas entre formato largo e longo.
  library(ggplot2) # Construção e exportação dos gráficos.
})

# Importa as funções compartilhadas de configuração e construção de caminhos.
# O script pressupõe execução a partir da raiz do projeto.
source("R/config.R")

# Define a pasta onde os gráficos serão salvos e a cria, se necessário.
dir_graficos <- diretorio_saida("outputs", "graficos")
dir.create(dir_graficos, recursive = TRUE, showWarnings = FALSE)

# Função auxiliar para ler tabelas que foram produzidas pelo script 03.
# Recebe o nome do arquivo dentro de outputs/exploracao/.
ler_exploracao <- function(nome) {
  read_csv(
    diretorio_saida("outputs", "exploracao", nome),
    show_col_types = FALSE
  )
}

# -----------------------------------------------------------------------------
# 1. FUNÇÃO PARA SALVAR OS GRÁFICOS
# -----------------------------------------------------------------------------

# Salva cada gráfico em dois formatos:
# - PNG: imagem rasterizada de alta resolução, adequada para slides e documentos;
# - PDF: formato vetorial, útil para ampliar ou inserir em documentos acadêmicos.
#
# largura e altura são especificadas em polegadas.
# O PNG usa 320 dpi e fundo branco.
# A exportação em PDF é protegida por tryCatch(): se a criação do PDF falhar,
# uma mensagem é emitida e a execução pode continuar. A gravação do PNG não
# está protegida por esse tratamento e interromperá a execução se falhar.
salvar_grafico <- function(grafico, nome, largura, altura) {
  ggsave(
    file.path(dir_graficos, paste0(nome, ".png")),
    grafico,
    width = largura,
    height = altura,
    units = "in",
    dpi = 320,
    bg = "white"
  )

  tryCatch(
    ggsave(
      file.path(dir_graficos, paste0(nome, ".pdf")),
      grafico,
      width = largura,
      height = altura,
      units = "in",
      bg = "white"
    ),
    error = function(e) {
      message(
        "PDF não gerado para ", nome, ": ",
        conditionMessage(e)
      )
    }
  )
}

# -----------------------------------------------------------------------------
# 2. LEITURA DAS TABELAS DE ENTRADA
# -----------------------------------------------------------------------------

# O painel contém os indicadores nacionais mensais calculados no script 03.
# A coluna AnoMes é convertida em uma data R correspondente ao primeiro dia do
# mês e os registros são ordenados cronologicamente.
painel <- ler_exploracao("painel_nacional_indicadores.csv") %>%
  mutate(
    data = as.Date(paste0(AnoMes, "01"), format = "%Y%m%d")
  ) %>%
  arrange(data)

# Tabela com os meses considerados atípicos pelo critério IQR.
atipicos <- ler_exploracao("meses_atipicos_variacoes.csv")

# Tabela com os totais municipais para todo o período disponível.
municipios <- ler_exploracao("municipios_periodo.csv")

# Tabela resumida com medidas de concentração municipal, produzida no script 03.
concentracao <- ler_exploracao("concentracao_municipal.csv")

# -----------------------------------------------------------------------------
# 3. VALIDAÇÕES DE ENTRADA
# -----------------------------------------------------------------------------

# Confirma que há pelo menos dois meses e que AnoMes não se repete no painel.
# Essas condições são importantes para calcular evolução temporal.
stopifnot(
  nrow(painel) >= 2,
  !anyDuplicated(painel$AnoMes)
)

# Confirma a presença das variáveis principais usadas nos gráficos.
# Se alguma coluna estiver ausente, stopifnot() interrompe a execução.
stopifnot(
  all(
    c(
      "QtdePixcontestados",
      "ValorPixcontestadosaceitos",
      "quantidade_total",
      "valor_total"
    ) %in% names(painel)
  )
)

# Verifica o período esperado para esta versão dos dados.
# Se as bases forem atualizadas com meses anteriores ou posteriores, esta
# validação precisará ser revisada; ela não é uma regra universal do projeto.
stopifnot(
  min(painel$data) == as.Date("2022-01-01"),
  max(painel$data) == as.Date("2026-04-01")
)

# Texto usado nos subtítulos dos gráficos, com o primeiro e o último mês.
periodo <- sprintf(
  "%s a %s",
  format(min(painel$data), "%b/%Y"),
  format(max(painel$data), "%b/%Y")
)

# Função auxiliar para formatar percentuais com vírgula decimal.
# Exemplo: 12.34 passa a ser exibido como "12,3%".
fmt_pct <- function(x, casas = 1) {
  paste0(
    formatC(
      x,
      format = "f",
      digits = casas,
      decimal.mark = ","
    ),
    "%"
  )
}

# Função auxiliar preservada do script original para formatar índices inteiros
# com separador de milhar. Não é utilizada nos três gráficos abaixo.
fmt_indice <- function(x) {
  formatC(round(x), format = "d", big.mark = ".")
}

# =============================================================================
# GRÁFICO 1 — EVOLUÇÃO RELATIVA: CONTESTAÇÕES E TRANSAÇÕES
# =============================================================================

# Transforma as quatro séries em índices com base 100 no primeiro mês comum.
# Fórmula de cada série:
#   índice = 100 * valor do mês / valor do primeiro mês.
#
# Com essa normalização, é possível comparar ritmos de crescimento, mesmo que
# as séries tenham unidades e magnitudes muito diferentes. O índice não mostra
# os valores absolutos nem demonstra que as séries tenham relação causal.
series_indice <- painel %>%
  transmute(
    data,
    `Quantidade de PIX contestados\n(não são fraudes comprovadas)` =
      100 * QtdePixcontestados / first(QtdePixcontestados),
    `Valor contestado aceito\n(não é valor de fraude comprovada)` =
      100 * ValorPixcontestadosaceitos / first(ValorPixcontestadosaceitos),
    `Quantidade total de transações` =
      100 * quantidade_total / first(quantidade_total),
    `Valor total transacionado` =
      100 * valor_total / first(valor_total)
  ) %>%
  # Converte as quatro colunas de séries em duas colunas: nome e índice.
  # Esse formato longo facilita desenhar todas as linhas no mesmo gráfico.
  pivot_longer(
    -data,
    names_to = "serie",
    values_to = "indice"
  )

# Recupera o índice final da quantidade de PIX contestados.
indice_final_contestados <- series_indice %>%
  filter(
    serie ==
      "Quantidade de PIX contestados\n(não são fraudes comprovadas)"
  ) %>%
  slice_max(data, n = 1, with_ties = FALSE) %>%
  pull(indice)

# Recupera o índice final da quantidade total de transações.
indice_final_transacoes <- series_indice %>%
  filter(serie == "Quantidade total de transações") %>%
  slice_max(data, n = 1, with_ties = FALSE) %>%
  pull(indice)

# Verificação específica da narrativa do gráfico: nesta versão dos dados,
# espera-se que o índice final das contestações exceda o das transações.
# Se os dados forem atualizados e essa relação deixar de ocorrer, a execução
# será interrompida para evitar produzir um título incompatível com os dados.
stopifnot(indice_final_contestados > indice_final_transacoes)

# Constrói o gráfico de linhas com uma referência horizontal em 100.
# As cores são atribuídas manualmente para manter consistência visual entre
# séries e gráficos. Os rótulos deixam explícito que contestações não são
# sinônimo de fraudes comprovadas.
g1 <- ggplot(
  series_indice,
  aes(data, indice, color = serie)
) +
  geom_hline(
    yintercept = 100,
    color = "grey70",
    linewidth = 0.35
  ) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "Quantidade de PIX contestados\n(não são fraudes comprovadas)" = "#C43C39",
      "Valor contestado aceito\n(não é valor de fraude comprovada)" = "#E69F00",
      "Quantidade total de transações" = "#0072B2",
      "Valor total transacionado" = "#009E73"
    )
  ) +
  # Exibe marcações de seis em seis meses no eixo temporal.
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(x), "")
  ) +
  labs(
    title = "PIX contestados cresceram relativamente mais que as transações",
    subtitle = paste0(
      periodo,
      " | Índice 100 no primeiro mês comum; índices mostram evolução relativa, não valores absolutos."
    ),
    x = NULL,
    y = "Índice (base 100)",
    color = NULL,
    caption = paste(
      "Contestações e valores aceitos não equivalem a fraudes comprovadas.",
      "Comparação mensal nacional e descritiva."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# GRÁFICO 2 — VARIAÇÕES MENSAIS E MESES ATÍPICOS
# =============================================================================

# Seleciona as variações percentuais mensais das duas séries apresentadas.
# pivot_longer() converte as colunas em linhas para que ggplot desenhe uma
# linha por série. O primeiro mês é removido porque não possui mês anterior e,
# portanto, não tem variação percentual calculável.
series_variacao <- painel %>%
  select(
    data,
    AnoMes,
    variacao_pct_QtdePixcontestados,
    variacao_pct_quantidade_total
  ) %>%
  pivot_longer(
    -c(data, AnoMes),
    names_to = "indicador",
    values_to = "variacao_pct"
  ) %>%
  filter(!is.na(variacao_pct)) %>%
  # Substitui nomes técnicos por rótulos mais legíveis no gráfico.
  mutate(
    serie = recode(
      indicador,
      variacao_pct_QtdePixcontestados = "PIX contestados",
      variacao_pct_quantidade_total = "Quantidade total de transações"
    )
  )

# Seleciona da tabela de atípicos apenas os dois indicadores exibidos no gráfico.
# A tabela gerada pelo script 03 contém também outras variações e divergências.
atipicos_g2 <- atipicos %>%
  filter(
    indicador %in% c(
      "variacao_pct_QtdePixcontestados",
      "variacao_pct_quantidade_total"
    )
  ) %>%
  transmute(
    AnoMes,
    serie = recode(
      indicador,
      variacao_pct_QtdePixcontestados = "PIX contestados",
      variacao_pct_quantidade_total = "Quantidade total de transações"
    )
  ) %>%
  # Recupera a data e o valor da variação associado a cada mês atípico.
  inner_join(series_variacao, by = c("AnoMes", "serie")) %>%
  # Cria rótulos curtos para identificar os meses no gráfico.
  # O posicionamento vertical alterna entre as séries para reduzir sobreposição.
  mutate(
    rotulo = format(data, "%b/%y"),
    vjust_rotulo = if_else(
      serie == "PIX contestados",
      -0.8,
      1.4
    )
  )

# Garante que haja pelo menos um mês atípico entre as duas séries exibidas.
# Se não houver nenhum, o gráfico ainda poderia ser desenhado, mas esta
# versão do script exige observações para destacar e rotular.
stopifnot(nrow(atipicos_g2) > 0)

# Desenha as variações percentuais mensais.
# - Linha horizontal em zero: referência de ausência de variação;
# - linhas e pontos: variação de cada mês;
# - círculos com contorno: meses identificados como atípicos pelo IQR;
# - rótulos: mês/ano das observações atípicas.
g2 <- ggplot(
  series_variacao,
  aes(data, variacao_pct, color = serie)
) +
  geom_hline(
    yintercept = 0,
    color = "grey40",
    linewidth = 0.45
  ) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.35) +
  geom_point(
    data = atipicos_g2,
    shape = 21,
    fill = "white",
    color = "#D55E00",
    stroke = 1.05,
    size = 3.5
  ) +
  geom_text(
    data = atipicos_g2,
    aes(
      x = data,
      y = variacao_pct,
      label = rotulo,
      vjust = vjust_rotulo
    ),
    inherit.aes = FALSE,
    color = "#8A3D00",
    size = 3,
    check_overlap = TRUE
  ) +
  scale_color_manual(
    values = c(
      "PIX contestados" = "#C43C39",
      "Quantidade total de transações" = "#0072B2"
    )
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(x), "%")
  ) +
  labs(
    title = "Variações mensais de contestações e transações nem sempre coincidem",
    subtitle = paste0(
      periodo,
      " | Círculos destacam meses atípicos pela regra IQR em cada série, sem atribuição de causa."
    ),
    x = NULL,
    y = "Variação em relação ao mês anterior",
    color = NULL,
    caption = paste(
      "PIX contestados não são fraudes comprovadas;",
      "o primeiro mês não tem variação mensal."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# GRÁFICO 3 — CONCENTRAÇÃO DO VALOR PAGO NOS DEZ MUNICÍPIOS LÍDERES
# =============================================================================

# Seleciona os dez municípios de maior valor agregado.
# A coluna ordem foi calculada no script 03 após ordenar os municípios pelo
# valor pago. A tabela cobre o período agregado disponível, não um único mês.
top10 <- municipios %>%
  arrange(ordem) %>%
  slice_head(n = 10)

# Soma as participações percentuais dos dez municípios selecionados.
top10_pct <- sum(top10$participacao_valor_pct)

# Obtém a participação dos dez primeiros na tabela de concentração, calculada
# independentemente no script 03. A comparação funciona como uma validação
# cruzada entre duas saídas que deveriam concordar.
top10_pct_tabela <- concentracao$participacao_top10_pct[[1]]

# Verifica se há exatamente dez municípios e se as participações calculadas
# a partir da tabela municipal coincidem com o resumo de concentração.
stopifnot(
  nrow(top10) == 10,
  abs(top10_pct - top10_pct_tabela) < 1e-8
)

# Cria o rótulo "Município - Estado", define a ordem visual das barras e
# formata a participação percentual para exibição ao lado de cada barra.
# A ordenação dos níveis do fator segue o valor crescente para que, com
# coord_flip(), os municípios de maior valor apareçam no topo do gráfico.
top10 <- top10 %>%
  mutate(
    municipio_rotulo = paste(Municipio, "-", Estado),
    municipio_rotulo = factor(
      municipio_rotulo,
      levels = municipio_rotulo[order(valor_pagador)]
    ),
    rotulo_pct = fmt_pct(participacao_valor_pct)
  )

# Gráfico de barras horizontais:
# - comprimento da barra: valor agregado de PIX pagos;
# - rótulo: participação percentual de cada município no total municipal;
# - título: participação conjunta dos dez municípios líderes.
#
# O eixo monetário é apresentado em trilhões de reais. A escala divide os
# valores por 10^12 e adiciona o sufixo "tri" aos rótulos.
g3 <- ggplot(
  top10,
  aes(municipio_rotulo, valor_pagador)
) +
  geom_col(
    fill = "#0072B2",
    width = 0.72
  ) +
  geom_text(
    aes(label = rotulo_pct),
    hjust = -0.12,
    size = 3.4,
    color = "#17324D"
  ) +
  # Inverte a orientação das barras para facilitar a leitura dos nomes.
  # clip = "off" permite que rótulos fora da área do painel sejam desenhados.
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = function(x) {
      paste0(
        "R$ ",
        formatC(
          x / 1e12,
          format = "f",
          digits = 0,
          decimal.mark = ","
        ),
        " tri"
      )
    },
    # Reserva espaço adicional para os rótulos percentuais no final das barras.
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = sprintf(
      "Os 10 municípios líderes concentram %s do valor PIX pago",
      fmt_pct(top10_pct)
    ),
    subtitle = paste(
      "Agregado de todo o período da tabela municipal;",
      "rótulos mostram a participação no valor total."
    ),
    x = NULL,
    y = "Valor agregado de PIX pagos",
    caption = paste(
      "Registros sem identificação municipal foram excluídos,",
      "conforme a metodologia documentada."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    # Aumenta a margem direita para acomodar os rótulos das barras.
    plot.margin = margin(5.5, 40, 5.5, 5.5)
  )

# -----------------------------------------------------------------------------
# 4. EXPORTAÇÃO DOS GRÁFICOS
# -----------------------------------------------------------------------------

# Exporta os três gráficos em PNG (320 dpi) e tenta também gerar suas versões
# em PDF. Os arquivos são gravados em outputs/graficos/.
salvar_grafico(
  g1,
  "grafico_01_evolucao_comparada",
  largura = 11,
  altura = 6.5
)
salvar_grafico(
  g2,
  "grafico_02_divergencias_mensais",
  largura = 11,
  altura = 6.5
)
salvar_grafico(
  g3,
  "grafico_03_concentracao_municipal",
  largura = 11,
  altura = 7
)

# Mensagem final com o diretório de saída para facilitar a localização dos arquivos.
message("Três gráficos finais gerados em: ", dir_graficos)
