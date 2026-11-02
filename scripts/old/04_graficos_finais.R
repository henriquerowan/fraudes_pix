# Gráficos finais da apresentação acadêmica.
# Executar da raiz do projeto após os scripts 01, 02 e 03.
# Usa apenas as tabelas agregadas já produzidas em outputs/exploracao/.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

source("R/config.R")

dir_graficos <- diretorio_saida("outputs", "graficos")
dir.create(dir_graficos, recursive = TRUE, showWarnings = FALSE)

ler_exploracao <- function(nome) {
  read_csv(diretorio_saida("outputs", "exploracao", nome), show_col_types = FALSE)
}

# Salva cada um dos três gráficos em formato de apresentação e vetorial.
salvar_grafico <- function(grafico, nome, largura, altura) {
  ggsave(file.path(dir_graficos, paste0(nome, ".png")), grafico,
    width = largura, height = altura, units = "in", dpi = 320, bg = "white"
  )
  tryCatch(
    ggsave(file.path(dir_graficos, paste0(nome, ".pdf")), grafico,
      width = largura, height = altura, units = "in", bg = "white"
    ),
    error = function(e) message("PDF não gerado para ", nome, ": ", conditionMessage(e))
  )
}

painel <- ler_exploracao("painel_nacional_indicadores.csv") %>%
  mutate(data = as.Date(paste0(AnoMes, "01"), format = "%Y%m%d")) %>%
  arrange(data)
atipicos <- ler_exploracao("meses_atipicos_variacoes.csv")
municipios <- ler_exploracao("municipios_periodo.csv")
concentracao <- ler_exploracao("concentracao_municipal.csv")

# Validações dos números que aparecem nos títulos/subtítulos e rótulos.
stopifnot(nrow(painel) >= 2, !anyDuplicated(painel$AnoMes))
stopifnot(all(c("QtdePixcontestados", "ValorPixcontestadosaceitos",
  "quantidade_total", "valor_total") %in% names(painel)))
stopifnot(min(painel$data) == as.Date("2022-01-01"), max(painel$data) == as.Date("2026-04-01"))

periodo <- sprintf("%s a %s", format(min(painel$data), "%b/%Y"), format(max(painel$data), "%b/%Y"))
fmt_pct <- function(x, casas = 1) paste0(formatC(x, format = "f", digits = casas, decimal.mark = ","), "%")
fmt_indice <- function(x) formatC(round(x), format = "d", big.mark = ".")

# GRÁFICO 1 - todas as séries partem de 100 no primeiro mês comum.
series_indice <- painel %>%
  transmute(
    data,
    `Quantidade de PIX contestados\n(não são fraudes comprovadas)` = 100 * QtdePixcontestados / first(QtdePixcontestados),
    `Valor contestado aceito\n(não é valor de fraude comprovada)` = 100 * ValorPixcontestadosaceitos / first(ValorPixcontestadosaceitos),
    `Quantidade total de transações` = 100 * quantidade_total / first(quantidade_total),
    `Valor total transacionado` = 100 * valor_total / first(valor_total)
  ) %>%
  pivot_longer(-data, names_to = "serie", values_to = "indice")

indice_final_contestados <- series_indice %>%
  filter(serie == "Quantidade de PIX contestados\n(não são fraudes comprovadas)") %>%
  slice_max(data, n = 1, with_ties = FALSE) %>% pull(indice)
indice_final_transacoes <- series_indice %>%
  filter(serie == "Quantidade total de transações") %>%
  slice_max(data, n = 1, with_ties = FALSE) %>% pull(indice)
stopifnot(indice_final_contestados > indice_final_transacoes)

g1 <- ggplot(series_indice, aes(data, indice, color = serie)) +
  geom_hline(yintercept = 100, color = "grey70", linewidth = 0.35) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c(
    "Quantidade de PIX contestados\n(não são fraudes comprovadas)" = "#C43C39",
    "Valor contestado aceito\n(não é valor de fraude comprovada)" = "#E69F00",
    "Quantidade total de transações" = "#0072B2",
    "Valor total transacionado" = "#009E73"
  )) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = function(x) paste0(round(x), "")) +
  labs(
    title = "PIX contestados cresceram relativamente mais que as transações",
    subtitle = paste0(periodo, " | Índice 100 no primeiro mês comum; índices mostram evolução relativa, não valores absolutos."),
    x = NULL, y = "Índice (base 100)", color = NULL,
    caption = "Contestações e valores aceitos não equivalem a fraudes comprovadas. Comparação mensal nacional e descritiva."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold"), panel.grid.minor = element_blank()
  )

# GRÁFICO 2 - somente os outliers IQR das duas séries exibidas são destacados.
series_variacao <- painel %>%
  select(data, AnoMes, variacao_pct_QtdePixcontestados, variacao_pct_quantidade_total) %>%
  pivot_longer(-c(data, AnoMes), names_to = "indicador", values_to = "variacao_pct") %>%
  filter(!is.na(variacao_pct)) %>%
  mutate(serie = recode(indicador,
    variacao_pct_QtdePixcontestados = "PIX contestados",
    variacao_pct_quantidade_total = "Quantidade total de transações"
  ))

atipicos_g2 <- atipicos %>%
  filter(indicador %in% c("variacao_pct_QtdePixcontestados", "variacao_pct_quantidade_total")) %>%
  transmute(
    AnoMes,
    serie = recode(indicador,
      variacao_pct_QtdePixcontestados = "PIX contestados",
      variacao_pct_quantidade_total = "Quantidade total de transações"
    )
  ) %>%
  inner_join(series_variacao, by = c("AnoMes", "serie")) %>%
  mutate(rotulo = format(data, "%b/%y"), vjust_rotulo = if_else(serie == "PIX contestados", -0.8, 1.4))
stopifnot(nrow(atipicos_g2) > 0)

g2 <- ggplot(series_variacao, aes(data, variacao_pct, color = serie)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.45) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.35) +
  geom_point(data = atipicos_g2, shape = 21, fill = "white", color = "#D55E00", stroke = 1.05, size = 3.5) +
  geom_text(data = atipicos_g2, aes(x = data, y = variacao_pct, label = rotulo, vjust = vjust_rotulo),
    inherit.aes = FALSE, color = "#8A3D00", size = 3, check_overlap = TRUE
  ) +
  scale_color_manual(values = c("PIX contestados" = "#C43C39", "Quantidade total de transações" = "#0072B2")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
  labs(
    title = "Variações mensais de contestações e transações nem sempre coincidem",
    subtitle = paste0(periodo, " | Círculos destacam meses atípicos pela regra IQR em cada série, sem atribuição de causa."),
    x = NULL, y = "Variação em relação ao mês anterior", color = NULL,
    caption = "PIX contestados não são fraudes comprovadas; o primeiro mês não tem variação mensal."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom", plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# GRÁFICO 3 - a tabela já exclui registros sem Municipio_Ibge na agregação de 03.
top10 <- municipios %>% arrange(ordem) %>% slice_head(n = 10)
top10_pct <- sum(top10$participacao_valor_pct)
top10_pct_tabela <- concentracao$participacao_top10_pct[[1]]
stopifnot(nrow(top10) == 10, abs(top10_pct - top10_pct_tabela) < 1e-8)

top10 <- top10 %>%
  mutate(
    municipio_rotulo = paste(Municipio, "-", Estado),
    municipio_rotulo = factor(municipio_rotulo, levels = municipio_rotulo[order(valor_pagador)]),
    rotulo_pct = fmt_pct(participacao_valor_pct)
  )

g3 <- ggplot(top10, aes(municipio_rotulo, valor_pagador)) +
  geom_col(fill = "#0072B2", width = 0.72) +
  geom_text(aes(label = rotulo_pct), hjust = -0.12, size = 3.4, color = "#17324D") +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = function(x) paste0("R$ ", formatC(x / 1e12, format = "f", digits = 0, decimal.mark = ","), " tri"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = sprintf("Os 10 municípios líderes concentram %s do valor PIX pago", fmt_pct(top10_pct)),
    subtitle = "Agregado de todo o período da tabela municipal; rótulos mostram a participação no valor total.",
    x = NULL, y = "Valor agregado de PIX pagos",
    caption = "Registros sem identificação municipal foram excluídos, conforme a metodologia documentada."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), plot.margin = margin(5.5, 40, 5.5, 5.5)
  )

salvar_grafico(g1, "grafico_01_evolucao_comparada", largura = 11, altura = 6.5)
salvar_grafico(g2, "grafico_02_divergencias_mensais", largura = 11, altura = 6.5)
salvar_grafico(g3, "grafico_03_concentracao_municipal", largura = 11, altura = 7)

message("Três gráficos finais gerados em: ", dir_graficos)
