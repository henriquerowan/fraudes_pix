# =============================================================================
# SCRIPT 05 — PIPELINE ANALÍTICO INTEGRADO DAS BASES PIX
# =============================================================================
# Objetivo:
#   Diagnosticar as três bases PIX, padronizar e agregar dados sem alterar os
#   arquivos brutos, testar hipóteses descritivas e produzir seis gráficos.
#
# Entradas:
#   - estatisticas_fraudes_pix.csv
#   - estatisticas_transacoes_pix.csv
#   - transacoes_pix_municipio.csv
#   - R/config.R (caminhos, tipos, locale e códigos de ausência do projeto).
#
# Etapas:
#   1. Ler e diagnosticar esquema, ausências, parsing e cobertura temporal.
#   2. Validar tipos, datas e medidas; preservar linhas e explicitar ausências.
#   3. Agregar por mês, município e categorias; integrar somente por AnoMes.
#   4. Testar três hipóteses exploratórias, com correlações e tendências.
#   5. Criar e imprimir g1–g6; exportar PNGs em 300 dpi e PDFs opcionais.
#   6. Exportar tabelas e relatório metodológico em Markdown.
#
# Saídas:
#   - outputs/pipeline/diagnostico/: esquema, ausências, dimensões, cobertura,
#     problemas de parsing e validações.
#   - outputs/pipeline/resultados/: painel mensal, hipóteses, concentrações,
#     agregações categóricas e resumo numérico.
#   - outputs/graficos/: seis PNGs numerados (e PDFs quando possível).
#   - docs/relatorio_pipeline_fraude_pix.md.
#
# Dependências: readr, dplyr, tidyr, ggplot2; configuração existente em R/config.R.
# Execução na raiz do projeto:
#   source("scripts/05_pipeline_fraude_pix.R")
# Terminal:
#   Rscript scripts/05_pipeline_fraude_pix.R
#
# Limitações metodológicas:
#   Contestações não são fraudes comprovadas. As bases nacional de transações e
#   de contestações só são integradas por mês; não há vínculo transacional para
#   inferência individual nem chave para atribuir contestações a municípios.
#   Correlação, tendência e valores atípicos são descritivos, não causais. A
#   base municipal descreve valor pago agregado e pode ter cobertura diferente.
#   Valores ausentes são mantidos no diagnóstico; somas usam na.rm=TRUE, portanto
#   devem ser lidas junto dos indicadores de ausências. Não se imputam zeros.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

if (!file.exists("R/config.R")) {
  stop("Execute a partir da raiz do projeto: R/config.R não foi encontrado.")
}
source("R/config.R")

# Diretórios próprios do script 05; os scripts anteriores e os CSVs brutos não
# são modificados por este pipeline.
dir_diag <- diretorio_saida("outputs", "pipeline", "diagnostico")
dir_res <- diretorio_saida("outputs", "pipeline", "resultados")
dir_graficos <- diretorio_saida("outputs", "graficos")
dir_docs <- "docs"
for (p in c(dir_diag, dir_res, dir_graficos, dir_docs)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

arquivos <- c(
  fraudes = arquivo_bruto("estatisticas_fraudes_pix.csv"),
  transacoes = arquivo_bruto("estatisticas_transacoes_pix.csv"),
  municipios = arquivo_bruto("transacoes_pix_municipio.csv")
)
if (any(!file.exists(arquivos))) {
  stop("Arquivo(s) bruto(s) não encontrado(s): ",
       paste(names(arquivos)[!file.exists(arquivos)], collapse = ", "),
       ". Confira R/config.R e a pasta de dados.")
}

# Leitura comum: reutiliza esquemas, locale e códigos de NA do projeto.
ler_bruto <- function(caminho, tipos) {
  read_csv(caminho, col_types = tipos, locale = locale_pix, na = na_pix,
           progress = FALSE, show_col_types = FALSE, name_repair = "unique")
}

# 1. LEITURA E DIAGNÓSTICO ----------------------------------------------------
# A base de contestações costuma ser menor e é lida integralmente. As bases
# detalhadas são lidas integralmente para manter um pipeline simples e coeso;
# em máquinas com RAM limitada, considere adaptar para read_csv_chunked().
fraudes_raw <- ler_bruto(arquivos[["fraudes"]], tipos_fraudes)
transacoes_raw <- ler_bruto(arquivos[["transacoes"]], tipos_transacoes)
municipios_raw <- ler_bruto(arquivos[["municipios"]], tipos_municipios)

bases <- list(fraudes = fraudes_raw, transacoes = transacoes_raw,
              municipios = municipios_raw)
colunas_obrigatorias <- list(
  fraudes = c("AnoMes", "QtdePixcontestados", "Qtdecontestacoesaceitas",
              "ValorPixcontestadosaceitos"),
  transacoes = c("AnoMes", "VALOR", "QUANTIDADE"),
  municipios = c("AnoMes", "Municipio_Ibge", "Municipio", "Estado",
                 "VL_PagadorPF", "VL_PagadorPJ", "QT_PagadorPF", "QT_PagadorPJ")
)
for (nm in names(bases)) {
  faltantes <- setdiff(colunas_obrigatorias[[nm]], names(bases[[nm]]))
  if (length(faltantes)) {
    stop("Colunas obrigatórias ausentes em ", nm, ": ",
         paste(faltantes, collapse = ", "))
  }
}

esquema <- bind_rows(lapply(names(bases), function(nm) {
  x <- bases[[nm]]
  tibble(base = nm, coluna = names(x), tipo_R = vapply(x, function(z) class(z)[1], character(1)),
         registros = nrow(x), ausentes = vapply(x, function(z) sum(is.na(z)), numeric(1)),
         percentual_ausentes = if (nrow(x)) 100 * ausentes / registros else NA_real_)
}))
write_csv(esquema, file.path(dir_diag, "esquema_ausencias.csv"))

# Problemas de parsing são registrados por base, sem descartar as linhas.
problemas_parse <- bind_rows(lapply(names(bases), function(nm) {
  p <- problems(bases[[nm]])
  if (!nrow(p)) return(tibble(base = character(), row = integer(), col = character(),
                              expected = character(), actual = character()))
  as_tibble(p) %>% mutate(col = as.character(col), row = as.integer(row),
                          base = nm, .before = 1)
}))
write_csv(problemas_parse, file.path(dir_diag, "problemas_parsing.csv"))

dimensoes <- tibble(base = names(bases), registros = vapply(bases, nrow, integer(1)),
                    colunas = vapply(bases, ncol, integer(1)))
write_csv(dimensoes, file.path(dir_diag, "dimensoes_bases.csv"))

# Padroniza AnoMes para texto YYYYMM e data mensal. Valores inválidos viram NA,
# são contabilizados e ficam fora apenas das agregações que exigem mês válido.
padronizar_mes <- function(x) {
  mes <- trimws(as.character(x))
  mes[!grepl("^[0-9]{6}$", mes)] <- NA_character_
  data <- as.Date(paste0(mes, "01"), format = "%Y%m%d")
  invalido <- is.na(data) | is.na(mes) | format(data, "%Y%m") != mes
  data[invalido] <- as.Date(NA)
  list(AnoMes = ifelse(is.na(data), NA_character_, format(data, "%Y%m")), data = data)
}
for (nm in names(bases)) {
  pm <- padronizar_mes(bases[[nm]]$AnoMes)
  bases[[nm]]$AnoMes <- pm$AnoMes
  bases[[nm]]$data_mes <- pm$data
}
fraudes_raw <- bases$fraudes
transacoes_raw <- bases$transacoes
municipios_raw <- bases$municipios

validacao <- bind_rows(lapply(names(bases), function(nm) {
  x <- bases[[nm]]
  tibble(base = nm, registros = nrow(x), meses_invalidos_ou_ausentes = sum(is.na(x$AnoMes)),
         valores_negativos_VALOR = if ("VALOR" %in% names(x)) sum(x$VALOR < 0, na.rm = TRUE) else NA_integer_,
         quantidades_negativas = if ("QUANTIDADE" %in% names(x)) sum(x$QUANTIDADE < 0, na.rm = TRUE) else NA_integer_)
}))
write_csv(validacao, file.path(dir_diag, "validacao_periodos_medidas.csv"))

# 2. TRATAMENTO E AGREGAÇÕES -------------------------------------------------
# Não remove registros por conterem ausências. Para somas, NA não contribui para
# o total; métricas de ausência são mantidas nos diagnósticos para contextualizar.
fraudes_mensal <- fraudes_raw %>%
  filter(!is.na(AnoMes)) %>%
  group_by(AnoMes) %>%
  summarise(across(any_of(c("QtdePixcontestados", "Qtdecontestacoesaceitas",
                           "ValorPixcontestadosaceitos")),
                   ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
            registros_fonte = n(), .groups = "drop") %>%
  mutate(data = as.Date(paste0(AnoMes, "01"), format = "%Y%m%d")) %>% arrange(data)

transacoes_mensal <- transacoes_raw %>%
  filter(!is.na(AnoMes)) %>%
  group_by(AnoMes) %>%
  summarise(valor_total = if (all(is.na(VALOR))) NA_real_ else sum(VALOR, na.rm = TRUE),
            quantidade_total = if (all(is.na(QUANTIDADE))) NA_real_ else sum(QUANTIDADE, na.rm = TRUE),
            registros_fonte = n(), .groups = "drop") %>%
  mutate(data = as.Date(paste0(AnoMes, "01"), format = "%Y%m%d")) %>% arrange(data)

# Integração nacional exclusivamente por AnoMes; primeiro agrega e valida a
# unicidade mensal para impedir joins muitos-para-muitos.
stopifnot(!anyDuplicated(fraudes_mensal$AnoMes), !anyDuplicated(transacoes_mensal$AnoMes))
painel <- inner_join(fraudes_mensal, transacoes_mensal,
                     by = c("AnoMes", "data"), suffix = c("_contestacoes", "_transacoes")) %>%
  arrange(data) %>%
  mutate(
    valor_medio_transacao = if_else(quantidade_total > 0, valor_total / quantidade_total, NA_real_),
    contestacoes_por_milhao_transacoes = if_else(quantidade_total > 0,
      1e6 * QtdePixcontestados / quantidade_total, NA_real_),
    indice_mes = row_number(),
    variacao_pct_contestacoes = if_else(lag(QtdePixcontestados) > 0,
      100 * (QtdePixcontestados / lag(QtdePixcontestados) - 1), NA_real_),
    variacao_pct_quantidade = if_else(lag(quantidade_total) > 0,
      100 * (quantidade_total / lag(quantidade_total) - 1), NA_real_),
    indice100_contestacoes = if (is.finite(first(QtdePixcontestados)) && first(QtdePixcontestados) > 0)
      100 * QtdePixcontestados / first(QtdePixcontestados) else rep(NA_real_, n()),
    indice100_quantidade = if (is.finite(first(quantidade_total)) && first(quantidade_total) > 0)
      100 * quantidade_total / first(quantidade_total) else rep(NA_real_, n()),
    indice100_valor_contestado_aceito = if (is.finite(first(ValorPixcontestadosaceitos)) && first(ValorPixcontestadosaceitos) > 0)
      100 * ValorPixcontestadosaceitos / first(ValorPixcontestadosaceitos) else rep(NA_real_, n()),
    indice100_valor_transacionado = if (is.finite(first(valor_total)) && first(valor_total) > 0)
      100 * valor_total / first(valor_total) else rep(NA_real_, n())
  )
if (nrow(painel) < 3) warning("Menos de três meses comuns; inferências temporais serão limitadas.")
write_csv(fraudes_mensal, file.path(dir_res, "contestacoes_mensais.csv"))
write_csv(transacoes_mensal, file.path(dir_res, "transacoes_mensais.csv"))
write_csv(painel, file.path(dir_res, "painel_nacional_mensal.csv"))

cobertura <- bind_rows(lapply(names(bases), function(nm) {
  bases[[nm]] %>% filter(!is.na(AnoMes)) %>% distinct(AnoMes) %>% mutate(base = nm)
})) %>% mutate(disponivel = TRUE) %>% pivot_wider(names_from = base, values_from = disponivel,
                                                    values_fill = FALSE) %>% arrange(AnoMes)
write_csv(cobertura, file.path(dir_diag, "cobertura_temporal.csv"))

# Agregação municipal. Mantém municípios identificados; linhas sem código são
# quantificadas no diagnóstico e não são atribuídas arbitrariamente a local.
colunas_municipio <- c("Municipio_Ibge", "Municipio", "Estado", "Regiao",
                       "VL_PagadorPF", "VL_PagadorPJ", "QT_PagadorPF", "QT_PagadorPJ")
colunas_municipio <- intersect(colunas_municipio, names(municipios_raw))
municipios_periodo <- municipios_raw %>%
  filter(!is.na(Municipio_Ibge)) %>%
  mutate(valor_pago = rowSums(across(any_of(c("VL_PagadorPF", "VL_PagadorPJ"))), na.rm = TRUE),
         qtd_paga = rowSums(across(any_of(c("QT_PagadorPF", "QT_PagadorPJ"))), na.rm = TRUE)) %>%
  group_by(across(any_of(c("Municipio_Ibge", "Municipio", "Estado", "Regiao")))) %>%
  summarise(valor_pago = sum(valor_pago, na.rm = TRUE), quantidade_paga = sum(qtd_paga, na.rm = TRUE),
            .groups = "drop") %>%
  filter(is.finite(valor_pago)) %>% arrange(desc(valor_pago)) %>%
  mutate(participacao_pct = if (sum(valor_pago) > 0) 100 * valor_pago / sum(valor_pago) else NA_real_,
         acumulado_pct = cumsum(replace_na(participacao_pct, 0)), ordem = row_number())
write_csv(municipios_periodo, file.path(dir_res, "municipios_agregados.csv"))
write_csv(tibble(registros_sem_codigo_municipal = sum(is.na(municipios_raw$Municipio_Ibge)),
                 registros_municipais_total = nrow(municipios_raw)),
          file.path(dir_diag, "identificacao_municipal.csv"))

# Perfil mensal por dimensão categórica. Seleciona apenas colunas presentes.
dimensoes_categoricas <- intersect(c("FORMAINICIACAO", "FINALIDADE", "NATUREZA",
  "PAG_PFPJ", "REC_PFPJ", "PAG_REGIAO", "REC_REGIAO"), names(transacoes_raw))
perfil_categorico <- bind_rows(lapply(dimensoes_categoricas, function(v) {
  transacoes_raw %>% filter(!is.na(AnoMes)) %>%
    mutate(categoria = if_else(is.na(as.character(.data[[v]])), "Não informado", as.character(.data[[v]]))) %>%
    group_by(AnoMes, categoria) %>%
    summarise(valor_total = if (all(is.na(VALOR))) NA_real_ else sum(VALOR, na.rm = TRUE),
              quantidade_total = if (all(is.na(QUANTIDADE))) NA_real_ else sum(QUANTIDADE, na.rm = TRUE),
              registros = n(), .groups = "drop") %>% mutate(dimensao = v, .before = 1)
}))
write_csv(perfil_categorico, file.path(dir_res, "perfil_mensal_categorico.csv"))

# 3. HIPÓTESES EXPLORATÓRIAS -------------------------------------------------
# Testes condicionados a amostra suficiente e variância não nula. Correlação
# temporal em séries agregadas pode ser afetada por tendência e autocorrelação.
teste_cor <- function(x, y, nome) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) {
    return(tibble(hipotese = nome, n = sum(ok), correlacao_pearson = NA_real_,
                  p_valor = NA_real_, status = "Não testável: menos de 3 pares ou variância zero"))
  }
  z <- suppressWarnings(cor.test(x[ok], y[ok], method = "pearson"))
  tibble(hipotese = nome, n = sum(ok), correlacao_pearson = unname(z$estimate),
         p_valor = z$p.value, status = "Testada; associação não implica causalidade")
}

h1 <- teste_cor(painel$variacao_pct_contestacoes, painel$variacao_pct_quantidade,
  "H1: variações mensais nas contestações estão associadas às variações na quantidade de transações")
# Tendência temporal: regressão simples de log(y) em índice mensal para valores positivos.
teste_tendencia <- function(y, nome) {
  ok <- is.finite(y) & y > 0 & is.finite(painel$indice_mes)
  if (sum(ok) < 3) return(tibble(hipotese = nome, n = sum(ok), crescimento_mensal_estimado_pct = NA_real_,
                                  r2 = NA_real_, p_valor = NA_real_, status = "Não testável: menos de 3 valores positivos"))
  fit <- lm(log(y[ok]) ~ painel$indice_mes[ok])
  tibble(hipotese = nome, n = sum(ok), crescimento_mensal_estimado_pct = 100 * (exp(coef(fit)[2]) - 1),
         r2 = summary(fit)$r.squared, p_valor = summary(fit)$coefficients[2, 4],
         status = "Tendência descritiva; não demonstra causalidade")
}
h2 <- teste_tendencia(painel$QtdePixcontestados,
  "H2: a quantidade mensal de PIX contestados apresenta tendência temporal positiva")
h3 <- teste_tendencia(painel$quantidade_total,
  "H3: a quantidade mensal total de transações PIX apresenta tendência temporal positiva")
resultados_hipoteses <- bind_rows(
  h1 %>% mutate(tipo_teste = "Correlação de Pearson", .before = 1),
  h2 %>% mutate(tipo_teste = "Tendência log-linear", .before = 1),
  h3 %>% mutate(tipo_teste = "Tendência log-linear", .before = 1)
)
write_csv(resultados_hipoteses, file.path(dir_res, "hipoteses_testadas.csv"))

# Meses atípicos pelo intervalo interquartil (Tukey), aplicado a variações mensais.
variacoes <- painel %>% select(AnoMes, data, variacao_pct_contestacoes, variacao_pct_quantidade) %>%
  pivot_longer(cols = starts_with("variacao_pct_"), names_to = "indicador", values_to = "variacao_pct") %>%
  filter(is.finite(variacao_pct))
limites_iqr <- variacoes %>% group_by(indicador) %>%
  summarise(q1 = quantile(variacao_pct, .25), q3 = quantile(variacao_pct, .75), .groups = "drop") %>%
  mutate(iqr = q3 - q1, inferior = q1 - 1.5 * iqr, superior = q3 + 1.5 * iqr)
atipicos <- variacoes %>% inner_join(limites_iqr, by = "indicador") %>%
  filter(variacao_pct < inferior | variacao_pct > superior)
write_csv(atipicos, file.path(dir_res, "meses_atipicos_iqr.csv"))

# 4. SELEÇÃO DA DIMENSÃO CATEGÓRICA MAIS ADEQUADA ----------------------------
# Escolhe a dimensão com maior quantidade de categorias observadas, priorizando
# dimensões existentes e com pelo menos duas categorias; não pressupõe conteúdo.
qualidade_dimensao <- perfil_categorico %>% group_by(dimensao) %>%
  summarise(categorias = n_distinct(categoria), meses = n_distinct(AnoMes),
            registros_agregados = n(), valor_observado = sum(!is.na(valor_total)),
            quantidade_observada = sum(!is.na(quantidade_total)), .groups = "drop") %>%
  arrange(desc(categorias), desc(meses))
write_csv(qualidade_dimensao, file.path(dir_res, "avaliacao_dimensoes_categoricas.csv"))
if (nrow(qualidade_dimensao) && any(qualidade_dimensao$categorias >= 2)) {
  dimensao_escolhida <- qualidade_dimensao %>% filter(categorias >= 2) %>% slice(1) %>% pull(dimensao)
  perfil_escolhido <- perfil_categorico %>% filter(dimensao == dimensao_escolhida)
} else {
  dimensao_escolhida <- NA_character_
  perfil_escolhido <- tibble()
}

# 5. GRÁFICOS ----------------------------------------------------------------
# Os gráficos usam somente dados até julho de 2026, sem alterar os arquivos
# brutos. O corte é aplicado às séries temporais nacionais e municipais.
data_limite_graficos <- as.Date("2026-07-01")
painel <- painel %>% filter(data <= data_limite_graficos)
municipios_graficos <- municipios_raw %>% filter(data_mes <= data_limite_graficos)
periodo <- if (nrow(painel)) paste(format(min(painel$data), "%m/%Y"), "a",
                                    format(max(painel$data), "%m/%Y")) else "Sem período comum"
fmt_num <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)

# g1: índices base 100; comparabilidade é de evolução relativa, não magnitude.
long_g1 <- painel %>% select(data, indice100_contestacoes, indice100_quantidade,
                             indice100_valor_contestado_aceito, indice100_valor_transacionado) %>%
  pivot_longer(-data, names_to = "serie", values_to = "indice") %>% filter(is.finite(indice)) %>%
  mutate(serie = recode(serie,
    indice100_contestacoes = "Quantidade contestada",
    indice100_quantidade = "Quantidade total de transações",
    indice100_valor_contestado_aceito = "Valor de contestações aceitas",
    indice100_valor_transacionado = "Valor total transacionado"))
g1 <- ggplot(long_g1, aes(data, indice, color = serie)) + geom_hline(yintercept = 100, color = "grey65") +
  geom_line(linewidth = .8, na.rm = TRUE) + scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  labs(title = "A evolução relativa das contestações pode divergir da atividade PIX",
       subtitle = paste0(periodo, " | Índice 100 no primeiro mês comum; séries com unidades distintas."),
       x = "Mês", y = "Índice (base 100)", color = NULL,
       caption = "Contestações não são fraudes comprovadas; comparação agregada e descritiva.") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# g2: variações mensais e pontos fora dos limites IQR.
long_g2 <- variacoes %>% mutate(serie = recode(indicador,
  variacao_pct_contestacoes = "Contestações", variacao_pct_quantidade = "Quantidade de transações"))
atipicos_g2 <- atipicos %>% mutate(serie = recode(indicador,
  variacao_pct_contestacoes = "Contestações", variacao_pct_quantidade = "Quantidade de transações"))
g2 <- ggplot(long_g2, aes(data, variacao_pct, color = serie)) + geom_hline(yintercept = 0, color = "grey50") +
  geom_line(linewidth = .7, na.rm = TRUE) + geom_point(size = 1.5, na.rm = TRUE) +
  geom_point(data = atipicos_g2, shape = 21, fill = "white", size = 3, stroke = 1.1, na.rm = TRUE) +
  geom_text(data = atipicos_g2, aes(label = format(data, "%m/%Y")),
            vjust = -0.8, size = 3, show.legend = FALSE, na.rm = TRUE) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  labs(title = "Os movimentos mensais incluem oscilações incomuns",
       subtitle = paste0(periodo, " | Destaques: observações além de 1,5 intervalo interquartil."),
       x = "Mês", y = "Variação frente ao mês anterior (%)", color = NULL,
       caption = "Atipicidade estatística não significa erro nem identifica causa; primeiro mês sem variação.") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# g3: top 10 municípios por valor pago, ou menos se a base válida tiver menos de 10.
top10 <- municipios_periodo %>% slice_head(n = 10) %>%
  mutate(rotulo = paste(Municipio, Estado, sep = " / "),
         rotulo = factor(rotulo, levels = rotulo[order(valor_pago)]))
g3 <- ggplot(top10, aes(rotulo, valor_pago)) + geom_col(fill = "#2878A5") + coord_flip() +
  scale_y_continuous(labels = function(x) paste0("R$ ", fmt_num(x))) +
  labs(title = "O valor PIX pago se distribui de forma desigual entre municípios",
       subtitle = paste0("Até dez municípios, agregados no período disponível; participação conjunta: ",
         if (nrow(top10) && sum(municipios_periodo$valor_pago) > 0) paste0(round(sum(top10$valor_pago) / sum(municipios_periodo$valor_pago) * 100, 1), "%") else "não calculável"),
       x = NULL, y = "Valor pago agregado (R$)",
       caption = "Municípios sem código IBGE não são atribuídos; cobertura pode diferir da base nacional.") +
  theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())

# g4: curva acumulada de participação dos municípios ordenados pelo valor pago.
curva_concentracao <- municipios_periodo %>% filter(is.finite(participacao_pct)) %>%
  arrange(desc(valor_pago)) %>% mutate(ordem = row_number(), acumulado_pct = cumsum(participacao_pct))
g4 <- ggplot(curva_concentracao, aes(ordem, acumulado_pct)) + geom_line(linewidth = .9, color = "#7A5195") +
  geom_hline(yintercept = c(50, 80, 90), linetype = "dashed", color = "grey65") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
  labs(title = "Uma parcela dos municípios pode concentrar grande parte do valor pago",
       subtitle = "Municípios ordenados do maior para o menor valor agregado; curva de participação acumulada.",
       x = "Número acumulado de municípios", y = "Participação acumulada no valor pago (%)",
       caption = "A concentração é descritiva e depende dos municípios identificados na base.") +
  theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))

# g5: composição por forma de iniciação, se disponível; compara valor e volume.
if ("FORMAINICIACAO" %in% names(transacoes_raw)) {
  composicao <- perfil_categorico %>% filter(dimensao == "FORMAINICIACAO") %>%
    group_by(categoria) %>% summarise(valor_total = sum(valor_total, na.rm = TRUE),
      quantidade_total = sum(quantidade_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(valor_pct = if (sum(valor_total, na.rm = TRUE) > 0) 100 * valor_total / sum(valor_total, na.rm = TRUE) else NA_real_,
           quantidade_pct = if (sum(quantidade_total, na.rm = TRUE) > 0) 100 * quantidade_total / sum(quantidade_total, na.rm = TRUE) else NA_real_) %>%
    pivot_longer(c(valor_pct, quantidade_pct), names_to = "medida", values_to = "participacao_pct") %>%
    mutate(medida = recode(medida, valor_pct = "Valor", quantidade_pct = "Quantidade"))
  g5 <- ggplot(composicao, aes(reorder(categoria, participacao_pct), participacao_pct, fill = medida)) +
    geom_col(position = "dodge") + coord_flip() +
    labs(title = "A composição por forma de iniciação difere entre valor e quantidade",
      subtitle = "Participação de cada categoria no total acumulado do período disponível.",
      x = "Forma de iniciação", y = "Participação no total (%)", fill = "Medida",
      caption = "A comparação exige ambas as medidas; categorias ausentes são rotuladas como 'Não informado'.") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
} else {
  g5 <- ggplot() + annotate("text", x = 0, y = 0, label = "Forma de iniciação indisponível na base") +
    labs(title = "Composição das transações por forma de iniciação",
         subtitle = "Não foi possível construir a comparação: coluna FORMAINICIACAO ausente.",
         x = NULL, y = NULL, caption = "Nenhum resultado foi imputado ou inventado.") + theme_void()
}

# G3-G9: gráficos adicionais solicitados ------------------------------------
# Funções auxiliares para índices relativos e nomes sem acentuação.
indice_base100 <- function(x) {
  base <- dplyr::first(x[is.finite(x) & x > 0])
  if (length(base) == 0 || !is.finite(base) || base <= 0) rep(NA_real_, length(x)) else 100 * x / base
}
sem_acento <- function(x) iconv(toupper(trimws(x)), to = "ASCII//TRANSLIT")

# G3: evolução de quantidade, valor e ticket médio das transações nacionais.
g3_dados <- painel %>% transmute(data, quantidade = quantidade_total, valor = valor_total,
                                  ticket_medio = valor_total / quantidade_total) %>%
  pivot_longer(-data, names_to = "indicador", values_to = "valor") %>%
  mutate(indicador = recode(indicador, quantidade = "Quantidade de transações",
                            valor = "Valor das transações", ticket_medio = "Ticket médio"))
g3 <- ggplot(g3_dados, aes(data, valor, color = indicador)) + geom_line(linewidth = .8, na.rm = TRUE) +
  facet_wrap(~indicador, scales = "free_y", ncol = 1, axes = "all") +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  labs(title = "Evolução das transações PIX", subtitle = "Quantidade, valor transacionado e ticket médio mensal.",
       x = "Mês", y = NULL, color = NULL) + theme_minimal(base_size = 11) + theme(legend.position = "none")

# G4: participação percentual do valor pago por região.
regioes <- municipios_graficos %>% filter(!is.na(Regiao)) %>%
  mutate(valor_pago = rowSums(across(any_of(c("VL_PagadorPF", "VL_PagadorPJ"))), na.rm = TRUE)) %>%
  group_by(Regiao) %>% summarise(valor_pago = sum(valor_pago, na.rm = TRUE), .groups = "drop") %>%
  mutate(participacao_pct = 100 * valor_pago / sum(valor_pago)) %>% arrange(desc(participacao_pct))
g4 <- ggplot(regioes, aes(reorder(Regiao, participacao_pct), participacao_pct)) +
  geom_col(fill = "#2878A5") + coord_flip() + geom_text(aes(label = paste0(round(participacao_pct, 1), "%")), hjust = -0.1) +
  scale_y_continuous(limits = c(0, max(regioes$participacao_pct, 1) * 1.15), labels = function(x) paste0(x, "%")) +
  labs(title = "Participação do valor pago por região", x = NULL, y = "% do valor pago") +
  theme_minimal(base_size = 11)

# G5: dez cidades com maior valor pago e participação no total.
cidades <- municipios_graficos %>% filter(!is.na(Municipio_Ibge)) %>%
  mutate(valor_pago = rowSums(across(any_of(c("VL_PagadorPF", "VL_PagadorPJ"))), na.rm = TRUE)) %>%
  group_by(Municipio_Ibge, Municipio, Estado) %>% summarise(valor_pago = sum(valor_pago, na.rm = TRUE), .groups = "drop") %>%
  mutate(participacao_pct = 100 * valor_pago / sum(valor_pago)) %>% arrange(desc(valor_pago)) %>% slice_head(n = 10)
g5 <- ggplot(cidades, aes(reorder(paste(Municipio, Estado, sep = " / "), participacao_pct), participacao_pct)) +
  geom_col(fill = "#4C78A8") + coord_flip() + geom_text(aes(label = paste0(round(participacao_pct, 2), "%")), hjust = -0.1, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, .15)), labels = function(x) paste0(x, "%")) +
  labs(title = "Dez cidades com maior volume de pagamentos", subtitle = "Participação no valor total pago no período.",
       x = NULL, y = "% do valor pago") + theme_minimal(base_size = 11)

# G6: Pareto dos municípios pagadores.
pareto <- cidades_base <- municipios_graficos %>% filter(!is.na(Municipio_Ibge)) %>%
  mutate(valor_pago = rowSums(across(any_of(c("VL_PagadorPF", "VL_PagadorPJ"))), na.rm = TRUE)) %>%
  group_by(Municipio_Ibge) %>% summarise(valor_pago = sum(valor_pago, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(valor_pago), valor_pago >= 0) %>% arrange(desc(valor_pago)) %>%
  mutate(ordem = row_number(), municipio_pct = 100 * ordem / n(), valor_pct = 100 * valor_pago / sum(valor_pago), acumulado_pct = cumsum(valor_pct))

# g6: dimensão com melhor cobertura categórica entre as colunas disponíveis.
if (nrow(perfil_escolhido)) {
  top_categorias <- perfil_escolhido %>% group_by(categoria) %>%
    summarise(valor = sum(valor_total, na.rm = TRUE), .groups = "drop") %>%
    slice_max(valor, n = 8, with_ties = FALSE) %>% pull(categoria)
  serie_categoria <- perfil_escolhido %>% filter(categoria %in% top_categorias) %>%
    group_by(AnoMes, categoria) %>% summarise(valor_total = sum(valor_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(data = as.Date(paste0(AnoMes, "01"), format = "%Y%m%d"))
  g6 <- ggplot(serie_categoria, aes(data, valor_total, color = categoria)) +
    geom_line(linewidth = .75, na.rm = TRUE) + scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
    scale_y_continuous(labels = function(x) paste0("R$ ", fmt_num(x))) +
    labs(title = paste("A evolução do valor transacionado varia por", dimensao_escolhida),
      subtitle = "Até oito categorias com maior valor acumulado; agregação mensal nacional.",
      x = "Mês", y = "Valor transacionado (R$)", color = "Categoria",
      caption = "Dimensão selecionada pela variedade de categorias observadas; série descritiva, sem inferência causal.") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
} else {
  g6 <- ggplot() + annotate("text", x = 0, y = 0, label = "Não há dimensão categórica com cobertura suficiente") +
    labs(title = "Evolução temporal por dimensão categórica", subtitle = "Dados adequados não disponíveis.",
         x = NULL, y = NULL, caption = "Nenhum resultado foi inventado.") + theme_void()
}

# G6: Pareto completo; linha de 80% e marca dos 20% de municípios.
pareto_20 <- max(1, ceiling(nrow(pareto) * .20))
valor_20 <- pareto %>% slice_head(n = pareto_20) %>% summarise(x = sum(valor_pct)) %>% pull(x)
g6 <- ggplot(pareto, aes(municipio_pct, acumulado_pct)) + geom_col(aes(y = valor_pct), fill = "#BDBDBD", alpha = .7) +
  geom_line(linewidth = 1, color = "#D62728") + geom_hline(yintercept = 80, linetype = "dashed") +
  geom_vline(xintercept = 20, linetype = "dashed") +
  annotate("text", x = 20, y = 84, label = paste0("20% dos municípios: ", round(valor_20, 1), "% do valor"), hjust = 0, size = 3.5) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) + scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Pareto dos municípios pagadores", subtitle = "Barras: participação dos municípios; linha: valor acumulado.",
       x = "% acumulado de municípios", y = "% acumulado do valor pago") + theme_minimal(base_size = 11)

# G7: ponto de corte dos municípios necessários para alcançar 80% do valor.
corte80 <- pareto %>% filter(acumulado_pct >= 80) %>% slice(1)
g7 <- ggplot(pareto, aes(ordem, acumulado_pct)) + geom_line(color = "#7A5195", linewidth = 1) +
  geom_hline(yintercept = 80, linetype = "dashed") +
  geom_vline(xintercept = if (nrow(corte80)) corte80$ordem else NA, linetype = "dashed", color = "#D62728") +
  geom_point(data = corte80, size = 3, color = "#D62728") +
  geom_text(data = corte80, aes(label = paste0(ordem, "/", nrow(pareto), " municípios (", round(100 * ordem / nrow(pareto), 1), "%)")),
            hjust = -.05, vjust = -0.8, size = 3.5, color = "#D62728") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Corte acumulativo dos municípios pagadores", subtitle = "Quantos municípios respondem por 80% do valor pago?",
       x = "Número acumulado de municípios", y = "% acumulado do valor pago") + theme_minimal(base_size = 11)

# Indicadores municipais mensais para Itajubá/MG.
itajuba <- municipios_graficos %>% filter(sem_acento(Municipio) == "ITAJUBA", sem_acento(Estado) %in% c("MG", "MINAS GERAIS")) %>%
  group_by(AnoMes, data_mes) %>% summarise(across(any_of(c("VL_PagadorPF", "VL_PagadorPJ", "VL_RecebedorPF", "VL_RecebedorPJ",
    "QT_PagadorPF", "QT_PagadorPJ", "QT_RecebedorPF", "QT_RecebedorPJ", "QT_PES_PagadorPF", "QT_PES_PagadorPJ",
    "QT_PES_RecebedorPF", "QT_PES_RecebedorPJ")), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>% arrange(data_mes)
itajuba_long <- function(cols, nomes, titulo, ylab, arquivo = NULL) {
  d <- itajuba %>% select(data_mes, all_of(cols)) %>% pivot_longer(-data_mes, names_to = "serie", values_to = "valor") %>%
    mutate(serie = recode(serie, !!!setNames(nomes, cols))) %>% group_by(serie) %>%
    arrange(data_mes, .by_group = TRUE) %>% mutate(crescimento_pct = 100 * (valor / first(valor) - 1)) %>% ungroup()
  ggplot(d, aes(data_mes, valor, color = serie)) + geom_line(linewidth = .8, na.rm = TRUE) + geom_point() +
  geom_text(data = d %>% group_by(serie) %>% slice_tail(n = 1), aes(label = ifelse(is.finite(crescimento_pct),
                               paste0(ifelse(crescimento_pct >= 0, "+", ""), round(crescimento_pct, 1), "% vs 1º mês"), "")),
              vjust = -0.7, size = 2.8, show.legend = FALSE, na.rm = TRUE) +
    scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y", expand = expansion(mult = c(.02, .08))) +
    labs(title = titulo, subtitle = "Itajubá/MG; rótulos mostram crescimento frente ao mês anterior.", x = "Mês", y = ylab, color = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
}
g8_1 <- itajuba_long(c("VL_PagadorPF", "VL_PagadorPJ", "VL_RecebedorPF", "VL_RecebedorPJ"),
  c("Pagador PF", "Pagador PJ", "Recebedor PF", "Recebedor PJ"), "Itajubá: valor PIX por pagador/recebedor", "Valor (R$)")
g8_2 <- itajuba_long(c("QT_PagadorPF", "QT_PagadorPJ", "QT_RecebedorPF", "QT_RecebedorPJ"),
  c("Pagador PF", "Pagador PJ", "Recebedor PF", "Recebedor PJ"), "Itajubá: quantidade de PIX por pagador/recebedor", "Quantidade de PIX")
g8_3 <- itajuba_long(c("QT_PES_PagadorPF", "QT_PES_PagadorPJ", "QT_PES_RecebedorPF", "QT_PES_RecebedorPJ"),
  c("Pagador PF", "Pagador PJ", "Recebedor PF", "Recebedor PJ"), "Itajubá: pessoas por pagador/recebedor", "Quantidade de pessoas")

# G9: Itajubá, Minas Gerais e Brasil em índices base 100.
mg <- municipios_graficos %>% filter(sem_acento(Estado) %in% c("MG", "MINAS GERAIS")) %>% group_by(AnoMes, data_mes) %>%
  summarise(valor = sum(VL_PagadorPF + VL_PagadorPJ, na.rm = TRUE), pessoas = sum(QT_PES_PagadorPF + QT_PES_PagadorPJ, na.rm = TRUE), .groups = "drop")
br <- municipios_graficos %>% group_by(AnoMes, data_mes) %>%
  summarise(valor = sum(VL_PagadorPF + VL_PagadorPJ, na.rm = TRUE), pessoas = sum(QT_PES_PagadorPF + QT_PES_PagadorPJ, na.rm = TRUE), .groups = "drop")
it <- itajuba %>% transmute(AnoMes, data_mes, valor = VL_PagadorPF + VL_PagadorPJ, pessoas = QT_PES_PagadorPF + QT_PES_PagadorPJ)
g9_dados <- bind_rows(Itajubá = it, `Minas Gerais` = mg, Brasil = br, .id = "local") %>%
  pivot_longer(c(valor, pessoas), names_to = "medida", values_to = "valor") %>% group_by(local, medida) %>%
  arrange(data_mes, .by_group = TRUE) %>% mutate(indice = indice_base100(valor), crescimento_pct = 100 * (indice / first(indice) - 1)) %>% ungroup()
g9 <- function(medida, titulo, ylab) {
  d <- filter(g9_dados, medida == !!medida)
  ggplot(d, aes(data_mes, indice, color = local)) + geom_line(linewidth = .9) +
    geom_text(data = d %>% group_by(local) %>% slice_tail(n = 1), aes(label = paste0("+", round(crescimento_pct, 1), "%")), hjust = -.1, show.legend = FALSE) +
    scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
    labs(title = titulo, subtitle = "Índice base 100 no primeiro mês comum; rótulo final mostra crescimento acumulado.", x = "Mês", y = ylab, color = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
}
g9_1 <- g9("valor", "Crescimento relativo do valor transacionado", "Índice base 100")
g9_2 <- g9("pessoas", "Crescimento relativo da quantidade de pessoas", "Índice base 100")

# Imprime primeiro no dispositivo ativo e depois exporta exatamente o mesmo
# objeto. Em Rscript sem dispositivo interativo, print() pode não abrir janela;
# ggsave() cria seus próprios dispositivos e continua a exportação normalmente.
graficos <- list(
  list(obj = g1, nome = "grafico_01_evolucao_contestacoes_transacoes", w = 12, h = 7),
  list(obj = g2, nome = "grafico_02_variacoes_mensais_atipicos", w = 12, h = 7),
  list(obj = g3, nome = "grafico_03_evolucao_quantidade_valor_ticket", w = 12, h = 8),
  list(obj = g4, nome = "grafico_04_pagamentos_por_regiao", w = 11, h = 7),
  list(obj = g5, nome = "grafico_05_top10_cidades_pagamentos", w = 11, h = 7),
  list(obj = g6, nome = "grafico_06_pareto_municipios_pagadores", w = 11, h = 7),
  list(obj = g7, nome = "grafico_07_corte_80_municipios", w = 11, h = 7),
  list(obj = g8_1, nome = "grafico_08_1_itajuba_valor", w = 12, h = 7),
  list(obj = g8_2, nome = "grafico_08_2_itajuba_quantidade_pix", w = 12, h = 7),
  list(obj = g8_3, nome = "grafico_08_3_itajuba_pessoas", w = 12, h = 7),
  list(obj = g9_1, nome = "grafico_09_1_crescimento_valor_itajuba_mg_brasil", w = 12, h = 7),
  list(obj = g9_2, nome = "grafico_09_2_crescimento_pessoas_itajuba_mg_brasil", w = 12, h = 7)
)
for (i in seq_along(graficos)) {
  obj <- graficos[[i]]$obj
  nome <- graficos[[i]]$nome
  print(obj)
  ggsave(file.path(dir_graficos, paste0(nome, ".png")), plot = obj,
         width = graficos[[i]]$w, height = graficos[[i]]$h, units = "in", dpi = 300, bg = "white")
  tryCatch(ggsave(file.path(dir_graficos, paste0(nome, ".pdf")), plot = obj,
                 width = graficos[[i]]$w, height = graficos[[i]]$h, units = "in", bg = "white"),
           error = function(e) message("PDF opcional não gerado para ", nome, ": ", conditionMessage(e)))
}

# 6. RESUMO DOS RESULTADOS E RELATÓRIO ---------------------------------------
resumo <- tibble(
  indicador = c("Meses comuns nacional", "Municípios identificados", "Dimensão categórica escolhida",
                "Variações mensais atípicas pelo IQR", "Gráficos PNG gerados"),
  valor = c(as.character(nrow(painel)), as.character(nrow(municipios_periodo)),
            ifelse(is.na(dimensao_escolhida), "Não disponível", dimensao_escolhida),
            as.character(nrow(atipicos)), as.character(length(graficos)))
)
write_csv(resumo, file.path(dir_res, "resumo_pipeline.csv"))

fmt_md <- function(x) {
  if (!nrow(x)) return("Nenhum resultado disponível.")
  paste(capture.output(print(x, n = Inf, width = Inf)), collapse = "\n")
}
relatorio <- c(
  "# Relatório do pipeline analítico PIX", "",
  paste0("**Execução:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "",
  "## Metodologia", "",
  "As três bases foram lidas usando os tipos, a localidade e os códigos de ausência definidos em `R/config.R`. Os CSVs originais não foram alterados. Foram produzidos diagnósticos de esquema, ausências, parsing, dimensões e cobertura temporal. `AnoMes` foi validado como `YYYYMM`; meses inválidos foram registrados e não entram nas agregações temporais.", "",
  "As séries nacionais foram agregadas por mês antes da junção, evitando junções muitos-para-muitos. A integração é exclusivamente por `AnoMes` e compara apenas meses comuns. A base municipal foi agregada por município identificado e a base nacional por dimensões categóricas disponíveis.", "",
  "## Resumo", "", paste0("- Meses nacionais em comum: **", nrow(painel), "**."),
  paste0("- Municípios agregados: **", nrow(municipios_periodo), "**."),
  paste0("- Dimensão selecionada para o gráfico 6: **", ifelse(is.na(dimensao_escolhida), "não disponível", dimensao_escolhida), "**."),
  paste0("- Meses-indicador sinalizados pelo IQR: **", nrow(atipicos), "**."), "",
  "## Hipóteses e resultados", "",
  paste0("```\n", fmt_md(resultados_hipoteses), "\n```"), "",
  "Os testes são exploratórios. Um p-valor pequeno não comprova causalidade, nem valida contestações como fraude. Tendências log-lineares são ajustadas somente a valores positivos e não constituem previsão.", "",
  "## Gráficos produzidos", "",
  "1. Evolução comparada das contestações e transações (índice base 100).",
  "2. Variações percentuais mensais e observações atípicas pelo IQR.",
  "3. Até dez municípios com maior valor PIX pago.",
  "4. Concentração acumulada do valor pago por município.",
  "5. Composição por forma de iniciação, comparando participação em valor e quantidade quando a dimensão está disponível.",
  paste0("6. Evolução temporal por categoria; dimensão escolhida: ", ifelse(is.na(dimensao_escolhida), "não disponível", dimensao_escolhida), "."), "",
  "## Limitações", "",
  "- PIX contestado ou contestação aceita não equivale a fraude comprovada.",
  "- As bases não possuem chave transacional compartilhada; associação nacional é apenas mensal e não identifica causa.",
  "- Correlação entre séries temporais pode refletir tendência, sazonalidade ou autocorrelação; não é evidência causal.",
  "- Ausências são documentadas, não imputadas. Somas ignoram `NA`, e resultados devem ser interpretados junto aos diagnósticos.",
  "- Municípios sem código IBGE não são atribuídos; isso pode alterar a participação e concentração calculadas.",
  "- Se a coluna de forma de iniciação ou categorias adequadas estiver ausente, o gráfico correspondente registra indisponibilidade em vez de inventar dados.", "",
  "## Arquivos", "",
  "- Diagnósticos: `outputs/pipeline/diagnostico/`.",
  "- Resultados: `outputs/pipeline/resultados/`.",
  "- Gráficos PNG (300 dpi) e PDFs opcionais: `outputs/graficos/`."
)
writeLines(relatorio, file.path(dir_docs, "relatorio_pipeline_fraude_pix.md"), useBytes = TRUE)

# Verificações finais: todos os PNGs definidos em `graficos` devem existir e ter tamanho positivo.
pngs <- file.path(dir_graficos, paste0(vapply(graficos, `[[`, character(1), "nome"), ".png"))
validacao_final <- tibble(arquivo = basename(pngs), existe = file.exists(pngs),
                          bytes = ifelse(file.exists(pngs), file.info(pngs)$size, NA_real_))
write_csv(validacao_final, file.path(dir_diag, "validacao_graficos.csv"))
if (!all(validacao_final$existe & validacao_final$bytes > 0)) {
  stop("Falha ao gerar um ou mais PNGs; consulte outputs/pipeline/diagnostico/validacao_graficos.csv")
}
message("Pipeline concluído. Meses comuns: ", nrow(painel),
        "; municípios: ", nrow(municipios_periodo),
        "; ", nrow(validacao_final), " PNGs validados em ", dir_graficos,
        ". Relatório: docs/relatorio_pipeline_fraude_pix.md")
