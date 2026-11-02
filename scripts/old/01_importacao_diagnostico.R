# Executar da raiz: Rscript scripts/01_importacao_diagnostico.R
# Os arquivos grandes são lidos em blocos; somente resumos e amostras ficam na memória.
suppressPackageStartupMessages({ library(readr); library(dplyr); library(tidyr) })
source("R/config.R")
dir.create(diretorio_saida("data", "amostras"), recursive = TRUE, showWarnings = FALSE)
dir.create(diretorio_saida("outputs", "diagnosticos"), recursive = TRUE, showWarnings = FALSE)
dir.create(diretorio_saida("outputs", "tabelas"), recursive = TRUE, showWarnings = FALSE)
amostra_e_esquema <- function(arquivo, tipos, nome, n = 10000) {
  dados <- read_csv(arquivo_bruto(arquivo), col_types = tipos, locale = locale_pix, na = na_pix, n_max = n, progress = FALSE, show_col_types = FALSE)
  write_csv(dados, diretorio_saida("data", "amostras", paste0(nome, "_amostra.csv")))
  tibble(base = nome, coluna = names(dados), tipo_R = vapply(dados, function(x) class(x)[1], character(1)), ausentes_na_amostra = vapply(dados, function(x) sum(is.na(x)), numeric(1)), problemas_parse_amostra = nrow(problems(dados)))
}
processar_em_blocos <- function(arquivo, tipos, nome, resumir_bloco) {
  perfil <- tibble(); mensal <- tibble(); linhas <- 0L
  callback <- DataFrameCallback$new(function(bloco, pos) {
    linhas <<- linhas + nrow(bloco)
    perfil <<- bind_rows(perfil, tibble(base = nome, coluna = names(bloco), ausentes = vapply(bloco, function(x) sum(is.na(x)), numeric(1)), registros = nrow(bloco)))
    mensal <<- bind_rows(mensal, resumir_bloco(bloco)); invisible()
  })
  read_csv_chunked(arquivo_bruto(arquivo), callback = callback, chunk_size = 50000, col_types = tipos, locale = locale_pix, na = na_pix, progress = FALSE)
  list(perfil = perfil %>% group_by(base, coluna) %>% summarise(registros = sum(registros), ausentes = sum(ausentes), percentual_ausentes = 100 * ausentes / registros, .groups = "drop"), mensal = mensal %>% group_by(AnoMes) %>% summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop"), linhas = linhas)
}
esquemas <- bind_rows(amostra_e_esquema("estatisticas_fraudes_pix.csv", tipos_fraudes, "fraudes"), amostra_e_esquema("estatisticas_transacoes_pix.csv", tipos_transacoes, "transacoes"), amostra_e_esquema("transacoes_pix_municipio.csv", tipos_municipios, "municipios"))
write_csv(esquemas, diretorio_saida("outputs", "diagnosticos", "esquema_amostral.csv"))
fraudes <- read_csv(arquivo_bruto("estatisticas_fraudes_pix.csv"), col_types = tipos_fraudes, locale = locale_pix, na = na_pix, progress = FALSE, show_col_types = FALSE)
perfil_fraudes <- tibble(base = "fraudes", coluna = names(fraudes), registros = nrow(fraudes), ausentes = vapply(fraudes, function(x) sum(is.na(x)), numeric(1))) %>% mutate(percentual_ausentes = 100 * ausentes / registros)
transacoes <- processar_em_blocos("estatisticas_transacoes_pix.csv", tipos_transacoes, "transacoes", function(x) x %>% group_by(AnoMes) %>% summarise(linhas = n(), valor_total = sum(VALOR, na.rm = TRUE), quantidade_total = sum(QUANTIDADE, na.rm = TRUE), .groups = "drop"))
municipios <- processar_em_blocos("transacoes_pix_municipio.csv", tipos_municipios, "municipios", function(x) x %>% group_by(AnoMes) %>% summarise(linhas = n(), valor_pagador = sum(VL_PagadorPF, VL_PagadorPJ, na.rm = TRUE), valor_recebedor = sum(VL_RecebedorPF, VL_RecebedorPJ, na.rm = TRUE), quantidade_pagador = sum(QT_PagadorPF, QT_PagadorPJ, na.rm = TRUE), quantidade_recebedor = sum(QT_RecebedorPF, QT_RecebedorPJ, na.rm = TRUE), .groups = "drop"))
perfil <- bind_rows(perfil_fraudes, transacoes$perfil, municipios$perfil) %>% arrange(base, coluna)
dimensoes <- tibble(base = c("fraudes", "transacoes", "municipios"), registros = c(nrow(fraudes), transacoes$linhas, municipios$linhas), colunas = c(ncol(fraudes), nrow(esquemas %>% filter(base == "transacoes")), nrow(esquemas %>% filter(base == "municipios"))))
write_csv(perfil, diretorio_saida("outputs", "diagnosticos", "perfil_ausencias.csv")); write_csv(dimensoes, diretorio_saida("outputs", "diagnosticos", "dimensoes.csv"))
write_csv(fraudes, diretorio_saida("outputs", "tabelas", "fraudes_mensal.csv")); write_csv(transacoes$mensal, diretorio_saida("outputs", "tabelas", "transacoes_mensal.csv")); write_csv(municipios$mensal, diretorio_saida("outputs", "tabelas", "municipios_mensal.csv"))
message("Diagnóstico concluído. Registros processados: ", paste(dimensoes$registros, collapse = ", "))
