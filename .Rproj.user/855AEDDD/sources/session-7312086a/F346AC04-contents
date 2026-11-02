# Pré-requisito: Rscript scripts/01_importacao_diagnostico.R
suppressPackageStartupMessages({ library(readr); library(dplyr); library(tidyr); library(ggplot2) })
source("R/config.R")
ler_saida <- function(...) read_csv(diretorio_saida(...), show_col_types = FALSE)
fraudes <- ler_saida("outputs", "tabelas", "fraudes_mensal.csv")
transacoes <- ler_saida("outputs", "tabelas", "transacoes_mensal.csv")
municipios <- ler_saida("outputs", "tabelas", "municipios_mensal.csv")
# Junção apenas mensal/nacional: não há chave de município, região ou modalidade em fraudes.
painel_fraude_transacoes <- fraudes %>% inner_join(transacoes, by = "AnoMes") %>% mutate(taxa_pix_contestados_por_milhao = 1e6 * QtdePixcontestados / quantidade_total, valor_medio_transacao = valor_total / quantidade_total)
cobertura_temporal <- bind_rows(transacoes %>% transmute(base = "transacoes", AnoMes), municipios %>% transmute(base = "municipios", AnoMes), fraudes %>% transmute(base = "fraudes", AnoMes)) %>% distinct() %>% mutate(disponivel = TRUE) %>% pivot_wider(names_from = base, values_from = disponivel, values_fill = FALSE)
write_csv(painel_fraude_transacoes, diretorio_saida("outputs", "tabelas", "painel_fraude_transacoes_mensal.csv"))
write_csv(cobertura_temporal, diretorio_saida("outputs", "diagnosticos", "cobertura_temporal.csv"))
message("Painel mensal criado com ", nrow(painel_fraude_transacoes), " meses em comum.")
