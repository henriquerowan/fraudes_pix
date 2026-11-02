# Configuração central do projeto
raiz_projeto <- normalizePath(".", mustWork = TRUE)
arquivo_bruto <- function(nome) file.path(raiz_projeto, nome)
diretorio_saida <- function(...) file.path(raiz_projeto, ...)
locale_pix <- readr::locale(decimal_mark = ",", grouping_mark = ".")
na_pix <- c("", "null")
# Esquemas explícitos preservam códigos IBGE e tornam a importação reprodutível.
tipos_fraudes <- readr::cols(AnoMes = readr::col_character(), .default = readr::col_double())
tipos_transacoes <- readr::cols(AnoMes = readr::col_character(), PAG_PFPJ = readr::col_character(), REC_PFPJ = readr::col_character(), PAG_REGIAO = readr::col_character(), REC_REGIAO = readr::col_character(), PAG_IDADE = readr::col_character(), REC_IDADE = readr::col_character(), FORMAINICIACAO = readr::col_character(), NATUREZA = readr::col_character(), FINALIDADE = readr::col_character(), VALOR = readr::col_double(), QUANTIDADE = readr::col_double())
tipos_municipios <- readr::cols(AnoMes = readr::col_character(), Municipio_Ibge = readr::col_character(), Municipio = readr::col_character(), Estado_Ibge = readr::col_character(), Estado = readr::col_character(), Sigla_Regiao = readr::col_character(), Regiao = readr::col_character(), .default = readr::col_double())
