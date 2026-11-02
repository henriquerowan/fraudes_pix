# =============================================================================
# SCRIPT 01 — IMPORTAÇÃO, DIAGNÓSTICO E AGREGAÇÃO INICIAL DAS BASES DO PIX
# =============================================================================
# Descrição:
#   Faz a leitura inicial das três bases de dados do projeto, registra amostras
#   para inspeção do esquema, avalia valores ausentes e problemas de parsing e
#   produz tabelas mensais agregadas para as etapas analíticas seguintes.
#
# Objetivos:
#   1. Inspecionar uma amostra de cada arquivo e registrar nomes/tipos de colunas;
#   2. processar arquivos grandes em blocos, reduzindo o uso de memória;
#   3. quantificar ausências por coluna e dimensões básicas de cada base;
#   4. agregar valores e quantidades por mês quando aplicável;
#   5. salvar tabelas padronizadas para os scripts 02, 03 e 04.
#
# Execução:
#   Executar a partir da raiz do projeto:
#     Rscript scripts/01_importacao_diagnostico.R
#
# Entradas:
#   - estatisticas_fraudes_pix.csv
#   - estatisticas_transacoes_pix.csv
#   - transacoes_pix_municipio.csv
#   - R/config.R, que define caminhos, tipos de colunas, localidade e códigos
#     reconhecidos como valores ausentes.
#
# Saídas:
#   - data/amostras/fraudes_amostra.csv
#   - data/amostras/transacoes_amostra.csv
#   - data/amostras/municipios_amostra.csv
#   - outputs/diagnosticos/esquema_amostral.csv
#   - outputs/diagnosticos/perfil_ausencias.csv
#   - outputs/diagnosticos/dimensoes.csv
#   - outputs/tabelas/fraudes_mensal.csv
#   - outputs/tabelas/transacoes_mensal.csv
#   - outputs/tabelas/municipios_mensal.csv
#
# Observações metodológicas:
#   - A função amostra_e_esquema() lê até 10.000 linhas por base para gerar uma
#     amostra e um diagnóstico preliminar de tipos e problemas de parsing.
#   - As bases de transações e municípios são processadas em blocos de 50.000
#     linhas. Os registros brutos não são acumulados integralmente na memória;
#     são mantidos resumos por bloco e, ao final, consolidados.
#   - A base de contestações é lida integralmente no código original deste
#     script; seu tamanho pode afetar o consumo de memória.
#   - Os indicadores de ausência das bases processadas em blocos são calculados
#     somando ausências e registros de cada bloco.
#   - As agregações usam as colunas e os tipos definidos em R/config.R. Alterar
#     os arquivos de origem ou seus nomes de coluna pode exigir mudanças aqui.
# =============================================================================

# Carrega os pacotes usados para leitura, transformação e gravação dos dados.
# As mensagens rotineiras de inicialização dos pacotes ficam ocultas.
suppressPackageStartupMessages({
  library(readr)  # Leitura e escrita de CSV; leitura em blocos.
  library(dplyr)  # Transformação, agrupamento e sumarização.
  library(tidyr)  # Utilitários de organização tabular.
})

# Importa as configurações compartilhadas pelo projeto.
# Entre outros objetos, R/config.R deve definir:
# - arquivo_bruto(nome): caminho para um arquivo de entrada;
# - diretorio_saida(...): caminho para um arquivo ou diretório de saída;
# - tipos_fraudes, tipos_transacoes, tipos_municipios: esquemas de leitura;
# - locale_pix: convenções de leitura de números e texto;
# - na_pix: strings que devem ser interpretadas como ausentes.
#
# O uso de caminhos relativos pressupõe que o script seja iniciado na raiz
# do projeto, e não diretamente de dentro da pasta scripts/.
source("R/config.R")

# Cria os diretórios necessários antes de salvar qualquer resultado.
# recursive = TRUE cria também diretórios intermediários.
# showWarnings = FALSE evita avisos quando os diretórios já existem.
dir.create(
  diretorio_saida("data", "amostras"),
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  diretorio_saida("outputs", "diagnosticos"),
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  diretorio_saida("outputs", "tabelas"),
  recursive = TRUE,
  showWarnings = FALSE
)

# -----------------------------------------------------------------------------
# 1. AMOSTRAGEM E DIAGNÓSTICO DO ESQUEMA
# -----------------------------------------------------------------------------

# Lê uma amostra inicial de um arquivo, salva essa amostra em CSV e retorna
# um resumo do esquema observado.
#
# Argumentos:
# - arquivo: nome do arquivo bruto, resolvido por arquivo_bruto();
# - tipos: especificação de colunas/tipos definida em R/config.R;
# - nome: nome curto usado nos arquivos e no diagnóstico;
# - n: número máximo de linhas da amostra (padrão: 10.000).
#
# Resultado:
# - grava <nome>_amostra.csv em data/amostras/;
# - retorna uma linha por coluna, com tipo R, número de ausências na amostra
#   e quantidade de problemas de parsing encontrados pelo readr.
#
# Importante:
#   O diagnóstico de ausências e parsing desta função é amostral; não representa
#   necessariamente a qualidade de todas as linhas do arquivo de origem.
amostra_e_esquema <- function(arquivo, tipos, nome, n = 10000) {
  dados <- read_csv(
    arquivo_bruto(arquivo),
    col_types = tipos,
    locale = locale_pix,
    na = na_pix,
    n_max = n,
    progress = FALSE,
    show_col_types = FALSE
  )

  # Persiste a amostra para inspeção manual sem precisar reler o arquivo grande.
  write_csv(
    dados,
    diretorio_saida("data", "amostras", paste0(nome, "_amostra.csv"))
  )

  # Monta um resumo com uma linha por coluna da amostra.
  # class(x)[1] registra a primeira classe R do vetor; is.na() conta os valores
  # ausentes já reconhecidos durante a leitura; problems() lista falhas de
  # interpretação identificadas pelo readr.
  tibble(
    base = nome,
    coluna = names(dados),
    tipo_R = vapply(dados, function(x) class(x)[1], character(1)),
    ausentes_na_amostra = vapply(
      dados, function(x) sum(is.na(x)), numeric(1)
    ),
    problemas_parse_amostra = nrow(problems(dados))
  )
}

# -----------------------------------------------------------------------------
# 2. PROCESSAMENTO EM BLOCOS E AGREGAÇÃO MENSAL
# -----------------------------------------------------------------------------

# Processa um arquivo CSV grande em blocos e calcula dois conjuntos de resumos:
# 1. perfil por coluna: total de registros e ausências;
# 2. mensal: resultado da função resumir_bloco aplicada a cada bloco e depois
#    somado por AnoMes.
#
# Argumentos:
# - arquivo: nome do arquivo bruto;
# - tipos: esquema de leitura da base;
# - nome: identificador da base para o diagnóstico;
# - resumir_bloco: função que recebe um bloco e retorna uma tabela agregada.
#
# O callback é chamado pelo read_csv_chunked() para cada bloco de dados.
# O operador <<- atualiza os acumuladores definidos no escopo externo da função.
# O argumento pos é fornecido pelo callback do readr, mas não é usado aqui.
#
# Ao final, a função retorna uma lista com:
# - perfil: registros, ausências e percentual de ausências por coluna;
# - mensal: indicadores agregados por mês;
# - linhas: número total de linhas lidas.
processar_em_blocos <- function(arquivo, tipos, nome, resumir_bloco) {
  perfil <- tibble()
  mensal <- tibble()
  linhas <- 0L

  # Callback executado para cada bloco lido do arquivo.
  callback <- DataFrameCallback$new(
    function(bloco, pos) {
      # Conta quantas linhas foram processadas no bloco atual.
      linhas <<- linhas + nrow(bloco)

      # Registra ausências por coluna no bloco atual.
      # O total final será obtido somando os resultados de todos os blocos.
      perfil <<- bind_rows(
        perfil,
        tibble(
          base = nome,
          coluna = names(bloco),
          ausentes = vapply(
            bloco, function(x) sum(is.na(x)), numeric(1)
          ),
          registros = nrow(bloco)
        )
      )

      # A função passada em resumir_bloco define quais métricas mensais serão
      # calculadas para cada tipo de base.
      mensal <<- bind_rows(mensal, resumir_bloco(bloco))

      # O callback não precisa imprimir o conteúdo de cada bloco.
      invisible()
    }
  )

  # Lê o arquivo em blocos de 50.000 linhas, aplicando o esquema e as regras
  # de leitura definidos na configuração do projeto.
  read_csv_chunked(
    arquivo_bruto(arquivo),
    callback = callback,
    chunk_size = 50000,
    col_types = tipos,
    locale = locale_pix,
    na = na_pix,
    progress = FALSE
  )

  # Consolida o perfil por coluna, somando os resultados dos blocos.
  # percentual_ausentes é a proporção de valores NA entre as células lidas
  # daquela coluna. registros corresponde ao número de linhas da base.
  perfil_final <- perfil %>%
    group_by(base, coluna) %>%
    summarise(
      registros = sum(registros),
      ausentes = sum(ausentes),
      percentual_ausentes = 100 * ausentes / registros,
      .groups = "drop"
    )

  # Consolida as métricas mensais calculadas por bloco. Como um mesmo mês pode
  # aparecer em diversos blocos, os valores numéricos são somados por AnoMes.
  # Este passo pressupõe que as colunas numéricas retornadas por resumir_bloco
  # sejam métricas aditivas (por exemplo, contagens e valores totais).
  mensal_final <- mensal %>%
    group_by(AnoMes) %>%
    summarise(
      across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )

  list(
    perfil = perfil_final,
    mensal = mensal_final,
    linhas = linhas
  )
}

# Executa a leitura amostral nas três bases.
# bind_rows() combina os resumos de esquema em uma única tabela.
esquemas <- bind_rows(
  amostra_e_esquema(
    "estatisticas_fraudes_pix.csv",
    tipos_fraudes,
    "fraudes"
  ),
  amostra_e_esquema(
    "estatisticas_transacoes_pix.csv",
    tipos_transacoes,
    "transacoes"
  ),
  amostra_e_esquema(
    "transacoes_pix_municipio.csv",
    tipos_municipios,
    "municipios"
  )
)

# Salva o diagnóstico amostral de nomes, tipos, ausências e problemas de parsing.
write_csv(
  esquemas,
  diretorio_saida("outputs", "diagnosticos", "esquema_amostral.csv")
)

# -----------------------------------------------------------------------------
# 3. LEITURA E PERFIL COMPLETO DA BASE DE CONTESTAÇÕES
# -----------------------------------------------------------------------------

# No script original, a base de contestações é carregada integralmente.
# Diferentemente das outras duas bases, não é processada por blocos nesta etapa.
# O esquema, a localidade e os valores ausentes seguem R/config.R.
fraudes <- read_csv(
  arquivo_bruto("estatisticas_fraudes_pix.csv"),
  col_types = tipos_fraudes,
  locale = locale_pix,
  na = na_pix,
  progress = FALSE,
  show_col_types = FALSE
)

# Calcula o número de registros e ausências de cada coluna na base completa.
# Este perfil é completo para as linhas lidas, não apenas para a amostra.
perfil_fraudes <- tibble(
  base = "fraudes",
  coluna = names(fraudes),
  registros = nrow(fraudes),
  ausentes = vapply(fraudes, function(x) sum(is.na(x)), numeric(1))
) %>%
  mutate(percentual_ausentes = 100 * ausentes / registros)

# -----------------------------------------------------------------------------
# 4. AGREGAÇÃO MENSAL DA BASE NACIONAL DE TRANSAÇÕES
# -----------------------------------------------------------------------------

# Processa estatisticas_transacoes_pix.csv em blocos.
#
# Para cada mês, calcula:
# - linhas: quantidade de registros da base detalhada no mês;
# - valor_total: soma da coluna VALOR;
# - quantidade_total: soma da coluna QUANTIDADE.
#
# sum(..., na.rm = TRUE) ignora valores ausentes nas somas. A contagem de linhas
# representa o número de registros, mesmo que algumas métricas estejam ausentes.
transacoes <- processar_em_blocos(
  "estatisticas_transacoes_pix.csv",
  tipos_transacoes,
  "transacoes",
  function(x) {
    x %>%
      group_by(AnoMes) %>%
      summarise(
        linhas = n(),
        valor_total = sum(VALOR, na.rm = TRUE),
        quantidade_total = sum(QUANTIDADE, na.rm = TRUE),
        .groups = "drop"
      )
  }
)

# -----------------------------------------------------------------------------
# 5. AGREGAÇÃO MENSAL DA BASE MUNICIPAL
# -----------------------------------------------------------------------------

# Processa transacoes_pix_municipio.csv em blocos.
#
# Calcula por mês:
# - linhas: quantidade de registros da base municipal;
# - valor_pagador: soma dos valores pagos por pessoas físicas e jurídicas;
# - valor_recebedor: soma dos valores recebidos por pessoas físicas e jurídicas;
# - quantidade_pagador: soma das quantidades pagas por PF e PJ;
# - quantidade_recebedor: soma das quantidades recebidas por PF e PJ.
#
# As somas ignoram valores ausentes. Quando ambos os componentes de uma soma
# estiverem ausentes, sum(..., na.rm = TRUE) retorna zero; essa convenção deve
# ser considerada na interpretação dos totais.
municipios <- processar_em_blocos(
  "transacoes_pix_municipio.csv",
  tipos_municipios,
  "municipios",
  function(x) {
    x %>%
      group_by(AnoMes) %>%
      summarise(
        linhas = n(),
        valor_pagador = sum(
          VL_PagadorPF, VL_PagadorPJ, na.rm = TRUE
        ),
        valor_recebedor = sum(
          VL_RecebedorPF, VL_RecebedorPJ, na.rm = TRUE
        ),
        quantidade_pagador = sum(
          QT_PagadorPF, QT_PagadorPJ, na.rm = TRUE
        ),
        quantidade_recebedor = sum(
          QT_RecebedorPF, QT_RecebedorPJ, na.rm = TRUE
        ),
        .groups = "drop"
      )
  }
)

# -----------------------------------------------------------------------------
# 6. CONSOLIDAÇÃO DOS DIAGNÓSTICOS
# -----------------------------------------------------------------------------

# Combina o perfil completo da base de contestações com os perfis por coluna
# obtidos durante a leitura em blocos das bases nacional e municipal.
# arrange() organiza o resultado por base e nome de coluna.
perfil <- bind_rows(
  perfil_fraudes,
  transacoes$perfil,
  municipios$perfil
) %>%
  arrange(base, coluna)

# Monta uma tabela com as dimensões principais de cada base:
# - registros: número de linhas processadas;
# - colunas: quantidade de colunas identificadas.
#
# Para contestações, os números vêm da leitura integral.
# Para transações e municípios, o total de registros vem da contagem em blocos.
# A quantidade de colunas dessas duas bases é obtida do esquema da amostra.
dimensoes <- tibble(
  base = c("fraudes", "transacoes", "municipios"),
  registros = c(
    nrow(fraudes),
    transacoes$linhas,
    municipios$linhas
  ),
  colunas = c(
    ncol(fraudes),
    nrow(esquemas %>% filter(base == "transacoes")),
    nrow(esquemas %>% filter(base == "municipios"))
  )
)

# Grava os diagnósticos gerais de qualidade e dimensão das bases.
write_csv(
  perfil,
  diretorio_saida("outputs", "diagnosticos", "perfil_ausencias.csv")
)
write_csv(
  dimensoes,
  diretorio_saida("outputs", "diagnosticos", "dimensoes.csv")
)

# -----------------------------------------------------------------------------
# 7. GRAVAÇÃO DAS TABELAS MENSAIS PARA OS PRÓXIMOS SCRIPTS
# -----------------------------------------------------------------------------

# Salva a base de contestações lida integralmente. O nome do arquivo de saída
# é fraudes_mensal.csv, conforme o pipeline original; a gravação, por si só,
# não realiza uma nova agregação mensal neste ponto do código.
write_csv(
  fraudes,
  diretorio_saida("outputs", "tabelas", "fraudes_mensal.csv")
)

# Salva os resumos mensais calculados para as bases nacional e municipal.
# Esses arquivos são as principais entradas do script 02.
write_csv(
  transacoes$mensal,
  diretorio_saida("outputs", "tabelas", "transacoes_mensal.csv")
)
write_csv(
  municipios$mensal,
  diretorio_saida("outputs", "tabelas", "municipios_mensal.csv")
)

# Mensagem final com o total de registros processados em cada base.
# A ordem corresponde a fraudes, transações e municípios, respectivamente.
message(
  "Diagnóstico concluído. Registros processados: ",
  paste(dimensoes$registros, collapse = ", ")
)
