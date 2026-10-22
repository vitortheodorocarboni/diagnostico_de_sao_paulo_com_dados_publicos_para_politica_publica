################################################################################
##################################            ##################################
################ 07.08) BUSCAR OS TRATADOS DA EXTENSÃO DA RENDA ################
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("tidyverse")

# Carregando bibliotecas
library(arrow)
library(tidyverse)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto e as funções de leitura dos tratados
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Carregando os universos de aplicação das variáveis da amostra
source(file.path("scripts", "_config", "universos_amostra.R"), encoding = "UTF-8")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o caminho e o SHA-256 da amostra por área de ponderação
arquivo_amostra <- "censo_2022/areas_de_ponderacao/tratados/sp/tabulares/censo_amostra_areas_ponderacao.parquet"
sha256_amostra  <- "7bec0b2f873264908f1304fb3c6e2ee6e015b4df9231cc0f6eec419b7137bc6b"

# Definindo as variáveis da amostra usadas na extensão
codigos_amostra <- c(
  # Rendimento do trabalho, existência e valor do rendimento de aposentadoria,
  # pensão, benefícios e aluguel, e rendimento de todas as fontes
  "P1080",
  "P1090",
  "P1100",
  "P1110",
  # Rendimento familiar per capita (peso das famílias)
  "F0260"
)

# Definindo os campos da amostra usados na extensão (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativa total de pessoas da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  # Rendimento do trabalho (P1080), de outras fontes (P1100) e de todas as
  # fontes (P1110): soma ponderada, para a média
  "p1080_soma",
  "p1100_soma",
  "p1110_soma",
  # Rendimento familiar per capita (F0260): soma ponderada pelas famílias
  "f0260_soma",
  # Existência de rendimento de aposentadoria, pensão, benefícios e aluguel
  # (P1090): sim e não
  sprintf("p1090_cat_codigo_%d_estimativa", 1:2),
  # Pesos observado, ignorado e de referência dos universos de aplicação
  campos_universos(codigos_amostra)
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("07_renda")


################################################################################
##### III. Dados
################################################################################

# Lendo as APs da capital com os campos da extensão
amostra <- ler_tratado(
  caminho_relativo = arquivo_amostra,
  sha256           = sha256_amostra,
  campos           = unique(c(
    "codigo_area_ponderacao",
    "codigo_municipio",
    campos_amostra
  )),
  # Filtrando no Arrow as APs da capital
  codigo_municipio == codigo_municipio_sp
)

# Informando o resumo da leitura
message("APs da capital: ", nrow(amostra))


################################################################################
##### IV. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando as APs
write_parquet(
  x    = amostra,
  sink = file.path(diretorio_saida, "extensao_amostra.parquet")
)

# Informando o fim do script
message("Extratos da extensão da renda gravados em ", diretorio_saida)
