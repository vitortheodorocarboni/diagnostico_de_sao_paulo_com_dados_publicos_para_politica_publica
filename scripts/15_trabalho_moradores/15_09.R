################################################################################
##################################            ##################################
############## 15.09) BUSCAR OS TRATADOS DE ATIVIDADE E OCUPAÇÃO ###############
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

# Definindo o caminho e o SHA-256 da tabela de rótulos das variáveis codificadas
arquivo_rotulos <- "censo_2022/areas_de_ponderacao/tratados/sp/tabulares/censo_amostra_rotulos.parquet"
sha256_rotulos  <- "e5482945a242ba3b74623090d1ef4724a6f6de9106cf8e68878a2c5f9ce21caa"

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c(
  # Ocupação e atividade do trabalho principal (códigos do IBGE)
  "P0970",
  "P0980",
  # Município do local de trabalho de quem trabalha em outro município
  "P1140"
)

# Definindo os campos da amostra usados no grupo (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativa total de pessoas da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  # Ocupações, atividades e municípios de trabalho, em listas de categorias por AP
  "p0970_distribuicao",
  "p0980_distribuicao",
  "p1140_distribuicao",
  # Pesos observado, ignorado e de referência dos universos de aplicação
  campos_universos(codigos_amostra)
)

# Definindo as variáveis cujos rótulos o grupo usa
variaveis_rotulos <- c("P0970", "P0980", "P1140")

# Definindo o diretório de saída
diretorio_saida <- caminho_data("15_trabalho_moradores")


################################################################################
##### III. Dados
################################################################################

# Lendo as APs da capital com os campos do grupo
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

# Lendo os rótulos das ocupações, das atividades e dos municípios, com o grupo
# de cada código (grande grupo da ocupação e seção da atividade)
rotulos <- ler_tratado(
  caminho_relativo = arquivo_rotulos,
  sha256           = sha256_rotulos,
  campos           = c(
    "codigo_original",
    "codigo_categoria",
    "descricao_categoria",
    "grupo_codigo",
    "grupo_rotulo"
  ),
  # Filtrando no Arrow as variáveis usadas
  codigo_original %in% variaveis_rotulos
)

# Informando o resumo dos rótulos
message("Rótulos lidos: ", nrow(rotulos))


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
  sink = file.path(diretorio_saida, "atividade_amostra.parquet")
)

# Gravando os rótulos
write_parquet(
  x    = rotulos,
  sink = file.path(diretorio_saida, "rotulos.parquet")
)

# Informando o fim do script
message("Extratos de atividade, ocupação e destino gravados em ", diretorio_saida)
