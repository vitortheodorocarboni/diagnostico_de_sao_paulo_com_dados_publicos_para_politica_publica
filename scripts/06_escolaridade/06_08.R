################################################################################
##################################            ##################################
################ 06.08) BUSCAR OS TRATADOS DA EXTENSÃO ESCOLAR #################
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

# Definindo as variáveis da amostra usadas na extensão
codigos_amostra <- c(
  # Frequência escolar, curso frequentado, área do curso superior e anos de
  # estudo
  "P0650",
  "P0660",
  "P0750",
  "P0790"
)

# Definindo os campos da amostra usados na extensão (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativa total de pessoas da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  # Frequência escolar (P0650): sim, não mas já frequentou e nunca frequentou
  sprintf("p0650_cat_codigo_%d_estimativa", 1:3),
  # Curso frequentado (P0660): de creche (01) a doutorado (11)
  sprintf("p0660_cat_codigo_%02d_estimativa", 1:11),
  # Anos de estudo (P0790): soma ponderada, para a média
  "p0790_soma",
  "p0790_n_observado",
  # Áreas do curso superior (P0750), em lista de categorias por AP
  "p0750_distribuicao",
  # Pesos observado, ignorado e de referência dos universos de aplicação
  campos_universos(codigos_amostra)
)

# Definindo as variáveis cujos rótulos a extensão usa
variaveis_rotulos <- "P0750"

# Definindo o diretório de saída
diretorio_saida <- caminho_data("06_escolaridade")


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

# Lendo os rótulos das áreas do curso superior
rotulos <- ler_tratado(
  caminho_relativo = arquivo_rotulos,
  sha256           = sha256_rotulos,
  campos           = c(
    "codigo_original",
    "codigo_categoria",
    "descricao_categoria"
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
  sink = file.path(diretorio_saida, "extensao_amostra.parquet")
)

# Gravando os rótulos
write_parquet(
  x    = rotulos,
  sink = file.path(diretorio_saida, "extensao_rotulos.parquet")
)

# Informando o fim do script
message("Extratos da extensão da escolaridade gravados em ", diretorio_saida)
