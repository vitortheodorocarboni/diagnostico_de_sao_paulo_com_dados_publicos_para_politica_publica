################################################################################
##################################            ##################################
#################### 14.01) BUSCAR OS TRATADOS DA MIGRAÇÃO #####################
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
arquivo_amostra <- "censo_2022/areas_de_ponderacao/tratados/tabulares/censo_amostra_areas_ponderacao.parquet"
sha256_amostra  <- "7bec0b2f873264908f1304fb3c6e2ee6e015b4df9231cc0f6eec419b7137bc6b"

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c(
  # Local de nascimento (município, outro município do Brasil, outro país)
  "P0480",
  # Unidade da Federação de nascimento dos nascidos em outro município
  "P0490",
  # Nacionalidade
  "P0520",
  # Tempo de moradia no município de quem nasceu ou já morou fora
  "P0550"
)

# Definindo os códigos de UF de nascimento (P0490): os 27 códigos do IBGE, mais
# 88 e 99 (não especificada)
codigos_uf <- c(11:17, 21:29, 31:33, 35, 41:43, 50:53, 88, 99)

# Definindo os campos da amostra usados no grupo (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativa total de pessoas da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  # Estimativas por categoria: local de nascimento, UF de nascimento e
  # nacionalidade
  sprintf("p0480_cat_codigo_%d_estimativa", 1:3),
  sprintf("p0490_cat_codigo_%d_estimativa", codigos_uf),
  sprintf("p0520_cat_codigo_%d_estimativa", 1:3),
  # Tempo de moradia no município: soma ponderada e média publicada
  "p0550_soma",
  "p0550_media",
  # Pesos observado, ignorado e de referência dos universos de aplicação
  campos_universos(codigos_amostra)
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("14_migracao")


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
  sink = file.path(diretorio_saida, "amostra.parquet")
)

# Informando o fim do script
message("Extratos da migração gravados em ", diretorio_saida)
