################################################################################
##################################            ##################################
################ 14.08) BUSCAR OS TRATADOS DA MIGRAÇÃO RECENTE #################
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

# Definindo as variáveis da amostra usadas na migração recente e nos países
codigos_amostra <- c(
  # Idade dos moradores, para a população de 5 anos ou mais
  "P0180",
  # País e município de nascimento
  "P0510",
  "P0500",
  # Local de moradia há 5 anos e Unidade da Federação de origem
  "P0600",
  "P0610"
)

# Definindo os códigos de UF de moradia há 5 anos (P0610): os 27 códigos do
# IBGE, mais 88 e 99 (não especificada)
codigos_uf <- c(11:17, 21:29, 31:33, 35, 41:43, 50:53, 88, 99)

# Definindo os campos da amostra usados no grupo (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativa total de pessoas da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  # Pessoas de 0 a 4 anos, que ficam fora da população de 5 anos ou mais
  "p0180_cat_codigo_01_estimativa",
  # Local de moradia há 5 anos: neste município, em outro município do Brasil e
  # em outro país
  sprintf("p0600_cat_codigo_%d_estimativa", 1:3),
  # UF de moradia há 5 anos de quem veio de outro município do Brasil
  sprintf("p0610_cat_codigo_%d_estimativa", codigos_uf),
  # Países e municípios de nascimento, em listas de categorias por AP
  "p0510_distribuicao",
  "p0500_distribuicao",
  # Pesos observado, ignorado e de referência dos universos de aplicação
  campos_universos(codigos_amostra)
)

# Definindo as variáveis cujos rótulos o grupo usa
variaveis_rotulos <- c("P0510", "P0500")

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

# Lendo os rótulos dos países e dos municípios de nascimento
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
  sink = file.path(diretorio_saida, "recente_amostra.parquet")
)

# Gravando os rótulos
write_parquet(
  x    = rotulos,
  sink = file.path(diretorio_saida, "rotulos.parquet")
)

# Informando o fim do script
message("Extratos da migração recente gravados em ", diretorio_saida)
