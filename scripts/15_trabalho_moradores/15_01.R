################################################################################
##################################            ##################################
############# 15.01) BUSCAR OS TRATADOS DO TRABALHO DOS MORADORES ##############
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

# Definindo o caminho e o SHA-256 das viagens da OD por zona
arquivo_viagens <- "pesquisa_origem_e_destino_2023/tratados/tabulares/viagens/od_2023_viagens_zonas.parquet"
sha256_viagens  <- "713aebef0b3fb5499d67cbba4c87a8240526298f8137e9e8754f968674e4b021"

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c(
  # Força de trabalho, ocupação e contribuição para a previdência
  "P0930",
  "P0950",
  "P0940",
  # Carteira assinada, posição na ocupação e grandes grupos ocupacionais
  "P1000",
  "P0990",
  "P1040",
  # Local de trabalho, meio de transporte e tempo de deslocamento
  "P1120",
  "P1170",
  "P1180",
  "P1190",
  # Rendimento do trabalho
  "P1080"
)

# Definindo os campos da amostra usados no grupo (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativa total de pessoas da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  # Estimativas por categoria das variáveis categóricas
  sprintf("p0930_cat_codigo_%d_estimativa", 0:1),
  sprintf("p0950_cat_codigo_%d_estimativa", 0:1),
  sprintf("p0940_cat_codigo_%d_estimativa", 0:1),
  sprintf("p1000_cat_codigo_%d_estimativa", 1:2),
  sprintf("p0990_cat_codigo_%02d_estimativa", 1:9),
  sprintf("p1040_cat_codigo_%02d_estimativa", 1:11),
  sprintf("p1120_cat_codigo_%d_estimativa", 1:5),
  sprintf("p1170_cat_codigo_%02d_estimativa", 1:14),
  sprintf("p1180_cat_codigo_%d_estimativa", 0:7),
  # Tempo de deslocamento e rendimento do trabalho: somas ponderadas e médias
  # publicadas
  "p1190_soma",
  "p1190_media",
  "p1080_soma",
  "p1080_media",
  # Pesos observado, ignorado e de referência dos universos de aplicação
  campos_universos(codigos_amostra)
)

# Definindo os campos da OD usados no grupo: viagens casa → trabalho por zona
# de residência (registros, estimativa e tipo de viagem: coletivo,
# individual, a pé e bicicleta)
campos_viagens <- c(
  "codigo_zona_od",
  "casa_trabalho_residencia_viagens_n_registros",
  "casa_trabalho_residencia_viagens_estimativa_total",
  sprintf("casa_trabalho_residencia_tipvg_cat_codigo_%d_estimativa", 1:4),
  "casa_trabalho_residencia_tipvg_peso_observado"
)

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

# Informando o resumo da leitura da amostra
message("APs da capital: ", nrow(amostra))

# Lendo as viagens casa → trabalho por zona de residência (todas as zonas da
# RMSP; as da capital são separadas na preparação, pela ligação zona →
# distrito)
viagens <- ler_tratado(
  caminho_relativo = arquivo_viagens,
  sha256           = sha256_viagens,
  campos           = campos_viagens
) |>
  mutate(
    codigo_zona_od = as.character(codigo_zona_od)
  )

# Informando o resumo da leitura da OD
message("Zonas da OD: ", nrow(viagens))


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

# Gravando as viagens da OD
write_parquet(
  x    = viagens,
  sink = file.path(diretorio_saida, "viagens.parquet")
)

# Informando o fim do script
message("Extratos do trabalho dos moradores gravados em ", diretorio_saida)
