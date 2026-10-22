################################################################################
##################################            ##################################
################### 16.01) BUSCAR OS TRATADOS DA DEFICIÊNCIA ###################
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

# Definindo o caminho e o SHA-256 do universo por setor
arquivo_setores <- "censo_2022/setores_censitarios/tratados/sp/tabulares/censo_universo_setores.parquet"
sha256_setores  <- "d5839bb3e9ea6f8829e2822b8b8fb18d1a62493b24389856195d8629efc7e0e9"

# Definindo o caminho e o SHA-256 da tabela municipal do universo
arquivo_municipios <- "censo_2022/municipios/tratados/sp/tabulares/censo_universo_municipios.parquet"
sha256_municipios  <- "820914a3bd6d1f43053e5bace602ebba6673a0a3d5f3dd580b5df2ff92e12c9e"

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c(
  # Dificuldade para enxergar, ouvir, andar ou subir degraus e pegar objetos
  "P0420",
  "P0430",
  "P0440",
  "P0450",
  # Dificuldade nas atividades habituais por limitação nas funções mentais
  "P0460",
  # Pessoa com deficiência (indicadora do IBGE, 2 anos ou mais)
  "P0470",
  # Diagnóstico de autismo por profissional de saúde
  "P1200"
)

# Definindo os campos da amostra usados no grupo (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativa total de pessoas da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  # Graus de dificuldade de cada tipo (1 não consegue de modo algum, 2 muita,
  # 3 alguma, 4 nenhuma)
  as.vector(outer(
    X   = 1:4,
    Y   = c("p0420", "p0430", "p0440", "p0450", "p0460"),
    FUN = \(codigo, variavel) sprintf("%s_cat_codigo_%d_estimativa", variavel, codigo)
  )),
  # Pessoas com e sem deficiência
  sprintf("p0470_cat_codigo_%d_estimativa", 1:2),
  # Pessoas com e sem diagnóstico de autismo
  sprintf("p1200_cat_codigo_%d_estimativa", 1:2),
  # Pesos observado, ignorado e de referência dos universos de aplicação
  campos_universos(codigos_amostra)
)

# Definindo os campos do universo usados no grupo: moradores e os 11 grupos de
# idade (0 a 4 até 70 anos ou mais), para o % de 60 anos ou mais
campos_universo <- c(
  "v0001",
  sprintf("v%05d", 1031:1041)
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("16_deficiencia")


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

# Lendo os setores da capital com os campos do grupo
setores <- ler_tratado(
  caminho_relativo = arquivo_setores,
  sha256           = sha256_setores,
  campos           = c(
    "codigo_setor",
    "codigo_municipio",
    campos_universo
  )
) |>
  filter(codigo_municipio == codigo_municipio_sp)

# Lendo a linha da capital na tabela municipal do universo
municipio <- ler_tratado(
  caminho_relativo = arquivo_municipios,
  sha256           = sha256_municipios,
  campos           = c(
    "codigo_municipio",
    "nome_municipio",
    campos_universo
  )
) |>
  filter(codigo_municipio == codigo_municipio_sp)

# Informando o resumo da leitura do universo
message("Setores da capital: ", nrow(setores))


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

# Gravando os setores
write_parquet(
  x    = setores,
  sink = file.path(diretorio_saida, "setores.parquet")
)

# Gravando a linha do município
write_parquet(
  x    = municipio,
  sink = file.path(diretorio_saida, "municipio.parquet")
)

# Informando o fim do script
message("Extratos da deficiência gravados em ", diretorio_saida)
