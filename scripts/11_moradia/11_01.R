################################################################################
##################################            ##################################
##################### 11.01) BUSCAR OS TRATADOS DA MORADIA #####################
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

# Definindo o caminho e o SHA-256 das ausências do universo por setor
arquivo_ausencias <- "censo_2022/setores_censitarios/tratados/sp/tabulares/censo_setores_ausencias.parquet"
sha256_ausencias  <- "ce2cbb6e9a5b294bc737997b35bc5c6cc031ad03539e37d5d2a188fd67d43193"

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c(
  # Condição de ocupação do domicílio
  "D0190",
  # Material das paredes externas
  "D0210",
  # Moradores por cômodo usado como dormitório
  "D0240",
  # Máquina de lavar roupa
  "D0320",
  # Acesso à internet
  "D0330"
)

# Definindo os campos da amostra usados no grupo (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativas totais de pessoas e de domicílios da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  "domicilios_n_registros",
  "domicilios_estimativa_total",
  # Estimativas por categoria: condição de ocupação (1 a 7), material das
  # paredes (1 a 7), máquina de lavar (1 e 2) e internet (1 e 2)
  sprintf("d0190_cat_codigo_%d_estimativa", 1:7),
  sprintf("d0210_cat_codigo_%d_estimativa", 1:7),
  sprintf("d0320_cat_codigo_%d_estimativa", 1:2),
  sprintf("d0330_cat_codigo_%d_estimativa", 1:2),
  # Moradores por dormitório: soma ponderada e média publicada
  "d0240_soma",
  "d0240_media",
  # Pesos observado, ignorado e de referência dos universos de aplicação
  campos_universos(codigos_amostra)
)

# Definindo os campos do universo usados no grupo (significados no dicionário
# dos setores do Dados)
campos_universo <- c(
  # Domicílios particulares permanentes ocupados
  "v00001",
  # Banheiros de uso exclusivo com chuveiro e vaso sanitário: 1, 2, 3, 4 ou
  # mais; apenas banheiro de uso comum; apenas sanitário ou buraco; sem
  # banheiro nem sanitário
  sprintf("v%05d", 232:238)
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("11_moradia")


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

# Lendo as células ausentes (sigilo e outras) dos campos do universo na capital
ausencias <- ler_tratado(
  caminho_relativo = arquivo_ausencias,
  sha256           = sha256_ausencias,
  campos           = c(
    "codigo_setor",
    "codigo_original",
    "ausencia_original"
  ),
  # Filtrando no Arrow os setores da capital e os campos do grupo
  str_starts(codigo_setor, codigo_municipio_sp),
  str_to_lower(codigo_original) %in% campos_universo
) |>
  mutate(
    codigo_original = str_to_lower(codigo_original)
  )

# Informando o resumo das ausências
message(
  "Células ausentes: ", nrow(ausencias), " em ",
  n_distinct(ausencias$codigo_original), " campos; símbolos: ",
  paste(unique(ausencias$ausencia_original), collapse = ", ")
)


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

# Gravando as ausências
write_parquet(
  x    = ausencias,
  sink = file.path(diretorio_saida, "ausencias.parquet")
)

# Informando o fim do script
message("Extratos da moradia gravados em ", diretorio_saida)
