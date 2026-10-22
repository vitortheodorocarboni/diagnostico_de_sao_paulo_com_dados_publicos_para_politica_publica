################################################################################
##################################            ##################################
###################### 03.01) BUSCAR OS TRATADOS DE SEXO #######################
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

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o caminho e o SHA-256 do universo por setor
arquivo_setores <- "censo_2022/setores_censitarios/tratados/sp/tabulares/censo_universo_setores.parquet"
sha256_setores  <- "d5839bb3e9ea6f8829e2822b8b8fb18d1a62493b24389856195d8629efc7e0e9"

# Definindo o caminho e o SHA-256 da tabela municipal do universo
arquivo_municipios <- "censo_2022/municipios/tratados/sp/tabulares/censo_universo_municipios.parquet"
sha256_municipios  <- "820914a3bd6d1f43053e5bace602ebba6673a0a3d5f3dd580b5df2ff92e12c9e"

# Definindo o caminho e o SHA-256 das ausências do universo por setor
arquivo_ausencias <- "censo_2022/setores_censitarios/tratados/sp/tabulares/censo_setores_ausencias.parquet"
sha256_ausencias  <- "ce2cbb6e9a5b294bc737997b35bc5c6cc031ad03539e37d5d2a188fd67d43193"

# Definindo os campos do universo usados no grupo (significados no dicionário
# dos setores do Dados)
campos_sexo <- c(
  # Total de pessoas
  "v0001",
  # Sexo masculino e sexo feminino
  "v01007",
  "v01008",
  # Sexo masculino por faixa de idade (0 a 4, 5 a 9, 10 a 14, 15 a 19, 20 a 24,
  # 25 a 29, 30 a 39, 40 a 49, 50 a 59, 60 a 69 e 70 anos ou mais)
  sprintf("v%05d", 1009:1019),
  # Sexo feminino pelas mesmas faixas de idade
  sprintf("v%05d", 1020:1030)
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("03_sexo")


################################################################################
##### III. Dados
################################################################################

# Lendo os setores da capital com os campos do grupo
setores <- ler_tratado(
  caminho_relativo = arquivo_setores,
  sha256           = sha256_setores,
  campos           = c(
    "codigo_setor",
    "codigo_municipio",
    campos_sexo
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
    campos_sexo
  )
) |>
  filter(codigo_municipio == codigo_municipio_sp)

# Lendo as células ausentes (sigilo e outras) dos campos do grupo na capital
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
  str_to_lower(codigo_original) %in% campos_sexo
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
message("Extratos de sexo gravados em ", diretorio_saida)
