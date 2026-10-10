################################################################################
##################################            ##################################
################### 02.01) BUSCAR OS TRATADOS DOS DOMICÍLIOS ###################
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
arquivo_setores <- "censo_2022/setores_censitarios/tratados/tabulares/censo_universo_setores.parquet"
sha256_setores  <- "d5839bb3e9ea6f8829e2822b8b8fb18d1a62493b24389856195d8629efc7e0e9"

# Definindo o caminho e o SHA-256 da tabela municipal do universo
arquivo_municipios <- "censo_2022/municipios/tabulares/censo_universo_municipios.parquet"
sha256_municipios  <- "820914a3bd6d1f43053e5bace602ebba6673a0a3d5f3dd580b5df2ff92e12c9e"

# Definindo o caminho e o SHA-256 das ausências do universo por setor
arquivo_ausencias <- "censo_2022/setores_censitarios/tratados/tabulares/censo_setores_ausencias.parquet"
sha256_ausencias  <- "ce2cbb6e9a5b294bc737997b35bc5c6cc031ad03539e37d5d2a188fd67d43193"

# Definindo os campos do universo usados no grupo (significados no dicionário
# dos setores do Dados)
campos_basico <- c(
  # Total de domicílios particulares
  "v0003",
  # Total de domicílios particulares ocupados
  "v0007",
  # Total de domicílios particulares de uso ocasional
  "v0008",
  # Total de domicílios particulares permanentes vagos
  "v0009"
)
campos_domicilios <- c(
  # Domicílios particulares permanentes ocupados e improvisados ocupados
  "v00001",
  "v00002",
  # Tipo de espécie: casa, casa de vila ou condomínio, apartamento, cômodos ou
  # cortiço, habitação indígena e estrutura degradada
  sprintf("v%05d", 47:52),
  # Forma de abastecimento de água
  sprintf("v%05d", 111:118),
  # Destinação do esgoto do banheiro ou sanitário
  sprintf("v%05d", 309:316),
  # Destino do lixo
  sprintf("v%05d", 397:402),
  # Banheiro de uso exclusivo
  sprintf("v%05d", 494:495)
)
campos_entorno <- c(
  # Domicílios em setores com entorno pesquisado
  "v05000",
  # Via pavimentada, bueiro, iluminação, ponto de ônibus, via sinalizada para
  # bicicleta, calçada, obstáculo na calçada, rampa e arborização (cada
  # característica com sim, não e não declarado)
  sprintf("v%05d", 5006:5034)
)

# Reunindo os campos do setor e os campos que também existem na tabela municipal
campos_setores   <- c(campos_basico, campos_domicilios, campos_entorno)
campos_municipio <- c(campos_basico, campos_domicilios)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("02_domicilios")


################################################################################
##### III. Dados
################################################################################

# Lendo os setores da capital com situação, área e campos do grupo
setores <- ler_tratado(
  caminho_relativo = arquivo_setores,
  sha256           = sha256_setores,
  campos           = c(
    "codigo_setor",
    "codigo_municipio",
    "situacao",
    campos_setores
  )
) |>
  filter(codigo_municipio == codigo_municipio_sp)

# Lendo a linha da capital na tabela municipal do universo (que não traz o
# entorno)
municipio <- ler_tratado(
  caminho_relativo = arquivo_municipios,
  sha256           = sha256_municipios,
  campos           = c(
    "codigo_municipio",
    "nome_municipio",
    campos_municipio
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
  str_to_lower(codigo_original) %in% campos_setores
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
message("Extratos dos domicílios gravados em ", diretorio_saida)
