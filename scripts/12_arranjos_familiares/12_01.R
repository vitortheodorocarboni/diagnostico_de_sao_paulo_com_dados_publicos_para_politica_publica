################################################################################
##################################            ##################################
############## 12.01) BUSCAR OS TRATADOS DOS ARRANJOS FAMILIARES ###############
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
# dos setores do Dados, tema parentesco)
campos_universo <- c(
  # Moradores
  "v01006",
  # Cônjuges de sexo diferente e do mesmo sexo
  "v01043",
  "v01044",
  # Pai, mãe, padrasto ou madrasta; sogro(a); neto(a)
  "v01049",
  "v01050",
  "v01051",
  # Responsável pelo domicílio por sexo (masculino, feminino) e por idade (12
  # a 17, 18 a 24, 25 a 39, 40 a 59, 60 ou mais)
  sprintf("v%05d", 1062:1068),
  # Filhos do responsável e do cônjuge por idade (0 a 4 até 70 ou mais)
  sprintf("v%05d", 1084:1094),
  # Domicílios por presença de cônjuge: de sexo diferente, do mesmo sexo,
  # dos dois tipos e sem cônjuge
  sprintf("v%05d", 1176:1179),
  # Composição: casal sem filhos, casal com filhos de ambos, casal com filho
  # de só um, responsável sem cônjuge com filhos e outros
  sprintf("v%05d", 1191:1195),
  # Responsável sem cônjuge com filhos, por sexo do responsável
  "v01203",
  "v01204",
  # Espécie de unidade doméstica: unipessoal, nuclear, estendida e composta
  sprintf("v%05d", 1209:1212)
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("12_arranjos_familiares")


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
message("Extratos dos arranjos familiares gravados em ", diretorio_saida)
