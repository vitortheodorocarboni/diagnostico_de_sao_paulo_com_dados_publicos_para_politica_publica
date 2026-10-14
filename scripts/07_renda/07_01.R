################################################################################
##################################            ##################################
###################### 07.01) BUSCAR OS TRATADOS DA RENDA ######################
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

# Definindo o caminho e o SHA-256 da amostra por área de ponderação
arquivo_amostra <- "censo_2022/areas_de_ponderacao/tratados/tabulares/censo_amostra_areas_ponderacao.parquet"
sha256_amostra  <- "7bec0b2f873264908f1304fb3c6e2ee6e015b4df9231cc0f6eec419b7137bc6b"

# Definindo os campos da amostra usados no grupo (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros e estimativas totais de pessoas e de domicílios da AP
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  "domicilios_n_registros",
  "domicilios_estimativa_total",
  # Rendimento domiciliar per capita (D0360) dos moradores de domicílios
  # particulares permanentes ocupados, ponderado por pessoa: registros, peso
  # observado, soma ponderada e média
  "d0360_moradores_n_observado",
  "d0360_moradores_peso_observado",
  "d0360_moradores_peso_ausente",
  "d0360_moradores_soma",
  "d0360_moradores_media",
  # Domicílios por classe de salário mínimo (R$ 1.212, 2022): estimativas das
  # dez classes e pesos observado, ausente e ignorado
  sprintf("d0360_classe_salario_minimo_cat_codigo_%02d_estimativa", 1:10),
  "d0360_classe_salario_minimo_peso_observado",
  "d0360_classe_salario_minimo_peso_ausente",
  "d0360_classe_salario_minimo_peso_ignorado"
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("07_renda")


################################################################################
##### III. Dados
################################################################################

# Lendo as APs da capital com os campos do grupo
amostra <- ler_tratado(
  caminho_relativo = arquivo_amostra,
  sha256           = sha256_amostra,
  campos           = c(
    "codigo_area_ponderacao",
    "codigo_municipio",
    campos_amostra
  ),
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
message("Extratos da renda gravados em ", diretorio_saida)
