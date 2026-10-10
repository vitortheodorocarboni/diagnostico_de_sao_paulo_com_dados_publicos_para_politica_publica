################################################################################
##################################            ##################################
################# 09.01) BUSCAR OS TRATADOS DOS TRABALHADORES ##################
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

# Definindo o caminho e o SHA-256 da tabela de pessoas por zona da OD
arquivo_pessoas <- "pesquisa_origem_e_destino_2023/tratados/tabulares/pessoas/od_2023_pessoas_zonas.parquet"
sha256_pessoas  <- "2b44c0a3c69f3e40b97010c5638876bd29a353f4410713381ffa48e744e0d333"

# Definindo os campos usados no grupo (significados no dicionário da OD do
# Dados): registros e estimativa de moradores da zona e o bloco por zona do
# primeiro trabalho (trabalho_*), sem as proporções, que são recalculadas
campos_pessoas <- c(
  # Moradores da zona (zona de residência)
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  # Trabalhadores com primeiro local fixo na zona
  "trabalho_pessoas_n_registros",
  "trabalho_pessoas_estimativa_total",
  # Sexo, cor ou raça e instrução dos trabalhadores: estimativas e pesos
  sprintf("trabalho_sexo_cat_codigo_%d_estimativa", 1:3),
  "trabalho_sexo_peso_observado",
  "trabalho_sexo_peso_ignorado",
  sprintf("trabalho_raca_cat_codigo_%d_estimativa", 1:6),
  "trabalho_raca_peso_observado",
  "trabalho_raca_peso_ignorado",
  sprintf("trabalho_grau_ins_cat_codigo_%d_estimativa", 1:5),
  "trabalho_grau_ins_peso_observado",
  # Renda familiar per capita em 21 classes de R$ 500 e o peso observado
  sprintf("trabalho_renda_familiar_per_capita_classe_cat_codigo_%02d_estimativa", 1:21),
  "trabalho_renda_familiar_per_capita_classe_peso_observado",
  # Idade por ano e zona de residência (matriz residência → trabalho), em
  # listas de categorias, e os pesos observados
  "trabalho_idade_distribuicao",
  "trabalho_idade_peso_observado",
  "trabalho_zona_residencia_distribuicao",
  "trabalho_zona_residencia_peso_observado"
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("09_trabalhadores")


################################################################################
##### III. Dados
################################################################################

# Lendo as zonas da capital com os campos do grupo
pessoas <- ler_tratado(
  caminho_relativo = arquivo_pessoas,
  sha256           = sha256_pessoas,
  campos           = c(
    "codigo_zona_od",
    "codigo_municipio",
    campos_pessoas
  ),
  # Filtrando no Arrow as zonas da capital
  codigo_municipio == codigo_municipio_sp
)

# Informando o resumo da leitura
message("Zonas da capital: ", nrow(pessoas))


################################################################################
##### IV. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando as zonas
write_parquet(
  x    = pessoas,
  sink = file.path(diretorio_saida, "pessoas.parquet")
)

# Informando o fim do script
message("Extratos dos trabalhadores gravados em ", diretorio_saida)
