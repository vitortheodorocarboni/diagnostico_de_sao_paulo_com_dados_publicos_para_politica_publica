################################################################################
##################################            ##################################
################# 10.01) BUSCAR OS TRATADOS DOS DESLOCAMENTOS ##################
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("geoarrow")
# install.packages("sf")
# install.packages("tidyverse")

# Carregando bibliotecas
library(arrow)
library(geoarrow)
library(sf)
library(tidyverse)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto e as funções de leitura dos tratados
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Definindo o caminho e o SHA-256 da tabela de viagens por zona da OD
arquivo_viagens <- "pesquisa_origem_e_destino_2023/tratados/tabulares/viagens/od_2023_viagens_zonas.parquet"
sha256_viagens  <- "713aebef0b3fb5499d67cbba4c87a8240526298f8137e9e8754f968674e4b021"

# Definindo o caminho e o SHA-256 da tabela de pessoas por zona da OD
arquivo_pessoas <- "pesquisa_origem_e_destino_2023/tratados/tabulares/pessoas/od_2023_pessoas_zonas.parquet"
sha256_pessoas  <- "2b44c0a3c69f3e40b97010c5638876bd29a353f4410713381ffa48e744e0d333"

# Definindo o caminho e o SHA-256 das geometrias das zonas da OD
arquivo_zonas_geometria <- "pesquisa_origem_e_destino_2023/tratados/geometrias/od_zonas_geometria.parquet"
sha256_zonas_geometria  <- "1640da891ebd24592608460b6fd1d5a09bb19a14e8e602af2dfad9536f242c07"

# Definindo o caminho e o SHA-256 das geometrias dos municípios da OD
arquivo_municipios_geometria <- "pesquisa_origem_e_destino_2023/tratados/geometrias/od_municipios_geometria.parquet"
sha256_municipios_geometria  <- "ae2d24e2498dad54fe6152c8159c3c3bf26f4dc6ee92958f35f2f98bdabe90fe"

# Definindo o caminho e o SHA-256 dos atributos dos municípios da OD
arquivo_municipios <- "pesquisa_origem_e_destino_2023/tratados/tabulares/od_municipios_atributos.parquet"
sha256_municipios  <- "f37432bbd4b44e204702bbb3a8a5cfbd5d6287ed34ef088d63669761bccec60c"

# Definindo os campos de viagens usados no grupo (significados no dicionário da
# OD do Dados); todos do bloco de origem ou dos blocos casa → trabalho, que não
# se somam entre si
campos_viagens <- c(
  # Município da zona e total de viagens com origem na zona
  "codigo_municipio",
  "nome_municipio",
  "origem_viagens_n_registros",
  "origem_viagens_estimativa_total",
  # Tipo de viagem (coletivo, individual, a pé, bicicleta) das viagens com
  # origem na zona e os pesos
  sprintf("origem_tipvg_cat_codigo_%d_estimativa", 1:4),
  "origem_tipvg_peso_observado",
  "origem_tipvg_peso_ausente",
  "origem_tipvg_peso_ignorado",
  # Duração das viagens com origem na zona: soma ponderada e peso observado
  "origem_duracao_peso_observado",
  "origem_duracao_soma",
  # Zona de destino das viagens com origem na zona (matriz OD) e os pesos
  "origem_zona_d_distribuicao",
  "origem_zona_d_peso_observado",
  "origem_zona_d_peso_ausente",
  "origem_zona_d_peso_ignorado",
  # Tipo das viagens casa → trabalho pela zona de residência e pela de destino
  "casa_trabalho_residencia_viagens_n_registros",
  "casa_trabalho_residencia_viagens_estimativa_total",
  sprintf("casa_trabalho_residencia_tipvg_cat_codigo_%d_estimativa", 1:4),
  "casa_trabalho_residencia_tipvg_peso_observado",
  "casa_trabalho_destino_viagens_n_registros",
  "casa_trabalho_destino_viagens_estimativa_total",
  sprintf("casa_trabalho_destino_tipvg_cat_codigo_%d_estimativa", 1:4),
  "casa_trabalho_destino_tipvg_peso_observado",
  # Viagens com destino na zona e o motivo no destino (11 categorias, entre
  # elas trabalho, escola e residência) e o peso observado
  "destino_viagens_n_registros",
  "destino_viagens_estimativa_total",
  sprintf("destino_motivo_d_cat_codigo_%d_estimativa", 1:11),
  "destino_motivo_d_peso_observado"
)

# Definindo os campos de pessoas usados no grupo: os moradores da zona, a matriz
# residência → trabalho (pela zona do primeiro trabalho) e a matriz residência
# → escola (pela zona de residência)
campos_pessoas <- c(
  "codigo_municipio",
  "pessoas_n_registros",
  "pessoas_estimativa_total",
  "trabalho_pessoas_n_registros",
  "trabalho_pessoas_estimativa_total",
  "trabalho_zona_residencia_distribuicao",
  "trabalho_zona_residencia_peso_observado",
  "zona_esc_distribuicao",
  "zona_esc_peso_observado"
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("10_deslocamentos")


################################################################################
##### III. Dados
################################################################################

# Lendo as viagens das 527 zonas da RMSP (os fluxos usam toda a RMSP)
viagens <- ler_tratado(
  caminho_relativo = arquivo_viagens,
  sha256           = sha256_viagens,
  campos           = c(
    "codigo_zona_od",
    campos_viagens
  )
)

# Lendo os moradores e as matrizes residência → trabalho e residência → escola
# das 527 zonas da RMSP
pessoas <- ler_tratado(
  caminho_relativo = arquivo_pessoas,
  sha256           = sha256_pessoas,
  campos           = c(
    "codigo_zona_od",
    campos_pessoas
  )
)

# Lendo as geometrias das zonas da OD
zonas_geometria <- ler_tratado_geo(
  caminho_relativo = arquivo_zonas_geometria,
  sha256           = sha256_zonas_geometria,
  epsg             = 31983
)

# Lendo os atributos dos municípios da OD, com o código do IBGE como texto
municipios <- ler_tratado(
  caminho_relativo = arquivo_municipios,
  sha256           = sha256_municipios
) |>
  mutate(
    cd_ibge = as.character(cd_ibge)
  )

# Lendo as geometrias dos municípios da OD e juntando o código do IBGE
municipios_geometria <- ler_tratado_geo(
  caminho_relativo = arquivo_municipios_geometria,
  sha256           = sha256_municipios_geometria,
  epsg             = 31983
) |>
  left_join(
    municipios |> select(codigo_municipio_od, codigo_municipio = cd_ibge, nome_municipio = nome_munici),
    by = "codigo_municipio_od"
  )

# Informando o resumo das leituras
message(
  "Zonas com viagens: ", nrow(viagens), "; zonas com pessoas: ", nrow(pessoas),
  "; geometrias de zonas: ", nrow(zonas_geometria), "; municípios: ", nrow(municipios_geometria)
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

# Gravando as viagens
write_parquet(
  x    = viagens,
  sink = file.path(diretorio_saida, "viagens.parquet")
)

# Gravando os moradores e as matrizes residência → trabalho e residência →
# escola por zona
write_parquet(
  x    = pessoas,
  sink = file.path(diretorio_saida, "pessoas.parquet")
)

# Gravando as geometrias das zonas
write_parquet(
  x    = zonas_geometria,
  sink = file.path(diretorio_saida, "zonas_geometria.parquet")
)

# Gravando as geometrias dos municípios
write_parquet(
  x    = municipios_geometria,
  sink = file.path(diretorio_saida, "municipios_geometria.parquet")
)

# Informando o fim do script
message("Extratos dos deslocamentos gravados em ", diretorio_saida)
