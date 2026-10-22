################################################################################
##################################            ##################################
################### 00.01) BUSCAR OS TRATADOS DO TERRITÓRIO ####################
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

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o código do Município de São Paulo na OD
codigo_municipio_sp_od <- 36L

# Definindo o caminho e o SHA-256 do universo por setor
arquivo_setores <- "censo_2022/setores_censitarios/tratados/sp/tabulares/censo_universo_setores.parquet"
sha256_setores  <- "d5839bb3e9ea6f8829e2822b8b8fb18d1a62493b24389856195d8629efc7e0e9"

# Definindo o caminho e o SHA-256 da geometria dos distritos do Censo
arquivo_distritos_geometria <- "censo_2022/distritos/tratados/sp/geometrias/censo_distritos_geometria.parquet"
sha256_distritos_geometria  <- "9292af22b4aa87063e95cba5afa71e4a2abe14a757a5697f2f24a83bca61df82"

# Definindo o caminho e o SHA-256 da relação entre APs e distritos
arquivo_ap_distritos <- "censo_2022/areas_de_ponderacao/tratados/sp/tabulares/censo_ap_distritos.parquet"
sha256_ap_distritos  <- "64b5ff2c9bd05f11193c8dec2407191a7b3c142dca83afeef9f996418e43afc4"

# Definindo o caminho e o SHA-256 da geometria das APs
arquivo_ap_geometria <- "censo_2022/areas_de_ponderacao/tratados/sp/geometrias/censo_ap_controlada_geometria.parquet"
sha256_ap_geometria  <- "764e237ab68b3c1365a7090e73a17e62ad0c1242d422c3b292c361526339f202"

# Definindo o caminho e o SHA-256 da tabela municipal do universo
arquivo_municipios <- "censo_2022/municipios/tratados/sp/tabulares/censo_universo_municipios.parquet"
sha256_municipios  <- "820914a3bd6d1f43053e5bace602ebba6673a0a3d5f3dd580b5df2ff92e12c9e"

# Definindo o caminho e o SHA-256 dos atributos das zonas da OD
arquivo_od_zonas <- "pesquisa_origem_e_destino_2023/tratados/tabulares/od_zonas_atributos.parquet"
sha256_od_zonas  <- "e1d856db29ed13f31abd4b2086a8dadee216a993d0ee0c886b1ef3b8ebc15e7d"

# Definindo o caminho e o SHA-256 da geometria das zonas da OD
arquivo_od_zonas_geometria <- "pesquisa_origem_e_destino_2023/tratados/geometrias/od_zonas_geometria.parquet"
sha256_od_zonas_geometria  <- "1640da891ebd24592608460b6fd1d5a09bb19a14e8e602af2dfad9536f242c07"

# Definindo o caminho e o SHA-256 dos atributos dos distritos da OD
arquivo_od_distritos <- "pesquisa_origem_e_destino_2023/tratados/tabulares/od_distritos_atributos.parquet"
sha256_od_distritos  <- "40317d9e9b3c79d40792be5449346ce89d669892c17905b5e7926c7d63ab197e"

# Definindo o caminho e o SHA-256 da geometria dos distritos da OD
arquivo_od_distritos_geometria <- "pesquisa_origem_e_destino_2023/tratados/geometrias/od_distritos_geometria.parquet"
sha256_od_distritos_geometria  <- "7d14cd4edac450ce87735aef29e09c240c713ff5f1d559ca7afd234e8d982ceb"

# Definindo o diretório de saída
diretorio_saida <- caminho_data("00_territorio")


################################################################################
##### III. Dados do Censo
################################################################################

# Lendo os campos de identificação, área e totais dos setores da capital
setores <- ler_tratado(
  caminho_relativo = arquivo_setores,
  sha256           = sha256_setores,
  campos           = c(
    "codigo_setor",
    "codigo_municipio",
    "nome_municipio",
    "codigo_distrito",
    "nome_distrito",
    "situacao",
    "area_km2",
    "v0001",
    "v0002"
  )
) |>
  filter(codigo_municipio == codigo_municipio_sp)

# Lendo a geometria dos distritos da capital
distritos_geometria <- ler_tratado_geo(
  caminho_relativo = arquivo_distritos_geometria,
  sha256           = sha256_distritos_geometria,
  epsg             = 4674
) |>
  # Mantendo os distritos cujo código começa pelo código do município
  filter(str_starts(codigo_distrito, codigo_municipio_sp))

# Lendo a relação entre APs e distritos da capital
ap_distritos <- ler_tratado(
  caminho_relativo = arquivo_ap_distritos,
  sha256           = sha256_ap_distritos
) |>
  filter(codigo_municipio == codigo_municipio_sp)

# Lendo a geometria das APs da capital
ap_geometria <- ler_tratado_geo(
  caminho_relativo = arquivo_ap_geometria,
  sha256           = sha256_ap_geometria,
  epsg             = 4674
) |>
  # Mantendo as APs cujo código começa pelo código do município
  filter(str_starts(codigo_area_ponderacao, codigo_municipio_sp))

# Lendo a linha da capital na tabela municipal do universo
municipio <- ler_tratado(
  caminho_relativo = arquivo_municipios,
  sha256           = sha256_municipios,
  campos           = c(
    "codigo_municipio",
    "nome_municipio",
    "area_km2",
    "v0001",
    "v0002"
  )
) |>
  filter(codigo_municipio == codigo_municipio_sp)


################################################################################
##### IV. Dados da OD
################################################################################

# Lendo os atributos das zonas da capital
od_zonas <- ler_tratado(
  caminho_relativo = arquivo_od_zonas,
  sha256           = sha256_od_zonas
) |>
  filter(numero_muni == codigo_municipio_sp_od)

# Conferindo se o código da OD corresponde mesmo ao Município de São Paulo
stopifnot(all(od_zonas$nome_munici == "São Paulo"))

# Lendo a geometria das zonas da capital
od_zonas_geometria <- ler_tratado_geo(
  caminho_relativo = arquivo_od_zonas_geometria,
  sha256           = sha256_od_zonas_geometria,
  epsg             = 31983
) |>
  filter(codigo_zona_od %in% od_zonas$codigo_zona_od)

# Listando os códigos dos distritos da OD que têm zonas na capital
codigos_distritos_od_sp <- as.character(sort(unique(od_zonas$num_distrit)))

# Lendo os atributos dos distritos da OD na capital
od_distritos <- ler_tratado(
  caminho_relativo = arquivo_od_distritos,
  sha256           = sha256_od_distritos
) |>
  filter(codigo_distrito_od %in% codigos_distritos_od_sp)

# Lendo a geometria dos distritos da OD na capital
od_distritos_geometria <- ler_tratado_geo(
  caminho_relativo = arquivo_od_distritos_geometria,
  sha256           = sha256_od_distritos_geometria,
  epsg             = 31983
) |>
  filter(codigo_distrito_od %in% codigos_distritos_od_sp)


################################################################################
##### V. Exportação
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

# Gravando a geometria dos distritos
write_parquet(
  x    = distritos_geometria,
  sink = file.path(diretorio_saida, "distritos_geometria.parquet")
)

# Gravando a relação entre APs e distritos
write_parquet(
  x    = ap_distritos,
  sink = file.path(diretorio_saida, "ap_distritos.parquet")
)

# Gravando a geometria das APs
write_parquet(
  x    = ap_geometria,
  sink = file.path(diretorio_saida, "ap_geometria.parquet")
)

# Gravando a linha do município
write_parquet(
  x    = municipio,
  sink = file.path(diretorio_saida, "municipio.parquet")
)

# Gravando os atributos das zonas da OD
write_parquet(
  x    = od_zonas,
  sink = file.path(diretorio_saida, "od_zonas.parquet")
)

# Gravando a geometria das zonas da OD
write_parquet(
  x    = od_zonas_geometria,
  sink = file.path(diretorio_saida, "od_zonas_geometria.parquet")
)

# Gravando os atributos dos distritos da OD
write_parquet(
  x    = od_distritos,
  sink = file.path(diretorio_saida, "od_distritos.parquet")
)

# Gravando a geometria dos distritos da OD
write_parquet(
  x    = od_distritos_geometria,
  sink = file.path(diretorio_saida, "od_distritos_geometria.parquet")
)

# Informando o fim do script
message("Extratos do território gravados em ", diretorio_saida)
