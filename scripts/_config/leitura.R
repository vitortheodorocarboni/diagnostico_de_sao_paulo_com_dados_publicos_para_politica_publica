################################################################################
##################################            ##################################
############################# LEITURA DOS TRATADOS #############################
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("geoarrow")
# install.packages("jsonlite")
# install.packages("openssl")
# install.packages("sf")
# install.packages("tidyverse")

# Carregando bibliotecas
library(arrow)
library(geoarrow)
library(jsonlite)
library(openssl)
library(sf)
library(tidyverse)


################################################################################
##### II. Funções
################################################################################

# Criando uma função para validar o caminho de um tratado dentro do Dados
validar_caminho_tratado <- function(caminho_relativo) {

  # Exigindo um único caminho em texto
  if (!is.character(caminho_relativo) || length(caminho_relativo) != 1) {
    stop("O caminho do tratado deve ser um único texto.")
  }

  # Padronizando as barras do caminho
  caminho <- gsub("\\\\", "/", caminho_relativo)

  # Exigindo caminho relativo à raiz do Dados
  if (grepl("^([A-Za-z]:)?/", caminho)) {
    stop("Use o caminho relativo à raiz do Dados, não absoluto: ", caminho)
  }

  # Separando as partes do caminho
  partes <- strsplit(caminho, "/", fixed = TRUE)[[1]]

  # Impedindo a leitura de brutos e a saída da pasta do Dados
  if (any(partes %in% c("brutos", ".."))) {
    stop("Este projeto só lê tratados do Dados; caminho recusado: ", caminho)
  }

  # Exigindo arquivo Parquet
  if (!grepl("\\.parquet$", caminho)) {
    stop("Os tratados do Dados são Parquet; caminho recusado: ", caminho)
  }

  # Montando o caminho completo do arquivo
  arquivo <- caminho_dados_acervo(caminho)

  # Interrompendo se o arquivo não existir
  if (!file.exists(arquivo)) {
    stop("Tratado não encontrado no Dados: ", caminho)
  }

  # Devolvendo o caminho completo
  arquivo
}

# Criando uma função para conferir o SHA-256 de um arquivo
conferir_sha256 <- function(arquivo, sha256) {

  # Exigindo um hash esperado com 64 caracteres hexadecimais
  if (!is.character(sha256) || length(sha256) != 1 || !grepl("^[0-9a-fA-F]{64}$", sha256)) {
    stop("SHA-256 esperado inválido para ", basename(arquivo), ": ", sha256)
  }

  # Abrindo o arquivo em modo binário
  conexao <- file(arquivo, open = "rb")

  # Fechando a conexão ao sair da função
  on.exit(close(conexao))

  # Calculando o SHA-256 do arquivo (com o pacote explícito, porque o argumento
  # sha256 tem o mesmo nome da função) e retirando a classe hash do openssl
  obtido <- paste0(as.character(openssl::sha256(conexao)))

  # Interrompendo se o hash divergir do esperado
  if (!identical(obtido, tolower(sha256))) {
    stop(
      "SHA-256 divergente em ", basename(arquivo), ".\n",
      "  Esperado: ", tolower(sha256), "\n",
      "  Obtido:   ", obtido, "\n",
      "O tratado mudou no Dados: confira a versão e atualize o hash no script."
    )
  }

  # Devolvendo o arquivo de forma invisível
  invisible(arquivo)
}

# Criando uma função para ler um tratado tabular com conferência de SHA-256.
# A leitura passa pelo Dataset do Arrow, como em ler_tratado() do Dados, porque
# read_parquet(col_select = ...) já devolveu colunas erradas em esquemas com
# listas. Condições em ... filtram as linhas no próprio Arrow, antes da carga
# (útil nos tratados grandes, como o de ausências).
ler_tratado <- function(caminho_relativo, sha256, campos = NULL, ...) {

  # Validando o caminho e conferindo o hash
  arquivo <- validar_caminho_tratado(caminho_relativo)
  conferir_sha256(arquivo, sha256)

  # Informando o andamento
  message("Lendo ", caminho_relativo, " (SHA-256 conferido)")

  # Abrindo o arquivo como Dataset
  conjunto <- open_dataset(arquivo, format = "parquet")

  # Usando todos os campos quando nenhum for indicado
  if (is.null(campos)) {
    campos <- conjunto$schema$names
  }

  # Conferindo se os campos pedidos existem e não se repetem
  ausentes <- setdiff(campos, conjunto$schema$names)
  if (length(ausentes) > 0) {
    stop("Campos ausentes em ", basename(arquivo), ": ", paste(ausentes, collapse = ", "))
  }
  if (anyDuplicated(campos) > 0) {
    stop("Campos repetidos no pedido de leitura de ", basename(arquivo))
  }

  # Lendo os campos pedidos, filtrando as linhas se houver condições
  tabela <- conjunto |>
    filter(...) |>
    select(all_of(campos)) |>
    collect() |>
    as_tibble()

  # Conferindo se as colunas lidas são as pedidas, na mesma ordem
  if (!identical(names(tabela), campos)) {
    stop("A leitura de ", basename(arquivo), " devolveu colunas diferentes das pedidas.")
  }

  # Devolvendo a tabela
  tabela
}

# Criando uma função para ler um tratado GeoParquet como sf, com conferência de
# SHA-256 e dos metadados geo (versão 1.1.0, codificação WKB e CRS)
ler_tratado_geo <- function(caminho_relativo, sha256, epsg = NULL) {

  # Validando o caminho e conferindo o hash
  arquivo <- validar_caminho_tratado(caminho_relativo)
  conferir_sha256(arquivo, sha256)

  # Informando o andamento
  message("Lendo ", caminho_relativo, " (SHA-256 conferido)")

  # Lendo o arquivo como tabela do Arrow, preservando os metadados
  tabela <- read_parquet(
    file          = arquivo,
    as_data_frame = FALSE,
    mmap          = FALSE
  )

  # Interrompendo se o arquivo não tiver metadados geo
  if (is.null(tabela$schema$metadata$geo)) {
    stop(basename(arquivo), " não tem metadados GeoParquet.")
  }

  # Lendo os metadados geo
  metadados <- fromJSON(tabela$schema$metadata$geo, simplifyVector = FALSE)

  # Obtendo a coluna de geometria principal e seus metadados
  coluna         <- metadados$primary_column
  geometria_meta <- metadados$columns[[coluna]]

  # Conferindo versão e codificação
  if (!identical(metadados$version, "1.1.0") || !identical(geometria_meta$encoding, "WKB")) {
    stop(
      basename(arquivo), " não é GeoParquet 1.1.0 com WKB (versão ",
      metadados$version, ", codificação ", geometria_meta$encoding, ")."
    )
  }

  # Obtendo o código EPSG registrado no PROJJSON
  epsg_arquivo <- as.integer(geometria_meta$crs$id$code)

  # Interrompendo se o CRS não tiver código EPSG
  if (length(epsg_arquivo) != 1 || is.na(epsg_arquivo) ||
      !identical(geometria_meta$crs$id$authority, "EPSG")) {
    stop(basename(arquivo), " não registra o CRS por código EPSG.")
  }

  # Conferindo o CRS esperado, se informado
  if (!is.null(epsg) && !identical(epsg_arquivo, as.integer(epsg))) {
    stop(basename(arquivo), " está em EPSG:", epsg_arquivo, ", não em EPSG:", epsg, ".")
  }

  # Convertendo a tabela do Arrow em tibble
  atributos <- as_tibble(as.data.frame(tabela))

  # Separando a coluna de geometria
  wkb <- atributos[[coluna]]
  atributos[[coluna]] <- NULL

  # Convertendo a geometria em sfc, conforme o tipo devolvido pelo Arrow
  if (inherits(wkb, "geoarrow_vctr")) {
    geometria <- st_set_crs(st_as_sfc(wkb), epsg_arquivo)
  } else {
    wkb <- unclass(wkb)
    class(wkb) <- "WKB"
    geometria <- st_as_sfc(wkb, EWKB = FALSE, crs = epsg_arquivo)
  }

  # Devolvendo o objeto sf com a coluna geometry
  st_sf(atributos, geometry = geometria)
}

# Criando uma função para ler como sf um GeoParquet gravado pelo próprio
# projeto (adaptada de read_geoparquet_sf() do Transporte versus Adensamento).
# O write_parquet() com o geoarrow grava a geometria como extensão geoarrow, e
# a leitura devolve um geoarrow_vctr que o sf não usa até ser convertido.
ler_geoparquet <- function(arquivo) {

  # Lendo o arquivo como tibble, sem a classe sf restaurada pelo Arrow
  camada <- read_parquet(arquivo, as_data_frame = FALSE) |>
    as.data.frame() |>
    as_tibble()

  # Identificando a coluna de geometria
  coluna <- names(camada)[vapply(camada, inherits, logical(1), "geoarrow_vctr")]

  # Interrompendo se não houver exatamente uma coluna de geometria
  if (length(coluna) != 1) {
    stop(basename(arquivo), " deve ter exatamente uma coluna de geometria.")
  }

  # Convertendo a geometria em sfc, com o CRS gravado na extensão
  camada[[coluna]] <- st_as_sfc(camada[[coluna]])

  # Padronizando o nome da coluna de geometria
  names(camada)[names(camada) == coluna] <- "geometry"

  # Devolvendo o objeto sf
  st_as_sf(camada, sf_column_name = "geometry")
}
