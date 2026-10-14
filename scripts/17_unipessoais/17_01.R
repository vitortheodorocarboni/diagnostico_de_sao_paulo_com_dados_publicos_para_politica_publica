################################################################################
##################################            ##################################
################## 17.01) BUSCAR OS TRATADOS DOS UNIPESSOAIS ###################
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

# Definindo o caminho e o SHA-256 da amostra por área de ponderação (versão de
# 2026-10-13, com os domicílios unipessoais pelo perfil do responsável)
arquivo_amostra <- "censo_2022/areas_de_ponderacao/tratados/tabulares/censo_amostra_areas_ponderacao.parquet"
sha256_amostra  <- "7bec0b2f873264908f1304fb3c6e2ee6e015b4df9231cc0f6eec419b7137bc6b"

# Definindo o caminho e o SHA-256 do universo por setor
arquivo_setores <- "censo_2022/setores_censitarios/tratados/tabulares/censo_universo_setores.parquet"
sha256_setores  <- "d5839bb3e9ea6f8829e2822b8b8fb18d1a62493b24389856195d8629efc7e0e9"

# Definindo o caminho e o SHA-256 da tabela municipal do universo
arquivo_municipios <- "censo_2022/municipios/tabulares/censo_universo_municipios.parquet"
sha256_municipios  <- "820914a3bd6d1f43053e5bace602ebba6673a0a3d5f3dd580b5df2ff92e12c9e"

# Descrevendo os cinco recortes da amostra: o campo do total de domicílios
# particulares ocupados, o dos unipessoais e as categorias de cada um (códigos
# do tratado: sexo do responsável, faixa de idade, cor ou raça, instrução de
# P0770 e classe de salário mínimo da renda per capita)
recortes_amostra <- tribble(
  ~recorte,      ~campo_total,                  ~campo_unipessoal,                         ~categorias,
  "sexo",        "d0170_dpo",                   "d0170_dpo_unipessoal",                    list(c("1", "2", "9")),
  "idade",       "d0180_dpo",                   "d0180_dpo_unipessoal",                    list(c(sprintf("%02d", 1:17), "99")),
  "cor_raca",    "p0210_responsavel_dpo",       "p0210_responsavel_dpo_unipessoal",        list(c(as.character(1:5), "9")),
  "instrucao",   "p0770_responsavel_dpo",       "p0770_responsavel_dpo_unipessoal",        list(as.character(1:5)),
  "renda",       "d0360_classe_salario_minimo", "d0360_classe_salario_minimo_unipessoal",  list(sprintf("%02d", 1:10))
)

# Montando os campos da amostra de um campo e suas categorias (estimativas e
# pesos observado, ignorado e ausente) e, nos unipessoais, os registros
campos_do_campo <- function(campo, categorias, com_registros) {
  categorias <- unlist(categorias)
  c(
    sprintf("%s_cat_codigo_%s_estimativa", campo, categorias),
    paste0(campo, c("_peso_observado", "_peso_ignorado", "_peso_ausente")),
    if (com_registros) sprintf("%s_cat_codigo_%s_n_registros", campo, categorias)
  )
}

# Definindo os campos da amostra usados no grupo (significados no dicionário
# das áreas de ponderação do Dados)
campos_amostra <- c(
  # Registros de pessoas da AP, para o aviso de amostra menor geral
  "pessoas_n_registros",
  pmap(
    .l = recortes_amostra,
    .f = \(recorte, campo_total, campo_unipessoal, categorias) c(
      campos_do_campo(campo_total, categorias, com_registros = FALSE),
      campos_do_campo(campo_unipessoal, categorias, com_registros = TRUE)
    )
  ) |>
    unlist()
)

# Definindo os campos do universo usados no grupo: as quatro espécies de unidade
# doméstica (unipessoal v01209) e o sexo do responsável nos unipessoais
# (v01214 masculino e v01215 feminino)
campos_universo <- c(
  "v0001",
  "v01209",
  "v01210",
  "v01211",
  "v01212",
  "v01214",
  "v01215"
)

# Definindo o diretório de saída
diretorio_saida <- caminho_data("17_unipessoais")


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
message("APs da capital: ", nrow(amostra), "; campos da amostra: ", ncol(amostra))

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
message("Extratos dos unipessoais gravados em ", diretorio_saida)
