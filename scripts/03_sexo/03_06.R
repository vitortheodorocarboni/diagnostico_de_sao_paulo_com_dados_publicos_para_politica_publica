################################################################################
##################################            ##################################
##################### 03.06) GRÁFICOS POR DISTRITO DE SEXO #####################
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

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Carregando a identidade visual
source(file.path("scripts", "_config", "identidade_visual.R"), encoding = "UTF-8")

# Carregando as funções dos gráficos por distrito
source(file.path("scripts", "_config", "graficos_distritos.R"), encoding = "UTF-8")

# Definindo o diretório de dados do grupo
diretorio_grupo <- caminho_data("03_sexo")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("03_sexo", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo os indicadores com gráfico por distrito
indicadores_distritais <- c(
  "razao_sexo",
  "razao_30_59",
  "razao_60_mais"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  razao_sexo    = "Razão de sexo = homens para cada 100 mulheres; 100 indica paridade e valores abaixo de 100, mais mulheres que homens.",
  razao_30_59   = "Soma das faixas de 30 a 39, 40 a 49 e 50 a 59 anos; 100 indica paridade e valores abaixo de 100, mais mulheres que homens.",
  razao_60_mais = "Soma das faixas de 60 a 69 e de 70 anos ou mais; 100 indica paridade e valores abaixo de 100, mais mulheres que homens."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de sexo
sexo <- read_parquet(file.path(diretorio_grupo, "sexo.parquet"))

# Lendo a descrição dos indicadores
indicadores <- read_parquet(file.path(diretorio_grupo, "indicadores.parquet"))

# Lendo os distritos e simplificando a geometria para o mapa de localização
mapa_base <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet")) |>
  st_transform(epsg_projetado) |>
  st_simplify(
    preserveTopology = TRUE,
    dTolerance       = 40
  ) |>
  select(codigo_distrito)


################################################################################
##### IV. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando as figuras de cada indicador e distrito
for (indicador_atual in indicadores_distritais) {

  # Obtendo a descrição do indicador
  descricao <- indicadores |>
    filter(indicador == indicador_atual)

  # Obtendo o valor do município
  valor_municipio <- sexo |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Obtendo os valores dos distritos com a posição de cada um
  valores <- sexo |>
    filter(nivel == "distrito", indicador == indicador_atual) |>
    mutate(
      posicao = min_rank(desc(valor))
    )

  # Informando o andamento
  message("Gerando os gráficos por distrito de ", indicador_atual)

  # Gravando a figura de cada distrito
  for (codigo in valores$codigo_distrito) {

    # Montando a figura do distrito
    figura <- figura_distrito(
      mapa_base       = mapa_base,
      valores         = valores,
      codigo          = codigo,
      valor_municipio = valor_municipio,
      descricao       = descricao,
      nota            = c(notas_indicadores[[indicador_atual]], "sigilo_pessoas")
    )

    # Gravando a figura com o código e o nome do distrito no nome do arquivo
    gravar_figura(
      grafico = figura,
      arquivo = file.path(
        diretorio_saida,
        nome_arquivo_distrito(
          codigo = codigo,
          nome   = valores$nome_rotulo[valores$codigo_distrito == codigo],
          sufixo = indicador_atual
        )
      ),
      tamanho = "grafico"
    )
  }
}
