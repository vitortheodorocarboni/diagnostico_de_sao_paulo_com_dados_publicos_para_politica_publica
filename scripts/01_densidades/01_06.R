################################################################################
##################################            ##################################
################# 01.06) GRÁFICOS POR DISTRITO DOS INDICADORES #################
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
diretorio_grupo <- caminho_data("01_densidades")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("01_densidades", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo os indicadores com gráfico por distrito
indicadores_distritais <- c(
  "densidade_demografica",
  "densidade_domiciliar",
  "moradores_por_domicilio"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  densidade_demografica   = "A densidade usa a área total do distrito, que inclui parques, represas e zona rural.",
  densidade_domiciliar    = "A densidade usa a área total do distrito, que inclui parques, represas e zona rural.",
  moradores_por_domicilio = paste(
    "Média em domicílios particulares ocupados; a média do distrito recompõe o total de",
    "moradores pela média de cada setor vezes os seus domicílios ocupados."
  )
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de densidade
densidades <- read_parquet(file.path(diretorio_grupo, "densidades.parquet"))

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
  valor_municipio <- densidades |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Obtendo os valores dos distritos com a posição de cada um
  valores <- densidades |>
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
      nota            = notas_indicadores[[indicador_atual]]
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
