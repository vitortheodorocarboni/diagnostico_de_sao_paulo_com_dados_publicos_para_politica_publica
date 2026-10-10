################################################################################
##################################            ##################################
##################### 01.03) MAPA DA DENSIDADE DEMOGRÁFICA #####################
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

# Definindo o diretório de dados do grupo
diretorio_grupo <- caminho_data("01_densidades")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("01_densidades", "mapa_densidade_demografica.png")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo os limites das classes de densidade (moradores por km²)
limites_classes <- c(5000, 10000, 15000, 20000, 25000)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de densidade
densidades <- read_parquet(file.path(diretorio_grupo, "densidades.parquet"))

# Lendo os distritos com geometria
distritos <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet"))


################################################################################
##### IV. Mapa
################################################################################

# Obtendo a densidade do município
densidade_municipio <- densidades |>
  filter(nivel == "municipio", indicador == "densidade_demografica") |>
  pull(valor)

# Juntando a densidade de cada distrito à geometria projetada
mapa_dados <- distritos |>
  st_transform(epsg_projetado) |>
  inner_join(
    densidades |>
      filter(nivel == "distrito", indicador == "densidade_demografica") |>
      select(codigo_distrito, valor),
    by = "codigo_distrito"
  )

# Desenhando o mapa coroplético em classes fixas
mapa <- ggplot(mapa_dados) +
  geom_sf(
    mapping   = aes(fill = valor),
    colour    = cor_fundo,
    linewidth = 0.2
  ) +
  escala_fill_sequencial(
    classes = TRUE,
    breaks  = limites_classes,
    limits  = c(0, 30000),
    name    = "Moradores\npor km²"
  ) +
  elementos_mapa() +
  labs(
    title    = "Densidade demográfica",
    subtitle = paste0(
      "Distritos do Município de São Paulo, 2022\n",
      "Município: ", formatar_numero(densidade_municipio), " moradores por km²"
    ),
    caption  = legenda_fonte(
      fontes  = "censo_universo",
      notas   = "A densidade usa a área total do distrito, que inclui parques, represas e zona rural.",
      largura = 95
    )
  ) +
  tema_mapa()


################################################################################
##### V. Exportação
################################################################################

# Gravando o mapa
gravar_figura(
  grafico = mapa,
  arquivo = arquivo_saida,
  tamanho = "mapa"
)
