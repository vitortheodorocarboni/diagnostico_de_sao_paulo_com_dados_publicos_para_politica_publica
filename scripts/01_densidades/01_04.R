################################################################################
##################################            ##################################
#################### 01.04) MAPA DE MORADORES POR DOMICÍLIO ####################
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
arquivo_saida <- caminho_resultados("01_densidades", "mapa_moradores_por_domicilio.png")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983


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

# Obtendo a média de moradores por domicílio do município
media_municipio <- densidades |>
  filter(nivel == "municipio", indicador == "moradores_por_domicilio") |>
  pull(valor)

# Juntando a média de cada distrito à geometria projetada
mapa_dados <- distritos |>
  st_transform(epsg_projetado) |>
  inner_join(
    densidades |>
      filter(nivel == "distrito", indicador == "moradores_por_domicilio") |>
      select(codigo_distrito, valor),
    by = "codigo_distrito"
  )

# Desenhando o mapa com escala divergente centrada no valor do município
mapa <- ggplot(mapa_dados) +
  geom_sf(
    mapping   = aes(fill = valor),
    colour    = cor_fundo,
    linewidth = 0.2
  ) +
  escala_fill_divergente(
    meio   = media_municipio,
    labels = rotulo_numero(accuracy = 0.1),
    name   = "Moradores por\ndomicílio"
  ) +
  elementos_mapa() +
  labs(
    title    = "Moradores por domicílio",
    subtitle = paste0(
      "Média em domicílios particulares ocupados, 2022\n",
      "Centro da escala: município, com ", formatar_numero(media_municipio, 2), " moradores por domicílio"
    ),
    caption  = legenda_fonte(
      fontes  = "censo_universo",
      notas   = paste(
        "A média do distrito recompõe o total de moradores pela média de cada setor",
        "vezes os seus domicílios ocupados."
      ),
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
