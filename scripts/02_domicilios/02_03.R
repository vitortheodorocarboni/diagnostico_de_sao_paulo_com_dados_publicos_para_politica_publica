################################################################################
##################################            ##################################
################### 02.03) MAPAS DOS DOMICÍLIOS POR DISTRITO ###################
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
diretorio_grupo <- caminho_data("02_domicilios")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("02_domicilios")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes de cada mapa (limites em pontos percentuais)
limites_mapas <- list(
  apartamento     = c(20, 40, 60, 80),
  sem_esgoto_rede = c(2, 5, 10, 20, 40),
  sem_arvores     = c(10, 20, 30, 40, 50)
)

# Definindo os mapas: indicador, título, subtítulo e nota
mapas <- tribble(
  ~indicador,        ~titulo,                            ~subtitulo,                                                                         ~nota,
  "apartamento",     "Domicílios em apartamentos",       "Proporção dos domicílios particulares permanentes ocupados, por distrito, 2022",  "Proporção sobre a soma publicada dos tipos de domicílio.",
  "sem_esgoto_rede", "Domicílios sem esgoto em rede",    "Proporção dos domicílios particulares permanentes ocupados, por distrito, 2022",  "Esgoto em rede: rede geral ou pluvial e fossa séptica ligada à rede. Os demais destinos (fossa não ligada, rudimentar, vala, rio, outra forma ou sem banheiro) contam como sem esgoto em rede.",
  "sem_arvores",     "Domicílios em faces sem árvores",  "Proporção dos domicílios do entorno com resposta sobre arborização, por distrito, 2022", "O entorno foi pesquisado em face de quadra; o saltado e o não declarado ficam fora do denominador."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos domicílios
dominios <- read_parquet(file.path(diretorio_grupo, "dominios.parquet"))

# Lendo a descrição dos indicadores
indicadores <- read_parquet(file.path(diretorio_grupo, "indicadores.parquet"))

# Lendo os distritos com geometria
distritos <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet")) |>
  st_transform(epsg_projetado)


################################################################################
##### IV. Mapas
################################################################################

# Gravando um mapa por indicador
for (i in seq_len(nrow(mapas))) {

  # Selecionando a linha do mapa
  mapa_atual <- mapas[i, ]

  # Obtendo o valor do município
  valor_municipio <- dominios |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      dominios |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
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
      breaks  = limites_mapas[[mapa_atual$indicador]],
      limits  = c(0, 100),
      labels  = rotulo_numero(suffix = "%"),
      name    = "Proporção"
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        mapa_atual$subtitulo, "\n",
        "Município: ", formatar_numero(valor_municipio, 1), "%"
      ),
      caption  = legenda_fonte(
        fontes  = "censo_universo",
        notas   = c(mapa_atual$nota, "sigilo_universo"),
        largura = 95
      )
    ) +
    tema_mapa()

  # Gravando o mapa
  gravar_figura(
    grafico = mapa,
    arquivo = file.path(diretorio_saida, paste0("mapa_", mapa_atual$indicador, ".png")),
    tamanho = "mapa"
  )
}
