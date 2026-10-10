################################################################################
##################################            ##################################
#################### 08.03) MAPAS DA RELIGIÃO POR DISTRITO #####################
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
diretorio_grupo <- caminho_data("08_religiosidade")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("08_religiosidade")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  pct_catolica     = c(42, 46, 50, 54, 58, 62),
  pct_evangelica   = c(10, 15, 20, 25, 30, 35),
  pct_sem_religiao = c(10, 12, 14, 16, 20, 24)
)
extremos_mapas <- list(
  pct_catolica     = c(36, 68),
  pct_evangelica   = c(0, 40),
  pct_sem_religiao = c(6, 32)
)

# Definindo os mapas: indicador, título, subtítulo e nota
mapas <- tribble(
  ~indicador,         ~titulo,                               ~subtitulo,                                                                                         ~nota,
  "pct_catolica",     "Religião católica",                   "Pessoas de 10 anos ou mais que declaram religião católica (apostólica romana, brasileira ou ortodoxa), por distrito, 2022", "A religião é autodeclarada; proporção sobre a soma das categorias declaradas.",
  "pct_evangelica",   "Religião evangélica",                 "Pessoas de 10 anos ou mais que declaram religião evangélica (missão, pentecostal, indígena ou não determinada), por distrito, 2022", "A religião é autodeclarada; proporção sobre a soma das categorias declaradas.",
  "pct_sem_religiao", "Sem religião, ateu ou agnóstico",     "Pessoas de 10 anos ou mais que declaram não ter religião, ser ateias ou agnósticas, por distrito, 2022", "A religião é autodeclarada; proporção sobre a soma das categorias declaradas."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da religiosidade
religiao <- read_parquet(file.path(diretorio_grupo, "religiao.parquet"))

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
  valor_municipio <- religiao |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      religiao |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
        select(codigo_distrito, valor, amostra_menor),
      by = "codigo_distrito"
    )

  # Descrevendo o município no subtítulo
  texto_municipio <- paste0("Município: ", formatar_numero(valor_municipio, 1), "%")

  # Desenhando o mapa coroplético em classes fixas, com tracejado nos distritos
  # de amostra menor
  mapa <- ggplot(mapa_dados) +
    geom_sf(
      mapping   = aes(fill = valor),
      colour    = cor_fundo,
      linewidth = 0.2
    ) +
    geom_sf(
      data      = filter(mapa_dados, amostra_menor),
      fill      = NA,
      colour    = cor_texto,
      linetype  = "22",
      linewidth = 0.45
    ) +
    escala_fill_sequencial(
      classes = TRUE,
      breaks  = limites_mapas[[mapa_atual$indicador]],
      limits  = extremos_mapas[[mapa_atual$indicador]],
      labels  = rotulo_numero(suffix = "%"),
      name    = "% das\npessoas"
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        str_wrap(mapa_atual$subtitulo, width = 80), "\n",
        texto_municipio
      ),
      caption  = legenda_fonte(
        fontes  = "censo_amostra",
        notas   = c(mapa_atual$nota, "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado"),
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
