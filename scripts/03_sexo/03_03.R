################################################################################
##################################            ##################################
################## 03.03) MAPAS DA RAZÃO DE SEXO POR DISTRITO ##################
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
diretorio_grupo <- caminho_data("03_sexo")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("03_sexo")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa (homens por 100 mulheres)
limites_mapas <- list(
  razao_sexo    = c(85, 90, 95, 100),
  razao_60_mais = c(60, 65, 70, 75, 80)
)
extremos_mapas <- list(
  razao_sexo    = c(75, 110),
  razao_60_mais = c(50, 105)
)

# Definindo os mapas: indicador, título, subtítulo e nota
mapas <- tribble(
  ~indicador,      ~titulo,                                    ~subtitulo,                                                        ~nota,
  "razao_sexo",    "Razão de sexo",                            "Homens por 100 mulheres, por distrito, 2022",                    "Razão de sexo = homens para cada 100 mulheres; 100 indica paridade e valores abaixo de 100, mais mulheres que homens.",
  "razao_60_mais", "Razão de sexo de 60 anos ou mais",         "Homens por 100 mulheres entre os moradores de 60 anos ou mais, por distrito, 2022", "Soma das faixas de 60 a 69 e de 70 anos ou mais; 100 indica paridade e valores abaixo de 100, mais mulheres que homens."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de sexo
sexo <- read_parquet(file.path(diretorio_grupo, "sexo.parquet"))

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
  valor_municipio <- sexo |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      sexo |>
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
      limits  = extremos_mapas[[mapa_atual$indicador]],
      labels  = rotulo_numero(),
      name    = "Homens por\n100 mulheres"
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        mapa_atual$subtitulo, "\n",
        "Município: ", formatar_numero(valor_municipio, 1), " homens por 100 mulheres"
      ),
      caption  = legenda_fonte(
        fontes  = "censo_universo",
        notas   = c(mapa_atual$nota, "sigilo_pessoas"),
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
