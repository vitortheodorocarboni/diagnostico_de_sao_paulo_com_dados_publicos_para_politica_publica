################################################################################
##################################            ##################################
################ 06.10) MAPAS DA EXTENSÃO ESCOLAR POR DISTRITO #################
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
diretorio_grupo <- caminho_data("06_escolaridade")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("06_escolaridade")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  pct_frequenta_escola    = c(22, 24, 26, 28, 30),
  pct_estudantes_superior = c(10, 15, 20, 30, 40),
  anos_estudo_medio       = c(8, 9, 10, 11, 12, 13)
)
extremos_mapas <- list(
  pct_frequenta_escola    = c(15, 40),
  pct_estudantes_superior = c(4, 60),
  anos_estudo_medio       = c(7, 14.5)
)

# Definindo os rótulos da legenda de cada mapa
rotulos_mapas <- list(
  pct_frequenta_escola    = rotulo_numero(suffix = "%"),
  pct_estudantes_superior = rotulo_numero(suffix = "%"),
  anos_estudo_medio       = rotulo_numero()
)

# Definindo os mapas: indicador, título, subtítulo, legenda, sufixo do valor do
# município e nota
mapas <- tribble(
  ~indicador,                ~titulo,                                    ~subtitulo,                                                                                                   ~legenda,                              ~sufixo,  ~nota,
  "pct_frequenta_escola",    "Moradores que frequentam escola ou creche", "Pessoas que frequentam escola ou creche, entre todos os moradores, por distrito, 2022",                  "% dos\nmoradores",                    "%",      "P0650. O valor depende da estrutura etária: distritos com mais crianças e jovens têm mais gente na escola; o tratado não permite padronizá-lo.",
  "pct_estudantes_superior", "Estudantes na graduação ou na pós",         "Estudantes de graduação, especialização, mestrado e doutorado, entre quem frequenta escola ou creche, por distrito, 2022", "% de quem\nfrequenta escola\nou creche", "%",      "P0660, códigos 08 a 11 sobre quem frequenta escola ou creche; não mede a taxa de acesso ao ensino superior da população de 18 a 24 anos.",
  "anos_estudo_medio",       "Anos de estudo médios",                    "Anos de estudo médios de todos os moradores, crianças inclusive, por distrito, 2022",                       "Anos de\nestudo",                     " anos",  "P0790, média ponderada por pessoa. Inclui as crianças, que ainda estudam: a média depende da estrutura etária, e o tratado não permite padronizá-la."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da extensão
extensao <- read_parquet(file.path(diretorio_grupo, "extensao.parquet"))

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
  valor_municipio <- extensao |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      extensao |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
        select(codigo_distrito, valor, amostra_menor),
      by = "codigo_distrito"
    )

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
      labels  = rotulos_mapas[[mapa_atual$indicador]],
      name    = mapa_atual$legenda
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        str_wrap(mapa_atual$subtitulo, width = 80), "\n",
        "Município: ", formatar_numero(valor_municipio, 1), mapa_atual$sufixo
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
