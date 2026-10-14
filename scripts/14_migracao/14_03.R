################################################################################
##################################            ##################################
#################### 14.03) MAPAS DA MIGRAÇÃO POR DISTRITO #####################
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
diretorio_grupo <- caminho_data("14_migracao")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("14_migracao")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  pct_nordeste           = c(8, 11, 14, 17, 20, 24),
  pct_exterior           = c(0.5, 1, 2, 3, 5, 10),
  pct_nascidos_municipio = c(55, 60, 65, 70, 74, 78),
  tempo_moradia          = c(20, 23, 26, 28, 30, 32)
)
extremos_mapas <- list(
  pct_nordeste           = c(0, 32),
  pct_exterior           = c(0, 21),
  pct_nascidos_municipio = c(45, 85),
  tempo_moradia          = c(15, 35)
)

# Definindo os rótulos da legenda de cada mapa
rotulos_mapas <- list(
  pct_nordeste           = rotulo_numero(suffix = "%"),
  pct_exterior           = rotulo_numero(accuracy = 0.1, suffix = "%"),
  pct_nascidos_municipio = rotulo_numero(suffix = "%"),
  tempo_moradia          = rotulo_numero()
)

# Definindo os mapas: indicador, título, subtítulo, legenda e nota
mapas <- tribble(
  ~indicador,               ~titulo,                                 ~subtitulo,                                                                                                   ~legenda,               ~nota,
  "pct_nordeste",           "Moradores nascidos no Nordeste",        "Moradores nascidos em um estado do Nordeste, entre todos os moradores, por distrito, 2022",                  "% dos\nmoradores",     "Região pelo primeiro dígito do código de UF do IBGE (P0490).",
  "pct_exterior",           "Moradores nascidos no exterior",        "Moradores nascidos em outro país, entre todos os moradores, por distrito, 2022",                              "% dos\nmoradores",     "Classes desiguais por causa da concentração no centro; inclui naturalizados e brasileiros nascidos no exterior.",
  "pct_nascidos_municipio", "Moradores nascidos na capital",         "Moradores nascidos no Município de São Paulo, entre todos os moradores, por distrito, 2022",                 "% dos\nmoradores",     "Nascido no município onde mora (P0480).",
  "tempo_moradia",          "Tempo de moradia na capital",           "Tempo médio de moradia no município de quem nasceu fora dele ou já morou fora, por distrito, 2022",           "Anos",                 "Média sem interrupção desde a última chegada ao município; não inclui quem sempre morou na capital."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da migração
migracao <- read_parquet(file.path(diretorio_grupo, "migracao.parquet"))

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
  valor_municipio <- migracao |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      migracao |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
        select(codigo_distrito, valor, amostra_menor),
      by = "codigo_distrito"
    )

  # Descrevendo o município no subtítulo
  texto_municipio <- if (mapa_atual$indicador == "tempo_moradia") {
    paste0("Município: ", formatar_numero(valor_municipio, 1), " anos")
  } else {
    paste0("Município: ", formatar_numero(valor_municipio, 1), "%")
  }

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
