################################################################################
##################################            ##################################
################ 14.10) MAPAS DA MIGRAÇÃO RECENTE POR DISTRITO #################
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
  pct_migrou_5anos     = c(1.5, 2.5, 3.5, 5, 8),
  pct_exterior_5anos   = c(0.1, 0.25, 0.5, 1, 2, 4),
  pct_recente_nordeste = c(10, 20, 30, 40, 50)
)
extremos_mapas <- list(
  pct_migrou_5anos     = c(0, 14),
  pct_exterior_5anos   = c(0, 8),
  pct_recente_nordeste = c(0, 70)
)

# Definindo os rótulos da legenda de cada mapa
rotulos_mapas <- list(
  pct_migrou_5anos     = rotulo_numero(accuracy = 0.1, suffix = "%"),
  pct_exterior_5anos   = rotulo_numero(accuracy = 0.1, suffix = "%"),
  pct_recente_nordeste = rotulo_numero(suffix = "%")
)

# Definindo os mapas: indicador, título, subtítulo, legenda e nota
mapas <- tribble(
  ~indicador,             ~titulo,                                         ~subtitulo,                                                                                                           ~legenda,                          ~nota,
  "pct_migrou_5anos",     "Moravam fora do município há 5 anos",           "Pessoas de 5 anos ou mais que moravam em outro município ou país há 5 anos, por distrito, 2022",                       "% das pessoas\nde 5 anos\nou mais", "Local de moradia há 5 anos (P0600): outro município do Brasil ou outro país. A pergunta vale para quem mora há menos de 6 anos no município.",
  "pct_exterior_5anos",   "Moravam no exterior há 5 anos",                 "Pessoas de 5 anos ou mais que moravam em outro país há 5 anos, por distrito, 2022",                                    "% das pessoas\nde 5 anos\nou mais", "Classes desiguais por causa da concentração em poucos distritos; categoria rara, de poucos registros na maioria dos distritos.",
  "pct_recente_nordeste", "Nordeste entre quem veio de fora há 5 anos",    "Pessoas que moravam no Nordeste há 5 anos, entre as que moravam em outro município ou país, por distrito, 2022",      "% de quem\nveio de fora",         "Região pelo primeiro dígito do código de UF do IBGE (P0610). Base pequena nos distritos de pouca migração recente: ler com cautela."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da migração recente
recente <- read_parquet(file.path(diretorio_grupo, "recente.parquet"))

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
  valor_municipio <- recente |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      recente |>
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
        "Município: ", formatar_numero(valor_municipio, 1), "%"
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
