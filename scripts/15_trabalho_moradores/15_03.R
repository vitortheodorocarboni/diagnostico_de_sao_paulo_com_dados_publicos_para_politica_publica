################################################################################
##################################            ##################################
#################### 15.03) MAPAS DO TRABALHO DOS MORADORES ####################
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
diretorio_grupo <- caminho_data("15_trabalho_moradores")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("15_trabalho_moradores")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  pct_mais_1h       = c(10, 20, 30, 40, 45, 50),
  pct_desocupacao   = c(3, 4, 5, 6, 7, 8),
  pct_informalidade = c(20, 22, 24, 26, 28, 31),
  pct_em_casa       = c(14, 17, 20, 23, 26, 30)
)
extremos_mapas <- list(
  pct_mais_1h       = c(0, 62),
  pct_desocupacao   = c(1, 11),
  pct_informalidade = c(15, 45),
  pct_em_casa       = c(8, 40)
)

# Definindo os rótulos da legenda de cada mapa
rotulos_mapas <- list(
  pct_mais_1h       = rotulo_numero(suffix = "%"),
  pct_desocupacao   = rotulo_numero(suffix = "%"),
  pct_informalidade = rotulo_numero(suffix = "%"),
  pct_em_casa       = rotulo_numero(suffix = "%")
)

# Definindo os mapas: indicador, título, subtítulo, legenda e nota
mapas <- tribble(
  ~indicador,          ~titulo,                                         ~subtitulo,                                                                                                                 ~legenda,                    ~nota,
  "pct_mais_1h",       "Mais de 1 hora até o trabalho",                 "Moradores ocupados que levam mais de 1 hora de casa até o trabalho, entre os que se deslocam, por distrito, 2022",         "% de quem\nse desloca",     "Tempo habitual de casa até o trabalho (P1180), de quem trabalha fora de casa e se desloca, com tempo informado.",
  "pct_desocupacao",   "Desocupação",                                   "Moradores desocupados, entre os de 14 anos ou mais na força de trabalho, por distrito, 2022",                              "% da força\nde trabalho",   "Desocupado: sem trabalho na semana de referência, que procurou trabalho e estava disponível (P0950).",
  "pct_informalidade", "Ocupados sem contribuir para a previdência",    "Moradores ocupados de 14 anos ou mais que não contribuem para instituto de previdência no trabalho principal, por distrito, 2022", "% dos\nocupados",       "Medida de informalidade pela contribuição previdenciária (P0940).",
  "pct_em_casa",       "Trabalho em casa",                              "Moradores ocupados que trabalham em casa ou na propriedade, por distrito, 2022",                                           "% dos\nocupados",           "Local do trabalho principal (P1120); o ignorado fica fora do cálculo."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores do trabalho dos moradores
trabalho <- read_parquet(file.path(diretorio_grupo, "trabalho.parquet"))

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
  valor_municipio <- trabalho |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      trabalho |>
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
