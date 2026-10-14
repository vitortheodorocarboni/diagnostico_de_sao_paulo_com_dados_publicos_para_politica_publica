################################################################################
##################################            ##################################
################ 15.11) MAPAS DE ATIVIDADE E LOCAL DE TRABALHO #################
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
  pct_comercio            = c(12, 14, 16, 18, 22),
  pct_servicos_domesticos = c(1, 2, 3, 4, 6),
  pct_outro_municipio     = c(1.5, 2.5, 3.5, 5, 6.5)
)
extremos_mapas <- list(
  pct_comercio            = c(6, 40),
  pct_servicos_domesticos = c(0, 11),
  pct_outro_municipio     = c(0, 9)
)

# Definindo os rótulos da legenda de cada mapa
rotulos_mapas <- list(
  pct_comercio            = rotulo_numero(suffix = "%"),
  pct_servicos_domesticos = rotulo_numero(suffix = "%"),
  pct_outro_municipio     = rotulo_numero(accuracy = 0.1, suffix = "%")
)

# Definindo os mapas: indicador, título, subtítulo, legenda e nota
mapas <- tribble(
  ~indicador,                ~titulo,                                    ~subtitulo,                                                                                             ~legenda,             ~nota,
  "pct_comercio",            "Ocupados no comércio",                     "Moradores ocupados cuja atividade do trabalho principal é o comércio, entre todos os ocupados, por distrito, 2022",   "% dos
ocupados",   "Seção G da CNAE-Domiciliar (comércio e reparação de veículos automotores e motocicletas), entre os ocupados de 10 anos ou mais com atividade (P0980).",
  "pct_servicos_domesticos", "Ocupados em serviços domésticos",          "Moradores ocupados em serviços domésticos, entre todos os ocupados, por distrito, 2022",                              "% dos
ocupados",   "Seção T da CNAE-Domiciliar, entre os ocupados de 10 anos ou mais com atividade (P0980).",
  "pct_outro_municipio",     "Trabalham em outro município",             "Moradores ocupados que trabalham em outro município, entre todos os ocupados, por distrito, 2022",                     "% dos
ocupados",   "Local de trabalho (P1120 código 3), entre os ocupados de 10 anos ou mais. Os municípios de trabalho mais frequentes estão na figura da cidade."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de atividade e o do trabalho em outro município
recente <- bind_rows(
  read_parquet(file.path(diretorio_grupo, "atividade.parquet")),
  read_parquet(file.path(diretorio_grupo, "trabalho.parquet")) |>
    filter(indicador == "pct_outro_municipio") |>
    select(nivel, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor, indicador, valor)
)

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
