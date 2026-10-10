################################################################################
##################################            ##################################
################ 04.03) MAPAS DA ESTRUTURA ETÁRIA POR DISTRITO #################
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
diretorio_grupo <- caminho_data("04_etarias")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("04_etarias")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  envelhecimento = c(75, 100, 150, 200, 250),
  pct_0_14       = c(10, 12, 14, 16, 18, 20)
)
extremos_mapas <- list(
  envelhecimento = c(0, 300),
  pct_0_14       = c(5, 25)
)

# Definindo os mapas: indicador, título, subtítulo, legenda, sufixo e nota
mapas <- tribble(
  ~indicador,       ~titulo,                        ~subtitulo,                                                             ~legenda,                                        ~sufixo, ~nota,
  "envelhecimento", "Índice de envelhecimento",     "Pessoas de 60 anos ou mais por 100 de 0 a 14 anos, por distrito, 2022", "60 anos ou mais\npor 100 de\n0 a 14 anos",    "",      "Valores acima de 100 indicam mais pessoas de 60 anos ou mais que de 0 a 14 anos. O padrão do IBGE usa 65 anos ou mais, que os grupos publicados não permitem.",
  "pct_0_14",       "Crianças e adolescentes",      "Moradores de 0 a 14 anos, por distrito, 2022",                          "% dos\nmoradores",                              "%",     "Proporção sobre a soma publicada dos 11 grupos de idade."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de idade
idade <- read_parquet(file.path(diretorio_grupo, "idade.parquet"))

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
  valor_municipio <- idade |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      idade |>
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
      labels  = rotulo_numero(suffix = mapa_atual$sufixo),
      name    = mapa_atual$legenda
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        mapa_atual$subtitulo, "\n",
        "Município: ", formatar_numero(valor_municipio, 1), mapa_atual$sufixo
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
