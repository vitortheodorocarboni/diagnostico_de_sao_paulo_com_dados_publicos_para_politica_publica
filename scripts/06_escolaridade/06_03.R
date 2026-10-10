################################################################################
##################################            ##################################
################## 06.03) MAPAS DA ESCOLARIDADE POR DISTRITO ###################
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
  pct_superior       = c(10, 20, 30, 40, 50, 60, 70),
  pct_sem_instrucao  = c(10, 15, 20, 25, 30, 35),
  taxa_alfabetizacao = c(94, 95, 96, 97, 98, 99)
)
extremos_mapas <- list(
  pct_superior       = c(0, 90),
  pct_sem_instrucao  = c(0, 45),
  taxa_alfabetizacao = c(90, 100)
)

# Definindo os mapas: indicador, título, subtítulo, base dos dados e nota
mapas <- tribble(
  ~indicador,           ~titulo,                                      ~subtitulo,                                                   ~base,      ~nota,
  "pct_superior",       "Superior completo",                          "Pessoas de 25 anos ou mais com superior completo, por distrito, 2022",           "amostra",  "Nível de instrução compatível com o Censo 2010 (P0770), sobre as pessoas de 25 anos ou mais.",
  "pct_sem_instrucao",  "Sem instrução ou fundamental incompleto",    "Pessoas de 25 anos ou mais sem instrução ou com fundamental incompleto, por distrito, 2022", "amostra", "Nível de instrução compatível com o Censo 2010 (P0770), sobre as pessoas de 25 anos ou mais.",
  "taxa_alfabetizacao", "Taxa de alfabetização",                      "Pessoas de 15 anos ou mais que sabem ler e escrever, por distrito, 2022",          "universo", "Pessoas alfabetizadas sobre as pessoas de 15 anos ou mais, somas publicadas do universo."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da escolaridade
escolaridade <- read_parquet(file.path(diretorio_grupo, "escolaridade.parquet"))

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
  valor_municipio <- escolaridade |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      escolaridade |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
        select(codigo_distrito, valor, amostra_menor),
      by = "codigo_distrito"
    )

  # Definindo fontes e notas conforme a base (amostra ou universo)
  if (mapa_atual$base == "amostra") {
    fontes <- "censo_amostra"
    notas  <- c(mapa_atual$nota, "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado")
  } else {
    fontes <- "censo_universo"
    notas  <- c(mapa_atual$nota, "sigilo_pessoas")
  }

  # Desenhando o mapa coroplético em classes fixas
  mapa <- ggplot(mapa_dados) +
    geom_sf(
      mapping   = aes(fill = valor),
      colour    = cor_fundo,
      linewidth = 0.2
    )

  # Contornando com tracejado os distritos com amostra menor (só na amostra)
  if (mapa_atual$base == "amostra") {
    mapa <- mapa +
      geom_sf(
        data      = filter(mapa_dados, amostra_menor),
        fill      = NA,
        colour    = cor_texto,
        linetype  = "22",
        linewidth = 0.45
      )
  }

  # Completando o mapa
  mapa <- mapa +
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
        "Município: ", formatar_numero(valor_municipio, 1), "%"
      ),
      caption  = legenda_fonte(
        fontes  = fontes,
        notas   = notas,
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
