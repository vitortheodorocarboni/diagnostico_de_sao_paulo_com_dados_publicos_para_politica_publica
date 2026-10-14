################################################################################
##################################            ##################################
################ 07.10) MAPAS DA EXTENSÃO DA RENDA POR DISTRITO ################
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
diretorio_grupo <- caminho_data("07_renda")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("07_renda")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  renda_trabalho_media     = c(2500, 3500, 5000, 7500, 10000),
  pct_outras_fontes        = c(20, 22, 24, 26, 28),
  renda_todas_fontes_media = c(1500, 2000, 3000, 4500, 7000, 10000),
  renda_familiar_pc_media  = c(1000, 1500, 2000, 3000, 5000, 8000)
)
extremos_mapas <- list(
  renda_trabalho_media     = c(1500, 17500),
  pct_outras_fontes        = c(15, 32),
  renda_todas_fontes_media = c(1000, 13500),
  renda_familiar_pc_media  = c(500, 13500)
)

# Definindo os rótulos da legenda de cada mapa
rotulos_mapas <- list(
  renda_trabalho_media     = rotulo_numero(prefix = "R$ "),
  pct_outras_fontes        = rotulo_numero(suffix = "%"),
  renda_todas_fontes_media = rotulo_numero(prefix = "R$ "),
  renda_familiar_pc_media  = rotulo_numero(prefix = "R$ ")
)

# Definindo os mapas: indicador, título, subtítulo, legenda, formato do valor do
# município e nota
mapas <- tribble(
  ~indicador,                 ~titulo,                                         ~subtitulo,                                                                                                         ~legenda,                                ~prefixo, ~sufixo, ~nota,
  "renda_trabalho_media",     "Rendimento médio do trabalho",                  "Rendimento bruto mensal habitual de todos os trabalhos dos ocupados de 10 anos ou mais com rendimento, por distrito, 2022", "R$ por mês,\npor ocupado\ncom rendimento", "R$ ",    "",      "P1080, valores nominais de 2022 (salário mínimo de R$ 1.212); média ponderada dos ocupados com rendimento, sem padronização por idade.",
  "pct_outras_fontes",        "Moradores com aposentadoria, pensão ou benefício", "Pessoas com rendimento de aposentadoria, pensão, Bolsa Família, BPC ou aluguel, entre as de 10 anos ou mais, por distrito, 2022", "% das pessoas\nde 10 anos\nou mais",         "",       "%",     "P1090. Junta aposentadoria, pensão, programas de transferência e aluguel; o valor depende da estrutura etária do distrito.",
  "renda_todas_fontes_media", "Rendimento médio de todas as fontes",            "Rendimento mensal de todas as fontes, média de todas as pessoas de 10 anos ou mais (com ou sem rendimento), por distrito, 2022", "R$ por mês,\npor pessoa de\n10 anos ou mais", "R$ ",    "",      "P1110, valores nominais de 2022; quem não tem rendimento entra com zero, por isso a média é menor que a do trabalho.",
  "renda_familiar_pc_media",  "Rendimento familiar per capita médio",         "Rendimento mensal da família dividido pelo número de integrantes, média das famílias de 2 pessoas ou mais, por distrito, 2022", "R$ por mês,\npor pessoa da\nfamília", "R$ ",    "",      "F0260, valores nominais de 2022; média por família (não por pessoa), só famílias de 2 pessoas ou mais: os domicílios unipessoais não entram."
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
        "Município: ", mapa_atual$prefixo, formatar_numero(valor_municipio, if_else(mapa_atual$sufixo == "%", 1, 0)), mapa_atual$sufixo
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
