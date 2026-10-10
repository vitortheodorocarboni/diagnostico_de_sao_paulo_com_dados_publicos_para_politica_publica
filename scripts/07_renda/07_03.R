################################################################################
##################################            ##################################
###################### 07.03) MAPAS DA RENDA POR DISTRITO ######################
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
  renda_media     = c(1500, 2000, 3000, 4000, 6000, 8000),
  pct_ate_meio_sm = c(5, 10, 15, 20, 25, 30),
  pct_mais_5_sm   = c(2, 5, 10, 20, 30, 45)
)
extremos_mapas <- list(
  renda_media     = c(800, 12500),
  pct_ate_meio_sm = c(0, 35),
  pct_mais_5_sm   = c(0, 60)
)

# Definindo os rótulos e os nomes da legenda de cada mapa
rotulos_mapas <- list(
  renda_media     = rotulo_numero(prefix = "R$ "),
  pct_ate_meio_sm = rotulo_numero(suffix = "%"),
  pct_mais_5_sm   = rotulo_numero(suffix = "%")
)
legendas_mapas <- c(
  renda_media     = "R$ por mês,\npor pessoa",
  pct_ate_meio_sm = "% dos\ndomicílios",
  pct_mais_5_sm   = "% dos\ndomicílios"
)

# Definindo os mapas: indicador, título, subtítulo e nota
mapas <- tribble(
  ~indicador,        ~titulo,                                       ~subtitulo,                                                                                       ~nota,
  "renda_media",     "Renda domiciliar per capita média",           "Renda domiciliar per capita média dos moradores, por distrito, 2022",                              "Valores nominais de 2022; média ponderada por pessoa, sem pensionistas e empregados domésticos e seus parentes.",
  "pct_ate_meio_sm", "Domicílios até 1/2 salário mínimo",           "Domicílios com renda domiciliar per capita de até 1/2 salário mínimo, por distrito, 2022", "Salário mínimo de 2022 (R$ 1.212), logo 1/2 equivale a R$ 606; renda zero entra na primeira classe; proporção sobre os domicílios particulares ocupados.",
  "pct_mais_5_sm",   "Domicílios com mais de 5 salários mínimos",   "Domicílios com renda domiciliar per capita de mais de 5 salários mínimos, por distrito, 2022", "Salário mínimo de 2022 (R$ 1.212), logo 5 salários mínimos equivalem a R$ 6.060; proporção sobre os domicílios particulares ocupados."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da renda
renda <- read_parquet(file.path(diretorio_grupo, "renda.parquet"))

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
  valor_municipio <- renda |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      renda |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
        select(codigo_distrito, valor, amostra_menor),
      by = "codigo_distrito"
    )

  # Descrevendo o município no subtítulo
  texto_municipio <- if (mapa_atual$indicador == "renda_media") {
    paste0("Município: R$ ", formatar_numero(valor_municipio), " por mês")
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
      name    = legendas_mapas[[mapa_atual$indicador]]
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
