################################################################################
##################################            ##################################
##################### 11.03) MAPAS DA MORADIA POR DISTRITO #####################
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
diretorio_grupo <- caminho_data("11_moradia")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("11_moradia")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  pct_alugado          = c(20, 25, 30, 35, 40, 45),
  pct_financiando      = c(4, 6, 8, 10, 13, 16),
  pct_3_mais_banheiros = c(2, 4, 8, 15, 25, 35),
  pct_sem_internet     = c(3, 5, 7, 9, 11, 13)
)
extremos_mapas <- list(
  pct_alugado          = c(5, 55),
  pct_financiando      = c(0, 28),
  pct_3_mais_banheiros = c(0, 50),
  pct_sem_internet     = c(0, 20)
)

# Definindo os mapas: indicador, base, título, subtítulo e nota
mapas <- tribble(
  ~indicador,             ~base,      ~titulo,                                ~subtitulo,                                                                                        ~nota,
  "pct_alugado",          "amostra",  "Domicílios alugados",                  "Domicílios particulares permanentes ocupados alugados, por distrito, 2022",                       "O Censo 2022 não pergunta o valor do aluguel, só a condição de ocupação.",
  "pct_financiando",      "amostra",  "Domicílios próprios em pagamento",     "Domicílios particulares permanentes ocupados próprios e ainda em pagamento, por distrito, 2022",  "Próprio de algum morador, ainda pagando (financiamento ou outra forma de pagamento parcelado).",
  "pct_3_mais_banheiros", "universo", "Domicílios com 3 banheiros ou mais",   "Domicílios particulares permanentes ocupados com 3 banheiros ou mais de uso exclusivo, por distrito, 2022", "Banheiro de uso exclusivo do domicílio, com chuveiro e vaso sanitário; proporção sobre a soma publicada das categorias de banheiro.",
  "pct_sem_internet",     "amostra",  "Domicílios sem internet",              "Domicílios particulares permanentes ocupados sem acesso à internet, por distrito, 2022",          "Acesso à internet no domicílio, por qualquer meio."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da moradia
moradia <- read_parquet(file.path(diretorio_grupo, "moradia.parquet"))

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

  # Identificando se o indicador vem da amostra
  da_amostra <- mapa_atual$base == "amostra"

  # Obtendo o valor do município
  valor_municipio <- moradia |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada; o aviso de
  # amostra menor só vale para os indicadores da amostra
  mapa_dados <- distritos |>
    inner_join(
      moradia |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
        select(codigo_distrito, valor, amostra_menor),
      by = "codigo_distrito"
    ) |>
    mutate(
      amostra_menor = amostra_menor & da_amostra
    )

  # Montando as notas do rodapé conforme a base
  notas_mapa <- if (da_amostra) {
    c(mapa_atual$nota, "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado")
  } else {
    c(mapa_atual$nota, "sigilo_universo")
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
      labels  = rotulo_numero(suffix = "%"),
      name    = "% dos\ndomicílios"
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        str_wrap(mapa_atual$subtitulo, width = 80), "\n",
        "Município: ", formatar_numero(valor_municipio, 1), "%"
      ),
      caption  = legenda_fonte(
        fontes  = if (da_amostra) "censo_amostra" else "censo_universo",
        notas   = notas_mapa,
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
