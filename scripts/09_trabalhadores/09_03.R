################################################################################
##################################            ##################################
################# 09.03) MAPAS DOS TRABALHADORES POR DISTRITO ##################
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
diretorio_grupo <- caminho_data("09_trabalhadores")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("09_trabalhadores")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes, os limites, a transformação e os rótulos de cada mapa
# (a razão de trabalhadores por morador fica em escala log)
limites_mapas <- list(
  trabalhadores_por_morador = c(0.2, 0.3, 0.5, 1, 2, 4),
  pct_mora_distrito         = c(10, 20, 30, 40, 50, 60, 70),
  pct_negra                 = c(30, 35, 40, 45, 50, 55, 60)
)
extremos_mapas <- list(
  trabalhadores_por_morador = c(0.1, 6),
  pct_mora_distrito         = c(0, 90),
  pct_negra                 = c(25, 72)
)
transformacoes_mapas <- c(
  trabalhadores_por_morador = "log10",
  pct_mora_distrito         = "identity",
  pct_negra                 = "identity"
)
rotulos_mapas <- list(
  trabalhadores_por_morador = rotulo_numero(accuracy = 0.1),
  pct_mora_distrito         = rotulo_numero(suffix = "%"),
  pct_negra                 = rotulo_numero(suffix = "%")
)
legendas_mapas <- c(
  trabalhadores_por_morador = "Trabalhadores\npor morador",
  pct_mora_distrito         = "% dos\ntrabalhadores",
  pct_negra                 = "% dos\ntrabalhadores"
)

# Definindo os mapas: indicador, título, subtítulo e nota
mapas <- tribble(
  ~indicador,                  ~titulo,                                   ~subtitulo,                                                                                          ~nota,
  "trabalhadores_por_morador", "Trabalhadores por morador",               "Trabalhadores com local fixo de trabalho no distrito sobre os moradores do distrito, 2023",        "Trabalhadores e moradores estimados pela OD; escala logarítmica. Acima de 1, o distrito recebe mais trabalhadores do que tem moradores.",
  "pct_mora_distrito",         "Trabalhadores que moram no distrito",     "Proporção dos trabalhadores com local fixo no distrito que moram no próprio distrito, 2023",        "No município, a soma dos trabalhadores que moram no distrito em que trabalham.",
  "pct_negra",                 "Pretos e pardos entre os trabalhadores",  "Proporção de pretos e pardos entre os trabalhadores com local fixo no distrito, 2023",              "Cor ou raça declarada; a sem declaração fica fora do denominador."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos trabalhadores
trabalhadores <- read_parquet(file.path(diretorio_grupo, "trabalhadores.parquet"))

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
  indicador_atual <- mapa_atual$indicador

  # Obtendo o valor do município
  valor_municipio <- trabalhadores |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      trabalhadores |>
        filter(nivel == "distrito", indicador == indicador_atual) |>
        select(codigo_distrito, valor, amostra_menor),
      by = "codigo_distrito"
    )

  # Descrevendo o município no subtítulo
  texto_municipio <- if (indicador_atual == "trabalhadores_por_morador") {
    paste0("Município: ", formatar_numero(valor_municipio, 2), " trabalhador por morador")
  } else {
    paste0("Município: ", formatar_numero(valor_municipio, 1), "%")
  }

  # Desenhando o mapa coroplético em classes fixas, com tracejado nos distritos
  # de amostra menor e cinza nos sem valor
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
      classes   = TRUE,
      breaks    = limites_mapas[[indicador_atual]],
      limits    = extremos_mapas[[indicador_atual]],
      transform = transformacoes_mapas[[indicador_atual]],
      labels    = rotulos_mapas[[indicador_atual]],
      name      = legendas_mapas[[indicador_atual]]
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        str_wrap(mapa_atual$subtitulo, width = 80), "\n",
        texto_municipio
      ),
      caption  = legenda_fonte(
        fontes  = "od",
        notas   = c(mapa_atual$nota, "amostra_menor_od", "od_destino", "incerteza_amostra"),
        largura = 95
      )
    ) +
    tema_mapa()

  # Gravando o mapa
  gravar_figura(
    grafico = mapa,
    arquivo = file.path(diretorio_saida, paste0("mapa_", indicador_atual, ".png")),
    tamanho = "mapa"
  )
}
