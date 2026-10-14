################################################################################
##################################            ##################################
################### 16.03) MAPAS DA DEFICIÊNCIA POR DISTRITO ###################
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
diretorio_grupo <- caminho_data("16_deficiencia")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("16_deficiencia")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  pct_deficiencia = c(4, 5, 6, 7, 8),
  pct_autismo     = c(1, 1.2, 1.4, 1.6, 2),
  pct_visual      = c(2, 2.5, 3, 3.5, 4, 4.5),
  pct_motora      = c(1.5, 2, 2.5, 3, 3.5)
)
extremos_mapas <- list(
  pct_deficiencia = c(3, 10),
  pct_autismo     = c(0.5, 3),
  pct_visual      = c(1, 6),
  pct_motora      = c(1, 4.5)
)

# Definindo os rótulos da legenda de cada mapa
rotulos_mapas <- list(
  pct_deficiencia = rotulo_numero(suffix = "%"),
  pct_autismo     = rotulo_numero(accuracy = 0.1, suffix = "%"),
  pct_visual      = rotulo_numero(accuracy = 0.1, suffix = "%"),
  pct_motora      = rotulo_numero(accuracy = 0.1, suffix = "%")
)

# Definindo os mapas: indicador, título, subtítulo, legenda e nota
mapas <- tribble(
  ~indicador,        ~titulo,                                ~subtitulo,                                                                                                                       ~legenda,                        ~nota,
  "pct_deficiencia", "Pessoas com deficiência",              "Pessoas que não conseguem de modo algum ou têm muita dificuldade em ao menos um tipo, entre as de 2 anos ou mais, por distrito, 2022", "% das pessoas\nde 2 anos\nou mais", "Tipos: enxergar, ouvir, andar ou subir degraus, pegar objetos e funções mentais (indicadora P0470 do IBGE). A deficiência cresce com a idade, e o tratado não permite padronizá-la; nos distritos, ela é maior onde há menos idosos.",
  "pct_autismo",     "Pessoas com diagnóstico de autismo",   "Pessoas com diagnóstico de autismo por profissional de saúde, entre todos os moradores, por distrito, 2022",                    "% dos\nmoradores",             "Pergunta nova em 2022 (P1200); o diagnóstico depende do acesso a profissional de saúde. Categoria rara: nos distritos com amostra menor, poucas dezenas de registros.",
  "pct_visual",      "Pessoas com deficiência visual",       "Pessoas que não conseguem de modo algum ou têm muita dificuldade para enxergar, entre as de 2 anos ou mais, por distrito, 2022", "% das pessoas\nde 2 anos\nou mais", "Mesmo com óculos ou lentes de contato (P0420).",
  "pct_motora",      "Pessoas com deficiência motora",       "Pessoas que não conseguem de modo algum ou têm muita dificuldade para andar ou subir degraus, entre as de 2 anos ou mais, por distrito, 2022", "% das pessoas\nde 2 anos\nou mais", "Mesmo com prótese, bengala ou aparelho de auxílio (P0440)."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da deficiência
deficiencia <- read_parquet(file.path(diretorio_grupo, "deficiencia.parquet"))

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
  valor_municipio <- deficiencia |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      deficiencia |>
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
