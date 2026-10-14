################################################################################
##################################            ##################################
############## 12.03) MAPAS DOS ARRANJOS FAMILIARES POR DISTRITO ###############
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
diretorio_grupo <- caminho_data("12_arranjos_familiares")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("12_arranjos_familiares")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  pct_unipessoal          = c(18, 21, 24, 28, 33, 40),
  pct_mulher_sem_conjuge  = c(9, 11, 13, 15, 17, 19),
  pct_conjuges_mesmo_sexo = c(0.5, 1, 1.5, 2, 4, 8),
  pct_responsavel_60_mais = c(22, 25, 28, 31, 35, 39)
)
extremos_mapas <- list(
  pct_unipessoal          = c(10, 50),
  pct_mulher_sem_conjuge  = c(5, 22),
  pct_conjuges_mesmo_sexo = c(0, 13),
  pct_responsavel_60_mais = c(15, 45)
)

# Definindo os mapas: indicador, título, subtítulo, legenda e nota
mapas <- tribble(
  ~indicador,                ~titulo,                                        ~subtitulo,                                                                                                    ~legenda,             ~nota,
  "pct_unipessoal",          "Domicílios unipessoais",                       "Domicílios particulares ocupados em que mora uma só pessoa, por distrito, 2022",                              "% dos\ndomicílios", "Proporção sobre as quatro espécies de unidade doméstica (unipessoal, nuclear, estendida e composta).",
  "pct_mulher_sem_conjuge",  "Mulher responsável sem cônjuge com filhos",    "Domicílios particulares ocupados com responsável mulher, sem cônjuge, com filhos ou enteados, por distrito, 2022", "% dos\ndomicílios", "Proporção sobre as cinco composições do domicílio; os filhos podem ter qualquer idade.",
  "pct_conjuges_mesmo_sexo", "Cônjuges do mesmo sexo",                       "Cônjuges ou companheiros(as) do mesmo sexo do responsável, entre todos os cônjuges, por distrito, 2022",       "% dos\ncônjuges",   "Classes desiguais por causa da concentração no centro; casais em que nenhum dos dois é o responsável não entram.",
  "pct_responsavel_60_mais", "Responsável com 60 anos ou mais",              "Responsáveis pelo domicílio com 60 anos ou mais, por distrito, 2022",                                          "% dos\nresponsáveis", "Proporção sobre os responsáveis por faixa de idade."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos arranjos familiares
arranjos <- read_parquet(file.path(diretorio_grupo, "arranjos.parquet"))

# Lendo a descrição dos indicadores
indicadores <- read_parquet(file.path(diretorio_grupo, "indicadores.parquet"))

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

  # Obtendo a nota de sigilo da base do indicador (domicílios ou pessoas)
  nota_sigilo <- indicadores$base_sigilo[indicadores$indicador == mapa_atual$indicador]

  # Obtendo o valor do município
  valor_municipio <- arranjos |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      arranjos |>
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
      labels  = rotulo_numero(accuracy = 0.1, suffix = "%"),
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
        fontes  = "censo_universo",
        notas   = c(mapa_atual$nota, nota_sigilo),
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
