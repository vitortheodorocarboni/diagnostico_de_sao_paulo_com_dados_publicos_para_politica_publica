################################################################################
##################################            ##################################
################## 17.03) MAPAS DE QUEM MORA SÓ POR DISTRITO ###################
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
diretorio_grupo <- caminho_data("17_unipessoais")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("17_unipessoais")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  pct_mulheres          = c(50, 54, 57, 60, 64),
  pct_mulheres_universo = c(50, 54, 57, 60, 64),
  pct_60_mais           = c(30, 36, 40, 44, 50),
  pct_preta_parda       = c(15, 25, 40, 50, 60),
  pct_superior          = c(10, 20, 35, 50, 65),
  pct_renda_ate_meio    = c(8, 10, 13, 16, 20)
)
extremos_mapas <- list(
  pct_mulheres          = c(30, 70),
  pct_mulheres_universo = c(30, 70),
  pct_60_mais           = c(20, 60),
  pct_preta_parda       = c(0, 75),
  pct_superior          = c(0, 85),
  pct_renda_ate_meio    = c(4, 25)
)

# Definindo os mapas: indicador, base, título, subtítulo, legenda e nota
mapas <- tribble(
  ~indicador,              ~base,      ~titulo,                                         ~subtitulo,                                                                                                      ~legenda,                        ~nota,
  "pct_mulheres",          "amostra",  "Mulheres entre quem mora só",                   "Moradores de domicílios unipessoais do sexo feminino, entre todos os que moram sós, por distrito, 2022",         "% dos\nunipessoais",           "Sexo do responsável, que é o único morador (D0170).",
  "pct_mulheres_universo", "universo", "Mulheres entre quem mora só (universo)",        "Domicílios unipessoais com responsável do sexo feminino, entre os unipessoais, por distrito, 2022",              "% dos\nunipessoais",           "Resultados do universo (v01214 e v01215); o mesmo indicador do mapa da amostra, sem erro amostral.",
  "pct_60_mais",           "amostra",  "60 anos ou mais entre quem mora só",            "Moradores de domicílios unipessoais com 60 anos ou mais, entre todos os que moram sós, por distrito, 2022",     "% dos\nunipessoais",           "Idade do responsável (D0180, faixas de 5 anos), entre os unipessoais com idade informada.",
  "pct_preta_parda",       "amostra",  "Pretos e pardos entre quem mora só",            "Moradores de domicílios unipessoais de cor ou raça preta ou parda, entre os que moram sós, por distrito, 2022", "% dos\nunipessoais",           "Cor ou raça do responsável (P0210), entre os unipessoais com cor ou raça informada.",
  "pct_superior",          "amostra",  "Superior completo entre quem mora só",          "Moradores de domicílios unipessoais com superior completo, entre os que moram sós, por distrito, 2022",         "% dos\nunipessoais",           "Instrução do responsável (P0770), de qualquer idade, entre os unipessoais com instrução determinada.",
  "pct_renda_ate_meio",    "amostra",  "Renda até 1/2 salário mínimo entre quem mora só", "Moradores de domicílios unipessoais com renda de até 1/2 salário mínimo (R$ 606), entre os que moram sós, por distrito, 2022", "% dos\nunipessoais",           "Renda do próprio morador, que é a renda domiciliar per capita (D0360), entre os unipessoais com renda informada."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos unipessoais
unipessoais <- read_parquet(file.path(diretorio_grupo, "unipessoais.parquet"))

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
  valor_municipio <- unipessoais |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada; o aviso e o piso
  # dos registros só valem para os indicadores da amostra
  mapa_dados <- distritos |>
    inner_join(
      unipessoais |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
        select(codigo_distrito, valor, amostra_menor, sem_valor),
      by = "codigo_distrito"
    ) |>
    mutate(
      amostra_menor = amostra_menor & !sem_valor & da_amostra
    )

  # Descrevendo no subtítulo o município e os distritos sem valor
  texto_municipio <- paste0("Município: ", formatar_numero(valor_municipio, 1), "%")

  # Montando as notas do rodapé conforme a base
  notas_mapa <- if (da_amostra) {
    c(mapa_atual$nota, "perfil_unipessoais", "amostra_menor_unipessoais", "se_republica", "incerteza_amostra", "acesso_controlado")
  } else {
    c(mapa_atual$nota, "sigilo_universo")
  }

  # Desenhando o mapa coroplético em classes fixas, com tracejado nos distritos
  # com poucos registros e cinza médio nos sem valor
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
    scale_fill_stepsn(
      colours  = paleta_sequencial(n = 7),
      breaks   = limites_mapas[[mapa_atual$indicador]],
      limits   = extremos_mapas[[mapa_atual$indicador]],
      labels   = rotulo_numero(suffix = "%"),
      na.value = cor_neutra,
      name     = mapa_atual$legenda
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        str_wrap(mapa_atual$subtitulo, width = 80), "\n",
        texto_municipio
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
