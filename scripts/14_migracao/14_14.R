################################################################################
##################################            ##################################
############### 14.14) GRÁFICOS POR DISTRITO DA MIGRAÇÃO RECENTE ###############
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("geoarrow")
# install.packages("patchwork")
# install.packages("sf")
# install.packages("tidyverse")

# Carregando bibliotecas
library(arrow)
library(geoarrow)
library(patchwork)
library(sf)
library(tidyverse)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Carregando a identidade visual
source(file.path("scripts", "_config", "identidade_visual.R"), encoding = "UTF-8")

# Carregando as funções dos gráficos por distrito
source(file.path("scripts", "_config", "graficos_distritos.R"), encoding = "UTF-8")

# Definindo o diretório de dados do grupo
diretorio_grupo <- caminho_data("14_migracao")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("14_migracao", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo quantos países aparecem no gráfico do distrito (os mais frequentes
# da capital); os demais vão para "Outros países"
n_paises_distrito <- 5

# Definindo o mínimo de registros em uma categoria do distrito abaixo do qual o
# valor leva † (estimativa instável)
minimo_registros_categoria <- 30

# Definindo a nota do indicador do molde
nota_molde <- "Local de moradia há 5 anos (P0600): outro município do Brasil ou outro país, entre as pessoas de 5 anos ou mais; a pergunta vale para quem mora há menos de 6 anos no município."


################################################################################
##### III. Funções
################################################################################

# (A função desenhar_comparacao() está em scripts/_config/graficos_distritos.R)


################################################################################
##### IV. Dados
################################################################################

# Lendo os indicadores da migração recente
recente <- read_parquet(file.path(diretorio_grupo, "recente.parquet"))

# Lendo a descrição dos indicadores
indicadores <- read_parquet(file.path(diretorio_grupo, "indicadores_recente.parquet"))

# Lendo a origem de quem veio de fora
origem_recente <- read_parquet(file.path(diretorio_grupo, "origem_recente.parquet")) |>
  filter(rotulo_origem != "UF não especificada") |>
  mutate(
    posicao     = origem_codigo,
    rotulo      = rotulo_origem,
    n_registros = NA_integer_
  )

# Lendo os países: os cinco principais em cada distrito e "Outros países" (os
# demais, somados), em % dos nascidos no exterior
paises <- read_parquet(file.path(diretorio_grupo, "paises.parquet")) |>
  mutate(
    posicao = pmin(posicao, n_paises_distrito + 1L),
    rotulo  = if_else(posicao > n_paises_distrito, "Outros países", rotulo_pais)
  ) |>
  group_by(nivel_territorial, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor, posicao, rotulo) |>
  summarise(
    pct         = sum(pct),
    n_registros = sum(n_registros),
    .groups     = "drop"
  ) |>
  # Completando com zero os países que não aparecem no distrito
  complete(
    nesting(nivel_territorial, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor),
    nesting(posicao, rotulo),
    fill = list(pct = 0, n_registros = 0L)
  )

# Lendo os distritos e simplificando a geometria para o mapa de localização
mapa_base <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet")) |>
  st_transform(epsg_projetado) |>
  st_simplify(
    preserveTopology = TRUE,
    dTolerance       = 40
  ) |>
  select(codigo_distrito)

# Listando os distritos e o aviso de amostra menor
distritos_lista <- recente |>
  filter(nivel == "distrito", indicador == "registros_amostra") |>
  distinct(codigo_distrito, nome_rotulo, amostra_menor)


################################################################################
##### V. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Obtendo a descrição do indicador da migração em 5 anos
descricao <- indicadores |>
  filter(indicador == "pct_migrou_5anos")

# Obtendo o valor do município
valor_municipio <- recente |>
  filter(nivel == "municipio", indicador == "pct_migrou_5anos") |>
  pull(valor)

# Obtendo os valores dos distritos com a posição de cada um
valores <- recente |>
  filter(nivel == "distrito", indicador == "pct_migrou_5anos") |>
  mutate(
    posicao = min_rank(desc(valor))
  )

# Informando o andamento
message("Gerando o molde da migração em 5 anos dos ", nrow(valores), " distritos")

# Gravando o molde da migração em 5 anos de cada distrito
for (codigo in valores$codigo_distrito) {

  # Montando a figura do distrito
  figura <- figura_distrito(
    mapa_base       = mapa_base,
    valores         = valores,
    codigo          = codigo,
    valor_municipio = valor_municipio,
    descricao       = descricao,
    nota            = c(nota_molde, "se_republica", "incerteza_amostra", "acesso_controlado"),
    fontes          = "censo_amostra",
    aviso           = if (valores$amostra_menor[valores$codigo_distrito == codigo]) aviso_amostra_menor else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(
      diretorio_saida,
      nome_arquivo_distrito(codigo, valores$nome_rotulo[valores$codigo_distrito == codigo], sufixo = "pct_migrou_5anos")
    ),
    tamanho = "grafico"
  )
}

# Gravando a composição dos nascidos no exterior e a origem de quem chegou
message("Gerando a composição dos estrangeiros e a origem recente dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]
  aviso  <- if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL

  # Desenhando a composição dos nascidos no exterior pelos países principais
  figura_paises <- desenhar_comparacao(
    dados       = bind_rows(
      filter(paises, codigo_distrito == codigo),
      filter(paises, nivel_territorial == "municipio")
    ),
    nome_titulo = nome,
    subtitulo   = "Países de nascimento dos moradores nascidos no exterior, % dos nascidos no exterior, 2022",
    nota        = paste0(
      "País de nascimento (P0510): os cinco mais frequentes do município; os demais em Outros países. Nomes como nos descritores do IBGE ",
      "(sem acento). †: menos de ", minimo_registros_categoria, " registros do país no distrito; estimativa instável."
    ),
    aviso       = aviso,
    limite      = 100,
    minimo_registros = minimo_registros_categoria
  )

  # Gravando a figura dos países
  gravar_figura(
    grafico = figura_paises,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "paises_nascimento")),
    tamanho = "grafico",
    altura  = 11
  )

  # Desenhando a origem de quem morava fora do município há 5 anos
  figura_origem <- desenhar_comparacao(
    dados       = bind_rows(
      filter(origem_recente, codigo_distrito == codigo),
      filter(origem_recente, nivel_territorial == "municipio")
    ) |>
      rename(pct_origem = pct) |>
      mutate(pct = pct_origem),
    nome_titulo = nome,
    subtitulo   = "Local de moradia há 5 anos de quem morava em outro município ou país, % de quem veio de fora, 2022",
    nota        = paste0(
      "Região pelo primeiro dígito do código de UF do IBGE (P0610); demais Sudeste: Minas Gerais, Espírito Santo e Rio de Janeiro. ",
      "A base é pequena nos distritos de pouca migração recente: ler com cautela. Fora do gráfico: UF não especificada (0,1% no município)."
    ),
    aviso       = aviso,
    limite      = 100,
    minimo_registros = minimo_registros_categoria
  )

  # Gravando a figura da origem
  gravar_figura(
    grafico = figura_origem,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "origem_recente")),
    tamanho = "grafico",
    altura  = 11
  )
}
