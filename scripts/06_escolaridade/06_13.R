################################################################################
##################################            ##################################
############# 06.13) CURSO E ÁREAS DO CURSO SUPERIOR POR DISTRITO ##############
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
diretorio_grupo <- caminho_data("06_escolaridade")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("06_escolaridade", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo o mínimo de registros em uma categoria do distrito abaixo do qual o
# valor leva † (estimativa instável)
minimo_registros_categoria <- 30

# Definindo os indicadores com molde por distrito
indicadores_distritais <- c(
  "pct_frequenta_escola",
  "anos_estudo_medio"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  pct_frequenta_escola = "P0650: pessoas que frequentam escola ou creche, entre todos os moradores. Depende da estrutura etária do distrito; o tratado não permite padronizá-la.",
  anos_estudo_medio    = "P0790: média ponderada por pessoa, com as crianças. Depende da estrutura etária do distrito; o tratado não permite padronizá-la."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da extensão
extensao <- read_parquet(file.path(diretorio_grupo, "extensao.parquet"))

# Lendo a descrição dos indicadores
indicadores <- read_parquet(file.path(diretorio_grupo, "indicadores_extensao.parquet"))

# Lendo o curso frequentado, com o nome do grupo quebrado em linhas e sem
# contagem de registros (o tratado traz só a estimativa por categoria)
cursos <- read_parquet(file.path(diretorio_grupo, "cursos.parquet")) |>
  mutate(
    posicao     = grupo_curso,
    rotulo      = rotulo_curso,
    n_registros = NA_integer_
  )

# Lendo as áreas do curso superior, com o nome quebrado em linhas
areas <- read_parquet(file.path(diretorio_grupo, "areas.parquet")) |>
  mutate(
    rotulo = str_wrap(rotulo_area, width = 40)
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
distritos_lista <- extensao |>
  filter(nivel == "distrito", indicador == "registros_amostra") |>
  distinct(codigo_distrito, nome_rotulo, amostra_menor)


################################################################################
##### IV. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando o molde de cada indicador e distrito
for (indicador_atual in indicadores_distritais) {

  # Obtendo a descrição do indicador
  descricao <- indicadores |>
    filter(indicador == indicador_atual)

  # Obtendo o valor do município
  valor_municipio <- extensao |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Obtendo os valores dos distritos com a posição de cada um
  valores <- extensao |>
    filter(nivel == "distrito", indicador == indicador_atual) |>
    mutate(
      posicao = min_rank(desc(valor))
    )

  # Informando o andamento
  message("Gerando os gráficos por distrito de ", indicador_atual)

  # Gravando a figura de cada distrito
  for (codigo in valores$codigo_distrito) {

    # Montando a figura do distrito
    figura <- figura_distrito(
      mapa_base       = mapa_base,
      valores         = valores,
      codigo          = codigo,
      valor_municipio = valor_municipio,
      descricao       = descricao,
      nota            = c(notas_indicadores[[indicador_atual]], "se_republica", "incerteza_amostra", "acesso_controlado"),
      fontes          = "censo_amostra",
      aviso           = if (valores$amostra_menor[valores$codigo_distrito == codigo]) aviso_amostra_menor else NULL
    )

    # Gravando a figura com o código e o nome do distrito no nome do arquivo
    gravar_figura(
      grafico = figura,
      arquivo = file.path(
        diretorio_saida,
        nome_arquivo_distrito(codigo, valores$nome_rotulo[valores$codigo_distrito == codigo], sufixo = indicador_atual)
      ),
      tamanho = "grafico"
    )
  }
}

# Gravando o curso frequentado e as áreas do curso superior de cada distrito
message("Gerando o curso frequentado e as áreas do curso superior dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]
  aviso  <- if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL

  # Desenhando o curso frequentado no distrito e no município
  figura_cursos <- desenhar_comparacao(
    dados            = bind_rows(
      filter(cursos, codigo_distrito == codigo),
      filter(cursos, nivel_territorial == "municipio")
    ),
    nome_titulo      = nome,
    subtitulo        = "Curso frequentado por quem frequenta escola ou creche, % dos estudantes, 2022",
    nota             = paste0(
      "Curso frequentado (P0660): fundamental regular (04); médio regular (06); graduação (08); pós-graduação (especialização, mestrado e doutorado, 09 a 11); ",
      "creche (01); pré-escola (02); EJA e alfabetização (03, 05 e 07)."
    ),
    aviso            = aviso,
    limite           = 80,
    minimo_registros = minimo_registros_categoria,
    tamanho_rotulo   = 9
  )

  # Gravando a figura dos cursos
  gravar_figura(
    grafico = figura_cursos,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "curso_frequentado")),
    tamanho = "grafico",
    altura  = 11
  )

  # Desenhando as áreas do curso superior do distrito e do município
  figura_areas <- desenhar_comparacao(
    dados            = bind_rows(
      filter(areas, codigo_distrito == codigo),
      filter(areas, nivel_territorial == "municipio")
    ),
    nome_titulo      = nome,
    subtitulo        = "Dez áreas do curso superior mais frequentes no município, % de quem tem curso superior completo, 2022",
    nota             = paste0(
      "Área detalhada do curso (P0750): as dez mais frequentes do município e as demais à parte; o não sabe fica fora. ",
      "Nomes como nos descritores do IBGE. †: menos de ", minimo_registros_categoria, " registros da área no distrito; estimativa instável."
    ),
    aviso            = aviso,
    limite           = 80,
    minimo_registros = minimo_registros_categoria,
    tamanho_rotulo   = 8.5
  )

  # Gravando a figura das áreas
  gravar_figura(
    grafico = figura_areas,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "areas_curso_superior")),
    tamanho = "grafico",
    altura  = 12
  )
}
