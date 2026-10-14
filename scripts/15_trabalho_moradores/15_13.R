################################################################################
##################################            ##################################
#################### 15.13) SETOR E OCUPAÇÕES POR DISTRITO #####################
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
diretorio_grupo <- caminho_data("15_trabalho_moradores")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("15_trabalho_moradores", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo quantas ocupações aparecem no gráfico do distrito (as mais
# frequentes da capital); o código sem rótulo e as demais ficam à parte
n_ocupacoes_distrito <- 5

# Definindo o mínimo de registros em uma categoria do distrito abaixo do qual o
# valor leva † (estimativa instável)
minimo_registros_categoria <- 30

# Definindo os indicadores com molde por distrito
indicadores_distritais <- c(
  "pct_comercio",
  "pct_servicos_domesticos"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  pct_comercio            = "Seção G da CNAE-Domiciliar (comércio e reparação de veículos automotores e motocicletas), entre os ocupados de 10 anos ou mais com atividade (P0980).",
  pct_servicos_domesticos = "Seção T da CNAE-Domiciliar (serviços domésticos), entre os ocupados de 10 anos ou mais com atividade (P0980)."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de atividade e ocupação
atividade <- read_parquet(file.path(diretorio_grupo, "atividade.parquet"))

# Lendo a descrição dos indicadores
indicadores <- read_parquet(file.path(diretorio_grupo, "indicadores_atividade.parquet"))

# Lendo a composição por grupo de setor, com o nome do grupo quebrado em linhas
setores <- read_parquet(file.path(diretorio_grupo, "setores.parquet")) |>
  mutate(
    posicao = grupo_setor,
    rotulo  = str_wrap(rotulo_setor, width = 34),
    pct     = pct
  )

# Lendo as ocupações: as cinco principais da capital, o código sem rótulo e as
# demais (as outras cinco entre as dez principais e as restantes), em % dos
# ocupados
ocupacoes <- read_parquet(file.path(diretorio_grupo, "ocupacoes.parquet")) |>
  mutate(
    posicao = case_when(
      posicao <= n_ocupacoes_distrito ~ posicao,
      posicao == 11L                  ~ n_ocupacoes_distrito + 1L,
      TRUE                            ~ n_ocupacoes_distrito + 2L
    ),
    rotulo  = case_when(
      posicao <= n_ocupacoes_distrito     ~ rotulo_ocupacao,
      posicao == n_ocupacoes_distrito + 1L ~ "Código 0000 (sem rótulo)",
      TRUE                                 ~ "Demais ocupações"
    )
  ) |>
  group_by(nivel_territorial, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor, posicao, rotulo) |>
  summarise(
    pct         = sum(pct),
    n_registros = sum(n_registros),
    .groups     = "drop"
  ) |>
  mutate(
    rotulo = str_wrap(rotulo, width = 40)
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
distritos_lista <- atividade |>
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
  valor_municipio <- atividade |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Obtendo os valores dos distritos com a posição de cada um
  valores <- atividade |>
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

# Gravando a composição por setor e as ocupações principais de cada distrito
message("Gerando o setor de atividade e as ocupações dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]
  aviso  <- if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL

  # Desenhando o setor de atividade do distrito e do município
  figura_setores <- desenhar_comparacao(
    dados            = bind_rows(
      filter(setores, codigo_distrito == codigo),
      filter(setores, nivel_territorial == "municipio")
    ),
    nome_titulo      = nome,
    subtitulo        = "Setor de atividade dos moradores ocupados de 10 anos ou mais, % dos ocupados, 2022",
    nota             = paste0(
      "Seções da CNAE-Domiciliar (P0980): comércio (G); informação, finanças e serviços profissionais (J a N); indústria e construção (B a F); ",
      "transporte, alojamento e alimentação (H e I); educação, saúde e administração pública (O a Q); serviços domésticos (T); outros (A, R, S, U) e ",
      "atividades mal definidas (V). †: menos de ", minimo_registros_categoria, " registros do grupo no distrito; estimativa instável."
    ),
    aviso            = aviso,
    limite           = 48,
    minimo_registros = minimo_registros_categoria,
    tamanho_rotulo   = 8.5
  )

  # Gravando a figura dos setores
  gravar_figura(
    grafico = figura_setores,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "setor_atividade")),
    tamanho = "grafico",
    altura  = 13
  )

  # Desenhando as ocupações principais do distrito e do município
  figura_ocupacoes <- desenhar_comparacao(
    dados            = bind_rows(
      filter(ocupacoes, codigo_distrito == codigo),
      filter(ocupacoes, nivel_territorial == "municipio")
    ),
    nome_titulo      = nome,
    subtitulo        = "Ocupações mais frequentes do município entre os moradores ocupados de 10 anos ou mais, % dos ocupados, 2022",
    nota             = paste0(
      "Ocupação pela família ocupacional (P0970): as cinco mais frequentes do município; o código 0000 (sem rótulo no descritor do IBGE) e as demais à parte. ",
      "Nomes como nos descritores do IBGE (sem acento). †: menos de ", minimo_registros_categoria, " registros da ocupação no distrito; estimativa instável."
    ),
    aviso            = aviso,
    limite           = 100,
    minimo_registros = minimo_registros_categoria,
    tamanho_rotulo   = 8.5
  )

  # Gravando a figura das ocupações
  gravar_figura(
    grafico = figura_ocupacoes,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "ocupacoes_principais")),
    tamanho = "grafico",
    altura  = 12
  )
}
