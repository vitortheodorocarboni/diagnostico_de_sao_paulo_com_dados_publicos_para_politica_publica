################################################################################
##################################            ##################################
############## 07.13) GRÁFICOS POR DISTRITO DA EXTENSÃO DA RENDA ###############
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
diretorio_grupo <- caminho_data("07_renda")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("07_renda", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo os indicadores com molde por distrito
indicadores_distritais <- c(
  "renda_trabalho_media",
  "pct_outras_fontes",
  "renda_todas_fontes_media",
  "renda_familiar_pc_media"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  renda_trabalho_media     = "P1080, valores nominais de 2022 (salário mínimo de R$ 1.212); média ponderada dos ocupados de 10 anos ou mais com rendimento, sem padronização por idade.",
  pct_outras_fontes        = "P1090: rendimento de aposentadoria, pensão, Bolsa Família, BPC ou aluguel, entre as pessoas de 10 anos ou mais; depende da estrutura etária do distrito.",
  renda_todas_fontes_media = "P1110, valores nominais de 2022; média de todas as pessoas de 10 anos ou mais, com ou sem rendimento (quem não tem rendimento entra com zero).",
  renda_familiar_pc_media  = "F0260, valores nominais de 2022; média por família (não por pessoa), só famílias de 2 pessoas ou mais: os domicílios unipessoais não entram."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da extensão
extensao <- read_parquet(file.path(diretorio_grupo, "extensao.parquet"))

# Lendo a descrição dos indicadores
indicadores <- read_parquet(file.path(diretorio_grupo, "indicadores_extensao.parquet"))

# Lendo os distritos e simplificando a geometria para o mapa de localização
mapa_base <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet")) |>
  st_transform(epsg_projetado) |>
  st_simplify(
    preserveTopology = TRUE,
    dTolerance       = 40
  ) |>
  select(codigo_distrito)


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
