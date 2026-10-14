################################################################################
##################################            ##################################
################### 13.07) GRÁFICOS POR DISTRITO DOS ÓBITOS ####################
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
diretorio_grupo <- caminho_data("13_obitos")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("13_obitos", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo os indicadores com gráfico por distrito
indicadores_distritais <- c(
  "rmp",
  "pct_domicilios_obito"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  rmp                  = "Óbitos de moradores de 2019 a julho de 2022 sobre os esperados se cada faixa de idade dos moradores tivesse a taxa do município (17 faixas); acima de 1, morre-se mais do que a idade explica.",
  pct_domicilios_obito = "Domicílios ocupados em que morreu algum morador de janeiro de 2019 a julho de 2022; depende da idade dos moradores e do tamanho dos domicílios."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos óbitos
obitos <- read_parquet(file.path(diretorio_grupo, "obitos.parquet"))

# Lendo a descrição dos indicadores
indicadores <- read_parquet(file.path(diretorio_grupo, "indicadores.parquet"))

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

# Gravando as figuras de cada indicador e distrito
for (indicador_atual in indicadores_distritais) {

  # Obtendo a descrição do indicador
  descricao <- indicadores |>
    filter(indicador == indicador_atual)

  # Identificando se o indicador vem da amostra
  da_amostra <- descricao$base == "amostra"

  # Obtendo o valor do município
  valor_municipio <- obitos |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Obtendo os valores dos distritos com valor e a posição de cada um
  valores <- obitos |>
    filter(nivel == "distrito", indicador == indicador_atual, !is.na(valor)) |>
    mutate(
      posicao = min_rank(desc(valor))
    )

  # Montando as notas do rodapé conforme a base
  notas_figura <- if (da_amostra) {
    c(notas_indicadores[[indicador_atual]], "captacao_obitos", "se_republica", "incerteza_amostra", "acesso_controlado")
  } else {
    c(notas_indicadores[[indicador_atual]], "captacao_obitos", "sigilo_universo")
  }

  # Informando o andamento
  message("Gerando os gráficos por distrito de ", indicador_atual, " (", nrow(valores), " distritos)")

  # Gravando a figura de cada distrito
  for (codigo in valores$codigo_distrito) {

    # Verificando se o distrito recebe o aviso de poucos registros
    com_aviso <- da_amostra && valores$amostra_menor[valores$codigo_distrito == codigo]

    # Montando a figura do distrito
    figura <- figura_distrito(
      mapa_base       = mapa_base,
      valores         = valores,
      codigo          = codigo,
      valor_municipio = valor_municipio,
      descricao       = descricao,
      nota            = notas_figura,
      fontes          = if (da_amostra) "censo_amostra" else "censo_universo",
      ano             = "2019 a julho de 2022",
      aviso           = if (com_aviso) aviso_amostra_menor_obitos else NULL
    )

    # Gravando a figura com o código e o nome do distrito no nome do arquivo
    gravar_figura(
      grafico = figura,
      arquivo = file.path(
        diretorio_saida,
        nome_arquivo_distrito(
          codigo = codigo,
          nome   = valores$nome_rotulo[valores$codigo_distrito == codigo],
          sufixo = indicador_atual
        )
      ),
      tamanho = "grafico"
    )
  }
}
