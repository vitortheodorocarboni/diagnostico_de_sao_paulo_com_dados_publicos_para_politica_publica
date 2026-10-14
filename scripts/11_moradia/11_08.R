################################################################################
##################################            ##################################
################### 11.08) GRÁFICOS POR DISTRITO DA MORADIA ####################
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
diretorio_grupo <- caminho_data("11_moradia")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("11_moradia", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo os indicadores com gráfico por distrito
indicadores_distritais <- c(
  "pct_alugado",
  "pct_financiando",
  "pct_3_mais_banheiros"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  pct_alugado          = "Proporção sobre os domicílios particulares permanentes ocupados. O Censo 2022 não pergunta o valor do aluguel.",
  pct_financiando      = "Próprio de algum morador, ainda pagando; proporção sobre os domicílios particulares permanentes ocupados.",
  pct_3_mais_banheiros = "Banheiros de uso exclusivo, com chuveiro e vaso sanitário; proporção sobre a soma publicada das categorias de banheiro."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da moradia
moradia <- read_parquet(file.path(diretorio_grupo, "moradia.parquet"))

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
  valor_municipio <- moradia |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Obtendo os valores dos distritos com a posição de cada um
  valores <- moradia |>
    filter(nivel == "distrito", indicador == indicador_atual) |>
    mutate(
      posicao = min_rank(desc(valor))
    )

  # Montando as notas do rodapé conforme a base
  notas_figura <- if (da_amostra) {
    c(notas_indicadores[[indicador_atual]], "se_republica", "incerteza_amostra", "acesso_controlado")
  } else {
    c(notas_indicadores[[indicador_atual]], "sigilo_universo")
  }

  # Informando o andamento
  message("Gerando os gráficos por distrito de ", indicador_atual)

  # Gravando a figura de cada distrito
  for (codigo in valores$codigo_distrito) {

    # Verificando se o distrito recebe o aviso de amostra menor
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
      aviso           = if (com_aviso) aviso_amostra_menor else NULL
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
