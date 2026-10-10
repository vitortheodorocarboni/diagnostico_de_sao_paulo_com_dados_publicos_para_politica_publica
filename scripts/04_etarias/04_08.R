################################################################################
##################################            ##################################
#################### 04.08) GRÁFICOS POR DISTRITO DE IDADE #####################
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

# Carregando as funções dos gráficos por distrito
source(file.path("scripts", "_config", "graficos_distritos.R"), encoding = "UTF-8")

# Definindo o diretório de dados do grupo
diretorio_grupo <- caminho_data("04_etarias")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("04_etarias", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo os indicadores com gráfico por distrito
indicadores_distritais <- c(
  "envelhecimento",
  "pct_0_14",
  "pct_60_mais"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  envelhecimento = "Moradores de 60 anos ou mais por 100 de 0 a 14 anos; acima de 100, há mais pessoas de 60 anos ou mais que de 0 a 14 anos. O padrão do IBGE usa 65 anos ou mais, que os grupos publicados não permitem.",
  pct_0_14       = "Proporção sobre a soma publicada dos 11 grupos de idade do IBGE (0 a 4, 5 a 9 e 10 a 14 anos).",
  pct_60_mais    = "Proporção sobre a soma publicada dos 11 grupos de idade do IBGE (60 a 69 e 70 anos ou mais)."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de idade
idade <- read_parquet(file.path(diretorio_grupo, "idade.parquet"))

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

  # Obtendo o valor do município
  valor_municipio <- idade |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Obtendo os valores dos distritos com a posição de cada um
  valores <- idade |>
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
      nota            = c(notas_indicadores[[indicador_atual]], "sigilo_pessoas")
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
