################################################################################
##################################            ##################################
################ 09.06) GRÁFICOS POR DISTRITO DOS TRABALHADORES ################
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
diretorio_grupo <- caminho_data("09_trabalhadores")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("09_trabalhadores", "distritos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo os indicadores com gráfico por distrito
indicadores_distritais <- c(
  "trabalhadores_por_morador",
  "pct_mulheres",
  "pct_superior"
)

# Definindo a nota de cada indicador
notas_indicadores <- c(
  trabalhadores_por_morador = "Trabalhadores com local fixo no distrito sobre os moradores do distrito, ambos estimados pela OD.",
  pct_mulheres              = "Sexo feminino sobre os trabalhadores com local fixo no distrito (o não respondeu fica fora).",
  pct_superior              = "Superior completo sobre os trabalhadores com local fixo no distrito."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos trabalhadores
trabalhadores <- read_parquet(file.path(diretorio_grupo, "trabalhadores.parquet"))

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
  valor_municipio <- trabalhadores |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Obtendo os valores dos distritos com a posição de cada um
  valores <- trabalhadores |>
    filter(nivel == "distrito", indicador == indicador_atual, !is.na(valor)) |>
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
      nota            = c(notas_indicadores[[indicador_atual]], "od_destino", "incerteza_amostra"),
      fontes          = "od",
      ano             = 2023,
      aviso           = if (valores$amostra_menor[valores$codigo_distrito == codigo]) aviso_amostra_menor_od else NULL
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
