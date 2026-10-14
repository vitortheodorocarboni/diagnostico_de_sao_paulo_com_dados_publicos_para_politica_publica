################################################################################
##################################            ##################################
##################### 13.03) MAPAS DOS ÓBITOS POR DISTRITO #####################
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
diretorio_grupo <- caminho_data("13_obitos")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("13_obitos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes e os limites da escala de cada mapa
limites_mapas <- list(
  rmp                  = c(0.6, 0.8, 0.9, 1.1, 1.2, 1.4),
  pct_domicilios_obito = c(3, 4, 4.5, 5, 5.5, 6.5),
  pct_obitos_menos_60  = c(15, 20, 25, 30, 35, 40),
  obitos_mil_ano       = c(4, 4.5, 5, 5.5, 6, 7)
)
extremos_mapas <- list(
  rmp                  = c(0.4, 1.6),
  pct_domicilios_obito = c(1.5, 8),
  pct_obitos_menos_60  = c(0, 65),
  obitos_mil_ano       = c(2, 8.5)
)

# Definindo os rótulos da legenda de cada mapa
rotulos_mapas <- list(
  rmp                  = rotulo_numero(accuracy = 0.1),
  pct_domicilios_obito = rotulo_numero(accuracy = 0.1, suffix = "%"),
  pct_obitos_menos_60  = rotulo_numero(suffix = "%"),
  obitos_mil_ano       = rotulo_numero(accuracy = 0.1)
)

# Definindo os mapas: indicador, base, título, subtítulo, legenda e nota
mapas <- tribble(
  ~indicador,             ~base,      ~titulo,                                         ~subtitulo,                                                                                                       ~legenda,                          ~nota,
  "rmp",                  "amostra",  "Mortalidade padronizada pela idade",            "Óbitos de moradores de 2019 a julho de 2022 em relação aos esperados pela idade dos moradores, por distrito",     "Óbitos por\nóbito esperado",      "Razão de mortalidade padronizada (padronização indireta, 17 faixas de idade, taxas do município). Acima de 1 (vermelho): morre-se mais do que a idade dos moradores explica.",
  "pct_domicilios_obito", "universo", "Domicílios com morador falecido",               "Domicílios ocupados em que morreu algum morador de janeiro de 2019 a julho de 2022, por distrito",                "% dos\ndomicílios",              "Depende da idade dos moradores e do tamanho dos domicílios.",
  "pct_obitos_menos_60",  "amostra",  "Óbitos antes dos 60 anos",                      "Falecidos com menos de 60 anos entre os moradores falecidos de 2019 a julho de 2022, por distrito",               "% dos\nóbitos",                  "Proporção sobre os falecidos com idade informada; depende também da idade dos moradores.",
  "obitos_mil_ano",       "amostra",  "Taxa bruta de mortalidade",                     "Óbitos de moradores por mil moradores ao ano, 2019 a julho de 2022, por distrito",                                "Óbitos por mil\nmoradores ao ano", "Taxa bruta, sem descontar a idade: distritos mais velhos têm taxa maior. Compare com a mortalidade padronizada."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos óbitos
obitos <- read_parquet(file.path(diretorio_grupo, "obitos.parquet"))

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
  valor_municipio <- obitos |>
    filter(nivel == "municipio", indicador == mapa_atual$indicador) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada; o aviso e o piso
  # dos registros de falecidos só valem para os indicadores da amostra
  mapa_dados <- distritos |>
    inner_join(
      obitos |>
        filter(nivel == "distrito", indicador == mapa_atual$indicador) |>
        select(codigo_distrito, valor, amostra_menor, sem_valor),
      by = "codigo_distrito"
    ) |>
    mutate(
      amostra_menor = amostra_menor & !sem_valor & da_amostra
    )

  # Escolhendo a escala: divergente centrada em 1 para a mortalidade
  # padronizada (com o sem valor em cinza médio, para não se confundir com o
  # centro neutro) e sequencial para os demais
  escala <- if (mapa_atual$indicador == "rmp") {
    scale_fill_stepsn(
      colours  = paleta_divergente(n = 7),
      breaks   = limites_mapas[[mapa_atual$indicador]],
      limits   = extremos_mapas[[mapa_atual$indicador]],
      labels   = rotulos_mapas[[mapa_atual$indicador]],
      na.value = cor_neutra,
      name     = mapa_atual$legenda
    )
  } else {
    escala_fill_sequencial(
      classes = TRUE,
      breaks  = limites_mapas[[mapa_atual$indicador]],
      limits  = extremos_mapas[[mapa_atual$indicador]],
      labels  = rotulos_mapas[[mapa_atual$indicador]],
      name    = mapa_atual$legenda
    )
  }

  # Descrevendo o município no subtítulo
  texto_municipio <- case_when(
    mapa_atual$indicador == "rmp"            ~ "Município: 1,0 (referência)",
    mapa_atual$indicador == "obitos_mil_ano" ~ paste0("Município: ", formatar_numero(valor_municipio, 1), " óbitos por mil moradores ao ano"),
    TRUE                                     ~ paste0("Município: ", formatar_numero(valor_municipio, 1), "%")
  )

  # Montando as notas do rodapé conforme a base
  notas_mapa <- if (da_amostra) {
    c(mapa_atual$nota, "captacao_obitos", "amostra_menor_obitos", "se_republica", "incerteza_amostra", "acesso_controlado")
  } else {
    c(mapa_atual$nota, "captacao_obitos", "sigilo_universo")
  }

  # Desenhando o mapa coroplético em classes fixas, com tracejado nos distritos
  # com poucos registros e cinza nos sem valor
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
    escala +
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
