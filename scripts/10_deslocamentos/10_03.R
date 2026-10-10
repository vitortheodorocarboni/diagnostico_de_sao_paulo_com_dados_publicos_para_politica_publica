################################################################################
##################################            ##################################
################# 10.03) MAPAS DOS DESLOCAMENTOS POR DISTRITO ##################
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
diretorio_grupo <- caminho_data("10_deslocamentos")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("10_deslocamentos")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo as classes, os limites, a transformação e os rótulos de cada mapa
# (as chegadas para trabalhar por morador ficam em escala log)
limites_mapas <- list(
  pct_coletivo                  = c(30, 35, 40, 45, 50, 55),
  duracao_media                 = c(28, 32, 36, 40, 44, 48),
  pct_viagens_internas          = c(15, 20, 30, 40, 50, 60),
  chegadas_trabalho_por_morador = c(0.2, 0.3, 0.5, 1, 2, 4),
  pct_chegadas_trabalho         = c(25, 35, 45, 55, 65, 75),
  chegadas_escola_por_morador   = c(0.15, 0.2, 0.25, 0.3, 0.4, 0.5),
  pct_chegadas_escola           = c(10, 20, 30, 40, 50, 60)
)
extremos_mapas <- list(
  pct_coletivo                  = c(20, 70),
  duracao_media                 = c(20, 60),
  pct_viagens_internas          = c(0, 75),
  chegadas_trabalho_por_morador = c(0.05, 6),
  pct_chegadas_trabalho         = c(10, 85),
  chegadas_escola_por_morador   = c(0.05, 1.1),
  pct_chegadas_escola           = c(0, 80)
)
transformacoes_mapas <- c(
  pct_coletivo                  = "identity",
  duracao_media                 = "identity",
  pct_viagens_internas          = "identity",
  chegadas_trabalho_por_morador = "log10",
  pct_chegadas_trabalho         = "identity",
  chegadas_escola_por_morador   = "identity",
  pct_chegadas_escola           = "identity"
)
rotulos_mapas <- list(
  pct_coletivo                  = rotulo_numero(suffix = "%"),
  duracao_media                 = rotulo_numero(suffix = " min"),
  pct_viagens_internas          = rotulo_numero(suffix = "%"),
  chegadas_trabalho_por_morador = rotulo_numero(accuracy = 0.1),
  pct_chegadas_trabalho         = rotulo_numero(suffix = "%"),
  chegadas_escola_por_morador   = rotulo_numero(accuracy = 0.01),
  pct_chegadas_escola           = rotulo_numero(suffix = "%")
)
legendas_mapas <- c(
  pct_coletivo                  = "% das\nviagens",
  duracao_media                 = "Minutos",
  pct_viagens_internas          = "% das\nviagens",
  chegadas_trabalho_por_morador = "Viagens por\nmorador",
  pct_chegadas_trabalho         = "% das\nchegadas",
  chegadas_escola_por_morador   = "Viagens por\nmorador",
  pct_chegadas_escola           = "% das\nchegadas"
)

# Definindo os mapas: indicador, título, subtítulo, nota e bloco dos registros
mapas <- tribble(
  ~indicador,                      ~titulo,                                  ~subtitulo,                                                                                                   ~nota,                                                                                                                        ~bloco,
  "pct_coletivo",                  "Viagens coletivas",                      "Proporção das viagens com origem no distrito feitas em transporte coletivo, dia útil, 2023",                  "Tipo de viagem da OD: coletivo, individual (motorizado), a pé e bicicleta; viagens com origem no distrito, de qualquer motivo.", "origem",
  "duracao_media",                 "Duração média das viagens",              "Duração média das viagens com origem no distrito, dia útil, 2023",                                            "Média da duração declarada, ponderada pelo fator de expansão das viagens; todos os modos e motivos.",                          "origem",
  "pct_viagens_internas",          "Viagens internas ao distrito",           "Proporção das viagens com origem no distrito que terminam no próprio distrito, dia útil, 2023",              "No município, a soma das viagens internas aos distritos sobre todas as viagens com origem na capital.",                        "origem",
  "chegadas_trabalho_por_morador", "Viagens que chegam para trabalhar",      "Viagens com destino no distrito e motivo trabalho, por morador do distrito, dia útil, 2023",                  "Motivo no destino trabalho (indústria, comércio e serviços); viagens e moradores estimados pela OD; escala logarítmica.",     "destino",
  "pct_chegadas_trabalho",         "Trabalho entre as viagens que chegam",   "Proporção das viagens que chegam ao distrito por motivo de trabalho, sem as voltas para casa, dia útil, 2023", "Motivo no destino trabalho (indústria, comércio e serviços) sobre as viagens com destino no distrito, sem as de motivo residência.", "destino",
  "chegadas_escola_por_morador",   "Viagens que chegam para estudar",        "Viagens com destino no distrito e motivo escola/educação, por morador do distrito, dia útil, 2023",          "Motivo no destino escola/educação; viagens e moradores estimados pela OD.",                                                  "destino",
  "pct_chegadas_escola",           "Escola entre as viagens que chegam",     "Proporção das viagens que chegam ao distrito por motivo de estudo, sem as voltas para casa, dia útil, 2023",  "Motivo no destino escola/educação sobre as viagens com destino no distrito, sem as de motivo residência.",                     "destino"
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos deslocamentos
deslocamentos <- read_parquet(file.path(diretorio_grupo, "deslocamentos.parquet"))

# Lendo os distritos com geometria
distritos <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet")) |>
  st_transform(epsg_projetado)


################################################################################
##### IV. Mapas
################################################################################

# Gravando um mapa por indicador
for (i in seq_len(nrow(mapas))) {

  # Selecionando a linha do mapa
  mapa_atual      <- mapas[i, ]
  indicador_atual <- mapa_atual$indicador

  # Obtendo o valor do município
  valor_municipio <- deslocamentos |>
    filter(nivel == "municipio", indicador == indicador_atual) |>
    pull(valor)

  # Juntando o valor de cada distrito à geometria projetada
  mapa_dados <- distritos |>
    inner_join(
      deslocamentos |>
        filter(nivel == "distrito", indicador == indicador_atual) |>
        select(codigo_distrito, valor, amostra_menor),
      by = "codigo_distrito"
    )

  # Descrevendo o município no subtítulo
  texto_municipio <- if (indicador_atual == "duracao_media") {
    paste0("Município: ", formatar_numero(valor_municipio, 1), " minutos")
  } else if (str_ends(indicador_atual, "_por_morador")) {
    paste0("Município: ", formatar_numero(valor_municipio, 2), " viagem por morador")
  } else {
    paste0("Município: ", formatar_numero(valor_municipio, 1), "%")
  }

  # Desenhando o mapa coroplético em classes fixas, com tracejado nos distritos
  # de amostra menor
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
    escala_fill_sequencial(
      classes   = TRUE,
      breaks    = limites_mapas[[indicador_atual]],
      limits    = extremos_mapas[[indicador_atual]],
      transform = transformacoes_mapas[[indicador_atual]],
      labels    = rotulos_mapas[[indicador_atual]],
      name      = legendas_mapas[[indicador_atual]]
    ) +
    elementos_mapa() +
    labs(
      title    = mapa_atual$titulo,
      subtitle = paste0(
        str_wrap(mapa_atual$subtitulo, width = 80), "\n",
        texto_municipio
      ),
      caption  = legenda_fonte(
        fontes  = "od",
        notas   = c(
          mapa_atual$nota,
          paste0(
            "Contorno tracejado: distrito com menos de ", minimo_registros_od,
            " registros de viagens com ", if_else(mapa_atual$bloco == "origem", "origem", "destino"),
            " nele; ler com cautela."
          ),
          # Nota dos recortes por destino, nos mapas do bloco de destino
          if (mapa_atual$bloco == "destino") "od_destino",
          "incerteza_amostra"
        ),
        largura = 95
      )
    ) +
    tema_mapa()

  # Gravando o mapa
  gravar_figura(
    grafico = mapa,
    arquivo = file.path(diretorio_saida, paste0("mapa_", indicador_atual, ".png")),
    tamanho = "mapa"
  )
}
