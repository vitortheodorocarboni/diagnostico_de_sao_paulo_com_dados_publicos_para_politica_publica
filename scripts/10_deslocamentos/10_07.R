################################################################################
##################################            ##################################
########## 10.07) MAPAS DE FLUXOS NA RMSP: VIAGENS, TRABALHO E ESCOLA ##########
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

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo o mínimo de registros de um par para desenhá-lo
minimo_registros_par <- 10

# Definindo os cortes dos fluxos: os maiores pares até somar metade do fluxo
# entre unidades diferentes, com no máximo mil pares
parcela_maxima <- 0.5
maximo_pares   <- 1000

# Descrevendo os três motivos dos mapas de fluxos
motivos <- tribble(
  ~motivo,    ~titulo,                                    ~medida,                      ~medida_interna,              ~nota,
  "todas",    "Fluxos de viagens na RMSP",                "Viagens por dia útil",       "Viagens internas",           "Viagens de um dia útil, de qualquer motivo, pela matriz de origem e destino da OD; ida e volta somadas numa linha.",
  "trabalho", "Fluxos entre moradia e trabalho na RMSP",  "Trabalhadores",              "Moram e trabalham na unidade", "Trabalhadores com local fixo de trabalho, pela matriz residência → trabalho da OD; os dois sentidos de cada par somados numa linha.",
  "escola",   "Fluxos entre moradia e escola na RMSP",    "Estudantes",                 "Moram e estudam na unidade", "Estudantes com escola numa das 527 zonas, pela matriz residência → escola da OD; os dois sentidos de cada par somados numa linha."
)

# Descrevendo a escala dos mapas de fluxos (só as unidades agregadas)
escalas <- tribble(
  ~escala,    ~plural,    ~complemento,                                     ~descricao,                                                                                         ~faixa_linhas, ~maximo_circulos,
  "unidades", "unidades", "por distrito da capital e município",            "Unidades: as zonas da capital agregadas nos 96 distritos e as dos demais 38 municípios agregadas no município", 5,             9,
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para ligar os pontos das duas pontas a cada par
ligar_pontos <- function(fluxos, pontos) {

  # Juntando as coordenadas das duas pontas
  fluxos |>
    left_join(pontos |> select(unidade_a = codigo, xa = x, ya = y), by = "unidade_a") |>
    left_join(pontos |> select(unidade_b = codigo, xb = x, yb = y), by = "unidade_b")
}

# Criando uma função para desenhar um mapa de fluxos
desenhar_fluxos <- function(linhas, internos, fundo, contorno_capital, titulo, subtitulo,
                            notas, nome_linhas, nome_circulos, faixa_linhas, maximo_circulos) {

  # Desenhando o fundo, os círculos dos fluxos internos e as linhas, das finas
  # para as grossas
  ggplot() +
    geom_sf(
      data      = fundo,
      fill      = cor_centro_divergente,
      colour    = cor_fundo,
      linewidth = 0.2
    ) +
    geom_sf(
      data      = contorno_capital,
      fill      = NA,
      colour    = cor_texto_apoio,
      linewidth = 0.4
    ) +
    geom_point(
      data    = internos,
      mapping = aes(x = x, y = y, size = fluxo),
      shape   = 21,
      fill    = cor_neutra,
      colour  = cor_fundo,
      alpha   = 0.5,
      stroke  = 0.2
    ) +
    geom_segment(
      data      = arrange(linhas, fluxo),
      mapping   = aes(x = xa, y = ya, xend = xb, yend = yb, linewidth = fluxo),
      colour    = paleta_insper[["vermelho"]],
      alpha     = 0.45,
      lineend   = "round"
    ) +
    scale_linewidth_continuous(
      range  = c(0.1, faixa_linhas),
      labels = rotulo_numero(),
      name   = nome_linhas
    ) +
    scale_size_area(
      max_size = maximo_circulos,
      labels   = rotulo_numero(),
      name     = nome_circulos
    ) +
    # Pondo a legenda das linhas antes da dos círculos
    guides(
      linewidth = guide_legend(order = 1),
      size      = guide_legend(order = 2)
    ) +
    coord_sf(
      crs    = st_crs(epsg_projetado),
      expand = FALSE
    ) +
    elementos_mapa() +
    labs(
      title    = titulo,
      subtitle = subtitulo,
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes  = "od",
        notas   = notas,
        largura = 120
      )
    ) +
    tema_mapa() +
    theme(
      legend.position = "right"
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo os fluxos (dois sentidos somados) dos três motivos nas duas escalas
fluxos <- read_parquet(file.path(diretorio_grupo, "fluxos.parquet"))

# Lendo os pontos internos das unidades
pontos <- list(
  unidades = read_parquet(file.path(diretorio_grupo, "unidades.parquet")) |>
    select(codigo = codigo_unidade, x, y)
)

# Montando os fundos: distritos da capital e demais municípios,
municipios_geometria <- ler_geoparquet(file.path(diretorio_grupo, "municipios_geometria.parquet"))
fundos <- list(
  unidades = bind_rows(
    ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet")) |>
      st_transform(epsg_projetado) |>
      transmute(codigo_unidade = codigo_distrito),
    municipios_geometria |>
      filter(codigo_municipio != codigo_municipio_sp) |>
      transmute(codigo_unidade = codigo_municipio)
  ) |>
    st_simplify(
      preserveTopology = TRUE,
      dTolerance       = 60
    )
)

# Obtendo o contorno da capital
contorno_capital <- municipios_geometria |>
  filter(codigo_municipio == codigo_municipio_sp)


################################################################################
##### V. Mapas
################################################################################

# Criando a lista dos pares desenhados, para conferência
desenhados <- list()

# Desenhando e gravando um mapa por motivo e escala
for (i in seq_len(nrow(motivos))) {
  for (j in seq_len(nrow(escalas))) {

    # Selecionando o motivo e a escala
    motivo_atual <- motivos[i, ]
    escala_atual <- escalas[j, ]
    fluxos_atual <- fluxos |>
      filter(motivo == motivo_atual$motivo, escala == escala_atual$escala)

    # Selecionando os maiores pares diferentes com o mínimo de registros, até a
    # parcela e o número máximo definidos
    total_entre <- sum(fluxos_atual$fluxo[!fluxos_atual$interno])
    linhas <- fluxos_atual |>
      filter(!interno, n_registros >= minimo_registros_par) |>
      arrange(desc(fluxo)) |>
      filter(cumsum(fluxo) <= parcela_maxima * total_entre) |>
      slice_head(n = maximo_pares) |>
      ligar_pontos(pontos[[escala_atual$escala]])

    # Obtendo os fluxos internos, com o ponto
    internos <- fluxos_atual |>
      filter(interno) |>
      left_join(pontos[[escala_atual$escala]], by = c("unidade_a" = "codigo"))

    # Interrompendo se algum par ficar sem ponto
    stopifnot(!anyNA(linhas$xa), !anyNA(linhas$xb), !anyNA(internos$x))

    # Calculando a parcela desenhada
    parcela <- sum(linhas$fluxo) / total_entre
    message(
      motivo_atual$motivo, " / ", escala_atual$escala, ": ", nrow(linhas), " pares (",
      round(100 * parcela, 1), "% do fluxo entre pontas diferentes)"
    )

    # Montando o subtítulo e as notas
    subtitulo <- paste0(
      "Os ", formatar_numero(nrow(linhas)), " maiores pares entre ", escala_atual$plural, " diferentes com pelo menos ",
      minimo_registros_par, " registros, que somam ", formatar_numero(100 * parcela, 0),
      "% do fluxo entre elas, 2023\n", escala_atual$descricao
    )
    notas <- c(
      paste0(
        motivo_atual$nota, " Linhas retas entre pontos internos, não o trajeto. Pares com menos de ",
        minimo_registros_par, " registros ficam de fora."
      ),
      "Círculos: fluxo com as duas pontas na mesma unidade.",
      # Nota dos recortes por local de trabalho, no mapa do trabalho
      if (motivo_atual$motivo == "trabalho") "od_destino",
      "incerteza_amostra"
    )

    # Desenhando e gravando o mapa
    gravar_figura(
      grafico = desenhar_fluxos(
        linhas           = linhas,
        internos         = internos,
        fundo            = fundos[[escala_atual$escala]],
        contorno_capital = contorno_capital,
        titulo           = paste0(motivo_atual$titulo, ", ", escala_atual$complemento),
        subtitulo        = subtitulo,
        notas            = notas,
        nome_linhas      = paste0(motivo_atual$medida, "\n(dois sentidos)"),
        nome_circulos    = motivo_atual$medida_interna,
        faixa_linhas     = escala_atual$faixa_linhas,
        maximo_circulos  = escala_atual$maximo_circulos
      ),
      arquivo = file.path(
        diretorio_saida,
        paste0("mapa_fluxos_", motivo_atual$motivo, "_", escala_atual$escala, ".png")
      ),
      tamanho = "mapa",
      largura = 26,
      altura  = 18
    )

    # Guardando os pares desenhados
    desenhados[[length(desenhados) + 1]] <- linhas |>
      mutate(motivo = motivo_atual$motivo, escala = escala_atual$escala)
  }
}


################################################################################
##### VI. Exportação
################################################################################

# Gravando os pares desenhados, para conferência
write_parquet(
  x    = bind_rows(desenhados),
  sink = file.path(diretorio_grupo, "fluxos_desenhados.parquet")
)
