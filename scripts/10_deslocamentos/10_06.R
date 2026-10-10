################################################################################
##################################            ##################################
############### 10.06) ONDE TRABALHAM E ONDE MORAM, POR DISTRITO ###############
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

# Definindo o diretório de saída dos mapas por distrito
diretorio_saida <- caminho_resultados("10_deslocamentos", "distritos")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Definindo o mínimo de registros de um par abaixo do qual a unidade recebe um
# ponto de cautela
minimo_registros_par <- 10

# Definindo as classes das proporções (limites inferiores, em %)
classes_pct <- c(0, 1, 2, 5, 10, 20, 40)
rotulos_classes <- c("Menos de 1%", "1 a 2%", "2 a 5%", "5 a 10%", "10 a 20%", "20 a 40%", "40% ou mais")
rotulo_vazio    <- "Sem registros"

# Definindo as cores das classes e da unidade sem registros
cores_classes <- c(
  setNames(paleta_sequencial(length(rotulos_classes)), rotulos_classes),
  setNames(cor_grade, rotulo_vazio)
)

# Definindo as duas leituras da matriz residência → trabalho
leituras <- tribble(
  ~leitura,        ~unidade_base,        ~unidade_par,         ~campo_pct,              ~titulo,                                         ~sufixo,
  "moradores",     "unidade_residencia", "unidade_trabalho",   "pct_dos_moradores",     "Onde trabalham os moradores do distrito",       "onde_trabalham",
  "trabalhadores", "unidade_trabalho",   "unidade_residencia", "pct_dos_trabalhadores", "Onde moram os que trabalham no distrito",       "onde_moram"
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar o mapa de uma leitura para um distrito
desenhar_mapa <- function(unidades_mapa, contorno_capital, codigo, titulo, subtitulo) {

  # Desenhando as unidades em classes, com o distrito e a capital em destaque
  ggplot(unidades_mapa) +
    geom_sf(
      mapping     = aes(fill = classe),
      colour      = cor_fundo,
      linewidth   = 0.15,
      # Mostrando na legenda também as classes ausentes no distrito
      show.legend = TRUE
    ) +
    geom_sf(
      data      = contorno_capital,
      fill      = NA,
      colour    = cor_texto_apoio,
      linewidth = 0.4
    ) +
    geom_sf(
      data      = filter(unidades_mapa, codigo_unidade == codigo),
      fill      = NA,
      colour    = cor_texto,
      linewidth = 0.8
    ) +
    # Marcando com um ponto as unidades com poucos registros no par
    geom_sf(
      data   = unidades_mapa |>
        filter(n_registros > 0, n_registros < minimo_registros_par) |>
        st_set_agr("constant") |>
        st_point_on_surface(),
      shape  = 16,
      size   = 0.5,
      colour = cor_texto_apoio
    ) +
    scale_fill_manual(
      values = cores_classes,
      breaks = names(cores_classes),
      drop   = FALSE,
      name   = NULL
    ) +
    # Mostrando todas as classes na legenda, com a cor de cada uma
    guides(
      fill = guide_legend(override.aes = list(fill = unname(cores_classes)))
    ) +
    elementos_mapa() +
    labs(
      title    = titulo,
      subtitle = subtitulo,
      caption  = legenda_fonte(
        fontes  = "od",
        notas   = c(
          paste0(
            "Matriz residência → trabalho da OD (trabalhadores residentes na RMSP com local fixo de trabalho); distritos na capital ",
            "e municípios no resto da RMSP. Contorno grosso: o distrito; contorno cinza: a capital."
          ),
          paste0(
            "Ponto: menos de ", minimo_registros_par, " registros no par, proporção pouco confiável; ",
            "cinza: nenhum registro."
          ),
          "od_destino", "incerteza_amostra"
        ),
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

# Lendo a matriz residência → trabalho nas unidades
matriz <- read_parquet(file.path(diretorio_grupo, "matriz_residencia_trabalho.parquet"))

# Lendo os nomes das unidades
nomes_unidades <- read_parquet(file.path(diretorio_grupo, "unidades.parquet")) |>
  select(codigo_unidade, nome_unidade, tipo_unidade)

# Montando os polígonos das unidades: distritos da capital e demais municípios,
# simplificados para os mapas
municipios_geometria <- ler_geoparquet(file.path(diretorio_grupo, "municipios_geometria.parquet"))
unidades_poligonos <- bind_rows(
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

# Obtendo o contorno da capital
contorno_capital <- municipios_geometria |>
  filter(codigo_municipio == codigo_municipio_sp) |>
  st_simplify(
    preserveTopology = TRUE,
    dTolerance       = 60
  )

# Listando os distritos da capital
distritos_lista <- nomes_unidades |>
  filter(tipo_unidade == "distrito") |>
  arrange(codigo_unidade)


################################################################################
##### V. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando as duas leituras de cada distrito
for (j in seq_len(nrow(leituras))) {

  # Selecionando a leitura
  leitura_atual <- leituras[j, ]
  message("Gerando os mapas de ", leitura_atual$leitura, " dos distritos")

  # Percorrendo os distritos
  for (i in seq_len(nrow(distritos_lista))) {

    # Selecionando o distrito e os seus pares na leitura atual
    codigo <- distritos_lista$codigo_unidade[i]
    nome   <- distritos_lista$nome_unidade[i]
    pares  <- matriz |>
      filter(.data[[leitura_atual$unidade_base]] == codigo) |>
      transmute(
        codigo_unidade = .data[[leitura_atual$unidade_par]],
        n_registros,
        pct            = .data[[leitura_atual$campo_pct]]
      )

    # Pulando o distrito abaixo do piso de registros da OD
    registros <- sum(pares$n_registros)
    if (registros < piso_registros_od) {
      message("  ", nome, ": ", registros, " registros, sem mapa")
      next
    }

    # Classificando cada unidade (as sem registros ficam em cinza)
    unidades_mapa <- unidades_poligonos |>
      left_join(pares, by = "codigo_unidade") |>
      mutate(
        n_registros = replace_na(n_registros, 0L),
        classe      = if_else(
          n_registros == 0,
          rotulo_vazio,
          rotulos_classes[findInterval(pct, classes_pct)]
        ),
        classe      = factor(classe, levels = names(cores_classes))
      )

    # Resumindo o próprio distrito, os outros distritos e os outros municípios
    resumo <- pares |>
      left_join(nomes_unidades, by = "codigo_unidade") |>
      summarise(
        proprio    = sum(pct[codigo_unidade == codigo]),
        capital    = sum(pct[tipo_unidade == "distrito" & codigo_unidade != codigo]),
        municipios = sum(pct[tipo_unidade == "municipio"])
      )
    subtitulo <- paste0(
      nome, ", 2023. No próprio distrito: ", formatar_numero(resumo$proprio, 1),
      "%; em outros distritos da capital: ", formatar_numero(resumo$capital, 1),
      "%; em outros municípios da RMSP: ", formatar_numero(resumo$municipios, 1), "%",
      if (registros < minimo_registros_od) paste0("\nAmostra menor (", registros, " registros): ler com cautela") else ""
    )

    # Desenhando e gravando o mapa
    gravar_figura(
      grafico = desenhar_mapa(
        unidades_mapa    = unidades_mapa,
        contorno_capital = contorno_capital,
        codigo           = codigo,
        titulo           = leitura_atual$titulo,
        subtitulo        = subtitulo
      ),
      arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = leitura_atual$sufixo)),
      tamanho = "grafico",
      largura = 24,
      altura  = 17
    )
  }
}
