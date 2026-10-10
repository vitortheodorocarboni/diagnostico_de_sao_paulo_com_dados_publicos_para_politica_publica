################################################################################
##################################            ##################################
############## 05.05) COMPOSIÇÃO POR COR OU RAÇA NOS 96 DISTRITOS ##############
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("tidyverse")

# Carregando bibliotecas
library(arrow)
library(tidyverse)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Carregando a identidade visual
source(file.path("scripts", "_config", "identidade_visual.R"), encoding = "UTF-8")

# Definindo o diretório de dados do grupo
diretorio_grupo <- caminho_data("05_raca_cor")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("05_raca_cor", "composicao_distritos.png")

# Definindo as cores ou raças, na ordem de empilhamento
cores <- c(
  branca   = "Branca",
  parda    = "Parda",
  preta    = "Preta",
  amarela  = "Amarela",
  indigena = "Indígena"
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de cor ou raça
cor <- read_parquet(file.path(diretorio_grupo, "cor.parquet"))

# Lendo a composição por cor ou raça
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de pretos e pardos do município
negra_municipio <- cor |>
  filter(nivel == "municipio", indicador == "pct_negra") |>
  pull(valor)

# Preparando os distritos com a posição pela proporção de pretos e pardos
ordem_distritos <- cor |>
  filter(nivel == "distrito", indicador == "pct_negra") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo)
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções das cinco cores ou raças
composicao_dados <- composicao |>
  filter(nivel == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    categoria = factor(cores[cor], levels = rev(cores)),
    rotulo    = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(composicao_dados, aes(x = pct, y = rotulo, fill = categoria)) +
  geom_col(
    width     = 0.8,
    colour    = cor_fundo,
    linewidth = 0.15
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = setNames(
      c(paleta_insper[["vermelho"]], paleta_insper[["turquesa"]], paleta_insper[["roxo"]],
        paleta_insper[["amarelo"]], paleta_insper[["rosa"]]),
      c("Preta", "Parda", "Branca", "Amarela", "Indígena")
    ),
    breaks = unname(cores),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Cor ou raça nos 96 distritos",
    subtitle = paste0(
      "Proporção dos moradores, 2022\n",
      "Distritos ordenados pela proporção de pretos e pardos, da maior à menor (município: ",
      formatar_numero(negra_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        paste0(
          "Proporção sobre a soma publicada das cinco categorias de cor ou raça, autodeclaradas. ",
          "O sigilo atinge mais as categorias pequenas: amarela e indígena ficam subestimadas."
        ),
        "sigilo_pessoas"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 5.5, colour = cor_texto),
    panel.grid.major.y = element_blank(),
    panel.spacing.x    = unit(1.2, "cm")
  )


################################################################################
##### V. Exportação
################################################################################

# Gravando o gráfico
gravar_figura(
  grafico = grafico,
  arquivo = arquivo_saida,
  tamanho = "grafico",
  altura  = 15
)
