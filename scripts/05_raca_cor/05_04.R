################################################################################
##################################            ##################################
############### 05.04) RANKING DOS DISTRITOS POR PRETOS E PARDOS ###############
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
arquivo_saida <- caminho_resultados("05_raca_cor", "ranking_pretos_e_pardos.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de cor ou raça
cor <- read_parquet(file.path(diretorio_grupo, "cor.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de pretos e pardos do município
negra_municipio <- cor |>
  filter(nivel == "municipio", indicador == "pct_negra") |>
  pull(valor)

# Ordenando os distritos e dividindo-os em dois painéis de 48
ranking <- cor |>
  filter(nivel == "distrito", indicador == "pct_negra") |>
  mutate(
    # Posição do distrito, da maior à menor proporção
    posicao = min_rank(desc(valor)),
    rotulo  = paste0(posicao, ". ", nome_rotulo),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º")
  ) |>
  mutate(
    rotulo = fct_reorder(rotulo, valor)
  )

# Desenhando o gráfico de pontos em dois painéis, com a linha do município
grafico <- ggplot(ranking, aes(x = valor, y = rotulo)) +
  geom_segment(
    mapping   = aes(x = 0, xend = valor, yend = rotulo),
    colour    = cor_grade,
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = negra_municipio,
    colour     = cor_texto_apoio,
    linetype   = "dashed",
    linewidth  = 0.3
  ) +
  geom_point(
    colour = paleta_insper[["vermelho"]],
    size   = 1
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(0, 70),
    breaks = seq(0, 70, by = 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Pretos e pardos nos 96 distritos",
    subtitle = paste0(
      "Proporção dos moradores que se declaram pretos ou pardos, 2022. Linha tracejada: município (",
      formatar_numero(negra_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Soma de preta e parda sobre a soma publicada das cinco categorias de cor ou raça. A cor ou raça é autodeclarada.",
        "sigilo_pessoas"
      )
    )
  ) +
  tema_insper() +
  theme(
    # Nomes dos distritos menores, para caberem os 48 de cada painel
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
