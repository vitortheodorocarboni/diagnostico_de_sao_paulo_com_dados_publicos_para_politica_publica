################################################################################
##################################            ##################################
############### 03.04) RANKING DOS DISTRITOS PELA RAZÃO DE SEXO ################
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
diretorio_grupo <- caminho_data("03_sexo")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("03_sexo", "ranking_razao_sexo.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de sexo
sexo <- read_parquet(file.path(diretorio_grupo, "sexo.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a razão de sexo do município
razao_municipio <- sexo |>
  filter(nivel == "municipio", indicador == "razao_sexo") |>
  pull(valor)

# Ordenando os distritos e dividindo-os em dois painéis de 48
ranking <- sexo |>
  filter(nivel == "distrito", indicador == "razao_sexo") |>
  mutate(
    # Posição do distrito, da maior à menor razão de sexo
    posicao = min_rank(desc(valor)),
    rotulo  = paste0(posicao, ". ", nome_rotulo),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º")
  ) |>
  mutate(
    rotulo = fct_reorder(rotulo, valor)
  )

# Desenhando o gráfico de pontos em dois painéis, com as linhas do município e
# da paridade
grafico <- ggplot(ranking, aes(x = valor, y = rotulo)) +
  geom_segment(
    mapping   = aes(x = 75, xend = valor, yend = rotulo),
    colour    = cor_grade,
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 100,
    colour     = cor_texto,
    linetype   = "solid",
    linewidth  = 0.3
  ) +
  geom_vline(
    xintercept = razao_municipio,
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
    labels = rotulo_numero(),
    limits = c(75, 107),
    breaks = seq(80, 105, by = 5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Razão de sexo dos 96 distritos",
    subtitle = paste0(
      "Homens por 100 mulheres, 2022. Linha cheia: paridade (100). Linha tracejada: município (",
      formatar_numero(razao_municipio, 1), ")"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Razão de sexo = homens para cada 100 mulheres; valores abaixo de 100 indicam mais mulheres que homens.",
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
