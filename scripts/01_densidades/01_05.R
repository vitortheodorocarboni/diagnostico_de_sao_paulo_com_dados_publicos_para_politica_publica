################################################################################
##################################            ##################################
################# 01.05) RANKING DOS DISTRITOS PELA DENSIDADE ##################
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
diretorio_grupo <- caminho_data("01_densidades")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("01_densidades", "ranking_densidade_demografica.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de densidade
densidades <- read_parquet(file.path(diretorio_grupo, "densidades.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a densidade do município
densidade_municipio <- densidades |>
  filter(nivel == "municipio", indicador == "densidade_demografica") |>
  pull(valor)

# Ordenando os distritos e dividindo-os em dois painéis de 48
ranking <- densidades |>
  filter(nivel == "distrito", indicador == "densidade_demografica") |>
  mutate(
    # Posição do distrito, do mais denso ao menos denso
    posicao = min_rank(desc(valor)),
    # Rótulo com a posição
    rotulo  = paste0(posicao, ". ", nome_rotulo),
    # Painel de cada metade do ranking
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º")
  ) |>
  # Ordenando os rótulos para o mais denso ficar no alto de cada painel
  mutate(
    rotulo = fct_reorder(rotulo, valor)
  )

# Desenhando o gráfico de pontos em dois painéis
grafico <- ggplot(ranking, aes(x = valor, y = rotulo)) +
  geom_segment(
    mapping   = aes(x = 0, xend = valor, yend = rotulo),
    colour    = cor_grade,
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = densidade_municipio,
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
    limits = c(0, 27000),
    breaks = seq(0, 25000, by = 5000),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Densidade demográfica dos 96 distritos",
    subtitle = paste0(
      "Moradores por km², 2022. Linha tracejada: município (",
      formatar_numero(densidade_municipio), " moradores por km²)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = "A densidade usa a área total do distrito, que inclui parques, represas e zona rural."
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
  tamanho = "grafico"
)
