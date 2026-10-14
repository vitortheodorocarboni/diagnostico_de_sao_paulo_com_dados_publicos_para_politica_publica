################################################################################
##################################            ##################################
#################### 11.04) RANKING DOS DOMICÍLIOS ALUGADOS ####################
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
diretorio_grupo <- caminho_data("11_moradia")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("11_moradia", "ranking_pct_alugado.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da moradia
moradia <- read_parquet(file.path(diretorio_grupo, "moradia.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de alugados do município
alugado_municipio <- moradia |>
  filter(nivel == "municipio", indicador == "pct_alugado") |>
  pull(valor)

# Ordenando os distritos e dividindo-os em dois painéis de 48
ranking <- moradia |>
  filter(nivel == "distrito", indicador == "pct_alugado") |>
  mutate(
    # Posição do distrito, da maior à menor proporção de alugados
    posicao = min_rank(desc(valor)),
    # Rótulo com a posição e asterisco nos distritos com amostra menor
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", "")),
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
    xintercept = alugado_municipio,
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
    limits = c(0, 60),
    breaks = seq(0, 60, by = 15),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Domicílios alugados nos 96 distritos",
    subtitle = paste0(
      "% dos domicílios particulares permanentes ocupados, 2022. Linha tracejada: município (",
      formatar_numero(alugado_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "O Censo 2022 não pergunta o valor do aluguel, só a condição de ocupação.",
        "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado"
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
