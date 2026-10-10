################################################################################
##################################            ##################################
########## 05.06) PRETOS E PARDOS POR FAIXA DE IDADE NOS 96 DISTRITOS ##########
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
arquivo_saida <- caminho_resultados("05_raca_cor", "pretos_e_pardos_por_idade_distritos.png")

# Definindo as colunas do mapa de calor
colunas <- c(
  pct_negra         = "Todas as\nidades",
  pct_negra_0_14    = "0 a 14\nanos",
  pct_negra_15_29   = "15 a 29\nanos",
  pct_negra_30_59   = "30 a 59\nanos",
  pct_negra_60_mais = "60 anos\nou mais"
)


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

# Obtendo o valor do município em cada coluna
municipio <- cor |>
  filter(nivel == "municipio", indicador %in% names(colunas)) |>
  select(indicador, valor_municipio = valor)

# Ordenando os distritos pela proporção de pretos e pardos, da maior à menor
ordem_distritos <- cor |>
  filter(nivel == "distrito", indicador == "pct_negra") |>
  arrange(desc(valor)) |>
  pull(nome_rotulo)

# Calculando a diferença de cada distrito para o município em cada coluna
mapa_calor <- cor |>
  filter(nivel == "distrito", indicador %in% names(colunas)) |>
  left_join(municipio, by = "indicador") |>
  mutate(
    diferenca   = valor - valor_municipio,
    posicao     = match(nome_rotulo, ordem_distritos),
    painel      = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    nome_rotulo = factor(nome_rotulo, levels = rev(ordem_distritos)),
    faixa_idade = factor(colunas[indicador], levels = colunas)
  )

# Desenhando o mapa de calor em dois painéis, com escala centrada em zero
grafico <- ggplot(mapa_calor, aes(x = faixa_idade, y = nome_rotulo, fill = diferenca)) +
  geom_tile(
    colour    = cor_fundo,
    linewidth = 0.25
  ) +
  geom_text(
    mapping = aes(label = formatar_numero(valor, 0)),
    family  = familia_fonte,
    size    = 1.6,
    colour  = cor_texto
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  escala_fill_divergente(
    meio   = 0,
    labels = rotulo_numero(),
    name   = "Diferença para o município (pontos percentuais)",
    guide  = guide_colourbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Pretos e pardos por faixa de idade nos 96 distritos",
    subtitle = paste0(
      "Proporção dos moradores de cada faixa que se declaram pretos ou pardos, 2022 (valor em cada célula)\n",
      "Distritos ordenados pela proporção geral, da maior à menor (município: ", formatar_numero(negra_municipio, 1), "%)\n",
      "Cor: diferença para o município na mesma faixa. Vermelho: acima; verde: abaixo"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Preta e parda sobre a soma publicada das cinco categorias de cor ou raça em cada faixa; a cor ou raça é autodeclarada.",
        "sigilo_pessoas"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 5.5, colour = cor_texto),
    axis.text.x        = element_text(size = 6.5, colour = cor_texto, lineheight = 0.9),
    legend.position    = "bottom",
    panel.grid.major   = element_blank(),
    panel.spacing.x    = unit(1, "cm"),
    strip.text         = element_blank(),
    legend.title       = element_text(size = 7.5)
  )


################################################################################
##### V. Exportação
################################################################################

# Gravando o gráfico
gravar_figura(
  grafico = grafico,
  arquivo = arquivo_saida,
  tamanho = "grafico",
  altura  = 17
)
