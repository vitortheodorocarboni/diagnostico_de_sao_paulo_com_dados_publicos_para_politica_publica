################################################################################
##################################            ##################################
################### 04.05) FAIXAS DE IDADE NOS 96 DISTRITOS ####################
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
diretorio_grupo <- caminho_data("04_etarias")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("04_etarias", "faixas_de_idade_distritos.png")

# Definindo as quatro faixas largas, na ordem de empilhamento
faixas <- c(
  pct_0_14    = "0 a 14 anos",
  pct_15_29   = "15 a 29 anos",
  pct_30_59   = "30 a 59 anos",
  pct_60_mais = "60 anos ou mais"
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de idade
idade <- read_parquet(file.path(diretorio_grupo, "idade.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo o índice de envelhecimento do município
envelhecimento_municipio <- idade |>
  filter(nivel == "municipio", indicador == "envelhecimento") |>
  pull(valor)

# Preparando os distritos com a posição pelo índice de envelhecimento
ordem_distritos <- idade |>
  filter(nivel == "distrito", indicador == "envelhecimento") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo)
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções das quatro faixas
faixas_dados <- idade |>
  filter(nivel == "distrito", indicador %in% names(faixas)) |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    faixa  = factor(faixas[indicador], levels = faixas),
    rotulo = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(faixas_dados, aes(x = valor, y = rotulo, fill = faixa)) +
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
  escala_fill_categorica(
    niveis = levels(faixas_dados$faixa),
    # Legenda na ordem das barras, da esquerda para a direita
    breaks = rev(levels(faixas_dados$faixa)),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Faixas de idade nos 96 distritos",
    subtitle = paste0(
      "Proporção dos moradores, 2022\n",
      "Distritos ordenados pelo índice de envelhecimento, do maior ao menor (município: ",
      formatar_numero(envelhecimento_municipio, 1), ")"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Proporção sobre a soma publicada dos 11 grupos de idade do IBGE, reunidos em quatro faixas.",
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
