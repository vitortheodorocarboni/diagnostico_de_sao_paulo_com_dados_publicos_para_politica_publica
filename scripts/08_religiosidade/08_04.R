################################################################################
##################################            ##################################
####################### 08.04) RELIGIÃO NOS 96 DISTRITOS #######################
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
diretorio_grupo <- caminho_data("08_religiosidade")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("08_religiosidade", "religiao_distritos.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da religiosidade
religiao <- read_parquet(file.path(diretorio_grupo, "religiao.parquet"))

# Lendo a composição nos seis grupos
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção católica do município
catolica_municipio <- religiao |>
  filter(nivel == "municipio", indicador == "pct_catolica") |>
  pull(valor)

# Preparando os distritos com a posição pela proporção católica
ordem_distritos <- religiao |>
  filter(nivel == "distrito", indicador == "pct_catolica") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções dos seis grupos
grupos_rotulos <- composicao |>
  distinct(grupo_codigo, rotulo_grupo) |>
  arrange(grupo_codigo) |>
  pull(rotulo_grupo)
composicao_dados <- composicao |>
  filter(nivel_territorial == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # A religião católica fica à esquerda
    grupo_religiao = factor(rotulo_grupo, levels = rev(grupos_rotulos)),
    rotulo         = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(composicao_dados, aes(x = pct, y = rotulo, fill = grupo_religiao)) +
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
    values = setNames(paleta_categorica(6), grupos_rotulos),
    breaks = grupos_rotulos,
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title    = "Religião nos 96 distritos",
    subtitle = paste0(
      "Pessoas de 10 anos ou mais, 2022\n",
      "Distritos ordenados pela proporção católica, da maior à menor (município: ",
      formatar_numero(catolica_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "A religião é autodeclarada; proporção sobre a soma das categorias declaradas.",
        "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado"
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
  altura  = 16
)
