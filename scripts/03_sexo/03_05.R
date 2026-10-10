################################################################################
##################################            ##################################
########### 03.05) RAZÃO DE SEXO POR FAIXA DE IDADE NOS 96 DISTRITOS ###########
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
arquivo_saida <- caminho_resultados("03_sexo", "razao_sexo_por_idade_distritos.png")

# Definindo as colunas do mapa de calor
colunas <- c(
  razao_sexo    = "Todas as\nidades",
  razao_0_14    = "0 a 14\nanos",
  razao_15_29   = "15 a 29\nanos",
  razao_30_59   = "30 a 59\nanos",
  razao_60_mais = "60 anos\nou mais"
)


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

# Obtendo a menor cobertura de sexo e idade entre os distritos (para a nota)
cobertura_minima <- sexo |>
  filter(nivel == "distrito", indicador == "cobertura_faixas") |>
  pull(valor) |>
  min()

# Ordenando os distritos pela razão de sexo geral, da maior à menor
ordem_distritos <- sexo |>
  filter(nivel == "distrito", indicador == "razao_sexo") |>
  arrange(desc(valor)) |>
  pull(nome_rotulo)

# Preparando os dados com o painel e a posição de cada distrito
mapa_calor <- sexo |>
  filter(nivel == "distrito", indicador %in% names(colunas)) |>
  mutate(
    posicao        = match(nome_rotulo, ordem_distritos),
    painel         = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    nome_rotulo    = factor(nome_rotulo, levels = rev(ordem_distritos)),
    faixa_idade    = factor(colunas[indicador], levels = colunas)
  )

# Desenhando o mapa de calor em dois painéis, com escala centrada na paridade
grafico <- ggplot(mapa_calor, aes(x = faixa_idade, y = nome_rotulo, fill = valor)) +
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
    meio   = 100,
    labels = rotulo_numero(),
    name   = "Homens por 100 mulheres (100 = paridade)",
    guide  = guide_colourbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Razão de sexo por faixa de idade nos 96 distritos",
    subtitle = paste0(
      "Homens por 100 mulheres, 2022. Distritos ordenados pela razão de sexo geral (município: ",
      formatar_numero(razao_municipio, 1), ")\n",
      "Vermelho: mais homens que mulheres; verde: mais mulheres que homens"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        paste0(
          "As faixas somam as faixas de idade do IBGE; as células com sigilo por sexo e idade ficam fora ",
          "e deixam de fora até ", formatar_numero(100 - cobertura_minima, 1), "% dos moradores de um distrito."
        ),
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
