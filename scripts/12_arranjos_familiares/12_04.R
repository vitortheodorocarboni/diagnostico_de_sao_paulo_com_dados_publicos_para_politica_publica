################################################################################
##################################            ##################################
################## 12.04) RANKING DOS CÔNJUGES DO MESMO SEXO ###################
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
diretorio_grupo <- caminho_data("12_arranjos_familiares")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("12_arranjos_familiares", "ranking_pct_conjuges_mesmo_sexo.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos arranjos familiares
arranjos <- read_parquet(file.path(diretorio_grupo, "arranjos.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de cônjuges do mesmo sexo do município
mesmo_sexo_municipio <- arranjos |>
  filter(nivel == "municipio", indicador == "pct_conjuges_mesmo_sexo") |>
  pull(valor)

# Ordenando os distritos e dividindo-os em dois painéis de 48
ranking <- arranjos |>
  filter(nivel == "distrito", indicador == "pct_conjuges_mesmo_sexo") |>
  mutate(
    # Posição do distrito, da maior à menor proporção de cônjuges do mesmo sexo
    posicao = min_rank(desc(valor)),
    # Rótulo com a posição
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
    xintercept = mesmo_sexo_municipio,
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
    limits = c(0, 13),
    breaks = seq(0, 12, by = 3),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Cônjuges do mesmo sexo nos 96 distritos",
    subtitle = paste0(
      "% dos cônjuges ou companheiros(as) do responsável, 2022. Linha tracejada: município (",
      formatar_numero(mesmo_sexo_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Cônjuge ou companheiro(a) do mesmo sexo do responsável pelo domicílio; casais em que nenhum dos dois é o responsável não entram.",
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
