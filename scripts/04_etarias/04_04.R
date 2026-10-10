################################################################################
##################################            ##################################
########## 04.04) RANKING DOS DISTRITOS PELO ÍNDICE DE ENVELHECIMENTO ##########
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
arquivo_saida <- caminho_resultados("04_etarias", "ranking_envelhecimento.png")


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

# Ordenando os distritos e dividindo-os em dois painéis de 48
ranking <- idade |>
  filter(nivel == "distrito", indicador == "envelhecimento") |>
  mutate(
    # Posição do distrito, do mais envelhecido ao menos envelhecido
    posicao = min_rank(desc(valor)),
    rotulo  = paste0(posicao, ". ", nome_rotulo),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º")
  ) |>
  mutate(
    rotulo = fct_reorder(rotulo, valor)
  )

# Desenhando o gráfico de pontos em dois painéis, com as linhas do município e
# do valor 100
grafico <- ggplot(ranking, aes(x = valor, y = rotulo)) +
  geom_segment(
    mapping   = aes(x = 50, xend = valor, yend = rotulo),
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
    xintercept = envelhecimento_municipio,
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
    limits = c(50, 300),
    breaks = seq(50, 300, by = 50),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Índice de envelhecimento dos 96 distritos",
    subtitle = paste0(
      "Pessoas de 60 anos ou mais por 100 de 0 a 14 anos, 2022. Linha cheia: 100. Linha tracejada: município (",
      formatar_numero(envelhecimento_municipio, 1), ")"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Valores acima de 100 indicam mais pessoas de 60 anos ou mais que de 0 a 14 anos. O padrão do IBGE usa 65 anos ou mais, que os grupos publicados não permitem.",
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
