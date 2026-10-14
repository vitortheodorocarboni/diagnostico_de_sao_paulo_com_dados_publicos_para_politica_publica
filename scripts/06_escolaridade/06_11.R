################################################################################
##################################            ##################################
############### 06.11) RANKING DOS ANOS DE ESTUDO DOS DISTRITOS ################
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
diretorio_grupo <- caminho_data("06_escolaridade")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("06_escolaridade", "ranking_anos_estudo.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da extensão
extensao <- read_parquet(file.path(diretorio_grupo, "extensao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo os anos de estudo médios do município
anos_municipio <- extensao |>
  filter(nivel == "municipio", indicador == "anos_estudo_medio") |>
  pull(valor)

# Ordenando os distritos e dividindo-os em dois painéis de 48
ranking <- extensao |>
  filter(nivel == "distrito", indicador == "anos_estudo_medio") |>
  mutate(
    # Posição do distrito, dos mais aos menos anos de estudo
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
    xintercept = anos_municipio,
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
    limits = c(0, 14),
    breaks = seq(0, 14, by = 2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Anos de estudo médios nos 96 distritos",
    subtitle = paste0(
      "Anos de estudo por pessoa, todos os moradores, 2022. Linha tracejada: município (",
      formatar_numero(anos_municipio, 1), " anos)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "Média ponderada por pessoa de P0790, com as crianças: ela depende da estrutura etária do distrito, e o tratado não permite padronizá-la.",
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
