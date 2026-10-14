################################################################################
##################################            ##################################
############### 07.11) RANKING DO RENDIMENTO MÉDIO DOS DISTRITOS ###############
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
diretorio_grupo <- caminho_data("07_renda")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("07_renda", "ranking_renda_todas_fontes.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da extensão
extensao <- read_parquet(file.path(diretorio_grupo, "extensao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo os anos de estudo médios do município
renda_municipio <- extensao |>
  filter(nivel == "municipio", indicador == "renda_todas_fontes_media") |>
  pull(valor)

# Ordenando os distritos e dividindo-os em dois painéis de 48
ranking <- extensao |>
  filter(nivel == "distrito", indicador == "renda_todas_fontes_media") |>
  mutate(
    # Posição do distrito, da maior à menor renda média
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
    xintercept = renda_municipio,
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
    labels = rotulo_numero(prefix = "R$ "),
    limits = c(0, 13500),
    breaks = seq(0, 12000, by = 3000),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Rendimento médio de todas as fontes nos 96 distritos",
    subtitle = paste0(
      "R$ por mês, por pessoa de 10 anos ou mais, 2022. Linha tracejada: município (R$ ",
      formatar_numero(renda_municipio), ")"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "P1110, valores nominais de 2022; média de todas as pessoas de 10 anos ou mais, com ou sem rendimento (quem não tem rendimento entra com zero), sem padronização por idade.",
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
