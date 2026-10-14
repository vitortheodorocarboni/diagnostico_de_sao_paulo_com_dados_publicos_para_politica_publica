################################################################################
##################################            ##################################
################## 13.04) RANKING DA MORTALIDADE PADRONIZADA ###################
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
diretorio_grupo <- caminho_data("13_obitos")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("13_obitos", "ranking_rmp.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos óbitos
obitos <- read_parquet(file.path(diretorio_grupo, "obitos.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Listando os distritos sem valor, para a nota
sem_valor <- obitos |>
  filter(nivel == "distrito", indicador == "rmp", is.na(valor)) |>
  pull(nome_rotulo)

# Ordenando os distritos com valor e dividindo-os em dois painéis
ranking <- obitos |>
  filter(nivel == "distrito", indicador == "rmp", !is.na(valor)) |>
  mutate(
    # Posição do distrito, da maior à menor mortalidade padronizada
    posicao = min_rank(desc(valor)),
    # Rótulo com a posição e asterisco nos distritos com poucos registros
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", "")),
    painel  = if_else(posicao <= ceiling(n() / 2), "Primeira metade", "Segunda metade")
  ) |>
  mutate(
    rotulo = fct_reorder(rotulo, valor)
  )

# Desenhando o gráfico de pontos em dois painéis, com a linha do município (1)
grafico <- ggplot(ranking, aes(x = valor, y = rotulo)) +
  geom_segment(
    mapping   = aes(x = 1, xend = valor, yend = rotulo),
    colour    = cor_grade,
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 1,
    colour     = cor_texto_apoio,
    linetype   = "dashed",
    linewidth  = 0.3
  ) +
  geom_point(
    mapping = aes(colour = valor > 1),
    size    = 1
  ) +
  scale_colour_manual(
    values = c(`TRUE` = paleta_insper[["vermelho"]], `FALSE` = paleta_insper[["turquesa"]]),
    guide  = "none"
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    labels = rotulo_numero(accuracy = 0.1),
    limits = c(0.4, 1.6),
    breaks = seq(0.4, 1.6, by = 0.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Mortalidade padronizada pela idade nos distritos",
    subtitle = paste0(
      "Óbitos de moradores de 2019 a julho de 2022 por óbito esperado pela idade dos moradores. ",
      "Linha tracejada: município (1,0)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Razão de mortalidade padronizada (padronização indireta, 17 faixas de idade, taxas do município). Sem valor (menos de ",
          piso_registros_obitos, " registros de falecidos): ", paste(sem_valor, collapse = ", "), "."
        ),
        "captacao_obitos", "amostra_menor_obitos", "se_republica", "incerteza_amostra", "acesso_controlado"
      )
    )
  ) +
  tema_insper() +
  theme(
    # Nomes dos distritos menores, para caberem os de cada painel
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
  altura  = 15.5
)
