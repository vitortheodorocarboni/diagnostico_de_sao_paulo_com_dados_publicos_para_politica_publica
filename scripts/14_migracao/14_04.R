################################################################################
##################################            ##################################
################## 14.04) RANKINGS DO NORDESTE E DO EXTERIOR ###################
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
diretorio_grupo <- caminho_data("14_migracao")

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("14_migracao")

# Definindo os rankings: indicador, título, limite e passo do eixo e nota
rankings <- tribble(
  ~indicador,     ~titulo,                                             ~limite, ~passo, ~nota,
  "pct_nordeste", "Moradores nascidos no Nordeste nos 96 distritos",   35,      5,      "Região pelo primeiro dígito do código de UF do IBGE (P0490).",
  "pct_exterior", "Moradores nascidos no exterior nos 96 distritos",   21,      3,      "Inclui naturalizados e brasileiros nascidos no exterior."
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da migração
migracao <- read_parquet(file.path(diretorio_grupo, "migracao.parquet"))


################################################################################
##### IV. Gráficos
################################################################################

# Gravando um ranking por indicador
for (i in seq_len(nrow(rankings))) {

  # Selecionando a linha do ranking
  ranking_atual <- rankings[i, ]

  # Obtendo o valor do município
  valor_municipio <- migracao |>
    filter(nivel == "municipio", indicador == ranking_atual$indicador) |>
    pull(valor)

  # Ordenando os distritos e dividindo-os em dois painéis de 48
  ranking <- migracao |>
    filter(nivel == "distrito", indicador == ranking_atual$indicador) |>
    mutate(
      # Posição do distrito, do maior ao menor valor
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
      xintercept = valor_municipio,
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
      limits = c(0, ranking_atual$limite),
      breaks = seq(0, ranking_atual$limite, by = ranking_atual$passo),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title    = ranking_atual$titulo,
      subtitle = paste0(
        "% dos moradores, 2022. Linha tracejada: município (",
        formatar_numero(valor_municipio, 1), "%)"
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(ranking_atual$nota, "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado")
      )
    ) +
    tema_insper() +
    theme(
      # Nomes dos distritos menores, para caberem os 48 de cada painel
      axis.text.y        = element_text(size = 5.5, colour = cor_texto),
      panel.grid.major.y = element_blank(),
      panel.spacing.x    = unit(1.2, "cm")
    )

  # Gravando o gráfico
  gravar_figura(
    grafico = grafico,
    arquivo = file.path(diretorio_saida, paste0("ranking_", ranking_atual$indicador, ".png")),
    tamanho = "grafico",
    altura  = 15
  )
}
