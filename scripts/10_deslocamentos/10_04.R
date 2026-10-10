################################################################################
##################################            ##################################
################### 10.04) TIPOS DE VIAGEM NOS 96 DISTRITOS ####################
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
diretorio_grupo <- caminho_data("10_deslocamentos")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("10_deslocamentos", "tipos_viagem_distritos.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os tipos de viagem das viagens com origem em cada distrito
tipos <- read_parquet(file.path(diretorio_grupo, "tipos.parquet")) |>
  filter(bloco == "origem")


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de viagens coletivas da capital
coletivo_municipio <- tipos |>
  filter(nivel_territorial == "municipio", tipo == "coletivo") |>
  pull(pct)

# Preparando os distritos com a posição pela proporção de viagens coletivas
ordem_distritos <- tipos |>
  filter(nivel_territorial == "distrito", tipo == "coletivo") |>
  mutate(
    posicao = min_rank(desc(pct)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções dos quatro tipos de viagem
tipos_rotulos <- tipos |>
  distinct(tipo_codigo, rotulo_tipo) |>
  arrange(tipo_codigo) |>
  pull(rotulo_tipo)
tipos_dados <- tipos |>
  filter(nivel_territorial == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # O coletivo fica à esquerda
    tipo_viagem = factor(rotulo_tipo, levels = rev(tipos_rotulos)),
    rotulo      = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(tipos_dados, aes(x = pct, y = rotulo, fill = tipo_viagem)) +
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
    values = setNames(paleta_categorica(4), tipos_rotulos),
    breaks = tipos_rotulos,
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Tipo das viagens com origem nos 96 distritos",
    subtitle = paste0(
      "Viagens de um dia útil, de qualquer motivo, 2023\n",
      "Distritos ordenados pela proporção de viagens coletivas, da maior à menor (município: ",
      formatar_numero(coletivo_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "od",
      notas  = c(
        "Tipo de viagem da OD: coletivo, individual (motorizado), a pé e bicicleta, sobre o peso observado das viagens com origem no distrito.",
        paste0("Asterisco (*): distrito com menos de ", minimo_registros_od, " registros de viagens com origem nele; ler com cautela."),
        "incerteza_amostra"
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
