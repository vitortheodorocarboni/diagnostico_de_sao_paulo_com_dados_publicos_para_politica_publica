################################################################################
##################################            ##################################
################## 06.05) NÍVEL DE INSTRUÇÃO NOS 96 DISTRITOS ##################
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
arquivo_saida <- caminho_resultados("06_escolaridade", "instrucao_distritos.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da escolaridade
escolaridade <- read_parquet(file.path(diretorio_grupo, "escolaridade.parquet"))

# Lendo a composição por nível de instrução, sem o não determinado (zerado na
# capital)
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet")) |>
  filter(nivel != "nao_determinado")


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção com superior completo do município
superior_municipio <- escolaridade |>
  filter(nivel == "municipio", indicador == "pct_superior") |>
  pull(valor)

# Preparando os distritos com a posição pela proporção com superior completo
ordem_distritos <- escolaridade |>
  filter(nivel == "distrito", indicador == "pct_superior") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções dos quatro níveis
niveis_rotulos <- composicao |>
  distinct(nivel_codigo, rotulo_nivel) |>
  arrange(nivel_codigo) |>
  pull(rotulo_nivel)
composicao_dados <- composicao |>
  filter(nivel_territorial == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # O superior completo fica à direita, perto do fim da barra
    nivel_instrucao = factor(rotulo_nivel, levels = rev(niveis_rotulos)),
    rotulo          = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(composicao_dados, aes(x = pct, y = rotulo, fill = nivel_instrucao)) +
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
    values = setNames(paleta_categorica(4), rev(niveis_rotulos)),
    breaks = niveis_rotulos,
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    title    = "Nível de instrução nos 96 distritos",
    subtitle = paste0(
      "Pessoas de 25 anos ou mais, 2022\n",
      "Distritos ordenados pela proporção com superior completo, da maior à menor (município: ",
      formatar_numero(superior_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "Nível de instrução compatível com o Censo 2010 (P0770); a categoria não determinado é nula na capital.",
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
