################################################################################
##################################            ##################################
############### 07.05) FAIXAS DE SALÁRIO MÍNIMO NOS 96 DISTRITOS ###############
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
arquivo_saida <- caminho_resultados("07_renda", "faixas_renda_distritos.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da renda
renda <- read_parquet(file.path(diretorio_grupo, "renda.parquet"))

# Lendo a composição por classe de salário mínimo
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a renda média do município
renda_municipio <- renda |>
  filter(nivel == "municipio", indicador == "renda_media") |>
  pull(valor)

# Preparando os distritos com a posição pela renda média
ordem_distritos <- renda |>
  filter(nivel == "distrito", indicador == "renda_media") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Reunindo as dez classes em cinco faixas de salário mínimo per capita
faixas_rotulos <- composicao |>
  distinct(faixa_codigo, rotulo_faixa) |>
  arrange(faixa_codigo) |>
  pull(rotulo_faixa)
faixas_dados <- composicao |>
  filter(nivel_territorial == "distrito") |>
  group_by(codigo_distrito, rotulo_faixa) |>
  summarise(
    pct = sum(pct),
    .groups = "drop"
  ) |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # A faixa mais baixa fica à esquerda e a mais alta, à direita
    faixa  = factor(rotulo_faixa, levels = rev(faixas_rotulos)),
    rotulo = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Definindo as cores: escala sequencial do mais baixo (claro) ao mais alto (escuro)
cores_faixas <- setNames(paleta_sequencial(6)[2:6], faixas_rotulos)

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(faixas_dados, aes(x = pct, y = rotulo, fill = faixa)) +
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
    values = cores_faixas,
    breaks = faixas_rotulos,
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Domicílios por faixa de salário mínimo per capita nos 96 distritos",
    subtitle = paste0(
      "Renda domiciliar per capita, salário mínimo de 2022 (R$ 1.212), 2022\n",
      "Distritos ordenados pela renda média, da maior à menor (município: R$ ",
      formatar_numero(renda_municipio), " por mês, por pessoa)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "Proporção sobre os domicílios particulares ocupados; renda zero entra na primeira faixa. A ordem usa a renda média, que pesa pessoas.",
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
