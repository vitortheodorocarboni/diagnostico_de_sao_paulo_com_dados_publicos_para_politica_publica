################################################################################
##################################            ##################################
############# 12.05) ESPÉCIE DE UNIDADE DOMÉSTICA NOS 96 DISTRITOS #############
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
arquivo_saida <- caminho_resultados("12_arranjos_familiares", "especie_unidade_domestica_distritos.png")

# Definindo as cores das espécies, na ordem da barra e da legenda (unipessoal
# primeiro, porque ordena os distritos)
cores_especies <- c(
  "Unipessoal" = paleta_insper[["vermelho"]],
  "Nuclear"    = paleta_insper[["turquesa"]],
  "Estendida"  = paleta_insper[["roxo"]],
  "Composta"   = paleta_insper[["amarelo"]]
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos arranjos familiares
arranjos <- read_parquet(file.path(diretorio_grupo, "arranjos.parquet"))

# Lendo as composições
composicoes <- read_parquet(file.path(diretorio_grupo, "composicoes.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de unipessoais do município
unipessoal_municipio <- arranjos |>
  filter(nivel == "municipio", indicador == "pct_unipessoal") |>
  pull(valor)

# Preparando os distritos com a posição pela proporção de unipessoais
ordem_distritos <- arranjos |>
  filter(nivel == "distrito", indicador == "pct_unipessoal") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo)
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções das quatro espécies
especie_dados <- composicoes |>
  filter(tabela == "especie", nivel_territorial == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # A primeira espécie fica à esquerda (níveis invertidos no empilhamento)
    especie = factor(rotulo_categoria, levels = rev(names(cores_especies))),
    rotulo  = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(especie_dados, aes(x = pct, y = rotulo, fill = especie)) +
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
    values = cores_especies,
    breaks = names(cores_especies),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Espécie de unidade doméstica nos 96 distritos",
    subtitle = paste0(
      "Proporção dos domicílios particulares ocupados, 2022\n",
      "Distritos ordenados pela proporção de unipessoais, da maior à menor (município: ",
      formatar_numero(unipessoal_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Unipessoal: uma só pessoa. Nuclear: responsável com cônjuge e/ou filhos. Estendida: com outros parentes. Composta: com pessoas sem parentesco com o responsável.",
        "sigilo_universo"
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
