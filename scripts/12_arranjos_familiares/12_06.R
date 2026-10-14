################################################################################
##################################            ##################################
############## 12.06) COMPOSIÇÃO DOS DOMICÍLIOS NOS 96 DISTRITOS ###############
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
arquivo_saida <- caminho_resultados("12_arranjos_familiares", "composicao_domicilios_distritos.png")

# Definindo as cores das composições, na ordem da barra e da legenda
# (responsável sem cônjuge com filhos primeiro, porque ordena os distritos)
cores_composicoes <- c(
  "Responsável sem cônjuge com filhos" = paleta_insper[["vermelho"]],
  "Casal com filhos de ambos"          = paleta_insper[["turquesa"]],
  "Casal com filho de só um"           = paleta_insper[["roxo"]],
  "Casal sem filhos"                   = paleta_insper[["amarelo"]],
  "Outros"                             = cor_neutra
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

# Obtendo a proporção de responsáveis sem cônjuge com filhos do município
sem_conjuge_municipio <- arranjos |>
  filter(nivel == "municipio", indicador == "pct_sem_conjuge_com_filhos") |>
  pull(valor)

# Preparando os distritos com a posição pela proporção de responsáveis sem
# cônjuge com filhos
ordem_distritos <- arranjos |>
  filter(nivel == "distrito", indicador == "pct_sem_conjuge_com_filhos") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo)
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções das cinco composições
composicao_dados <- composicoes |>
  filter(tabela == "composicao", nivel_territorial == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # A primeira composição fica à esquerda (níveis invertidos no empilhamento)
    composicao = factor(rotulo_categoria, levels = rev(names(cores_composicoes))),
    rotulo     = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(composicao_dados, aes(x = pct, y = rotulo, fill = composicao)) +
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
    values = cores_composicoes,
    breaks = names(cores_composicoes),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Composição dos domicílios nos 96 distritos",
    subtitle = paste0(
      "Proporção dos domicílios particulares ocupados, 2022\n",
      "Distritos ordenados pela proporção de responsáveis sem cônjuge com filhos, da maior à menor (município: ",
      formatar_numero(sem_conjuge_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Filhos inclui enteados e filhos de qualquer idade. Casal com filho de só um: ao menos um filho só do responsável ou só do cônjuge. Outros inclui quem mora sozinho e os domicílios sem casal nem filhos.",
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
