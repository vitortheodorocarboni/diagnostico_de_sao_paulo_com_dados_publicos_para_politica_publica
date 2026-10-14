################################################################################
##################################            ##################################
################# 11.05) CONDIÇÃO DE OCUPAÇÃO NOS 96 DISTRITOS #################
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
diretorio_grupo <- caminho_data("11_moradia")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("11_moradia", "condicao_ocupacao_distritos.png")

# Definindo a ordem das condições na barra e na legenda, da esquerda para a
# direita (alugado primeiro, porque ordena os distritos)
ordem_condicoes <- c(
  "Alugado",
  "Próprio quitado",
  "Próprio em pagamento",
  "Cedido",
  "Outra condição"
)

# Definindo as cores: vermelho no alugado e cinza em outra condição
cores_condicoes <- c(
  "Alugado"              = paleta_insper[["vermelho"]],
  "Próprio quitado"      = paleta_insper[["turquesa"]],
  "Próprio em pagamento" = paleta_insper[["roxo"]],
  "Cedido"               = paleta_insper[["amarelo"]],
  "Outra condição"       = cor_neutra
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da moradia
moradia <- read_parquet(file.path(diretorio_grupo, "moradia.parquet"))

# Lendo a composição por condição de ocupação
ocupacao <- read_parquet(file.path(diretorio_grupo, "ocupacao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de alugados do município
alugado_municipio <- moradia |>
  filter(nivel == "municipio", indicador == "pct_alugado") |>
  pull(valor)

# Preparando os distritos com a posição pela proporção de alugados
ordem_distritos <- moradia |>
  filter(nivel == "distrito", indicador == "pct_alugado") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções das cinco condições de ocupação
ocupacao_dados <- ocupacao |>
  filter(nivel_territorial == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # A primeira condição fica à esquerda (níveis invertidos no empilhamento)
    condicao = factor(rotulo_condicao, levels = rev(ordem_condicoes)),
    rotulo   = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(ocupacao_dados, aes(x = pct, y = rotulo, fill = condicao)) +
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
    values = cores_condicoes,
    breaks = ordem_condicoes,
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Condição de ocupação dos domicílios nos 96 distritos",
    subtitle = paste0(
      "Proporção dos domicílios particulares permanentes ocupados, 2022\n",
      "Distritos ordenados pela proporção de alugados, da maior à menor (município: ",
      formatar_numero(alugado_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "Próprio em pagamento: próprio de algum morador, ainda pagando. Cedido: por empregador, por familiar ou de outra forma. O Censo 2022 não pergunta o valor do aluguel.",
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
