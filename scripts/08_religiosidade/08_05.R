################################################################################
##################################            ##################################
################# 08.05) RELIGIÃO EM DETALHE NOS 96 DISTRITOS ##################
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
diretorio_grupo <- caminho_data("08_religiosidade")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("08_religiosidade", "religiao_detalhe_distritos.png")

# Definindo as colunas do mapa de calor: as categorias que passam de 0,5% no
# município, em ordem de grupo
colunas <- c(
  "1"  = "Católica\napostólica\nromana",
  "5"  = "Evangélica\npentecostal",
  "4"  = "Evangélica\nde missão",
  "7"  = "Evangélica\nnão\ndeterminada",
  "12" = "Espírita",
  "13" = "Umbanda",
  "14" = "Candomblé",
  "27" = "Sem\nreligião",
  "28" = "Ateu",
  "29" = "Agnóstico",
  "8"  = "Outras\ncristãs",
  "10" = "Testemunhas\nde Jeová",
  "18" = "Budismo"
)

# Definindo o limite da escala: razão de 1/4 a 4 vezes o município (em log2)
limite_razao <- 2


################################################################################
##### III. Dados
################################################################################

# Lendo as categorias por distrito e município
categorias <- read_parquet(file.path(diretorio_grupo, "categorias.parquet")) |>
  filter(codigo_categoria %in% names(colunas))

# Lendo os indicadores da religiosidade
religiao <- read_parquet(file.path(diretorio_grupo, "religiao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção católica do município
catolica_municipio <- religiao |>
  filter(nivel == "municipio", indicador == "pct_catolica") |>
  pull(valor)

# Obtendo o valor do município em cada categoria
municipio <- categorias |>
  filter(nivel_territorial == "municipio") |>
  select(codigo_categoria, pct_municipio = pct)

# Ordenando os distritos pela proporção católica, da maior à menor
ordem_distritos <- religiao |>
  filter(nivel == "distrito", indicador == "pct_catolica") |>
  arrange(desc(valor)) |>
  mutate(
    rotulo = paste0(nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  pull(rotulo)

# Calculando a razão de cada distrito ao município em cada categoria; a razão
# zero (categoria sem registros) fica no limite inferior da escala
mapa_calor <- categorias |>
  filter(nivel_territorial == "distrito") |>
  left_join(municipio, by = "codigo_categoria") |>
  mutate(
    rotulo_distrito = paste0(nome_rotulo, if_else(amostra_menor, " *", "")),
    razao_log2      = pmin(pmax(log2(pct / pct_municipio), -limite_razao), limite_razao),
    posicao         = match(rotulo_distrito, ordem_distritos),
    painel          = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    rotulo_distrito = factor(rotulo_distrito, levels = rev(ordem_distritos)),
    coluna          = factor(colunas[codigo_categoria], levels = colunas)
  )

# Desenhando o mapa de calor em dois painéis, com escala centrada em 1
grafico <- ggplot(mapa_calor, aes(x = coluna, y = rotulo_distrito, fill = razao_log2)) +
  geom_tile(
    colour    = cor_fundo,
    linewidth = 0.25
  ) +
  geom_text(
    mapping = aes(label = formatar_numero(pct, 1)),
    family  = familia_fonte,
    size    = 1.5,
    colour  = cor_texto
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  escala_fill_divergente(
    meio   = 0,
    limits = c(-limite_razao, limite_razao),
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("1/4", "1/2", "igual", "2 vezes", "4 vezes"),
    name   = "Proporção do distrito em relação ao município",
    guide  = guide_colourbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Religião em detalhe nos 96 distritos",
    subtitle = paste0(
      "Proporção das pessoas de 10 anos ou mais em cada religião, 2022 (valor em cada célula, em %)\n",
      "Distritos ordenados pela proporção católica, da maior à menor (município: ",
      formatar_numero(catolica_municipio, 1), "%)\n",
      "Cor: razão ao município na mesma religião. Vermelho: acima; verde: abaixo"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "Treze categorias com mais de 0,5% das pessoas no município; razões limitadas a 1/4 e 4 vezes. Categorias raras são pouco confiáveis em distritos pequenos.",
        "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 5.5, colour = cor_texto),
    axis.text.x        = element_text(size = 6, colour = cor_texto, angle = 90, hjust = 0, vjust = 0.5),
    legend.position    = "bottom",
    panel.grid.major   = element_blank(),
    panel.spacing.x    = unit(1, "cm"),
    strip.text         = element_blank(),
    legend.title       = element_text(size = 7.5)
  )


################################################################################
##### V. Exportação
################################################################################

# Gravando o gráfico
gravar_figura(
  grafico = grafico,
  arquivo = arquivo_saida,
  tamanho = "grafico",
  altura  = 17
)
