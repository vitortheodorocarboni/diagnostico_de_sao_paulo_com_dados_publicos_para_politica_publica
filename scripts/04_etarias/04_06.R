################################################################################
##################################            ##################################
########### 04.06) GRUPOS DE IDADE NOS 96 DISTRITOS (MAPA DE CALOR) ############
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
diretorio_grupo <- caminho_data("04_etarias")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("04_etarias", "grupos_de_idade_distritos.png")


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores de idade
idade <- read_parquet(file.path(diretorio_grupo, "idade.parquet"))

# Lendo os 11 grupos de idade
grupos <- read_parquet(file.path(diretorio_grupo, "grupos.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo o índice de envelhecimento do município
envelhecimento_municipio <- idade |>
  filter(nivel == "municipio", indicador == "envelhecimento") |>
  pull(valor)

# Obtendo a proporção do município em cada grupo
municipio <- grupos |>
  filter(nivel == "municipio") |>
  select(grupo, pct_municipio = pct)

# Ordenando os distritos pelo índice de envelhecimento, do maior ao menor
ordem_distritos <- idade |>
  filter(nivel == "distrito", indicador == "envelhecimento") |>
  arrange(desc(valor)) |>
  pull(nome_rotulo)

# Calculando a diferença de cada distrito para o município, em pontos percentuais
mapa_calor <- grupos |>
  filter(nivel == "distrito") |>
  left_join(municipio, by = "grupo") |>
  mutate(
    diferenca    = pct - pct_municipio,
    posicao      = match(nome_rotulo, ordem_distritos),
    painel       = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    nome_rotulo  = factor(nome_rotulo, levels = rev(ordem_distritos)),
    rotulo_grupo = factor(rotulo_grupo, levels = unique(rotulo_grupo[order(grupo)]))
  )

# Desenhando o mapa de calor em dois painéis, com escala centrada em zero
grafico <- ggplot(mapa_calor, aes(x = rotulo_grupo, y = nome_rotulo, fill = diferenca)) +
  geom_tile(
    colour    = cor_fundo,
    linewidth = 0.25
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  escala_fill_divergente(
    meio   = 0,
    labels = rotulo_numero(accuracy = 1),
    name   = "Diferença para o município (pontos percentuais)",
    guide  = guide_colourbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(
    position = "top",
    labels   = function(x) str_replace(x, " ou mais", " ou\nmais")
  ) +
  labs(
    title    = "Grupos de idade nos 96 distritos",
    subtitle = paste0(
      "Proporção dos moradores em cada grupo, diferença em relação ao município, 2022\n",
      "Distritos ordenados pelo índice de envelhecimento, do maior ao menor (município: ",
      formatar_numero(envelhecimento_municipio, 1), ")\n",
      "Vermelho: proporção acima do município; verde: abaixo"
    ),
    x        = "Grupo de idade (anos)",
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Proporção sobre a soma publicada dos 11 grupos de idade do IBGE. Os grupos de 30 a 69 anos têm 10 anos de largura; os demais, 5 anos, e o último é aberto.",
        "sigilo_pessoas"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 5.5, colour = cor_texto),
    axis.text.x        = element_text(size = 6, colour = cor_texto),
    axis.title.x       = element_text(size = 7, hjust = 0.5),
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
