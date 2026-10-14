################################################################################
##################################            ##################################
############## 16.06) DEFICIÊNCIA E MORADORES DE 60 ANOS OU MAIS ###############
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("ggrepel")
# install.packages("tidyverse")

# Carregando bibliotecas
library(arrow)
library(ggrepel)
library(tidyverse)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Carregando a identidade visual
source(file.path("scripts", "_config", "identidade_visual.R"), encoding = "UTF-8")

# Definindo o diretório de dados do grupo
diretorio_grupo <- caminho_data("16_deficiencia")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("16_deficiencia", "deficiencia_60_mais.png")

# Definindo quantos distritos rotular em cada ponta da deficiência
n_rotulos <- 6


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da deficiência
deficiencia <- read_parquet(file.path(diretorio_grupo, "deficiencia.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo os valores do município
municipio <- deficiencia |>
  filter(nivel == "municipio", indicador %in% c("pct_deficiencia", "pct_60_mais")) |>
  select(indicador, valor) |>
  deframe()

# Juntando a deficiência e os 60 anos ou mais de cada distrito
dispersao <- deficiencia |>
  filter(nivel == "distrito", indicador %in% c("pct_deficiencia", "pct_60_mais")) |>
  select(codigo_distrito, nome_rotulo, amostra_menor, indicador, valor) |>
  pivot_wider(names_from = indicador, values_from = valor) |>
  mutate(
    # Rotulando os distritos com mais e com menos deficiência e os de mais
    # idosos
    posicao = min_rank(desc(pct_deficiencia)),
    rotulo  = if_else(
      posicao <= n_rotulos | posicao > n() - n_rotulos | min_rank(desc(pct_60_mais)) <= 3,
      paste0(nome_rotulo, if_else(amostra_menor, " *", "")),
      ""
    )
  )

# Calculando a correlação entre a deficiência e os 60 anos ou mais
correlacao <- cor(dispersao$pct_60_mais, dispersao$pct_deficiencia)

# Desenhando a dispersão, com as linhas do município e a reta de tendência
grafico <- ggplot(dispersao, aes(x = pct_60_mais, y = pct_deficiencia)) +
  geom_vline(
    xintercept = municipio[["pct_60_mais"]],
    colour     = cor_texto_apoio,
    linetype   = "dashed",
    linewidth  = 0.3
  ) +
  geom_hline(
    yintercept = municipio[["pct_deficiencia"]],
    colour     = cor_texto_apoio,
    linetype   = "dashed",
    linewidth  = 0.3
  ) +
  geom_smooth(
    method    = "lm",
    formula   = y ~ x,
    se        = FALSE,
    colour    = cor_neutra,
    linewidth = 0.5
  ) +
  geom_point(
    mapping = aes(shape = amostra_menor),
    colour  = paleta_insper[["vermelho"]],
    size    = 1.8,
    stroke  = 0.6
  ) +
  geom_text_repel(
    mapping            = aes(label = rotulo),
    family             = familia_fonte,
    size               = 2.4,
    colour             = cor_texto,
    min.segment.length = 0,
    segment.colour     = cor_neutra,
    segment.size       = 0.2,
    max.overlaps       = Inf,
    seed               = 2022
  ) +
  scale_shape_manual(
    values = c(`FALSE` = 16, `TRUE` = 1),
    labels = c(`FALSE` = "2.000 registros de pessoas ou mais", `TRUE` = "Menos de 2.000 registros (*)"),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(10, 32),
    breaks = seq(10, 30, by = 5)
  ) +
  scale_y_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(2, 10),
    breaks = seq(2, 10, by = 2)
  ) +
  labs(
    title    = "Deficiência e idade nos 96 distritos",
    subtitle = paste0(
      "% com deficiência contra % de moradores de 60 anos ou mais; correlação entre os distritos: ", formatar_numero(correlacao, 2), "\n",
      "Município: ", formatar_numero(municipio[["pct_deficiencia"]], 1), "% com deficiência e ",
      formatar_numero(municipio[["pct_60_mais"]], 1), "% de 60 anos ou mais (linhas tracejadas)\n",
      "Linha cinza: tendência linear entre os distritos"
    ),
    x        = "60 anos ou mais (% dos moradores, universo)",
    y        = "Com deficiência (% de 2 anos ou mais)",
    caption  = legenda_fonte(
      fontes = c("censo_amostra", "censo_universo"),
      notas  = c(
        "A deficiência cresce com a idade, mas nos distritos ela é maior onde há menos idosos: o padrão entre os distritos não se explica pela idade. O tratado não cruza deficiência e idade, e por isso não há padronização; com ela, a distância entre os distritos tenderia a crescer.",
        paste0("Rótulos nos ", n_rotulos, " distritos com mais e com menos deficiência e nos 3 com mais idosos."),
        "se_republica", "incerteza_amostra", "acesso_controlado"
      ),
      largura = 120
    )
  ) +
  tema_insper() +
  theme(
    legend.position       = "top",
    plot.title.position   = "plot",
    plot.caption.position = "plot"
  )


################################################################################
##### V. Exportação
################################################################################

# Gravando o gráfico
gravar_figura(
  grafico = grafico,
  arquivo = arquivo_saida,
  tamanho = "grafico",
  altura  = 15
)
