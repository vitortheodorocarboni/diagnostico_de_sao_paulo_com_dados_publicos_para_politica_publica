################################################################################
##################################            ##################################
######### 07.12) RENDIMENTO DO TRABALHO E OUTRAS FONTES NOS DISTRITOS ##########
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
diretorio_grupo <- caminho_data("07_renda")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("07_renda", "renda_trabalho_outras_fontes.png")

# Definindo quantos distritos rotular em cada ponta da renda do trabalho
n_rotulos <- 6


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da extensão
extensao <- read_parquet(file.path(diretorio_grupo, "extensao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo os valores do município
municipio <- extensao |>
  filter(nivel == "municipio", indicador %in% c("pct_outras_fontes", "renda_trabalho_media")) |>
  select(indicador, valor) |>
  deframe()

# Juntando o rendimento do trabalho e a proporção com outras fontes de cada distrito
dispersao <- extensao |>
  filter(nivel == "distrito", indicador %in% c("pct_outras_fontes", "renda_trabalho_media")) |>
  select(codigo_distrito, nome_rotulo, amostra_menor, indicador, valor) |>
  pivot_wider(names_from = indicador, values_from = valor) |>
  mutate(
    # Rotulando os distritos de maior e de menor rendimento do trabalho
    posicao = min_rank(desc(renda_trabalho_media)),
    rotulo  = if_else(
      posicao <= n_rotulos | posicao > n() - n_rotulos,
      paste0(nome_rotulo, if_else(amostra_menor, " *", "")),
      ""
    )
  )

# Calculando a correlação entre o rendimento do trabalho e a proporção com outras fontes
correlacao <- cor(dispersao$renda_trabalho_media, dispersao$pct_outras_fontes)

# Desenhando a dispersão, com as linhas do município e a reta de tendência
grafico <- ggplot(dispersao, aes(x = renda_trabalho_media, y = pct_outras_fontes)) +
  geom_vline(
    xintercept = municipio[["renda_trabalho_media"]],
    colour     = cor_texto_apoio,
    linetype   = "dashed",
    linewidth  = 0.3
  ) +
  geom_hline(
    yintercept = municipio[["pct_outras_fontes"]],
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
    labels = rotulo_numero(prefix = "R$ "),
    limits = c(1000, 18000),
    breaks = seq(2000, 18000, by = 4000)
  ) +
  scale_y_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(14, 32),
    breaks = seq(15, 30, by = 5)
  ) +
  labs(
    title    = "Rendimento do trabalho e outras fontes de renda nos 96 distritos",
    subtitle = paste0(
      "Rendimento médio do trabalho contra % com aposentadoria, pensão, benefício ou aluguel; correlação entre os distritos: ", formatar_numero(correlacao, 2), "\n",
      "Município: R$ ", formatar_numero(municipio[["renda_trabalho_media"]]), " de rendimento médio do trabalho e ",
      formatar_numero(municipio[["pct_outras_fontes"]], 1), "% com outras fontes (linhas tracejadas)\n",
      "Linha cinza: tendência linear entre os distritos"
    ),
    x        = "Rendimento médio do trabalho (R$ por mês, por ocupado com rendimento)",
    y        = "Com outras fontes (% de 10 anos ou mais)",
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "P1080 e P1090, valores nominais de 2022. A proporção com aposentadoria e pensão depende da estrutura etária do distrito, e o tratado não cruza esses rendimentos com a idade, por isso não há padronização.",
        paste0("Rótulos nos ", n_rotulos, " distritos de maior e de menor rendimento do trabalho."),
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
