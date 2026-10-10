################################################################################
##################################            ##################################
############### 09.04) PERFIL DE QUEM TRABALHA NOS 96 DISTRITOS ################
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
diretorio_grupo <- caminho_data("09_trabalhadores")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("09_trabalhadores", "perfil_trabalhadores_distritos.png")

# Definindo as colunas do mapa de calor
colunas <- c(
  pct_mulheres          = "Mulheres",
  pct_negra             = "Pretos e\npardos",
  pct_superior          = "Superior\ncompleto",
  pct_renda_ate_1000    = "Renda per\ncapita até\nR$ 1.000",
  pct_mora_distrito     = "Moram no\npróprio\ndistrito",
  pct_mora_fora_capital = "Moram fora\nda capital"
)

# Definindo o limite da escala de diferenças, em pontos percentuais
limite_diferenca <- 30


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos trabalhadores
trabalhadores <- read_parquet(file.path(diretorio_grupo, "trabalhadores.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo os trabalhadores por morador do município
razao_municipio <- trabalhadores |>
  filter(nivel == "municipio", indicador == "trabalhadores_por_morador") |>
  pull(valor)

# Obtendo o valor do município em cada coluna
municipio <- trabalhadores |>
  filter(nivel == "municipio", indicador %in% names(colunas)) |>
  select(indicador, valor_municipio = valor)

# Ordenando os distritos pelos trabalhadores por morador, do maior ao menor; o
# distrito sem valor fica no fim
ordem_distritos <- trabalhadores |>
  filter(nivel == "distrito", indicador == "trabalhadores_por_morador") |>
  arrange(desc(valor)) |>
  mutate(
    rotulo = paste0(nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  pull(rotulo)

# Calculando a diferença de cada distrito para o município em cada coluna
mapa_calor <- trabalhadores |>
  filter(nivel == "distrito", indicador %in% names(colunas)) |>
  left_join(municipio, by = "indicador") |>
  mutate(
    rotulo_distrito = paste0(nome_rotulo, if_else(amostra_menor, " *", "")),
    diferenca       = pmin(pmax(valor - valor_municipio, -limite_diferenca), limite_diferenca),
    rotulo_valor    = if_else(is.na(valor), "s/v", formatar_numero(valor, 0)),
    posicao         = match(rotulo_distrito, ordem_distritos),
    painel          = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    rotulo_distrito = factor(rotulo_distrito, levels = rev(ordem_distritos)),
    coluna          = factor(colunas[indicador], levels = colunas)
  )

# Desenhando o mapa de calor em dois painéis, com escala centrada em zero
grafico <- ggplot(mapa_calor, aes(x = coluna, y = rotulo_distrito, fill = diferenca)) +
  geom_tile(
    colour    = cor_fundo,
    linewidth = 0.25
  ) +
  geom_text(
    mapping = aes(label = rotulo_valor),
    family  = familia_fonte,
    size    = 1.6,
    colour  = cor_texto
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  escala_fill_divergente(
    meio   = 0,
    limits = c(-limite_diferenca, limite_diferenca),
    labels = rotulo_numero(),
    name   = "Diferença para os trabalhadores da capital (pontos percentuais)",
    guide  = guide_colourbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Perfil de quem trabalha nos 96 distritos",
    subtitle = paste0(
      "Proporção entre os trabalhadores com local fixo no distrito, 2023 (valor em cada célula, em %)\n",
      "Distritos ordenados pelos trabalhadores por morador, do maior ao menor (município: ",
      formatar_numero(razao_municipio, 2), ")\n",
      "Cor: diferença para os trabalhadores de toda a capital. Vermelho: acima; verde: abaixo"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "od",
      notas  = c(
        paste0(
          "Diferenças limitadas a ", limite_diferenca, " pontos; s/v: sem valor. Na capital, a coluna dos que moram ",
          "no próprio distrito soma os trabalhadores que moram no distrito em que trabalham."
        ),
        "amostra_menor_od", "od_destino", "incerteza_amostra"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 5.5, colour = cor_texto),
    axis.text.x        = element_text(size = 6.5, colour = cor_texto, lineheight = 0.9),
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
