################################################################################
##################################            ##################################
################# 15.06) TRANSPORTE COLETIVO NO CENSO E NA OD ##################
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
diretorio_grupo <- caminho_data("15_trabalho_moradores")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("15_trabalho_moradores", "coletivo_censo_od.png")

# Definindo quantos distritos rotular entre os mais distantes da diagonal
n_rotulos <- 10


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores do trabalho dos moradores
trabalho <- read_parquet(file.path(diretorio_grupo, "trabalho.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo os valores do município no Censo e na OD
municipio <- trabalho |>
  filter(nivel == "municipio", indicador %in% c("pct_coletivo", "od_pct_coletivo")) |>
  select(indicador, valor) |>
  deframe()

# Juntando o Censo e a OD dos distritos com valor na OD
dispersao <- trabalho |>
  filter(nivel == "distrito", indicador %in% c("pct_coletivo", "od_pct_coletivo"), !od_sem_valor) |>
  select(codigo_distrito, nome_rotulo, amostra_menor, od_amostra_menor, indicador, valor) |>
  pivot_wider(names_from = indicador, values_from = valor) |>
  mutate(
    # Diferença entre a OD e o Censo, em pontos percentuais
    diferenca = od_pct_coletivo - pct_coletivo,
    # Rotulando os distritos mais distantes da diagonal
    rotulo    = if_else(
      min_rank(desc(abs(diferenca))) <= n_rotulos,
      paste0(nome_rotulo, if_else(amostra_menor, " *", ""), if_else(od_amostra_menor, " †", "")),
      ""
    )
  )

# Calculando a correlação entre as duas fontes
correlacao <- cor(dispersao$pct_coletivo, dispersao$od_pct_coletivo)

# Desenhando a dispersão, com a diagonal de igualdade
grafico <- ggplot(dispersao, aes(x = pct_coletivo, y = od_pct_coletivo)) +
  geom_abline(
    slope     = 1,
    intercept = 0,
    colour    = cor_texto_apoio,
    linetype  = "dashed",
    linewidth = 0.3
  ) +
  geom_point(
    mapping = aes(shape = od_amostra_menor),
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
    labels = c(`FALSE` = "100 registros na OD ou mais", `TRUE` = "Menos de 100 registros na OD (†)"),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(0, 80)
  ) +
  scale_y_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(0, 80)
  ) +
  coord_equal() +
  labs(
    title    = "Transporte coletivo até o trabalho no Censo e na OD",
    subtitle = paste0(
      "Distritos de residência; correlação entre os distritos: ", formatar_numero(correlacao, 2), "\n",
      "Município: ", formatar_numero(municipio[["pct_coletivo"]], 1), "% no Censo 2022 e ",
      formatar_numero(municipio[["od_pct_coletivo"]], 1), "% na OD 2023\n",
      "Linha tracejada: valores iguais nas duas fontes"
    ),
    x        = "Censo: % de quem se desloca ao trabalho em transporte coletivo",
    y        = "OD: % das viagens casa → trabalho coletivas",
    caption  = legenda_fonte(
      fontes = c("censo_amostra", "od"),
      notas  = c(
        "O Censo informa o meio de transporte habitual de quem trabalha fora de casa e se desloca (por pessoa, semana de referência); a OD, o tipo das viagens casa → trabalho de um dia útil (por viagem, pela zona de residência). Coletivo no Censo: van, ônibus, BRT, trem ou metrô, caminhão adaptado e embarcações.",
        paste0("Sem Marsilac, com menos de ", piso_registros_od, " registros na OD. Rótulos nos ", n_rotulos, " distritos mais distantes da diagonal; asterisco (*): menos de ", formatar_numero(minimo_registros_amostra), " registros de pessoas na amostra do Censo."),
        "se_republica", "incerteza_amostra", "acesso_controlado"
      ),
      largura = 120
    )
  ) +
  tema_insper() +
  theme(
    # Título, subtítulo e rodapé alinhados à figura, não ao painel quadrado
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
  altura  = 18
)
