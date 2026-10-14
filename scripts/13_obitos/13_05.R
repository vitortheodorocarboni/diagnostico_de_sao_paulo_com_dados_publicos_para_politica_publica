################################################################################
##################################            ##################################
################# 13.05) TAXA BRUTA E MORTALIDADE PADRONIZADA ##################
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
diretorio_grupo <- caminho_data("13_obitos")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("13_obitos", "taxa_bruta_e_rmp.png")

# Definindo quantos distritos rotular em cada extremo de cada eixo
n_rotulos <- 6


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos óbitos
obitos <- read_parquet(file.path(diretorio_grupo, "obitos.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a taxa bruta do município
taxa_municipio <- obitos |>
  filter(nivel == "municipio", indicador == "obitos_mil_ano") |>
  pull(valor)

# Juntando a taxa bruta e a mortalidade padronizada dos distritos com valor
dispersao <- obitos |>
  filter(nivel == "distrito", indicador %in% c("obitos_mil_ano", "rmp"), !sem_valor) |>
  select(codigo_distrito, nome_rotulo, amostra_menor, indicador, valor) |>
  pivot_wider(names_from = indicador, values_from = valor) |>
  mutate(
    # Rotulando os extremos de cada eixo
    rotular = min_rank(rmp) <= n_rotulos | min_rank(desc(rmp)) <= n_rotulos |
      min_rank(obitos_mil_ano) <= n_rotulos | min_rank(desc(obitos_mil_ano)) <= n_rotulos,
    rotulo  = if_else(rotular, paste0(nome_rotulo, if_else(amostra_menor, " *", "")), "")
  )

# Desenhando a dispersão, com as linhas do município nos dois eixos
grafico <- ggplot(dispersao, aes(x = obitos_mil_ano, y = rmp)) +
  geom_hline(
    yintercept = 1,
    colour     = cor_texto_apoio,
    linetype   = "dashed",
    linewidth  = 0.3
  ) +
  geom_vline(
    xintercept = taxa_municipio,
    colour     = cor_texto_apoio,
    linetype   = "dashed",
    linewidth  = 0.3
  ) +
  geom_point(
    mapping = aes(shape = amostra_menor),
    colour  = paleta_insper[["vermelho"]],
    size    = 1.8,
    stroke  = 0.6
  ) +
  geom_text_repel(
    mapping       = aes(label = rotulo),
    family        = familia_fonte,
    size          = 2.4,
    colour        = cor_texto,
    min.segment.length = 0,
    segment.colour = cor_neutra,
    segment.size  = 0.2,
    max.overlaps  = Inf,
    seed          = 2022
  ) +
  scale_shape_manual(
    values = c(`FALSE` = 16, `TRUE` = 1),
    labels = c(`FALSE` = "50 registros de falecidos ou mais", `TRUE` = "Menos de 50 registros (*)"),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(accuracy = 0.1)
  ) +
  scale_y_continuous(
    labels = rotulo_numero(accuracy = 0.1),
    breaks = seq(0.4, 1.6, by = 0.2)
  ) +
  labs(
    title    = "Taxa bruta e mortalidade padronizada pela idade",
    subtitle = paste0(
      "Distritos, óbitos de moradores de 2019 a julho de 2022. Linhas tracejadas: município (",
      formatar_numero(taxa_municipio, 1), " óbitos por mil moradores ao ano; razão 1,0)\n",
      "À direita e embaixo: distritos mais velhos, com muitos óbitos, mas menos do que a idade dos moradores faria esperar"
    ),
    x        = "Taxa bruta (óbitos por mil moradores ao ano)",
    y        = "Razão de mortalidade padronizada\n(óbitos por óbito esperado pela idade)",
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Sem os ", sum(obitos$sem_valor[obitos$nivel == "distrito" & obitos$indicador == "rmp"]),
          " distritos com menos de ", piso_registros_obitos, " registros de falecidos. Rótulos nos ",
          n_rotulos, " maiores e menores valores de cada eixo."
        ),
        "captacao_obitos", "se_republica", "incerteza_amostra", "acesso_controlado"
      )
    )
  ) +
  tema_insper() +
  theme(
    legend.position = "top"
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
