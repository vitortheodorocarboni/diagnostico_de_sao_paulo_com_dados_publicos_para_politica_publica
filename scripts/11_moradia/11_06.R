################################################################################
##################################            ##################################
################# 11.06) CONDIÇÕES MATERIAIS NOS 96 DISTRITOS ##################
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
arquivo_saida <- caminho_resultados("11_moradia", "condicoes_materiais_distritos.png")

# Definindo as cinco condições materiais, na ordem das colunas
colunas <- c(
  pct_sem_internet             = "Sem\ninternet",
  pct_sem_maquina              = "Sem máquina\nde lavar",
  moradores_dormitorio         = "Moradores por\ndormitório",
  pct_paredes_sem_revestimento = "Sem alvenaria\nrevestida",
  pct_3_mais_banheiros         = "3 banheiros\nou mais"
)

# Definindo o limite da razão ao município, em log2 (1/4 a 4 vezes)
limite_razao <- 2


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da moradia
moradia <- read_parquet(file.path(diretorio_grupo, "moradia.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de domicílios com 3 banheiros ou mais do município
banheiros_municipio <- moradia |>
  filter(nivel == "municipio", indicador == "pct_3_mais_banheiros") |>
  pull(valor)

# Obtendo o valor do município em cada condição
municipio <- moradia |>
  filter(nivel == "municipio", indicador %in% names(colunas)) |>
  select(indicador, valor_municipio = valor)

# Ordenando os distritos pela proporção com 3 banheiros ou mais, da maior à
# menor
ordem_distritos <- moradia |>
  filter(nivel == "distrito", indicador == "pct_3_mais_banheiros") |>
  arrange(desc(valor)) |>
  mutate(
    rotulo = paste0(nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  pull(rotulo)

# Calculando a razão de cada distrito ao município em cada condição
mapa_calor <- moradia |>
  filter(nivel == "distrito", indicador %in% names(colunas)) |>
  left_join(municipio, by = "indicador") |>
  mutate(
    rotulo_distrito = paste0(nome_rotulo, if_else(amostra_menor, " *", "")),
    # Razão em log2, limitada a 1/4 e 4 vezes
    razao_log2      = pmin(pmax(log2(valor / valor_municipio), -limite_razao), limite_razao),
    # Valor escrito na célula: média com duas casas e proporções com uma
    texto           = if_else(
      indicador == "moradores_dormitorio",
      formatar_numero(valor, 2),
      formatar_numero(valor, 1)
    ),
    # Texto branco nas células escuras (razão abaixo de 0,4 ou acima de 2,5)
    cor_valor       = if_else(abs(razao_log2) > 1.3, cor_fundo, cor_texto),
    posicao         = match(rotulo_distrito, ordem_distritos),
    painel          = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    rotulo_distrito = factor(rotulo_distrito, levels = rev(ordem_distritos)),
    coluna          = factor(colunas[indicador], levels = colunas)
  )

# Descrevendo os valores do município para o subtítulo
texto_municipio <- municipio |>
  mutate(
    texto = if_else(
      indicador == "moradores_dormitorio",
      paste0(formatar_numero(valor_municipio, 2), " moradores por dormitório"),
      paste0(formatar_numero(valor_municipio, 1), "% ", str_to_lower(str_replace_all(colunas[indicador], "\n", " ")))
    )
  ) |>
  arrange(match(indicador, names(colunas))) |>
  pull(texto) |>
  paste(collapse = "; ")

# Desenhando o mapa de calor em dois painéis, com escala centrada em 1
grafico <- ggplot(mapa_calor, aes(x = coluna, y = rotulo_distrito, fill = razao_log2)) +
  geom_tile(
    colour    = cor_fundo,
    linewidth = 0.25
  ) +
  geom_text(
    mapping = aes(label = texto, colour = cor_valor),
    family  = familia_fonte,
    size    = 1.5
  ) +
  scale_colour_identity() +
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
    name   = "Valor do distrito em relação ao município",
    guide  = guide_colourbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Condições materiais dos domicílios nos 96 distritos",
    subtitle = paste0(
      "% dos domicílios particulares permanentes ocupados e média de moradores por dormitório, 2022 (valor em cada célula)\n",
      "Distritos ordenados pela proporção com 3 banheiros ou mais, da maior à menor (município: ",
      formatar_numero(banheiros_municipio, 1), "%)\n",
      "Cor: razão ao município na mesma coluna. Vermelho: acima; verde: abaixo (não indica melhor ou pior)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = c("censo_amostra", "censo_universo"),
      notas  = c(
        paste0("Município: ", texto_municipio, "."),
        "Banheiros (de uso exclusivo, com chuveiro e vaso sanitário) vêm do universo; as demais colunas, da amostra. Sem alvenaria revestida: alvenaria ou taipa sem revestimento, madeira, outro material ou sem parede. Razões limitadas a 1/4 e 4 vezes.",
        "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 5.5, colour = cor_texto),
    axis.text.x        = element_text(size = 5.5, colour = cor_texto, lineheight = 0.9),
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
