################################################################################
##################################            ##################################
################# 12.07) PERFIL DOS ARRANJOS NOS 96 DISTRITOS ##################
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
arquivo_saida <- caminho_resultados("12_arranjos_familiares", "perfil_arranjos_distritos.png")

# Definindo as seis características do perfil, na ordem das colunas
colunas <- c(
  pct_responsavel_mulher  = "Responsável\nmulher",
  pct_responsavel_60_mais = "Responsável\n60 ou mais",
  pct_filhos_25_mais      = "Filhos do\ncasal 25+",
  pct_netos               = "Netos",
  pct_pais_sogros         = "Pais ou\nsogros",
  pct_conjuges_mesmo_sexo = "Cônjuges do\nmesmo sexo"
)

# Definindo o limite da razão ao município, em log2 (1/4 a 4 vezes)
limite_razao <- 2


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos arranjos familiares
arranjos <- read_parquet(file.path(diretorio_grupo, "arranjos.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de unipessoais do município
unipessoal_municipio <- arranjos |>
  filter(nivel == "municipio", indicador == "pct_unipessoal") |>
  pull(valor)

# Obtendo o valor do município em cada característica
municipio <- arranjos |>
  filter(nivel == "municipio", indicador %in% names(colunas)) |>
  select(indicador, valor_municipio = valor)

# Ordenando os distritos pela proporção de unipessoais, da maior à menor
ordem_distritos <- arranjos |>
  filter(nivel == "distrito", indicador == "pct_unipessoal") |>
  arrange(desc(valor)) |>
  pull(nome_rotulo)

# Calculando a razão de cada distrito ao município em cada característica
mapa_calor <- arranjos |>
  filter(nivel == "distrito", indicador %in% names(colunas)) |>
  left_join(municipio, by = "indicador") |>
  mutate(
    # Razão em log2, limitada a 1/4 e 4 vezes
    razao_log2  = pmin(pmax(log2(valor / valor_municipio), -limite_razao), limite_razao),
    # Texto branco nas células escuras (razão abaixo de 0,4 ou acima de 2,5)
    cor_valor   = if_else(abs(razao_log2) > 1.3, cor_fundo, cor_texto),
    posicao     = match(nome_rotulo, ordem_distritos),
    painel      = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    nome_rotulo = factor(nome_rotulo, levels = rev(ordem_distritos)),
    coluna      = factor(colunas[indicador], levels = colunas)
  )

# Descrevendo os valores do município para o rodapé
texto_municipio <- municipio |>
  arrange(match(indicador, names(colunas))) |>
  mutate(
    texto = paste0(str_replace_all(colunas[indicador], "\n", " "), " ", formatar_numero(valor_municipio, 1), "%")
  ) |>
  pull(texto) |>
  paste(collapse = "; ")

# Desenhando o mapa de calor em dois painéis, com escala centrada em 1
grafico <- ggplot(mapa_calor, aes(x = coluna, y = nome_rotulo, fill = razao_log2)) +
  geom_tile(
    colour    = cor_fundo,
    linewidth = 0.25
  ) +
  geom_text(
    mapping = aes(label = formatar_numero(valor, 1), colour = cor_valor),
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
    name   = "Proporção do distrito em relação ao município",
    guide  = guide_colourbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Perfil dos arranjos familiares nos 96 distritos",
    subtitle = paste0(
      "Proporção em cada característica, 2022 (valor em cada célula, em %)\n",
      "Distritos ordenados pela proporção de domicílios unipessoais, da maior à menor (município: ",
      formatar_numero(unipessoal_municipio, 1), "%)\n",
      "Cor: razão ao município na mesma coluna. Vermelho: acima; verde: abaixo (não indica melhor ou pior)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        paste0("Município: ", texto_municipio, "."),
        "Bases: responsáveis (mulher e 60 anos ou mais); filhos do responsável e do cônjuge (25 anos ou mais); moradores (netos e pais ou sogros do responsável); cônjuges (mesmo sexo). Razões limitadas a 1/4 e 4 vezes.",
        "sigilo_pessoas"
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
