################################################################################
##################################            ##################################
################ 02.05) ENTORNO DOS DOMICÍLIOS NOS 96 DISTRITOS ################
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
diretorio_grupo <- caminho_data("02_domicilios")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("02_domicilios", "entorno_distritos.png")

# Definindo as nove características do entorno, na ordem das colunas
caracteristicas <- c(
  via_pavimentada    = "Via\npavimentada",
  iluminacao_publica = "Iluminação\npública",
  calcada            = "Calçada",
  bueiro             = "Bueiro",
  com_arvores        = "Árvores\nna face",
  ponto_onibus       = "Ponto de\nônibus",
  rampa              = "Rampa\ncadeirante",
  via_ciclo          = "Via para\nbicicleta",
  obstaculo_calcada  = "Obstáculo\nna calçada"
)

# Definindo o limite de não declarado acima do qual um distrito recebe aviso
limite_nao_declarado <- 20


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos domicílios
dominios <- read_parquet(file.path(diretorio_grupo, "dominios.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo o valor do município em cada característica
municipio <- dominios |>
  filter(nivel == "municipio", indicador %in% names(caracteristicas)) |>
  select(indicador, valor_municipio = valor)

# Calculando a diferença de cada distrito para o município, em pontos percentuais
diferencas <- dominios |>
  filter(nivel == "distrito", indicador %in% names(caracteristicas)) |>
  left_join(municipio, by = "indicador") |>
  mutate(
    diferenca = valor - valor_municipio
  )

# Ordenando os distritos por semelhança (agrupamento hierárquico das diferenças
# padronizadas), de modo que distritos parecidos fiquem juntos
matriz <- diferencas |>
  select(nome_rotulo, indicador, diferenca) |>
  pivot_wider(names_from = indicador, values_from = diferenca) |>
  column_to_rownames("nome_rotulo")
ordem <- hclust(dist(scale(matriz)), method = "ward.D2")$order
distritos_ordenados <- rownames(matriz)[ordem]

# Avisando sobre os distritos com muito entorno não declarado em rampa e obstáculo
avisos <- dominios |>
  filter(nivel == "distrito", indicador == "nd_rampa", valor > limite_nao_declarado) |>
  arrange(desc(valor)) |>
  mutate(
    texto = paste0(nome_rotulo, " (", formatar_numero(valor), "%)")
  )

# Preparando os dados com o painel e a posição de cada distrito
mapa_calor <- diferencas |>
  mutate(
    posicao        = match(nome_rotulo, distritos_ordenados),
    painel         = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    nome_rotulo    = factor(nome_rotulo, levels = rev(distritos_ordenados)),
    caracteristica = factor(caracteristicas[indicador], levels = caracteristicas),
    # Limitando a diferença exibida a 50 pontos percentuais
    diferenca_cor  = pmax(pmin(diferenca, 50), -50)
  )

# Desenhando o mapa de calor em dois painéis
grafico <- ggplot(mapa_calor, aes(x = caracteristica, y = nome_rotulo, fill = diferenca_cor)) +
  geom_tile(
    colour    = cor_fundo,
    linewidth = 0.25
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  scale_fill_gradientn(
    colours  = paleta_divergente(n = 9),
    limits   = c(-50, 50),
    breaks   = seq(-50, 50, by = 25),
    labels   = c("-50 ou menos", "-25", "0", "+25", "+50 ou mais"),
    name     = "Diferença para o município (pontos percentuais)",
    na.value = cor_grade,
    guide    = guide_colourbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Entorno dos domicílios nos 96 distritos",
    subtitle = paste0(
      "Domicílios em face com cada característica, diferença em relação ao município, 2022\n",
      "Distritos ordenados por semelhança. Vermelho: proporção acima do município; verde: abaixo (não indica melhor ou pior)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        paste0(
          "Proporção entre os domicílios com resposta sim ou não; o não declarado fica fora do cálculo. ",
          "Rampa e obstáculo têm mais de ", limite_nao_declarado, "% de não declarado em: ",
          paste(avisos$texto, collapse = ", "), "."
        ),
        "sigilo_universo"
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
