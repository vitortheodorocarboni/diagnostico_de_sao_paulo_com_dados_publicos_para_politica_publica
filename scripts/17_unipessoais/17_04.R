################################################################################
##################################            ##################################
################ 17.04) PERFIL DE QUEM MORA SÓ NOS 96 DISTRITOS ################
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
diretorio_grupo <- caminho_data("17_unipessoais")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("17_unipessoais", "perfil_unipessoais_distritos.png")

# Definindo as seis características do perfil, na ordem das colunas
colunas <- c(
  pct_unipessoal     = "Domicílios\nunipessoais",
  pct_mulheres       = "Mulheres",
  pct_60_mais        = "60 anos\nou mais",
  pct_preta_parda    = "Pretos e\npardos",
  pct_superior       = "Superior\ncompleto",
  pct_renda_ate_meio = "Renda até\n1/2 SM"
)

# Definindo o limite da razão ao município, em log2 (1/4 a 4 vezes)
limite_razao <- 2


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos unipessoais
unipessoais <- read_parquet(file.path(diretorio_grupo, "unipessoais.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo o valor do município em cada característica
municipio <- unipessoais |>
  filter(nivel == "municipio", indicador %in% names(colunas)) |>
  select(indicador, valor_municipio = valor)

# Ordenando os distritos pela taxa de unipessoais, da maior à menor
ordem_distritos <- unipessoais |>
  filter(nivel == "distrito", indicador == "pct_unipessoal") |>
  arrange(desc(valor)) |>
  mutate(
    rotulo = paste0(nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  pull(rotulo)

# Calculando a razão de cada distrito ao município em cada característica
mapa_calor <- unipessoais |>
  filter(nivel == "distrito", indicador %in% names(colunas)) |>
  left_join(municipio, by = "indicador") |>
  mutate(
    rotulo_distrito = paste0(nome_rotulo, if_else(amostra_menor, " *", "")),
    # Razão em log2, limitada a 1/4 e 4 vezes (vazia nos distritos sem valor)
    razao_log2      = pmin(pmax(log2(valor / valor_municipio), -limite_razao), limite_razao),
    # Valor escrito na célula, em % (vazio nos distritos sem valor)
    texto           = if_else(is.na(valor), "", formatar_numero(valor, 1)),
    # Texto branco nas células escuras (razão abaixo de 0,4 ou acima de 2,5)
    cor_valor       = if_else(!is.na(razao_log2) & abs(razao_log2) > 1.3, cor_fundo, cor_texto),
    posicao         = match(rotulo_distrito, ordem_distritos),
    painel          = if_else(posicao <= 48, "Grupo 1", "Grupo 2"),
    rotulo_distrito = factor(rotulo_distrito, levels = rev(ordem_distritos)),
    coluna          = factor(colunas[indicador], levels = colunas)
  )

# Descrevendo os valores do município para o rodapé
texto_municipio <- municipio |>
  arrange(match(indicador, names(colunas))) |>
  mutate(
    texto = paste0(
      str_replace(str_to_lower(str_replace_all(colunas[indicador], "\n", " ")), " sm$", " SM"),
      " ", formatar_numero(valor_municipio, 1), "%"
    )
  ) |>
  pull(texto) |>
  paste(collapse = "; ")

# Listando os distritos sem valor no perfil, para a nota
sem_valor <- unipessoais |>
  filter(nivel == "distrito", indicador == "pct_mulheres", is.na(valor)) |>
  pull(nome_rotulo)

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
    title    = "Quem mora só nos 96 distritos",
    subtitle = paste0(
      "Domicílios unipessoais (% dos domicílios) e perfil de quem mora só (% dos unipessoais), 2022 (valor em cada célula)\n",
      "Distritos ordenados pela taxa de unipessoais, da maior à menor (município: ",
      formatar_numero(filter(municipio, indicador == "pct_unipessoal")$valor_municipio, 1), "%)\n",
      "Cor: razão ao município na mesma coluna. Vermelho: acima; verde: abaixo; cinza: sem valor"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0("Município: ", texto_municipio, "."),
        paste0(
          "Primeira coluna: unipessoais entre os domicílios particulares ocupados; demais colunas: composição de quem mora só, ",
          "pelo responsável (que é o único morador). SM: salário mínimo (R$ 606 é 1/2). Razões limitadas a 1/4 e 4 vezes. ",
          "Perfil sem valor: ", paste(sem_valor, collapse = ", "), "."
        ),
        "perfil_unipessoais", "amostra_menor_unipessoais", "se_republica", "incerteza_amostra", "acesso_controlado"
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
  altura  = 19
)
