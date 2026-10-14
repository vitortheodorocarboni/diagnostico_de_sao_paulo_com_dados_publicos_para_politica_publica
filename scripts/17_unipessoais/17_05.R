################################################################################
##################################            ##################################
#################### 17.05) TAXA DE UNIPESSOAIS POR PERFIL #####################
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
arquivo_saida <- caminho_resultados("17_unipessoais", "taxa_unipessoais_perfil.png")

# Descrevendo as categorias finas de cada recorte (as do tratado, sem agrupar),
# na ordem de leitura: o campo do total, o dos unipessoais, o código da
# categoria e o rótulo do IBGE
categorias_finas <- bind_rows(
  tibble(
    recorte = "Sexo do responsável",
    campo_total = "d0170_dpo", campo_unipessoal = "d0170_dpo_unipessoal",
    codigo = c("1", "2"),
    rotulo = c("Homens", "Mulheres")
  ),
  tibble(
    recorte = "Idade do responsável",
    campo_total = "d0180_dpo", campo_unipessoal = "d0180_dpo_unipessoal",
    # De 15 anos em diante: não há responsáveis de 0 a 14 anos nos domicílios
    codigo = sprintf("%02d", 4:17),
    rotulo = c(
      "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44",
      "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 ou mais"
    )
  ),
  tibble(
    recorte = "Cor ou raça do responsável",
    campo_total = "p0210_responsavel_dpo", campo_unipessoal = "p0210_responsavel_dpo_unipessoal",
    codigo = as.character(1:5),
    rotulo = c("Branca", "Preta", "Amarela", "Parda", "Indígena")
  ),
  tibble(
    recorte = "Instrução do responsável",
    campo_total = "p0770_responsavel_dpo", campo_unipessoal = "p0770_responsavel_dpo_unipessoal",
    codigo = as.character(1:4),
    rotulo = c(
      "Sem instrução e fundamental incompleto", "Fundamental completo e médio incompleto",
      "Médio completo e superior incompleto", "Superior completo"
    )
  ),
  tibble(
    recorte = "Renda per capita do domicílio",
    campo_total = "d0360_classe_salario_minimo", campo_unipessoal = "d0360_classe_salario_minimo_unipessoal",
    codigo = sprintf("%02d", 1:10),
    rotulo = c(
      "Até 1/4 SM", "1/4 a 1/2", "1/2 a 1", "1 a 2", "2 a 3", "3 a 4", "4 a 5", "5 a 10", "10 a 20", "Mais de 20"
    )
  )
)


################################################################################
##### III. Dados
################################################################################

# Lendo as APs da capital
amostra <- read_parquet(file.path(diretorio_grupo, "amostra.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Somando as APs da capital em cada categoria fina: domicílios, unipessoais e
# registros de unipessoais
taxas <- categorias_finas |>
  mutate(
    domicilios  = map2_dbl(campo_total, codigo, \(campo, cod) sum(amostra[[sprintf("%s_cat_codigo_%s_estimativa", campo, cod)]])),
    unipessoais = map2_dbl(campo_unipessoal, codigo, \(campo, cod) sum(amostra[[sprintf("%s_cat_codigo_%s_estimativa", campo, cod)]])),
    registros   = map2_dbl(campo_unipessoal, codigo, \(campo, cod) sum(amostra[[sprintf("%s_cat_codigo_%s_n_registros", campo, cod)]])),
    taxa        = 100 * unipessoais / domicilios
  ) |>
  mutate(
    # Mantendo a ordem de leitura dentro de cada recorte, de cima para baixo
    recorte = factor(recorte, levels = unique(categorias_finas$recorte)),
    rotulo  = factor(paste(recorte, rotulo, sep = "|"), levels = rev(paste(categorias_finas$recorte, categorias_finas$rotulo, sep = "|")))
  )

# Calculando a taxa do município (todos os domicílios particulares ocupados)
taxa_municipio <- 100 *
  sum(amostra$d0170_dpo_unipessoal_peso_observado + amostra$d0170_dpo_unipessoal_peso_ignorado + amostra$d0170_dpo_unipessoal_peso_ausente) /
  sum(amostra$d0170_dpo_peso_observado + amostra$d0170_dpo_peso_ignorado + amostra$d0170_dpo_peso_ausente)

# Desenhando as barras da taxa em cada categoria, um painel por recorte, com a
# linha do município
grafico <- ggplot(taxas, aes(x = taxa, y = rotulo)) +
  geom_col(
    fill  = paleta_insper[["vermelho"]],
    width = 0.7
  ) +
  geom_vline(
    xintercept = taxa_municipio,
    colour     = cor_texto_apoio,
    linetype   = "dashed",
    linewidth  = 0.3
  ) +
  geom_text(
    mapping  = aes(label = paste0(formatar_numero(taxa, 1), "%")),
    hjust    = -0.15,
    family   = familia_fonte,
    size     = 2.6,
    colour   = cor_texto
  ) +
  facet_grid(
    rows   = vars(recorte),
    scales = "free_y",
    space  = "free_y",
    switch = "y"
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(
    labels = \(x) str_remove(x, "^[^|]*[|]")
  ) +
  labs(
    title    = "Quem mora só, por perfil do responsável",
    subtitle = paste0(
      "Domicílios unipessoais entre os domicílios particulares ocupados de cada categoria, 2022\n",
      "Linha tracejada: município de São Paulo (", formatar_numero(taxa_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Cada barra é a parcela de domicílios com um morador entre os domicílios cujo responsável tem a característica. ",
          "Idade em faixas de 5 anos, a partir de 15 anos (não há responsáveis menores); renda domiciliar per capita em classes de salário mínimo (SM, R$ 1.212 em 2022), com a renda zero em até 1/4 SM; ",
          "o ignorado e a instrução não determinada ficam fora. Barras de categorias raras vêm de poucos registros."
        ),
        "perfil_unipessoais", "incerteza_amostra", "acesso_controlado"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 7.5, colour = cor_texto),
    strip.text.y.left  = element_text(size = 8.5, colour = cor_texto, face = "bold", angle = 0, hjust = 1),
    strip.placement    = "outside",
    panel.grid.major.y = element_blank(),
    panel.spacing.y    = unit(0.2, "cm")
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
