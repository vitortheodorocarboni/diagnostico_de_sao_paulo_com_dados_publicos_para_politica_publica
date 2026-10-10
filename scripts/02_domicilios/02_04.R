################################################################################
##################################            ##################################
################## 02.04) TIPO DE DOMICÍLIO NOS 96 DISTRITOS ###################
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
arquivo_saida <- caminho_resultados("02_domicilios", "tipo_de_domicilio_distritos.png")

# Definindo os tipos de domicílio, na ordem de empilhamento
tipos <- c(
  apartamento  = "Apartamento",
  casa         = "Casa",
  casa_vila    = "Casa de vila ou condomínio",
  comodos      = "Cômodos ou cortiço",
  outros_tipos = "Outros"
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores dos domicílios
dominios <- read_parquet(file.path(diretorio_grupo, "dominios.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de apartamentos do município
apartamento_municipio <- dominios |>
  filter(nivel == "municipio", indicador == "apartamento") |>
  pull(valor)

# Preparando os distritos com a posição pela proporção de apartamentos
ordem_distritos <- dominios |>
  filter(nivel == "distrito", indicador == "apartamento") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo)
  ) |>
  select(codigo_distrito, rotulo, painel)

# Juntando as proporções dos cinco tipos de domicílio
tipo_dados <- dominios |>
  filter(nivel == "distrito", indicador %in% names(tipos)) |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    tipo   = factor(tipos[indicador], levels = tipos),
    rotulo = fct_reorder(rotulo, as.integer(str_extract(rotulo, "^[0-9]+")), .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(tipo_dados, aes(x = valor, y = rotulo, fill = tipo)) +
  geom_col(
    width     = 0.8,
    colour    = cor_fundo,
    linewidth = 0.15
  ) +
  facet_wrap(
    facets = vars(painel),
    nrow   = 1,
    scales = "free_y"
  ) +
  escala_fill_categorica(
    niveis = levels(tipo_dados$tipo),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Tipo de domicílio nos 96 distritos",
    subtitle = paste0(
      "Proporção dos domicílios particulares permanentes ocupados, 2022\n",
      "Distritos ordenados pela proporção de apartamentos (município: ",
      formatar_numero(apartamento_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_universo",
      notas  = c(
        "Proporção sobre a soma publicada dos tipos de domicílio; Outros reúne habitação indígena e estrutura residencial degradada ou inacabada.",
        "sigilo_universo"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 5.5, colour = cor_texto),
    panel.grid.major.y = element_blank(),
    panel.spacing.x    = unit(1.2, "cm")
  )


################################################################################
##### V. Exportação
################################################################################

# Gravando o gráfico
gravar_figura(
  grafico = grafico,
  arquivo = arquivo_saida,
  tamanho = "grafico",
  altura  = 16
)
