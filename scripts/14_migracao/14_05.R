################################################################################
##################################            ##################################
################# 14.05) ORIGEM DOS MORADORES NOS 96 DISTRITOS #################
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
diretorio_grupo <- caminho_data("14_migracao")

# Definindo o arquivo de saída
arquivo_saida <- caminho_resultados("14_migracao", "origem_moradores_distritos.png")

# Definindo as cores das origens, na ordem da barra e da legenda (Nordeste
# primeiro, porque ordena os distritos; nascidos na capital por último, em
# cinza); a UF não especificada (0,06% no município) fica fora das barras
cores_origens <- c(
  "Nordeste"               = paleta_insper[["vermelho"]],
  "Outro município de SP"  = paleta_insper[["amarelo"]],
  "Demais Sudeste"         = paleta_insper[["turquesa"]],
  "Sul"                    = paleta_insper[["roxo"]],
  "Norte e Centro-Oeste"   = paleta_insper[["laranja"]],
  "Exterior"               = paleta_insper[["rosa"]],
  "Município de São Paulo" = cor_neutra
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da migração
migracao <- read_parquet(file.path(diretorio_grupo, "migracao.parquet"))

# Lendo a composição por origem
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet"))


################################################################################
##### IV. Gráfico
################################################################################

# Obtendo a proporção de nascidos no Nordeste do município
nordeste_municipio <- migracao |>
  filter(nivel == "municipio", indicador == "pct_nordeste") |>
  pull(valor)

# Preparando os distritos com a posição pela proporção de nascidos no Nordeste
ordem_distritos <- migracao |>
  filter(nivel == "distrito", indicador == "pct_nordeste") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções das sete origens
origem_dados <- composicao |>
  filter(nivel_territorial == "distrito", rotulo_origem %in% names(cores_origens)) |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # A primeira origem fica à esquerda (níveis invertidos no empilhamento)
    origem = factor(rotulo_origem, levels = rev(names(cores_origens))),
    rotulo = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico <- ggplot(origem_dados, aes(x = pct, y = rotulo, fill = origem)) +
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
  scale_fill_manual(
    values = cores_origens,
    breaks = names(cores_origens),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(0, 100),
    oob    = scales::oob_keep,
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title    = "Origem dos moradores nos 96 distritos",
    subtitle = paste0(
      "Proporção dos moradores por lugar de nascimento, 2022\n",
      "Distritos ordenados pela proporção de nascidos no Nordeste, da maior à menor (município: ",
      formatar_numero(nordeste_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        "Região pelo primeiro dígito do código de UF do IBGE (P0490); demais Sudeste: Minas Gerais, Espírito Santo e Rio de Janeiro. Os nascidos no Brasil com UF não especificada (0,06% no município) ficam fora das barras.",
        "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado"
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
