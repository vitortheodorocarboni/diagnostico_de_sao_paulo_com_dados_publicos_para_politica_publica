################################################################################
##################################            ##################################
################ 14.12) ORIGEM DE QUEM CHEGOU NOS 96 DISTRITOS #################
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

# Definindo os arquivos de saída
arquivo_barras <- caminho_resultados("14_migracao", "origem_recente_distritos.png")
arquivo_calor  <- caminho_resultados("14_migracao", "perfil_origem_recente_distritos.png")

# Definindo as cores das origens, na ordem da barra e da legenda (a UF não
# especificada, 0,1% no município, fica fora das barras)
cores_origens <- c(
  "Nordeste"               = paleta_insper[["vermelho"]],
  "Outro município de SP"  = paleta_insper[["amarelo"]],
  "Demais Sudeste"         = paleta_insper[["turquesa"]],
  "Sul"                    = paleta_insper[["roxo"]],
  "Norte e Centro-Oeste"   = paleta_insper[["laranja"]],
  "Exterior"               = paleta_insper[["rosa"]]
)

# Definindo as colunas do mapa de calor, na ordem das origens
colunas <- c(
  pct_migrou_5anos               = "Moravam fora\nhá 5 anos",
  pct_recente_nordeste           = "Do\nNordeste",
  pct_recente_sp                 = "Outro\nmunicípio\nde SP",
  pct_recente_demais_sudeste     = "Demais\nSudeste",
  pct_recente_sul                = "Do\nSul",
  pct_recente_norte_centro_oeste = "Norte e\nCentro-Oeste",
  pct_recente_exterior           = "Do\nexterior"
)

# Definindo o limite da razão ao município, em log2 (1/4 a 4 vezes)
limite_razao <- 2


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores da migração recente
recente <- read_parquet(file.path(diretorio_grupo, "recente.parquet"))

# Lendo a origem de quem veio de fora
origem_recente <- read_parquet(file.path(diretorio_grupo, "origem_recente.parquet"))

# Preparando os distritos com a posição pela proporção vinda do Nordeste
ordem_distritos <- recente |>
  filter(nivel == "distrito", indicador == "pct_recente_nordeste") |>
  mutate(
    posicao = min_rank(desc(valor)),
    painel  = if_else(posicao <= 48, "Primeiros 48", "Últimos 48"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Obtendo a proporção vinda do Nordeste no município
nordeste_municipio <- recente |>
  filter(nivel == "municipio", indicador == "pct_recente_nordeste") |>
  pull(valor)


################################################################################
##### IV. Barras empilhadas
################################################################################

# Juntando as proporções das seis origens
origem_dados <- origem_recente |>
  filter(nivel_territorial == "distrito", rotulo_origem %in% names(cores_origens)) |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # A primeira origem fica à esquerda (níveis invertidos no empilhamento)
    origem = factor(rotulo_origem, levels = rev(names(cores_origens))),
    rotulo = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico_barras <- ggplot(origem_dados, aes(x = pct, y = rotulo, fill = origem)) +
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
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(
    title    = "De onde vieram quem morava fora do município há 5 anos",
    subtitle = paste0(
      "Proporção por local de moradia há 5 anos, entre as pessoas que moravam em outro município ou país, 2022\n",
      "Distritos ordenados pela proporção do Nordeste, da maior à menor (município: ",
      formatar_numero(nordeste_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Região pelo primeiro dígito do código de UF do IBGE (P0610); demais Sudeste: Minas Gerais, Espírito Santo e Rio de Janeiro. ",
          "Os que vieram do Brasil com UF não especificada (0,1% no município) ficam fora das barras. A base é pequena nos distritos de pouca migração recente."
        ),
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
##### V. Mapa de calor
################################################################################

# Obtendo o valor do município em cada coluna
municipio <- recente |>
  filter(nivel == "municipio", indicador %in% names(colunas)) |>
  select(indicador, valor_municipio = valor)

# Calculando a razão de cada distrito ao município em cada coluna
mapa_calor <- recente |>
  filter(nivel == "distrito", indicador %in% names(colunas)) |>
  left_join(municipio, by = "indicador") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # Razão em log2, limitada a 1/4 e 4 vezes (vazia onde o valor é zero)
    razao_log2      = pmin(pmax(log2(valor / valor_municipio), -limite_razao), limite_razao),
    razao_log2      = if_else(valor == 0, -limite_razao, razao_log2),
    texto           = formatar_numero(valor, 1),
    cor_valor       = if_else(abs(razao_log2) > 1.3, cor_fundo, cor_texto),
    rotulo_distrito = fct_reorder(rotulo, posicao, .desc = TRUE),
    coluna          = factor(colunas[indicador], levels = colunas)
  )

# Descrevendo os valores do município para o rodapé
texto_municipio <- municipio |>
  arrange(match(indicador, names(colunas))) |>
  mutate(
    # Nome da coluna em minúsculas, com os nomes próprios (regiões e SP)
    # restaurados
    texto = paste0(
      str_replace_all(
        str_to_lower(str_replace_all(colunas[indicador], "\n", " ")),
        c(
          "nordeste"             = "Nordeste",
          "sudeste"              = "Sudeste",
          "\\bsul\\b"            = "Sul",
          "norte e centro-oeste" = "Norte e Centro-Oeste",
          " sp$"                 = " SP"
        )
      ),
      " ", formatar_numero(valor_municipio, 1), "%"
    )
  ) |>
  pull(texto) |>
  paste(collapse = "; ")

# Desenhando o mapa de calor em dois painéis, com escala centrada em 1
grafico_calor <- ggplot(mapa_calor, aes(x = coluna, y = rotulo_distrito, fill = razao_log2)) +
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
    title    = "Migração recente e origem nos 96 distritos",
    subtitle = paste0(
      "Primeira coluna: % das pessoas de 5 anos ou mais que moravam fora há 5 anos; demais: origem, % de quem veio de fora, 2022\n",
      "Distritos ordenados pela proporção do Nordeste, da maior à menor\n",
      "Cor: razão ao município na mesma coluna. Vermelho: acima; verde: abaixo (não indica melhor ou pior)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0("Município: ", texto_municipio, "."),
        "Razões limitadas a 1/4 e 4 vezes; valor zero em verde-escuro. Região pelo primeiro dígito do código de UF do IBGE (P0610). A base de quem veio de fora é pequena nos distritos de pouca migração recente.",
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
##### VI. Exportação
################################################################################

# Gravando as barras
gravar_figura(
  grafico = grafico_barras,
  arquivo = arquivo_barras,
  tamanho = "grafico",
  altura  = 16
)

# Gravando o mapa de calor
gravar_figura(
  grafico = grafico_calor,
  arquivo = arquivo_calor,
  tamanho = "grafico",
  altura  = 17
)
