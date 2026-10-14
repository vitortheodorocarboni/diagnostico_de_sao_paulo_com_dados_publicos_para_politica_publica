################################################################################
##################################            ##################################
################### 14.06) ORIGEM DOS MORADORES POR DISTRITO ###################
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

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("14_migracao", "distritos")

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 4

# Definindo as cores das origens, na ordem da barra e da legenda (a mesma do
# gráfico dos 96 distritos)
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
##### III. Funções
################################################################################

# Criando uma função para desenhar a origem dos moradores de um distrito e do
# município
desenhar_origem <- function(dados, nome_titulo, aviso = NULL) {

  # Preparando as duas barras (distrito e município)
  dados <- dados |>
    filter(rotulo_origem %in% names(cores_origens)) |>
    mutate(
      origem = factor(rotulo_origem, levels = rev(names(cores_origens))),
      barra  = factor(
        if_else(nivel_territorial == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      )
    )

  # Desenhando as barras 100% empilhadas, com os valores nas maiores origens
  ggplot(dados, aes(x = pct, y = barra, fill = origem)) +
    geom_col(
      width     = 0.65,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 1), "%"), ""), group = origem),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 3,
      fontface = "bold",
      colour   = cor_fundo
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
      title    = nome_titulo,
      subtitle = paste0(
        "Moradores por lugar de nascimento, 2022",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Região pelo primeiro dígito do código de UF do IBGE (P0490); demais Sudeste: Minas Gerais, Espírito Santo e Rio de Janeiro. ",
            "UF não especificada fica fora da barra. Valores abaixo de ", minimo_rotulo, "% não aparecem na barra."
          ),
          "se_republica", "incerteza_amostra", "acesso_controlado"
        )
      )
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_text(size = 10, colour = cor_texto, face = "bold"),
      panel.grid.major.y = element_blank()
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo a composição por origem
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet"))

# Obtendo a composição do município
composicao_municipio <- composicao |>
  filter(nivel_territorial == "municipio")

# Listando os distritos com o aviso de amostra menor
distritos_lista <- composicao |>
  filter(nivel_territorial == "distrito") |>
  distinct(codigo_distrito, nome_rotulo, amostra_menor)


################################################################################
##### V. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando a origem dos moradores de cada distrito
message("Gerando a origem dos moradores dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando a origem do distrito e do município
  figura <- desenhar_origem(
    dados       = bind_rows(filter(composicao, codigo_distrito == codigo), composicao_municipio),
    nome_titulo = nome,
    aviso       = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "origem_moradores")),
    tamanho = "grafico",
    altura  = 10
  )
}
