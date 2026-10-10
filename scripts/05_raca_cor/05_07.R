################################################################################
##################################            ##################################
############### 05.07) COMPOSIÇÃO POR COR OU RAÇA, POR DISTRITO ################
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
diretorio_grupo <- caminho_data("05_raca_cor")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("05_raca_cor", "distritos")

# Definindo as cores ou raças, na ordem de empilhamento, e as cores do gráfico
cores <- c(
  branca   = "Branca",
  parda    = "Parda",
  preta    = "Preta",
  amarela  = "Amarela",
  indigena = "Indígena"
)
cores_graficos <- setNames(
  c(paleta_insper[["roxo"]], paleta_insper[["turquesa"]], paleta_insper[["vermelho"]],
    paleta_insper[["amarelo"]], paleta_insper[["rosa"]]),
  cores
)

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 3


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar a composição de um distrito e do município
desenhar_composicao <- function(composicao, nome_titulo, cobertura) {

  # Preparando as duas barras: distrito e município
  dados <- composicao |>
    mutate(
      categoria = factor(cores[cor], levels = rev(cores)),
      barra     = factor(
        if_else(nivel == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      )
    )

  # Desenhando as barras 100% empilhadas, com os valores nas maiores categorias
  ggplot(dados, aes(x = pct, y = barra, fill = categoria)) +
    geom_col(
      width     = 0.65,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 1), "%"), ""), group = categoria),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 3.3,
      colour   = cor_fundo,
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = cores_graficos,
      breaks = unname(cores),
      name   = NULL
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      breaks = seq(0, 100, by = 25),
      expand = expansion(mult = c(0, 0))
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(
      title    = nome_titulo,
      subtitle = "Cor ou raça dos moradores, 2022",
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_universo",
        notas  = c(
          paste0(
            "Proporção sobre a soma publicada das cinco categorias de cor ou raça, autodeclaradas; valores abaixo de ",
            minimo_rotulo, "% não aparecem na barra. O sigilo atinge mais as categorias pequenas: amarela e indígena ficam subestimadas."
          ),
          paste0("A cor ou raça está publicada para ", formatar_numero(cobertura, 1), "% dos moradores do distrito."),
          "sigilo_pessoas"
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

# Lendo a composição por cor ou raça
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet"))

# Lendo os indicadores de cor ou raça
cor <- read_parquet(file.path(diretorio_grupo, "cor.parquet"))

# Obtendo a cobertura de cada distrito
cobertura <- cor |>
  filter(indicador == "cobertura_cor") |>
  select(codigo_distrito, cobertura = valor)

# Obtendo a composição do município
composicao_municipio <- composicao |>
  filter(nivel == "municipio")

# Listando os distritos
distritos_lista <- composicao |>
  filter(nivel == "distrito") |>
  distinct(codigo_distrito, nome_rotulo)


################################################################################
##### V. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando a composição de cada distrito
message("Gerando a composição dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando a composição do distrito e do município
  figura <- desenhar_composicao(
    composicao    = bind_rows(filter(composicao, codigo_distrito == codigo), composicao_municipio),
    nome_titulo   = nome,
    cobertura     = cobertura$cobertura[cobertura$codigo_distrito == codigo]
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "composicao")),
    tamanho = "grafico",
    altura  = 10
  )
}
