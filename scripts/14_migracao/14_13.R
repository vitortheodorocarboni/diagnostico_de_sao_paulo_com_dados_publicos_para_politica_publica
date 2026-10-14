################################################################################
##################################            ##################################
############## 14.13) PAÍSES E MUNICÍPIOS DE NASCIMENTO NA CIDADE ##############
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
arquivo_paises     <- caminho_resultados("14_migracao", "paises_nascimento.png")
arquivo_municipios <- caminho_resultados("14_migracao", "municipios_nascimento.png")


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar as barras de uma lista de categorias, com o
# percentual e os registros ao fim de cada barra
desenhar_barras <- function(dados, rotulo, titulo, subtitulo, nota, limite, passo, altura_texto = 2.8) {

  # Ordenando as categorias da maior à menor, de cima para baixo
  dados <- dados |>
    mutate(
      nome = fct_reorder(.data[[rotulo]], pct)
    )

  # Desenhando as barras da proporção
  ggplot(dados, aes(x = pct, y = nome)) +
    geom_col(
      fill  = paleta_insper[["vermelho"]],
      width = 0.7
    ) +
    geom_text(
      mapping  = aes(label = paste0(formatar_numero(pct, 1), "%  (", formatar_numero(n_registros), " registros)")),
      hjust    = -0.05,
      family   = familia_fonte,
      size     = altura_texto,
      colour   = cor_texto
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      limits = c(0, limite),
      breaks = seq(0, limite, by = passo),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title    = titulo,
      subtitle = subtitulo,
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(nota, "se_republica", "incerteza_amostra", "acesso_controlado")
      )
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_text(size = 9, colour = cor_texto),
      panel.grid.major.y = element_blank()
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo os países de nascimento do município, sem "Outros países"
paises <- read_parquet(file.path(diretorio_grupo, "paises.parquet")) |>
  filter(nivel_territorial == "municipio", codigo_pais != "outros")

# Calculando a parcela dos outros países, para o subtítulo
outros_paises <- read_parquet(file.path(diretorio_grupo, "paises.parquet")) |>
  filter(nivel_territorial == "municipio", codigo_pais == "outros") |>
  pull(pct)

# Lendo os municípios de nascimento mais frequentes da capital
municipios <- read_parquet(file.path(diretorio_grupo, "municipios_nascimento.parquet"))


################################################################################
##### V. Gráficos
################################################################################

# Desenhando os países de nascimento dos nascidos no exterior
grafico_paises <- desenhar_barras(
  dados     = paises,
  rotulo    = "rotulo_pais",
  titulo    = "Países de nascimento dos moradores nascidos no exterior",
  subtitulo = paste0(
    "Os dez países mais frequentes, % dos nascidos no exterior, Município de São Paulo, 2022. Outros países: ",
    formatar_numero(outros_paises, 1), "%"
  ),
  nota      = paste0(
    "País de nascimento (P0510), entre os nascidos no exterior com país informado; o ignorado e o não sabe ficam fora. ",
    "Nomes como nos descritores do IBGE (sem acento, em caixa de título). Entre parênteses, os registros da amostra."
  ),
  limite    = 40,
  passo     = 10
)

# Desenhando os municípios de nascimento dos nascidos em outro município
grafico_municipios <- desenhar_barras(
  dados     = municipios,
  rotulo    = "rotulo_municipio",
  titulo    = "Municípios de nascimento dos moradores nascidos em outro município do Brasil",
  subtitulo = "Os doze municípios mais frequentes, % dos nascidos em outro município do Brasil, Município de São Paulo, 2022",
  nota      = paste0(
    "Município de nascimento (P0500), entre os nascidos em outro município do Brasil com município informado; o ignorado e o não sabe ficam fora. ",
    "Entre parênteses, os registros da amostra."
  ),
  limite    = 3,
  passo     = 0.5
)


################################################################################
##### VI. Exportação
################################################################################

# Gravando os países
gravar_figura(
  grafico = grafico_paises,
  arquivo = arquivo_paises,
  tamanho = "grafico",
  altura  = 11
)

# Gravando os municípios
gravar_figura(
  grafico = grafico_municipios,
  arquivo = arquivo_municipios,
  tamanho = "grafico",
  altura  = 12
)
