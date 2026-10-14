################################################################################
##################################            ##################################
############### 12.08) ESPÉCIE DE UNIDADE DOMÉSTICA POR DISTRITO ###############
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

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("12_arranjos_familiares", "distritos")

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 4

# Definindo as cores das espécies, na ordem da barra e da legenda (a mesma do
# gráfico dos 96 distritos)
cores_especies <- c(
  "Unipessoal" = paleta_insper[["vermelho"]],
  "Nuclear"    = paleta_insper[["turquesa"]],
  "Estendida"  = paleta_insper[["roxo"]],
  "Composta"   = paleta_insper[["amarelo"]]
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar a composição de um distrito e do município
desenhar_especies <- function(dados, nome_titulo) {

  # Preparando as duas barras (distrito e município)
  dados <- dados |>
    mutate(
      especie = factor(rotulo_categoria, levels = rev(names(cores_especies))),
      barra   = factor(
        if_else(nivel_territorial == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      )
    )

  # Desenhando as barras 100% empilhadas, com os valores nas maiores espécies
  ggplot(dados, aes(x = pct, y = barra, fill = especie)) +
    geom_col(
      width     = 0.65,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 1), "%"), ""), group = especie),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 3,
      fontface = "bold",
      colour   = cor_fundo
    ) +
    scale_fill_manual(
      values = cores_especies,
      breaks = names(cores_especies),
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
      subtitle = "Domicílios particulares ocupados por espécie de unidade doméstica, 2022",
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_universo",
        notas  = c(
          paste0(
            "Unipessoal: uma só pessoa. Nuclear: responsável com cônjuge e/ou filhos. Estendida: com outros parentes. ",
            "Composta: com pessoas sem parentesco com o responsável. Valores abaixo de ", minimo_rotulo, "% não aparecem na barra."
          ),
          "sigilo_universo"
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

# Lendo a composição por espécie de unidade doméstica
especies <- read_parquet(file.path(diretorio_grupo, "composicoes.parquet")) |>
  filter(tabela == "especie")

# Obtendo a composição do município
especies_municipio <- especies |>
  filter(nivel_territorial == "municipio")

# Listando os distritos
distritos_lista <- especies |>
  filter(nivel_territorial == "distrito") |>
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
  figura <- desenhar_especies(
    dados       = bind_rows(filter(especies, codigo_distrito == codigo), especies_municipio),
    nome_titulo = nome
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "especie_unidade_domestica")),
    tamanho = "grafico",
    altura  = 10
  )
}
