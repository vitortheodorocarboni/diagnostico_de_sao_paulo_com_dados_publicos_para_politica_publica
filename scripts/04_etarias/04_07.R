################################################################################
##################################            ##################################
#################### 04.07) PIRÂMIDES ETÁRIAS POR DISTRITO #####################
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
diretorio_grupo <- caminho_data("04_etarias")

# Definindo o diretório de saída das pirâmides por distrito
diretorio_saida <- caminho_resultados("04_etarias", "distritos")

# Definindo o arquivo da pirâmide do município
arquivo_municipio <- caminho_resultados("04_etarias", "piramide_municipio.png")


################################################################################
##### III. Funções
################################################################################

# Criando uma função para preparar a geometria das barras da pirâmide. As
# faixas de 10 anos (30 a 69) recebem o dobro da altura e metade do comprimento,
# para que a área siga a população (proporção por faixa de 5 anos); o grupo
# aberto de 70 anos ou mais fica com altura de 5 anos e sem escala.
preparar_barras <- function(dados) {

  # Calculando altura e comprimento de cada barra
  dados |>
    arrange(sexo, grupo) |>
    group_by(sexo) |>
    mutate(
      # Largura do grupo em anos (5 no grupo aberto, só para a altura da barra)
      altura_anos = if_else(is.na(largura_anos), 5L, largura_anos),
      ymin        = lag(cumsum(altura_anos), default = 0L),
      ymax        = cumsum(altura_anos),
      # Proporção por faixa de 5 anos, exceto no grupo aberto
      valor_barra = if_else(is.na(largura_anos), pct, pct * 5 / largura_anos),
      # Homens à esquerda (valores negativos) e mulheres à direita
      xmin        = if_else(sexo == "Homens", -valor_barra, 0),
      xmax        = if_else(sexo == "Homens", 0, valor_barra)
    ) |>
    ungroup()
}

# Criando uma função para desenhar a pirâmide de um distrito, com o contorno do
# município sobreposto quando informado
desenhar_piramide <- function(barras, titulo, limite_x, contorno = NULL, nota_cobertura = NULL) {

  # Obtendo as posições e os rótulos dos grupos no eixo vertical
  eixo <- barras |>
    filter(sexo == "Mulheres") |>
    transmute(
      posicao = (ymin + ymax) / 2,
      rotulo  = rotulo_grupo
    )

  # Desenhando as barras
  grafico <- ggplot() +
    geom_rect(
      data      = barras,
      mapping   = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sexo),
      colour    = cor_fundo,
      linewidth = 0.3
    )

  # Sobrepondo o contorno do município
  if (!is.null(contorno)) {
    grafico <- grafico +
      geom_rect(
        data      = contorno,
        mapping   = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill      = NA,
        colour    = cor_texto,
        linewidth = 0.45
      )
  }

  # Completando o gráfico
  grafico +
    scale_fill_manual(
      values = c(Homens = paleta_insper[["turquesa"]], Mulheres = paleta_insper[["vermelho"]]),
      name   = NULL
    ) +
    scale_x_continuous(
      limits = c(-limite_x, limite_x),
      breaks = seq(-limite_x, limite_x, by = 2),
      labels = function(x) paste0(formatar_numero(abs(x), 0), "%"),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = eixo$posicao,
      labels = eixo$rotulo,
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(
      title    = titulo,
      subtitle = paste0(
        "Pirâmide etária, 2022. Proporção do total de moradores por faixa de 5 anos de idade",
        if (!is.null(contorno)) ".\nContorno preto: Município de São Paulo" else ""
      ),
      x        = NULL,
      y        = "Idade (anos)",
      caption  = legenda_fonte(
        fontes = "censo_universo",
        notas  = c(
          paste0(
            "As faixas de 30 a 69 anos têm 10 anos: a barra tem o dobro da altura e a metade do comprimento, ",
            "para a área seguir a população. A barra de 70 anos ou mais reúne todas as idades a partir de 70 e não segue essa escala."
          ),
          nota_cobertura,
          "sigilo_pessoas"
        )
      )
    ) +
    tema_insper() +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position    = "top",
      legend.key.size    = unit(0.4, "cm")
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo a pirâmide
piramide <- read_parquet(file.path(diretorio_grupo, "piramide.parquet"))

# Lendo os indicadores de idade
idade <- read_parquet(file.path(diretorio_grupo, "idade.parquet"))

# Preparando a geometria das barras de todos os distritos e do município
barras <- piramide |>
  group_by(codigo_distrito) |>
  group_modify(~ preparar_barras(.x)) |>
  ungroup()

# Obtendo o contorno do município
contorno_municipio <- barras |>
  filter(nivel == "municipio")

# Definindo o limite do eixo horizontal comum a todas as pirâmides, em múltiplo de 2
limite_x <- 2 * ceiling(max(abs(c(barras$xmin, barras$xmax))) / 2)


################################################################################
##### V. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Obtendo a cobertura de sexo e idade de cada distrito e do município
cobertura <- idade |>
  filter(indicador == "cobertura_piramide") |>
  select(codigo_distrito, cobertura = valor)

# Gravando a pirâmide do município
gravar_figura(
  grafico = desenhar_piramide(
    barras         = filter(barras, nivel == "municipio"),
    titulo         = "Município de São Paulo",
    limite_x       = limite_x,
    nota_cobertura = paste0(
      "A pirâmide cobre ", formatar_numero(cobertura$cobertura[cobertura$codigo_distrito == "3550308"], 1),
      "% dos moradores do município; os demais têm sexo ou idade com sigilo."
    )
  ),
  arquivo = arquivo_municipio,
  tamanho = "grafico"
)

# Gravando a pirâmide de cada distrito
distritos_lista <- barras |>
  filter(nivel == "distrito") |>
  distinct(codigo_distrito, nome_rotulo)
message("Gerando as pirâmides dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando a pirâmide com o contorno do município
  figura <- desenhar_piramide(
    barras         = filter(barras, codigo_distrito == codigo),
    titulo         = nome,
    limite_x       = limite_x,
    contorno       = contorno_municipio,
    nota_cobertura = paste0(
      "A pirâmide cobre ", formatar_numero(cobertura$cobertura[cobertura$codigo_distrito == codigo], 1),
      "% dos moradores do distrito; os demais têm sexo ou idade com sigilo."
    )
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "piramide")),
    tamanho = "grafico"
  )
}
