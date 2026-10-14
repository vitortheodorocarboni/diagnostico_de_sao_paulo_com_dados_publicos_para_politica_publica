################################################################################
##################################            ##################################
################### 16.08) GRAUS DE DIFICULDADE POR DISTRITO ###################
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
diretorio_grupo <- caminho_data("16_deficiencia")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("16_deficiencia", "distritos")

# Definindo as cores dos três graus com dificuldade, do mais ao menos grave
# (escala sequencial vermelha; "nenhuma dificuldade" fica fora do gráfico)
cores_graus <- setNames(
  rev(paleta_sequencial(4, cor = "vermelho")[2:4]),
  c("Não consegue de modo algum", "Muita dificuldade", "Alguma dificuldade")
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar os graus de dificuldade de cada tipo, no
# distrito e no município
desenhar_graus <- function(dados, nome_titulo, limite_eixo, aviso = NULL) {

  # Preparando as barras do distrito e do município em cada tipo
  dados <- dados |>
    mutate(
      grau  = factor(rotulo_grau, levels = rev(names(cores_graus))),
      tipo  = factor(rotulo_tipo, levels = unique(rotulo_tipo[order(tipo_codigo)])),
      barra = factor(
        if_else(nivel_territorial == "municipio", "Município", nome_titulo),
        levels = c("Município", nome_titulo)
      )
    )

  # Somando os três graus de cada barra, para o rótulo ao fim dela
  totais <- dados |>
    group_by(tipo, barra) |>
    summarise(pct = sum(pct), .groups = "drop")

  # Desenhando as barras empilhadas dos três graus, um painel por tipo
  ggplot(dados, aes(x = pct, y = barra)) +
    geom_col(
      mapping   = aes(fill = grau),
      width     = 0.7,
      colour    = cor_fundo,
      linewidth = 0.2
    ) +
    geom_text(
      data     = totais,
      mapping  = aes(label = paste0(formatar_numero(pct, 1), "%")),
      hjust    = -0.15,
      family   = familia_fonte,
      size     = 2.6,
      colour   = cor_texto
    ) +
    facet_wrap(
      facets         = vars(tipo),
      ncol           = 1,
      strip.position = "left"
    ) +
    scale_fill_manual(
      values = cores_graus,
      breaks = names(cores_graus),
      name   = NULL
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      limits = c(0, limite_eixo),
      breaks = seq(0, limite_eixo, by = 5),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title    = nome_titulo,
      subtitle = paste0(
        "Pessoas de 2 anos ou mais com dificuldade, por tipo e grau, 2022 (ao fim da barra, a soma dos três graus)",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Os dois graus mais escuros (não consegue de modo algum e muita dificuldade) definem a pessoa com deficiência; quem tem ",
            "alguma dificuldade não entra. O restante tem nenhuma dificuldade. Visual e auditiva: mesmo com óculos, lentes ou aparelho; ",
            "motora: andar ou subir degraus; mental ou intelectual: atividades habituais, por limitação nas funções mentais."
          ),
          "se_republica", "incerteza_amostra", "acesso_controlado"
        )
      )
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_text(size = 7.5, colour = cor_texto),
      strip.text.y.left  = element_text(size = 8.5, colour = cor_texto, face = "bold", angle = 0, hjust = 1),
      strip.placement    = "outside",
      panel.grid.major.y = element_blank(),
      panel.spacing.y    = unit(0.15, "cm")
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo os graus com dificuldade (códigos 1 a 3)
graus <- read_parquet(file.path(diretorio_grupo, "graus.parquet")) |>
  filter(grau_codigo %in% 1:3)

# Obtendo os graus do município
graus_municipio <- graus |>
  filter(nivel_territorial == "municipio")

# Calculando o limite do eixo: a maior soma dos três graus, arredondada para
# cima ao múltiplo de 5 seguinte
limite_eixo <- graus |>
  group_by(codigo_distrito, tipo_codigo) |>
  summarise(pct = sum(pct), .groups = "drop") |>
  pull(pct) |>
  max() |>
  (\(x) 5 * ceiling(x / 5 + 0.1))()

# Listando os distritos com o aviso de amostra menor
distritos_lista <- graus |>
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

# Gravando os graus de dificuldade de cada distrito
message("Gerando os graus de dificuldade dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando os graus do distrito e do município
  figura <- desenhar_graus(
    dados       = bind_rows(filter(graus, codigo_distrito == codigo), graus_municipio),
    nome_titulo = nome,
    limite_eixo = limite_eixo,
    aviso       = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "graus_dificuldade")),
    tamanho = "grafico",
    altura  = 15
  )
}
