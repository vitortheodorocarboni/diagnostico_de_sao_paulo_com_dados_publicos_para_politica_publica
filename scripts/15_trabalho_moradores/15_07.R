################################################################################
##################################            ##################################
#################### 15.07) MEIO DE TRANSPORTE POR DISTRITO ####################
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
diretorio_grupo <- caminho_data("15_trabalho_moradores")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("15_trabalho_moradores", "distritos")

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 4

# Definindo as cores dos meios de transporte, na ordem da barra e da legenda (a
# mesma do gráfico dos 96 distritos)
cores_transporte <- c(
  "Coletivo"              = paleta_insper[["vermelho"]],
  "Individual motorizado" = paleta_insper[["turquesa"]],
  "A pé"                  = paleta_insper[["roxo"]],
  "Bicicleta"             = paleta_insper[["amarelo"]],
  "Outros"                = cor_neutra
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar o meio de transporte de um distrito e do
# município
desenhar_transporte <- function(dados, nome_titulo, aviso = NULL) {

  # Preparando as duas barras (distrito e município)
  dados <- dados |>
    mutate(
      meio  = factor(rotulo_categoria, levels = rev(names(cores_transporte))),
      barra = factor(
        if_else(nivel_territorial == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      )
    )

  # Desenhando as barras 100% empilhadas, com os valores nos maiores meios
  ggplot(dados, aes(x = pct, y = barra, fill = meio)) +
    geom_col(
      width     = 0.65,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 1), "%"), ""), group = meio),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 3,
      fontface = "bold",
      colour   = cor_fundo
    ) +
    scale_fill_manual(
      values = cores_transporte,
      breaks = names(cores_transporte),
      name   = NULL
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      limits = c(0, 100),
      oob    = scales::oob_keep,
      breaks = seq(0, 100, by = 25),
      expand = expansion(mult = c(0, 0))
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(
      title    = nome_titulo,
      subtitle = paste0(
        "Moradores ocupados que se deslocam ao trabalho, por meio de transporte, 2022",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Coletivo: van, ônibus, BRT, trem ou metrô, caminhão adaptado e embarcações. Individual motorizado: motocicleta, ",
            "mototáxi, automóvel e táxi. O ignorado fica fora do cálculo. Valores abaixo de ", minimo_rotulo, "% não aparecem na barra."
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

# Lendo a composição por meio de transporte
transporte <- read_parquet(file.path(diretorio_grupo, "composicoes.parquet")) |>
  filter(tabela == "transporte")

# Obtendo a composição do município
transporte_municipio <- transporte |>
  filter(nivel_territorial == "municipio")

# Listando os distritos com o aviso de amostra menor
distritos_lista <- transporte |>
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

# Gravando o meio de transporte de cada distrito
message("Gerando o meio de transporte dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando o meio de transporte do distrito e do município
  figura <- desenhar_transporte(
    dados       = bind_rows(filter(transporte, codigo_distrito == codigo), transporte_municipio),
    nome_titulo = nome,
    aviso       = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "meio_transporte")),
    tamanho = "grafico",
    altura  = 10
  )
}
