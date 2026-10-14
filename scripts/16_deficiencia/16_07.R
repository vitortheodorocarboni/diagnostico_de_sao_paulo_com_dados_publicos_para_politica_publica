################################################################################
##################################            ##################################
################### 16.07) TIPOS DE DEFICIÊNCIA POR DISTRITO ###################
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

# Definindo as linhas do gráfico, de cima para baixo
linhas <- c(
  pct_deficiencia = "Com deficiência (qualquer tipo)",
  pct_visual      = "Visual",
  pct_auditiva    = "Auditiva",
  pct_motora      = "Motora",
  pct_pegar       = "Pegar objetos",
  pct_mental      = "Mental ou intelectual",
  pct_autismo     = "Diagnóstico de autismo"
)

# Definindo o limite do eixo, acima do maior valor entre os distritos (9,4%)
limite_eixo <- 11


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar os tipos de deficiência de um distrito e do
# município
desenhar_tipos <- function(dados, nome_titulo, aviso = NULL) {

  # Preparando as barras do distrito e do município em cada linha
  dados <- dados |>
    mutate(
      linha = factor(linhas[indicador], levels = rev(linhas)),
      barra = factor(
        if_else(nivel == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      )
    )

  # Desenhando as barras lado a lado, com o valor ao fim de cada uma
  ggplot(dados, aes(x = valor, y = linha, fill = barra)) +
    geom_col(
      position = position_dodge(width = 0.75),
      width    = 0.7
    ) +
    geom_text(
      mapping  = aes(label = paste0(formatar_numero(valor, 1), "%")),
      position = position_dodge(width = 0.75),
      hjust    = -0.15,
      family   = familia_fonte,
      size     = 2.8,
      colour   = cor_texto
    ) +
    scale_fill_manual(
      values = setNames(c(cor_neutra, paleta_insper[["vermelho"]]), c("Município de São Paulo", nome_titulo)),
      breaks = c(nome_titulo, "Município de São Paulo"),
      name   = NULL
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      limits = c(0, limite_eixo),
      breaks = seq(0, 10, by = 2),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title    = nome_titulo,
      subtitle = paste0(
        "Pessoas com deficiência, por tipo, e com diagnóstico de autismo, 2022",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Deficiência: não consegue de modo algum ou tem muita dificuldade, entre as pessoas de 2 anos ou mais; uma pessoa pode ter ",
            "mais de um tipo. Autismo: diagnóstico por profissional de saúde, entre todos os moradores. A deficiência cresce com a idade, ",
            "e o tratado não permite padronizá-la."
          ),
          "se_republica", "incerteza_amostra", "acesso_controlado"
        )
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

# Lendo os indicadores da deficiência das sete linhas
deficiencia <- read_parquet(file.path(diretorio_grupo, "deficiencia.parquet")) |>
  filter(indicador %in% names(linhas))

# Obtendo os valores do município
deficiencia_municipio <- deficiencia |>
  filter(nivel == "municipio")

# Listando os distritos com o aviso de amostra menor
distritos_lista <- deficiencia |>
  filter(nivel == "distrito") |>
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

# Gravando os tipos de deficiência de cada distrito
message("Gerando os tipos de deficiência dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando os tipos do distrito e do município
  figura <- desenhar_tipos(
    dados       = bind_rows(filter(deficiencia, codigo_distrito == codigo), deficiencia_municipio),
    nome_titulo = nome,
    aviso       = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "tipos_deficiencia")),
    tamanho = "grafico"
  )
}
