################################################################################
##################################            ##################################
################ 07.06) CLASSES DE SALÁRIO MÍNIMO POR DISTRITO #################
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
diretorio_grupo <- caminho_data("07_renda")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("07_renda", "distritos")

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 4


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar a composição de um distrito e do município
desenhar_composicao <- function(dados, nome_titulo, classes_rotulos, aviso = NULL) {

  # Preparando as duas barras (distrito e município) e a cor do valor, branca nas
  # classes escuras e cinza escuro nas claras
  dados <- dados |>
    mutate(
      classe_renda = factor(rotulo_classe, levels = rev(classes_rotulos)),
      barra        = factor(
        if_else(nivel_territorial == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      ),
      cor_valor    = if_else(classe_codigo >= 5, cor_fundo, cor_texto)
    )

  # Desenhando as barras 100% empilhadas, com os valores nas maiores classes
  ggplot(dados, aes(x = pct, y = barra, fill = classe_renda)) +
    geom_col(
      width     = 0.65,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 1), "%"), ""), colour = cor_valor, group = classe_renda),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 3,
      fontface = "bold"
    ) +
    scale_colour_identity() +
    scale_fill_manual(
      values = setNames(paleta_sequencial(11)[2:11], classes_rotulos),
      breaks = classes_rotulos,
      name   = "Salários mínimos\nper capita:"
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      breaks = seq(0, 100, by = 25),
      expand = expansion(mult = c(0, 0))
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(
      title    = nome_titulo,
      subtitle = paste0(
        "Domicílios por classe de renda domiciliar per capita, em salários mínimos de 2022 (R$ 1.212), 2022",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Proporção sobre os domicílios particulares ocupados; renda zero entra na primeira classe; valores abaixo de ",
            minimo_rotulo, "% não aparecem na barra."
          ),
          "se_republica", "incerteza_amostra", "acesso_controlado"
        )
      )
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_text(size = 10, colour = cor_texto, face = "bold"),
      panel.grid.major.y = element_blank(),
      legend.title       = element_text(size = 8, colour = cor_texto_apoio)
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo a composição por classe de salário mínimo
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet"))

# Obtendo os rótulos das classes na ordem
classes_rotulos <- composicao |>
  distinct(classe_codigo, rotulo_classe) |>
  arrange(classe_codigo) |>
  pull(rotulo_classe)

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

# Gravando a composição de cada distrito
message("Gerando a composição dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando a composição do distrito e do município
  figura <- desenhar_composicao(
    dados           = bind_rows(filter(composicao, codigo_distrito == codigo), composicao_municipio),
    nome_titulo     = nome,
    classes_rotulos = classes_rotulos,
    aviso           = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "classes_renda")),
    tamanho = "grafico",
    altura  = 10
  )
}
