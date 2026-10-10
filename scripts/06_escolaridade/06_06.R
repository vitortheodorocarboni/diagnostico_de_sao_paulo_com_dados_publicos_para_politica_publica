################################################################################
##################################            ##################################
#################### 06.06) NÍVEL DE INSTRUÇÃO POR DISTRITO ####################
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
diretorio_grupo <- caminho_data("06_escolaridade")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("06_escolaridade", "distritos")

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 4


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar a composição de um distrito e do município
desenhar_composicao <- function(dados, nome_titulo, niveis_rotulos, aviso = NULL) {

  # Preparando as duas barras: distrito e município
  dados <- dados |>
    mutate(
      nivel_instrucao = factor(rotulo_nivel, levels = rev(niveis_rotulos)),
      barra           = factor(
        if_else(nivel_territorial == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      )
    )

  # Desenhando as barras 100% empilhadas, com os valores nas maiores categorias
  ggplot(dados, aes(x = pct, y = barra, fill = nivel_instrucao)) +
    geom_col(
      width     = 0.65,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 1), "%"), ""), group = nivel_instrucao),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 3.3,
      colour   = cor_fundo,
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = setNames(paleta_categorica(4), rev(niveis_rotulos)),
      breaks = niveis_rotulos,
      name   = NULL
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      breaks = seq(0, 100, by = 25),
      expand = expansion(mult = c(0, 0))
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(
      title    = nome_titulo,
      subtitle = paste0(
        "Nível de instrução das pessoas de 25 anos ou mais, 2022",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Nível de instrução compatível com o Censo 2010 (P0770); valores abaixo de ", minimo_rotulo,
            "% não aparecem na barra."
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

# Lendo a composição por nível de instrução, sem o não determinado (zerado na
# capital)
composicao <- read_parquet(file.path(diretorio_grupo, "composicao.parquet")) |>
  filter(nivel != "nao_determinado")

# Obtendo os rótulos dos níveis na ordem
niveis_rotulos <- composicao |>
  distinct(nivel_codigo, rotulo_nivel) |>
  arrange(nivel_codigo) |>
  pull(rotulo_nivel)

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
    dados          = bind_rows(filter(composicao, codigo_distrito == codigo), composicao_municipio),
    nome_titulo    = nome,
    niveis_rotulos = niveis_rotulos,
    aviso          = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "instrucao")),
    tamanho = "grafico",
    altura  = 10
  )
}
