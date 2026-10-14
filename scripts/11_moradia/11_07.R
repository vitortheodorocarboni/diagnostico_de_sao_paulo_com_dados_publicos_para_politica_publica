################################################################################
##################################            ##################################
################### 11.07) CONDIÇÃO DE OCUPAÇÃO POR DISTRITO ###################
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
diretorio_grupo <- caminho_data("11_moradia")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("11_moradia", "distritos")

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 4

# Definindo a ordem das condições na barra e na legenda, da esquerda para a
# direita (a mesma do gráfico dos 96 distritos)
ordem_condicoes <- c(
  "Alugado",
  "Próprio quitado",
  "Próprio em pagamento",
  "Cedido",
  "Outra condição"
)

# Definindo as cores: vermelho no alugado e cinza em outra condição
cores_condicoes <- c(
  "Alugado"              = paleta_insper[["vermelho"]],
  "Próprio quitado"      = paleta_insper[["turquesa"]],
  "Próprio em pagamento" = paleta_insper[["roxo"]],
  "Cedido"               = paleta_insper[["amarelo"]],
  "Outra condição"       = cor_neutra
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar a composição de um distrito e do município
desenhar_composicao <- function(dados, nome_titulo, aviso = NULL) {

  # Preparando as duas barras (distrito e município); o valor fica em branco,
  # que contrasta com as cinco cores
  dados <- dados |>
    mutate(
      condicao = factor(rotulo_condicao, levels = rev(ordem_condicoes)),
      barra    = factor(
        if_else(nivel_territorial == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      )
    )

  # Desenhando as barras 100% empilhadas, com os valores nas maiores condições
  ggplot(dados, aes(x = pct, y = barra, fill = condicao)) +
    geom_col(
      width     = 0.65,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 1), "%"), ""), group = condicao),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 3,
      fontface = "bold",
      colour   = cor_fundo
    ) +
    scale_fill_manual(
      values = cores_condicoes,
      breaks = ordem_condicoes,
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
      subtitle = paste0(
        "Domicílios particulares permanentes ocupados por condição de ocupação, 2022",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Próprio em pagamento: próprio de algum morador, ainda pagando. Cedido: por empregador, por familiar ou de outra forma. ",
            "O Censo 2022 não pergunta o valor do aluguel. Valores abaixo de ", minimo_rotulo, "% não aparecem na barra."
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

# Lendo a composição por condição de ocupação
ocupacao <- read_parquet(file.path(diretorio_grupo, "ocupacao.parquet"))

# Obtendo a composição do município
ocupacao_municipio <- ocupacao |>
  filter(nivel_territorial == "municipio")

# Listando os distritos com o aviso de amostra menor
distritos_lista <- ocupacao |>
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
    dados       = bind_rows(filter(ocupacao, codigo_distrito == codigo), ocupacao_municipio),
    nome_titulo = nome,
    aviso       = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "condicao_ocupacao")),
    tamanho = "grafico",
    altura  = 10
  )
}
