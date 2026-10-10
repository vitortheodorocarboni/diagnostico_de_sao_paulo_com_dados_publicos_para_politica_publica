################################################################################
##################################            ##################################
############################ GRÁFICOS POR DISTRITO #############################
##################################            ##################################
################################################################################

# Molde aprovado no piloto de densidades (etapa 3): mapa de localização, valor
# em destaque, posição entre os 96 distritos, comparação com o município e
# faixa com os 96 distritos. Carregue depois de identidade_visual.R.

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("patchwork")
# install.packages("sf")
# install.packages("tidyverse")

# Carregando bibliotecas
library(patchwork)
library(sf)
library(tidyverse)


################################################################################
##### II. Identidade visual
################################################################################

# Carregando a identidade visual, se ainda não estiver carregada
if (!exists("tema_insper", mode = "function")) {
  source(file.path("scripts", "_config", "identidade_visual.R"), encoding = "UTF-8")
}


################################################################################
##### III. Funções
################################################################################

# Criando uma função para escolher o alinhamento de um rótulo pela posição
# relativa no eixo (à esquerda, ao centro ou à direita)
alinhamento_rotulo <- function(x, limites) {

  # Calculando a posição relativa do valor no eixo
  relativa <- (x - limites[1]) / diff(limites)

  # Alinhando à esquerda no começo do eixo e à direita no fim
  case_when(
    relativa < 0.15 ~ 0,
    relativa > 0.85 ~ 1,
    TRUE            ~ 0.5
  )
}

# Criando o mapa de localização do distrito na cidade
mapa_localizacao <- function(mapa_base, codigo) {

  # Destacando o distrito em vermelho sobre os demais em cinza
  ggplot(mapa_base) +
    geom_sf(
      mapping   = aes(fill = codigo_distrito == codigo),
      colour    = cor_fundo,
      linewidth = 0.1
    ) +
    scale_fill_manual(
      values = c(`TRUE` = paleta_insper[["vermelho"]], `FALSE` = cor_grade),
      guide  = "none"
    ) +
    tema_mapa() +
    theme(
      plot.margin = margin(0, 0, 0, 0)
    )
}

# Criando o bloco de texto com o valor, a posição e a comparação com o município
# (n_distritos: distritos com valor, 96 salvo quando algum fica sem valor)
bloco_texto <- function(linha, valor_municipio, descricao, n_distritos = 96) {

  # Comparando em pontos percentuais as proporções (descricao$comparacao igual
  # a "pontos") e em variação relativa os demais indicadores
  if (identical(descricao$comparacao, "pontos")) {

    # Calculando a diferença em pontos percentuais
    diferenca <- linha$valor - valor_municipio

    # Descrevendo a comparação com o município
    comparacao <- case_when(
      abs(diferenca) < 0.05 ~ "Igual ao município",
      diferenca > 0         ~ paste0(formatar_numero(diferenca, 1), " p.p. acima do município"),
      TRUE                  ~ paste0(formatar_numero(abs(diferenca), 1), " p.p. abaixo do município")
    )
  } else {

    # Calculando a diferença relativa ao município
    diferenca <- linha$valor / valor_municipio - 1

    # Descrevendo a comparação com o município
    comparacao <- case_when(
      abs(diferenca) < 0.005 ~ "Igual ao município",
      diferenca > 0          ~ paste0(formatar_numero(100 * diferenca), "% acima do município"),
      TRUE                   ~ paste0(formatar_numero(100 * abs(diferenca)), "% abaixo do município")
    )
  }

  # Montando as linhas do bloco
  ggplot() +
    annotate(
      geom     = "text",
      x        = 0,
      y        = 0.80,
      label    = formatar_numero(linha$valor, descricao$casas_decimais),
      hjust    = 0,
      family   = familia_fonte,
      fontface = "bold",
      size     = 12,
      colour   = paleta_insper[["vermelho"]]
    ) +
    annotate(
      geom   = "text",
      x      = 0,
      y      = 0.50,
      label  = descricao$unidade,
      hjust  = 0,
      family = familia_fonte,
      size   = 4,
      colour = cor_texto
    ) +
    annotate(
      geom   = "text",
      x      = 0,
      y      = 0.28,
      label  = case_when(
        linha$posicao == 1           ~ paste0("Maior valor entre os ", n_distritos, " distritos"),
        linha$posicao == n_distritos ~ paste0("Menor valor entre os ", n_distritos, " distritos"),
        TRUE                         ~ paste0(linha$posicao, "º maior valor entre os ", n_distritos, " distritos")
      ),
      hjust  = 0,
      family = familia_fonte,
      size   = 3.5,
      colour = cor_texto_apoio
    ) +
    annotate(
      geom   = "text",
      x      = 0,
      y      = 0.10,
      label  = paste0(
        comparacao, " (", formatar_numero(valor_municipio, descricao$casas_decimais), ")"
      ),
      hjust  = 0,
      family = familia_fonte,
      size   = 3.5,
      colour = cor_texto_apoio
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void()
}

# Criando a faixa com os 96 distritos, o distrito em destaque e o município
faixa_distritos <- function(valores, linha, valor_municipio, descricao) {

  # Definindo os limites do eixo com uma pequena folga
  limites <- range(c(valores$valor, valor_municipio))
  limites <- limites + c(-1, 1) * 0.03 * diff(limites)

  # Desenhando os distritos como traços verticais
  ggplot(valores) +
    geom_segment(
      mapping   = aes(x = valor, xend = valor, y = 0, yend = 1),
      colour    = "#C4C4C4",
      linewidth = 0.5
    ) +
    # Linha do município, abaixo da faixa
    annotate(
      geom      = "segment",
      x         = valor_municipio,
      xend      = valor_municipio,
      y         = -0.30,
      yend      = 1,
      colour    = cor_texto,
      linetype  = "dashed",
      linewidth = 0.4
    ) +
    annotate(
      geom   = "text",
      x      = valor_municipio,
      y      = -0.45,
      label  = paste0("Município: ", formatar_numero(valor_municipio, descricao$casas_decimais)),
      hjust  = alinhamento_rotulo(valor_municipio, limites),
      family = familia_fonte,
      size   = 3,
      colour = cor_texto
    ) +
    # Traço do distrito em destaque, acima da faixa
    annotate(
      geom      = "segment",
      x         = linha$valor,
      xend      = linha$valor,
      y         = 0,
      yend      = 1.30,
      colour    = paleta_insper[["vermelho"]],
      linewidth = 1.4
    ) +
    annotate(
      geom     = "text",
      x        = linha$valor,
      y        = 1.48,
      label    = linha$nome_rotulo,
      hjust    = alinhamento_rotulo(linha$valor, limites),
      family   = familia_fonte,
      fontface = "bold",
      size     = 3.2,
      colour   = paleta_insper[["vermelho"]]
    ) +
    scale_x_continuous(
      limits = limites,
      labels = rotulo_numero(accuracy = 10^-descricao$casas_decimais),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(limits = c(-0.55, 1.6)) +
    labs(
      x = paste0(descricao$rotulo, " (", descricao$unidade, "); cada traço cinza é um distrito"),
      y = NULL
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x        = element_line(colour = cor_texto_apoio, linewidth = 0.3),
      axis.title.x       = element_text(size = 7.5, hjust = 0, margin = margin(t = 6))
    )
}

# Criando a figura completa de um distrito para um indicador
# (valores: codigo_distrito, nome_rotulo, valor e posicao dos 96 distritos;
# descricao: uma linha com rotulo, unidade e casas_decimais do indicador)
# (aviso: texto acrescentado ao subtítulo, por exemplo o de amostra menor)
figura_distrito <- function(mapa_base, valores, codigo, valor_municipio, descricao,
                            nota = NULL, fontes = "censo_universo", ano = 2022,
                            aviso = NULL) {

  # Selecionando a linha do distrito
  linha <- valores |>
    filter(codigo_distrito == codigo)

  # Compondo mapa à esquerda e texto e faixa à direita
  (
    mapa_localizacao(mapa_base, codigo) |
      (bloco_texto(linha, valor_municipio, descricao, n_distritos = nrow(valores)) / faixa_distritos(valores, linha, valor_municipio, descricao) +
         plot_layout(heights = c(1, 1)))
  ) +
    plot_layout(widths = c(1, 2)) +
    plot_annotation(
      title    = linha$nome_rotulo,
      subtitle = paste0(descricao$rotulo, ", ", ano, if (!is.null(aviso)) paste0("\n", aviso) else ""),
      caption  = legenda_fonte(fontes, notas = nota),
      theme    = tema_insper()
    )
}

