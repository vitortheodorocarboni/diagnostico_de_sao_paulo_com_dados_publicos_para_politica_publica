################################################################################
##################################            ##################################
########### 17.07) TAXA DE UNIPESSOAIS POR SEXO E IDADE NO DISTRITO ############
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
diretorio_grupo <- caminho_data("17_unipessoais")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("17_unipessoais", "distritos")

# Definindo os recortes do gráfico, de cima para baixo
recortes_grafico <- c(
  sexo  = "Sexo do responsável",
  idade = "Idade do responsável"
)

# Definindo o limite do eixo, acima da maior taxa entre os distritos
limite_eixo <- 80


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar a taxa de unipessoais por sexo e por idade
# do responsável em um distrito e no município
desenhar_taxas <- function(dados, nome_titulo, aviso = NULL) {

  # Preparando as barras do distrito e do município em cada grupo
  dados <- dados |>
    mutate(
      recorte = factor(recortes_grafico[recorte], levels = recortes_grafico),
      rotulo  = factor(rotulo_grupo, levels = rev(unique(rotulo_grupo[order(match(recorte, recortes_grafico), grupo_codigo)]))),
      barra   = factor(
        if_else(nivel == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      )
    )

  # Desenhando as barras lado a lado, um painel por recorte, com o valor ao fim
  ggplot(dados, aes(x = valor, y = rotulo, fill = barra)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width    = 0.75
    ) +
    geom_text(
      mapping  = aes(label = paste0(formatar_numero(valor, 1), "%", if_else(registros_grupo < minimo_registros_grupo_unipessoais, " †", ""))),
      position = position_dodge(width = 0.8),
      hjust    = -0.15,
      family   = familia_fonte,
      size     = 2.6,
      colour   = cor_texto
    ) +
    facet_grid(
      rows   = vars(recorte),
      scales = "free_y",
      space  = "free_y",
      switch = "y"
    ) +
    scale_fill_manual(
      values = setNames(c(cor_neutra, paleta_insper[["vermelho"]]), c("Município de São Paulo", nome_titulo)),
      breaks = c(nome_titulo, "Município de São Paulo"),
      name   = NULL
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      limits = c(0, limite_eixo),
      breaks = seq(0, 75, by = 25),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title    = nome_titulo,
      subtitle = paste0(
        "Domicílios unipessoais entre os domicílios do sexo e da idade do responsável, 2022",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Cada barra é a parcela de domicílios com um morador entre os domicílios particulares ocupados cujo responsável tem a característica. ",
            "Nos distritos com menos de ", piso_registros_unipessoais, " registros de unipessoais, só o sexo tem valor. †: menos de ", minimo_registros_grupo_unipessoais, " registros de unipessoais no grupo; estimativa instável."
          ),
          "perfil_unipessoais", "se_republica", "incerteza_amostra", "acesso_controlado"
        )
      )
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_text(size = 9, colour = cor_texto),
      strip.text.y.left  = element_text(size = 9, colour = cor_texto, face = "bold", angle = 0, hjust = 1),
      strip.placement    = "outside",
      panel.grid.major.y = element_blank(),
      panel.spacing.y    = unit(0.3, "cm")
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo a taxa por grupo, só nos recortes do gráfico e nos grupos com valor
taxas <- read_parquet(file.path(diretorio_grupo, "taxas.parquet")) |>
  filter(recorte %in% names(recortes_grafico), !is.na(valor))

# Obtendo as taxas do município
taxas_municipio <- taxas |>
  filter(nivel == "municipio")

# Listando os distritos e o aviso de amostra menor
distritos_lista <- taxas |>
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

# Gravando a taxa por sexo e idade de cada distrito
message("Gerando a taxa por sexo e idade dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando as taxas do distrito e do município
  figura <- desenhar_taxas(
    dados       = bind_rows(filter(taxas, codigo_distrito == codigo), taxas_municipio),
    nome_titulo = nome,
    aviso       = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor_unipessoais else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "taxa_sexo_idade")),
    tamanho = "grafico",
    altura  = 11
  )
}
