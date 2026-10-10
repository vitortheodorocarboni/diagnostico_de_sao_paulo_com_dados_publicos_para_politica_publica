################################################################################
##################################            ##################################
##################### 10.05) TIPOS DE VIAGEM POR DISTRITO ######################
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
diretorio_grupo <- caminho_data("10_deslocamentos")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("10_deslocamentos", "distritos")

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 5


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar os tipos de viagem dos três blocos de um
# distrito, cada um ao lado do da capital
desenhar_tipos <- function(dados, nome_titulo, tipos_rotulos, blocos_rotulos) {

  # Preparando as barras: um painel por bloco, com o distrito e a capital; o
  # asterisco marca o bloco com amostra menor no distrito
  dados <- dados |>
    mutate(
      tipo_viagem = factor(rotulo_tipo, levels = rev(tipos_rotulos)),
      painel      = factor(rotulo_bloco, levels = blocos_rotulos),
      barra       = if_else(
        nivel_territorial == "municipio",
        "Capital",
        paste0(nome_titulo, if_else(amostra_menor, " *", ""))
      ),
      barra       = fct_relevel(factor(barra), "Capital")
    )

  # Desenhando as barras 100% empilhadas, com os valores nos maiores tipos
  ggplot(dados, aes(x = pct, y = barra, fill = tipo_viagem)) +
    geom_col(
      width     = 0.7,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(
        label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 0), "%"), ""),
        group = tipo_viagem
      ),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 2.8,
      colour   = cor_fundo,
      fontface = "bold"
    ) +
    facet_wrap(
      facets = vars(painel),
      ncol   = 1,
      scales = "free_y"
    ) +
    scale_fill_manual(
      values = setNames(paleta_categorica(4), tipos_rotulos),
      breaks = tipos_rotulos,
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
      subtitle = "Tipo das viagens de um dia útil, 2023: todas as viagens com origem no distrito e as idas ao trabalho",
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "od",
        notas  = c(
          paste0(
            "Os três blocos da OD não se somam: viagens com origem nas zonas do distrito, viagens casa → trabalho pela zona ",
            "de residência e pela zona de destino. Valores abaixo de ", minimo_rotulo, "% não aparecem na barra."
          ),
          paste0(
            "Asterisco (*): menos de ", minimo_registros_od, " registros no bloco; ler com cautela. Bloco com menos de ",
            piso_registros_od, " registros: sem barra do distrito."
          ),
          "od_destino", "incerteza_amostra"
        )
      )
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_text(size = 9, colour = cor_texto),
      panel.grid.major.y = element_blank(),
      strip.text         = element_text(size = 9, face = "bold", hjust = 0)
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo os tipos de viagem dos três blocos
tipos <- read_parquet(file.path(diretorio_grupo, "tipos.parquet"))

# Obtendo os rótulos dos tipos e dos blocos na ordem
tipos_rotulos <- tipos |>
  distinct(tipo_codigo, rotulo_tipo) |>
  arrange(tipo_codigo) |>
  pull(rotulo_tipo)
blocos_rotulos <- unique(tipos$rotulo_bloco[match(c("origem", "ct_residencia", "ct_destino"), tipos$bloco)])

# Obtendo os tipos de viagem da capital
tipos_municipio <- tipos |>
  filter(nivel_territorial == "municipio")

# Listando os distritos
distritos_lista <- tipos |>
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

# Gravando os tipos de viagem de cada distrito
message("Gerando os tipos de viagem dos ", nrow(distritos_lista), " distritos")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando os três blocos do distrito (sem os blocos abaixo do piso) e da
  # capital
  figura <- desenhar_tipos(
    dados           = bind_rows(
      filter(tipos, codigo_distrito == codigo, !sem_valor),
      tipos_municipio
    ),
    nome_titulo     = nome,
    tipos_rotulos   = tipos_rotulos,
    blocos_rotulos  = blocos_rotulos
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "tipos_viagem")),
    tamanho = "grafico",
    altura  = 13.5
  )
}
