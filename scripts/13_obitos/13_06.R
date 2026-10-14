################################################################################
##################################            ##################################
##################### 13.06) IDADE AO FALECER POR DISTRITO #####################
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
diretorio_grupo <- caminho_data("13_obitos")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("13_obitos", "distritos")

# Definindo a proporção mínima para escrever o valor dentro da barra
minimo_rotulo <- 4


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar a idade ao falecer de um distrito e do
# município
desenhar_idade <- function(dados, nome_titulo, faixas_rotulos, aviso = NULL) {

  # Preparando as duas barras (distrito e município) e a cor do valor, branca
  # nas faixas escuras e preta nas claras
  dados <- dados |>
    mutate(
      faixa     = factor(rotulo_faixa, levels = rev(faixas_rotulos)),
      barra     = factor(
        if_else(nivel_territorial == "municipio", "Município de São Paulo", nome_titulo),
        levels = c("Município de São Paulo", nome_titulo)
      ),
      cor_valor = if_else(faixa_codigo >= 3, cor_fundo, cor_texto)
    )

  # Desenhando as barras 100% empilhadas, com os valores nas maiores faixas
  ggplot(dados, aes(x = pct, y = barra, fill = faixa)) +
    geom_col(
      width     = 0.65,
      colour    = cor_fundo,
      linewidth = 0.3
    ) +
    geom_text(
      mapping  = aes(label = if_else(pct >= minimo_rotulo, paste0(formatar_numero(pct, 1), "%"), ""), colour = cor_valor, group = faixa),
      position = position_stack(vjust = 0.5),
      family   = familia_fonte,
      size     = 3,
      fontface = "bold"
    ) +
    scale_colour_identity() +
    scale_fill_manual(
      values = setNames(paleta_sequencial(6)[2:6], faixas_rotulos),
      breaks = faixas_rotulos,
      name   = "Idade ao falecer:"
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
        "Moradores falecidos de janeiro de 2019 a julho de 2022 por idade ao falecer",
        if (!is.null(aviso)) paste0("\n", aviso) else ""
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(
          paste0(
            "Proporção sobre os falecidos com idade informada. Depende também da idade dos moradores: ",
            "distritos mais jovens têm mais óbitos em idades baixas. Valores abaixo de ", minimo_rotulo, "% não aparecem na barra."
          ),
          "captacao_obitos", "se_republica", "incerteza_amostra", "acesso_controlado"
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

# Lendo a idade ao falecer em faixas largas
idade_obito <- read_parquet(file.path(diretorio_grupo, "idade_obito.parquet"))

# Obtendo os rótulos das faixas na ordem
faixas_rotulos <- idade_obito |>
  distinct(faixa_codigo, rotulo_faixa) |>
  arrange(faixa_codigo) |>
  pull(rotulo_faixa)

# Obtendo a idade ao falecer do município
idade_municipio <- idade_obito |>
  filter(nivel_territorial == "municipio")

# Listando os distritos com valor, com o aviso de poucos registros
distritos_lista <- idade_obito |>
  filter(nivel_territorial == "distrito", !sem_valor) |>
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

# Gravando a idade ao falecer de cada distrito
message("Gerando a idade ao falecer dos ", nrow(distritos_lista), " distritos com valor")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]

  # Desenhando a idade ao falecer do distrito e do município
  figura <- desenhar_idade(
    dados          = bind_rows(filter(idade_obito, codigo_distrito == codigo), idade_municipio),
    nome_titulo    = nome,
    faixas_rotulos = faixas_rotulos,
    aviso          = if (distritos_lista$amostra_menor[i]) aviso_amostra_menor_obitos else NULL
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "idade_ao_falecer")),
    tamanho = "grafico",
    altura  = 10
  )
}
