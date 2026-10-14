################################################################################
##################################            ##################################
############### 15.04) COMPOSIÇÕES DO TRABALHO NOS 96 DISTRITOS ################
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

# Definindo o diretório de saída
diretorio_saida <- caminho_resultados("15_trabalho_moradores")

# Definindo as três composições: tabela, indicador que ordena os distritos,
# arquivo, título, subtítulo e nota
graficos <- tribble(
  ~tabela,      ~indicador_ordem,               ~arquivo,                              ~titulo,                                                   ~subtitulo,                                                                                 ~nota,
  "transporte", "pct_coletivo",                 "meio_transporte_distritos.png",       "Meio de transporte até o trabalho nos 96 distritos",      "Moradores ocupados que se deslocam ao trabalho, por meio de transporte, 2022",             "Coletivo: van, ônibus, BRT, trem ou metrô, caminhão adaptado e embarcações. Individual motorizado: motocicleta, mototáxi, automóvel e táxi. O ignorado fica fora do cálculo.",
  "posicao",    "pct_conta_propria",            "posicao_ocupacao_distritos.png",      "Posição na ocupação nos 96 distritos",                    "Moradores ocupados de 10 anos ou mais, por posição no trabalho principal, 2022",           "Setor público e militares: estatutários, empregados não estatutários do setor público, empregados de estatais e militares.",
  "grupos",     "pct_dirigentes_profissionais", "grupos_ocupacionais_distritos.png",   "Grupos ocupacionais nos 96 distritos",                    "Moradores ocupados de 10 anos ou mais, por grande grupo ocupacional do trabalho principal, 2022", "Grandes grupos da classificação de ocupações do Censo (P1040) reunidos em cinco; operários, artesãos e operadores incluem os trabalhadores qualificados da agropecuária; outros: forças armadas, policiais e bombeiros militares e ocupações mal definidas."
)

# Definindo as cores de cada composição, na ordem da barra e da legenda (a
# primeira categoria ordena os distritos; outros em cinza)
cores_composicoes <- list(
  transporte = c(
    "Coletivo"              = paleta_insper[["vermelho"]],
    "Individual motorizado" = paleta_insper[["turquesa"]],
    "A pé"                  = paleta_insper[["roxo"]],
    "Bicicleta"             = paleta_insper[["amarelo"]],
    "Outros"                = cor_neutra
  ),
  posicao    = c(
    "Conta própria"              = paleta_insper[["vermelho"]],
    "Empregado do setor privado" = paleta_insper[["turquesa"]],
    "Setor público e militares"  = paleta_insper[["roxo"]],
    "Trabalhador doméstico"      = paleta_insper[["amarelo"]],
    "Empregador"                 = paleta_insper[["rosa"]],
    "Não remunerado"             = cor_neutra
  ),
  grupos     = c(
    "Dirigentes e profissionais das ciências" = paleta_insper[["vermelho"]],
    "Técnicos e apoio administrativo"         = paleta_insper[["turquesa"]],
    "Serviços e comércio"                     = paleta_insper[["roxo"]],
    "Operários, artesãos e operadores"        = paleta_insper[["amarelo"]],
    "Ocupações elementares"                   = paleta_insper[["rosa"]],
    "Outros"                                  = cor_neutra
  )
)


################################################################################
##### III. Dados
################################################################################

# Lendo os indicadores do trabalho dos moradores
trabalho <- read_parquet(file.path(diretorio_grupo, "trabalho.parquet"))

# Lendo as composições
composicoes <- read_parquet(file.path(diretorio_grupo, "composicoes.parquet"))


################################################################################
##### IV. Gráficos
################################################################################

# Gravando um gráfico por composição
for (i in seq_len(nrow(graficos))) {

  # Selecionando a linha do gráfico
  grafico_atual <- graficos[i, ]

  # Obtendo as cores da composição
  cores <- cores_composicoes[[grafico_atual$tabela]]

  # Obtendo o valor do município no indicador que ordena os distritos
  valor_municipio <- trabalho |>
    filter(nivel == "municipio", indicador == grafico_atual$indicador_ordem) |>
    pull(valor)

  # Preparando os distritos com a posição pelo indicador que os ordena
  ordem_distritos <- trabalho |>
    filter(nivel == "distrito", indicador == grafico_atual$indicador_ordem) |>
    mutate(
      posicao = min_rank(desc(valor)),
      painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
      rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
    ) |>
    select(codigo_distrito, posicao, rotulo, painel)

  # Juntando as proporções das categorias
  dados <- composicoes |>
    filter(tabela == grafico_atual$tabela, nivel_territorial == "distrito") |>
    inner_join(ordem_distritos, by = "codigo_distrito") |>
    mutate(
      # A primeira categoria fica à esquerda (níveis invertidos no empilhamento)
      categoria = factor(rotulo_categoria, levels = rev(names(cores))),
      rotulo    = fct_reorder(rotulo, posicao, .desc = TRUE)
    )

  # Desenhando as barras empilhadas em dois painéis de 48
  grafico <- ggplot(dados, aes(x = pct, y = rotulo, fill = categoria)) +
    geom_col(
      width     = 0.8,
      colour    = cor_fundo,
      linewidth = 0.15
    ) +
    facet_wrap(
      facets = vars(painel),
      nrow   = 1,
      scales = "free_y"
    ) +
    scale_fill_manual(
      values = cores,
      breaks = names(cores),
      name   = NULL
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      limits = c(0, 100),
      oob    = scales::oob_keep,
      breaks = seq(0, 100, by = 25),
      expand = expansion(mult = c(0, 0))
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(
      title    = grafico_atual$titulo,
      subtitle = paste0(
        grafico_atual$subtitulo, "\n",
        "Distritos ordenados por ", str_to_lower(names(cores)[1]), ", do maior ao menor (município: ",
        formatar_numero(valor_municipio, 1), "%)"
      ),
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "censo_amostra",
        notas  = c(grafico_atual$nota, "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado")
      )
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_text(size = 5.5, colour = cor_texto),
      panel.grid.major.y = element_blank(),
      panel.spacing.x    = unit(1.2, "cm")
    )

  # Gravando o gráfico
  gravar_figura(
    grafico = grafico,
    arquivo = file.path(diretorio_saida, grafico_atual$arquivo),
    tamanho = "grafico",
    altura  = 16
  )
}
