################################################################################
##################################            ##################################
################ 15.12) SETORES, OCUPAÇÕES E DESTINOS NA CIDADE ################
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

# Definindo os arquivos de saída
arquivo_setores    <- caminho_resultados("15_trabalho_moradores", "setores_atividade_distritos.png")
arquivo_ocupacoes  <- caminho_resultados("15_trabalho_moradores", "ocupacoes_principais.png")
arquivo_destinos   <- caminho_resultados("15_trabalho_moradores", "destinos_trabalho.png")

# Definindo as cores dos grupos de setor, na ordem da barra e da legenda (o
# primeiro ordena os distritos; outros e mal definidas em cinza)
cores_setores <- c(
  "Informação, finanças e serviços profissionais" = paleta_insper[["vermelho"]],
  "Comércio"                                      = paleta_insper[["turquesa"]],
  "Educação, saúde e administração pública"       = paleta_insper[["roxo"]],
  "Indústria e construção"                        = paleta_insper[["amarelo"]],
  "Transporte, alojamento e alimentação"          = paleta_insper[["rosa"]],
  "Serviços domésticos"                           = paleta_insper[["laranja"]],
  "Outros e mal definidas"                        = cor_neutra
)


################################################################################
##### III. Dados
################################################################################

# Lendo a composição por grupo de setor
setores <- read_parquet(file.path(diretorio_grupo, "setores.parquet"))

# Lendo as ocupações e os destinos de trabalho
ocupacoes <- read_parquet(file.path(diretorio_grupo, "ocupacoes.parquet"))
destinos  <- read_parquet(file.path(diretorio_grupo, "destinos.parquet"))


################################################################################
##### IV. Setores de atividade nos 96 distritos
################################################################################

# Obtendo a proporção do primeiro grupo no município
referencia_municipio <- setores |>
  filter(nivel_territorial == "municipio", rotulo_setor == names(cores_setores)[1]) |>
  pull(pct)

# Preparando os distritos com a posição pela proporção do primeiro grupo
ordem_distritos <- setores |>
  filter(nivel_territorial == "distrito", rotulo_setor == names(cores_setores)[1]) |>
  mutate(
    posicao = min_rank(desc(pct)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções dos sete grupos
setores_dados <- setores |>
  filter(nivel_territorial == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # O primeiro grupo fica à esquerda (níveis invertidos no empilhamento)
    setor  = factor(rotulo_setor, levels = rev(names(cores_setores))),
    rotulo = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico_setores <- ggplot(setores_dados, aes(x = pct, y = rotulo, fill = setor)) +
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
    values = cores_setores,
    breaks = names(cores_setores),
    name   = NULL
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(0, 100),
    oob    = scales::oob_keep,
    breaks = seq(0, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(
    title    = "Setor de atividade dos moradores ocupados nos 96 distritos",
    subtitle = paste0(
      "Moradores ocupados de 10 anos ou mais, por setor de atividade do trabalho principal, 2022\n",
      "Distritos ordenados pela proporção em informação, finanças e serviços profissionais, da maior à menor (município: ",
      formatar_numero(referencia_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Seções da CNAE-Domiciliar (P0980): comércio (G); informação, finanças e serviços profissionais (J a N); indústria e construção (B a F); ",
          "transporte, alojamento e alimentação (H e I); educação, saúde e administração pública (O a Q); serviços domésticos (T); ",
          "outros (A, R, S e U) e atividades mal definidas (V, 9% no município)."
        ),
        "amostra_menor", "se_republica", "incerteza_amostra", "acesso_controlado"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 5.5, colour = cor_texto),
    panel.grid.major.y = element_blank(),
    panel.spacing.x    = unit(1.2, "cm")
  )


################################################################################
##### V. Ocupações mais frequentes da capital
################################################################################

# Preparando as dez ocupações principais e o código sem rótulo do município
ocupacoes_municipio <- ocupacoes |>
  filter(nivel_territorial == "municipio", posicao <= 11) |>
  mutate(
    nome = fct_reorder(rotulo_ocupacao, if_else(posicao == 11, -1, pct))
  )

# Desenhando as barras da proporção de cada ocupação, com os registros
grafico_ocupacoes <- ggplot(ocupacoes_municipio, aes(x = pct, y = nome)) +
  geom_col(
    mapping = aes(fill = posicao == 11),
    width   = 0.7
  ) +
  geom_text(
    mapping  = aes(label = paste0(formatar_numero(pct, 1), "%  (", formatar_numero(n_registros), " registros)")),
    hjust    = -0.05,
    family   = familia_fonte,
    size     = 2.8,
    colour   = cor_texto
  ) +
  scale_fill_manual(
    values = c(`FALSE` = paleta_insper[["vermelho"]], `TRUE` = cor_neutra),
    guide  = "none"
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(0, 7),
    breaks = seq(0, 7, by = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title    = "Ocupações mais frequentes dos moradores ocupados",
    subtitle = "As dez ocupações mais frequentes, % dos ocupados de 10 anos ou mais, Município de São Paulo, 2022",
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Ocupação pela família ocupacional (P0970, quatro dígitos). Em cinza, o código 0000 (sem rótulo no descritor do IBGE, 4,5% dos ocupados), ",
          "que não entra entre as dez; o descritor tem K000 (mal definidas), sem equivalência certificada. Nomes como nos descritores do IBGE (sem acento). ",
          "Entre parênteses, os registros da amostra."
        ),
        "se_republica", "incerteza_amostra", "acesso_controlado"
      )
    )
  ) +
  tema_insper() +
  theme(
    axis.text.y        = element_text(size = 8.5, colour = cor_texto),
    panel.grid.major.y = element_blank()
  )


################################################################################
##### VI. Municípios de trabalho
################################################################################

# Desenhando as barras da proporção de cada município de trabalho
grafico_destinos <- ggplot(
  destinos |> mutate(nome = fct_reorder(rotulo_destino, pct)),
  aes(x = pct, y = nome)
) +
  geom_col(
    fill  = paleta_insper[["vermelho"]],
    width = 0.7
  ) +
  geom_text(
    mapping  = aes(label = paste0(formatar_numero(pct, 1), "%  (", formatar_numero(n_registros), " registros)")),
    hjust    = -0.05,
    family   = familia_fonte,
    size     = 2.8,
    colour   = cor_texto
  ) +
  scale_x_continuous(
    labels = rotulo_numero(suffix = "%"),
    limits = c(0, 20),
    breaks = seq(0, 20, by = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title    = "Para onde vão trabalhar os moradores que trabalham em outro município",
    subtitle = "Os dez municípios mais frequentes, % de quem trabalha em outro município, Município de São Paulo, 2022",
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Município do local de trabalho (P1140), só para quem trabalha em outro município (2,8% dos ocupados); o ignorado e o não sabe ficam fora. ",
          "Entre parênteses, os registros da amostra."
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


################################################################################
##### VII. Exportação
################################################################################

# Gravando as barras dos setores
gravar_figura(
  grafico = grafico_setores,
  arquivo = arquivo_setores,
  tamanho = "grafico",
  altura  = 16
)

# Gravando as ocupações
gravar_figura(
  grafico = grafico_ocupacoes,
  arquivo = arquivo_ocupacoes,
  tamanho = "grafico",
  altura  = 12
)

# Gravando os destinos
gravar_figura(
  grafico = grafico_destinos,
  arquivo = arquivo_destinos,
  tamanho = "grafico",
  altura  = 11
)
