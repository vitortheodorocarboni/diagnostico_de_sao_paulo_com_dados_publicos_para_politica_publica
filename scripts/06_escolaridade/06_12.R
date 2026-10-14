################################################################################
##################################            ##################################
############## 06.12) CURSOS E ÁREAS DO CURSO SUPERIOR NA CIDADE ###############
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

# Definindo os arquivos de saída
arquivo_cursos <- caminho_resultados("06_escolaridade", "curso_frequentado_distritos.png")
arquivo_areas  <- caminho_resultados("06_escolaridade", "areas_curso_superior.png")

# Definindo as cores dos grupos de curso, na ordem da barra e da legenda (o
# primeiro ordena os distritos)
cores_cursos <- c(
  "Fundamental regular"  = paleta_insper[["vermelho"]],
  "Médio regular"        = paleta_insper[["turquesa"]],
  "Graduação"            = paleta_insper[["roxo"]],
  "Pós-graduação"        = paleta_insper[["amarelo"]],
  "Creche"               = paleta_insper[["rosa"]],
  "Pré-escola"           = paleta_insper[["laranja"]],
  "EJA e alfabetização"  = cor_neutra
)


################################################################################
##### III. Dados
################################################################################

# Lendo o curso frequentado por grupo
cursos <- read_parquet(file.path(diretorio_grupo, "cursos.parquet"))

# Lendo as áreas do curso superior
areas <- read_parquet(file.path(diretorio_grupo, "areas.parquet"))


################################################################################
##### IV. Curso frequentado nos 96 distritos
################################################################################

# Obtendo a proporção da graduação e da pós no município
referencia_municipio <- cursos |>
  filter(nivel_territorial == "municipio", rotulo_curso %in% c("Graduação", "Pós-graduação")) |>
  summarise(pct = sum(pct)) |>
  pull(pct)

# Preparando os distritos com a posição pela proporção na graduação e na pós
ordem_distritos <- cursos |>
  filter(nivel_territorial == "distrito", rotulo_curso %in% c("Graduação", "Pós-graduação")) |>
  group_by(codigo_distrito, nome_rotulo, amostra_menor) |>
  summarise(pct = sum(pct), .groups = "drop") |>
  mutate(
    posicao = min_rank(desc(pct)),
    painel  = if_else(posicao <= 48, "1º ao 48º", "49º ao 96º"),
    rotulo  = paste0(posicao, ". ", nome_rotulo, if_else(amostra_menor, " *", ""))
  ) |>
  select(codigo_distrito, posicao, rotulo, painel)

# Juntando as proporções dos sete grupos
cursos_dados <- cursos |>
  filter(nivel_territorial == "distrito") |>
  inner_join(ordem_distritos, by = "codigo_distrito") |>
  mutate(
    # O primeiro grupo fica à esquerda (níveis invertidos no empilhamento)
    curso  = factor(rotulo_curso, levels = rev(names(cores_cursos))),
    rotulo = fct_reorder(rotulo, posicao, .desc = TRUE)
  )

# Desenhando as barras empilhadas em dois painéis de 48
grafico_cursos <- ggplot(cursos_dados, aes(x = pct, y = rotulo, fill = curso)) +
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
    values = cores_cursos,
    breaks = names(cores_cursos),
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
    title    = "Curso frequentado pelos estudantes nos 96 distritos",
    subtitle = paste0(
      "Pessoas que frequentam escola ou creche, por curso, % dos estudantes, 2022\n",
      "Distritos ordenados pela proporção na graduação e na pós, da maior à menor (município: ",
      formatar_numero(referencia_municipio, 1), "%)"
    ),
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Curso frequentado (P0660): fundamental regular (04); médio regular (06); graduação (08); pós-graduação (especialização, mestrado e doutorado, 09 a 11); ",
          "creche (01); pré-escola (02); EJA e alfabetização (03, 05 e 07). A proporção da graduação e da pós depende da composição por idade dos estudantes."
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
##### V. Áreas do curso superior mais frequentes da capital
################################################################################

# Preparando as dez áreas principais e o restante do município
areas_municipio <- areas |>
  filter(nivel_territorial == "municipio") |>
  mutate(
    nome = fct_reorder(rotulo_area, if_else(posicao == max(posicao), -1, pct))
  )

# Desenhando as barras da proporção de cada área, com os registros
grafico_areas <- ggplot(areas_municipio, aes(x = pct, y = nome)) +
  geom_col(
    mapping = aes(fill = posicao == max(posicao)),
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
    limits = c(0, 55),
    breaks = seq(0, 50, by = 10),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title    = "Áreas do curso superior mais frequentes",
    subtitle = "As dez áreas mais frequentes, % de quem tem curso superior completo, Município de São Paulo, 2022",
    x        = NULL,
    y        = NULL,
    caption  = legenda_fonte(
      fontes = "censo_amostra",
      notas  = c(
        paste0(
          "Área detalhada do curso de graduação (P0750), de quem tem curso superior completo; o não sabe fica fora. ",
          "Em cinza, as demais áreas. Nomes como nos descritores do IBGE. Entre parênteses, os registros da amostra."
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
##### VI. Exportação
################################################################################

# Gravando as barras dos cursos
gravar_figura(
  grafico = grafico_cursos,
  arquivo = arquivo_cursos,
  tamanho = "grafico",
  altura  = 16
)

# Gravando as áreas
gravar_figura(
  grafico = grafico_areas,
  arquivo = arquivo_areas,
  tamanho = "grafico",
  altura  = 12
)
