################################################################################
##################################            ##################################
############## 06.09) PREPARAR OS INDICADORES DA EXTENSÃO ESCOLAR ##############
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("geoarrow")
# install.packages("sf")
# install.packages("tidyverse")
# install.packages("writexl")

# Carregando bibliotecas
library(arrow)
library(geoarrow)
library(sf)
library(tidyverse)
library(writexl)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto, as funções de leitura e as regras
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Carregando os universos de aplicação das variáveis da amostra
source(file.path("scripts", "_config", "universos_amostra.R"), encoding = "UTF-8")

# Definindo o diretório dos extratos e das saídas do grupo
diretorio_grupo <- caminho_data("06_escolaridade")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("06_escolaridade")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Definindo as variáveis da amostra usadas na extensão
codigos_amostra <- c("P0650", "P0660", "P0750", "P0790")

# Definindo quantas áreas do curso superior entram nas figuras (as mais
# frequentes da capital)
n_areas <- 10

# Definindo o código de área que é "não sabe" (peso ignorado de P0750)
codigo_area_nao_sabe <- "1111"

# Agrupando os cursos frequentados (P0660) em sete grupos, na ordem do percurso
# escolar (códigos do questionário: 01 creche, 02 pré-escola, 03 alfabetização
# de jovens e adultos, 04 fundamental regular, 05 EJA do fundamental, 06 médio
# regular, 07 EJA do médio, 08 graduação, 09 especialização, 10 mestrado e 11
# doutorado)
grupos_curso <- tribble(
  ~grupo_curso, ~rotulo_curso,                ~codigos,
  1L,           "Creche",                     "01",
  2L,           "Pré-escola",                 "02",
  3L,           "Fundamental regular",        "04",
  4L,           "Médio regular",              "06",
  5L,           "EJA e alfabetização",        c("03", "05", "07"),
  6L,           "Graduação",                  "08",
  7L,           "Pós-graduação",              c("09", "10", "11")
) |>
  unnest(codigos) |>
  rename(codigo_curso = codigos)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar as estimativas das APs de cada grupo. As
# estimativas já vêm ponderadas pelo IBGE; somam-se as estimativas e os pesos, e
# os indicadores são recalculados a partir das somas.
somar_aps <- function(aps_agrupadas) {

  # Somando os registros, as estimativas e os pesos
  aps_agrupadas |>
    summarise(
      n_aps = n(),
      across(
        .cols = where(is.numeric),
        .fns  = sum
      ),
      .groups = "drop"
    )
}

# Criando uma função para somar uma variável em lista (por AP) por distrito e
# para o município, devolvendo a estimativa e os registros de cada categoria
# (a ausência, que é quem não se aplica, e os códigos de excluir ficam fora)
somar_lista <- function(amostra_distritos, campo, excluir = character()) {

  # Abrindo a lista de categorias de cada AP
  categorias <- amostra_distritos |>
    select(codigo_distrito, lista = all_of(campo)) |>
    unnest(lista) |>
    filter(!codigo_categoria %in% c("ausente", excluir))

  # Somando por distrito e para o município
  bind_rows(
    categorias |>
      group_by(codigo_distrito, codigo_categoria),
    categorias |>
      mutate(codigo_distrito = codigo_municipio_sp) |>
      group_by(codigo_distrito, codigo_categoria)
  ) |>
    summarise(
      estimativa  = sum(estimativa),
      n_registros = sum(n_registros),
      .groups     = "drop"
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo as APs da capital
amostra <- read_parquet(file.path(diretorio_grupo, "extensao_amostra.parquet"))

# Lendo os rótulos das áreas do curso superior
rotulos <- read_parquet(file.path(diretorio_grupo, "extensao_rotulos.parquet"))

# Lendo a ligação AP → distrito
ap_distrito <- read_parquet(file.path(diretorio_territorio, "ap_distrito.parquet"))

# Lendo os nomes dos distritos, sem a geometria
distritos <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet")) |>
  st_drop_geometry() |>
  as_tibble() |>
  select(
    codigo_distrito,
    nome_distrito,
    nome_rotulo
  )


################################################################################
##### V. Descrição dos indicadores
################################################################################

# Descrevendo os indicadores do grupo, na ordem de apresentação
indicadores <- tribble(
  ~indicador,              ~grupo,      ~rotulo,                                 ~unidade,                              ~comparacao, ~casas_decimais, ~descricao,
  "pct_frequenta_escola",  "frequencia", "Pessoas que frequentam escola ou creche", "% dos moradores",                     "pontos",    1L,              "P0650 código 1 (frequenta escola ou creche) sobre o peso observado de P0650 (todas as pessoas).",
  "pct_estudantes_superior", "curso",   "Estudantes na graduação ou na pós",      "% de quem frequenta escola ou creche", "pontos",    1L,              "P0660 códigos 08 a 11 (graduação, especialização, mestrado e doutorado) sobre o peso observado de P0660 (quem frequenta escola ou creche).",
  "anos_estudo_medio",     "anos",      "Anos de estudo médios",                  "anos de estudo por pessoa",           "relativa",  1L,              "Soma ponderada de P0790 pelo peso das pessoas sobre o peso observado, de todos os moradores (crianças inclusive).",
  "registros_amostra",     "qualidade", "Registros de pessoas na amostra",        "registros",                           "relativa",  0L,              "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor."
) |>
  mutate(
    ordem = row_number()
  )


################################################################################
##### VI. Indicadores por distrito e do município
################################################################################

# Conferindo os universos de aplicação das variáveis da amostra na capital
universos <- conferir_universos(amostra, codigos_amostra)

# Ligando as APs aos distritos pela regra da maioria de setores
amostra_distritos <- amostra |>
  inner_join(
    ap_distrito |> select(codigo_area_ponderacao, codigo_distrito),
    by = "codigo_area_ponderacao"
  )

# Somando as APs por distrito e para o município (só os campos de contagem, sem
# as listas)
totais_amostra <- bind_rows(
  amostra_distritos |>
    select(-ends_with("_distribuicao")) |>
    group_by(codigo_distrito) |>
    somar_aps(),
  amostra_distritos |>
    select(-ends_with("_distribuicao")) |>
    somar_aps() |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Descrevendo cada distrito e o município, com o aviso de amostra menor (o
# município não recebe aviso)
territorio <- totais_amostra |>
  transmute(
    codigo_distrito,
    n_aps,
    registros_amostra = pessoas_n_registros,
    nivel             = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor     = nivel == "distrito" & registros_amostra < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  )

# Somando o curso frequentado por grupo, em cada distrito e no município, como
# proporção de quem frequenta escola ou creche (peso observado de P0660)
cursos <- totais_amostra |>
  select(codigo_distrito, p0660_peso_observado, starts_with("p0660_cat_codigo_")) |>
  pivot_longer(
    cols            = starts_with("p0660_cat_codigo_"),
    names_to        = "codigo_curso",
    names_pattern   = "p0660_cat_codigo_(\\d+)_estimativa",
    values_to       = "estimativa"
  ) |>
  inner_join(grupos_curso, by = "codigo_curso") |>
  group_by(codigo_distrito, p0660_peso_observado, grupo_curso, rotulo_curso) |>
  summarise(
    estimativa = sum(estimativa),
    .groups    = "drop"
  ) |>
  mutate(
    pct = 100 * estimativa / p0660_peso_observado
  ) |>
  inner_join(
    territorio |> select(nivel_territorial = nivel, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor),
    by = "codigo_distrito"
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    grupo_curso,
    rotulo_curso,
    estimativa,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, grupo_curso)

# Somando as áreas do curso superior (sem o "não sabe", que é o peso ignorado) e
# escolhendo as dez mais frequentes da capital
areas_todas <- somar_lista(amostra_distritos, "p0750_distribuicao", excluir = codigo_area_nao_sabe) |>
  left_join(
    rotulos |> select(codigo_categoria, rotulo_area = descricao_categoria),
    by = "codigo_categoria"
  ) |>
  group_by(codigo_distrito) |>
  mutate(
    pct = 100 * estimativa / sum(estimativa)
  ) |>
  ungroup()
areas_principais <- areas_todas |>
  filter(codigo_distrito == codigo_municipio_sp) |>
  arrange(desc(estimativa)) |>
  slice_head(n = n_areas) |>
  mutate(
    posicao = row_number()
  ) |>
  select(codigo_categoria, posicao)

# Montando a tabela das áreas: as dez principais da capital e as demais, em %
# de quem tem curso superior completo, em cada distrito e no município (as
# principais que faltam no distrito entram com zero)
areas <- areas_todas |>
  left_join(areas_principais, by = "codigo_categoria") |>
  mutate(
    posicao          = replace_na(posicao, n_areas + 1L),
    codigo_categoria = if_else(posicao <= n_areas, codigo_categoria, "demais"),
    rotulo_area      = if_else(posicao <= n_areas, str_to_sentence(rotulo_area), "Demais áreas")
  ) |>
  group_by(codigo_distrito, posicao, codigo_categoria, rotulo_area) |>
  summarise(
    estimativa  = sum(estimativa),
    n_registros = sum(n_registros),
    pct         = sum(pct),
    .groups     = "drop"
  ) |>
  complete(
    codigo_distrito,
    nesting(posicao, codigo_categoria, rotulo_area),
    fill = list(estimativa = 0, n_registros = 0L, pct = 0)
  ) |>
  inner_join(
    territorio |> select(nivel_territorial = nivel, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor),
    by = "codigo_distrito"
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    posicao,
    codigo_area = codigo_categoria,
    rotulo_area,
    estimativa,
    n_registros,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, posicao)

# Calculando os indicadores em formato largo
extensao_largo <- totais_amostra |>
  inner_join(territorio |> select(-n_aps, -registros_amostra), by = "codigo_distrito") |>
  mutate(
    # Frequência escolar sobre o peso observado de P0650 (todas as pessoas)
    pct_frequenta_escola    = 100 * p0650_cat_codigo_1_estimativa / p0650_peso_observado,
    # Graduação e pós sobre o peso observado de P0660 (quem frequenta)
    pct_estudantes_superior = 100 * (p0660_cat_codigo_08_estimativa + p0660_cat_codigo_09_estimativa +
                                       p0660_cat_codigo_10_estimativa + p0660_cat_codigo_11_estimativa) /
      p0660_peso_observado,
    # Anos de estudo: média ponderada sobre o peso observado de P0790
    anos_estudo_medio       = p0790_soma / p0790_peso_observado,
    registros_amostra       = pessoas_n_registros
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
extensao <- extensao_largo |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    all_of(indicadores$indicador)
  ) |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  )


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(extensao_largo, nivel == "distrito")
largo_municipio <- filter(extensao_largo, nivel == "municipio")

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Universos de aplicação das quatro variáveis conferidos",
    "Frequência escolar: as três categorias somam o peso observado de P0650 em cada AP",
    "Curso frequentado: os 11 códigos somam o peso observado de P0660 em cada AP",
    "Os sete grupos de curso somam 100% em cada distrito",
    "Áreas do curso superior somam o peso observado de P0750 em cada AP (sem o não sabe)",
    "Áreas somam 100% em cada distrito",
    "As dez áreas principais têm rótulo",
    "Anos de estudo médios entre 5 e 15 em cada distrito",
    "Nenhuma proporção fora de 0 a 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    all(abs(amostra$p0650_cat_codigo_1_estimativa + amostra$p0650_cat_codigo_2_estimativa + amostra$p0650_cat_codigo_3_estimativa - amostra$p0650_peso_observado) < 1e-6),
    all(abs(rowSums(as.matrix(amostra[sprintf("p0660_cat_codigo_%02d_estimativa", 1:11)])) - amostra$p0660_peso_observado) < 1e-6),
    cursos |>
      group_by(codigo_distrito) |>
      summarise(s = sum(pct), n = n(), .groups = "drop") |>
      (\(x) all(abs(x$s - 100) < 1e-6) && all(x$n == 7L))(),
    amostra_distritos |>
      select(codigo_area_ponderacao, lista = p0750_distribuicao, peso = p0750_peso_observado) |>
      unnest(lista) |>
      filter(!codigo_categoria %in% c("ausente", codigo_area_nao_sabe)) |>
      group_by(codigo_area_ponderacao, peso) |>
      summarise(s = sum(estimativa), .groups = "drop") |>
      (\(x) all(abs(x$s - x$peso) < 1e-6))(),
    areas |>
      group_by(codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      (\(x) all(abs(x$s - 100) < 1e-6))(),
    !anyNA(filter(areas, nivel_territorial == "municipio", posicao <= n_areas)$rotulo_area),
    all(largo_distritos$anos_estudo_medio > 5 & largo_distritos$anos_estudo_medio < 15),
    extensao |>
      filter(str_starts(indicador, "pct_")) |>
      summarise(ok = all(valor >= 0 & valor <= 100, na.rm = TRUE)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 13
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Interrompendo se alguma conferência falhar
if (!all(conferencias$ok)) {
  stop("Conferências com falha: ", paste(conferencias$conferencia[!conferencias$ok], collapse = "; "))
}

# Informando os valores do município
message(
  "Município: frequentam escola ", round(largo_municipio$pct_frequenta_escola, 1),
  "%; estudantes no superior ", round(largo_municipio$pct_estudantes_superior, 1),
  "%; anos de estudo ", round(largo_municipio$anos_estudo_medio, 2)
)


################################################################################
##### VIII. Exportação
################################################################################

# Criando o diretório de dados do grupo
dir.create(
  path         = diretorio_grupo,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Criando o diretório de resultados do grupo
dir.create(
  path         = diretorio_resultados,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando os indicadores em formato longo
write_parquet(
  x    = extensao,
  sink = file.path(diretorio_grupo, "extensao.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores_extensao.parquet")
)

# Gravando o curso frequentado por grupo
write_parquet(
  x    = cursos,
  sink = file.path(diretorio_grupo, "cursos.parquet")
)

# Gravando as áreas do curso superior
write_parquet(
  x    = areas,
  sink = file.path(diretorio_grupo, "areas.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = extensao_largo |>
      select(
        codigo_distrito,
        nome_distrito,
        n_aps,
        amostra_menor,
        all_of(indicadores$indicador)
      ) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                = codigo_distrito,
        `Distrito`              = nome_distrito,
        `APs no distrito`       = n_aps,
        `Amostra menor (aviso)` = amostra_menor
      ),
    cursos      = cursos |>
      mutate(pct = round(pct, 3)),
    areas       = areas |>
      mutate(pct = round(pct, 3)),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), P0650, P0660, P0750 e P0790; rótulos das áreas pelo tratado auxiliar do Dados (descritores do IBGE).",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        "Anos de estudo (P0790) de todos os moradores, crianças inclusive: a média depende da estrutura etária do distrito, e o tratado não permite padronizá-la (não há cruzamento com a idade).",
        "Curso frequentado (P0660) só de quem frequenta escola ou creche; EJA e alfabetização juntam os códigos 03, 05 e 07; pós-graduação junta especialização, mestrado e doutorado.",
        "Áreas do curso superior (P0750, classificação detalhada) de quem tem curso superior completo; o código 1111 (não sabe) fica fora; nomes como nos descritores do IBGE.",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_escolaridade_extensao.xlsx")
)

# Informando o fim do script
message("Indicadores da extensão da escolaridade gravados em ", diretorio_grupo)
