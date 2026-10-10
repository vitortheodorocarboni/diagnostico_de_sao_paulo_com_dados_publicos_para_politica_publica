################################################################################
##################################            ##################################
################### 00.02) PREPARAR AS LIGAÇÕES TERRITORIAIS ###################
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

# Carregando bibliotecas
library(arrow)
library(geoarrow)
library(sf)
library(tidyverse)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Definindo o diretório dos extratos e das saídas
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o CRS projetado usado nas operações de área (o mesmo da OD)
epsg_projetado <- 31983


################################################################################
##### III. Dados
################################################################################

# Lendo os setores da capital
setores <- read_parquet(file.path(diretorio_territorio, "setores.parquet"))

# Lendo a geometria dos distritos do Censo
distritos_geometria <- ler_geoparquet(file.path(diretorio_territorio, "distritos_geometria.parquet"))

# Lendo a relação entre APs e distritos
ap_distritos <- read_parquet(file.path(diretorio_territorio, "ap_distritos.parquet"))

# Lendo a linha do município
municipio <- read_parquet(file.path(diretorio_territorio, "municipio.parquet"))

# Lendo os atributos das zonas da OD
od_zonas <- read_parquet(file.path(diretorio_territorio, "od_zonas.parquet"))

# Lendo os atributos dos distritos da OD
od_distritos <- read_parquet(file.path(diretorio_territorio, "od_distritos.parquet"))

# Lendo a geometria dos distritos da OD
od_distritos_geometria <- ler_geoparquet(file.path(diretorio_territorio, "od_distritos_geometria.parquet"))


################################################################################
##### IV. Distritos e setores
################################################################################

# Resumindo nome, área e número de setores de cada distrito pelos setores
distritos_atributos <- setores |>
  group_by(codigo_distrito) |>
  summarise(
    nome_distrito = first(nome_distrito),
    n_nomes       = n_distinct(nome_distrito),
    area_km2      = sum(area_km2),
    n_setores     = n(),
    .groups       = "drop"
  )

# Interrompendo se algum código de distrito tiver mais de um nome
stopifnot(all(distritos_atributos$n_nomes == 1))

# Juntando os atributos à geometria dos distritos
distritos <- distritos_geometria |>
  inner_join(
    distritos_atributos |> select(-n_nomes),
    by = "codigo_distrito"
  ) |>
  mutate(
    # Nome para rótulos, com os acentos que faltam no IBGE (decisão do usuário;
    # os códigos e o nome oficial ficam como no IBGE)
    nome_rotulo = recode(
      nome_distrito,
      "Cangaiba"     = "Cangaíba",
      "Cidade Lider" = "Cidade Líder"
    )
  ) |>
  select(
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    area_km2,
    n_setores,
    geometry
  ) |>
  arrange(codigo_distrito)

# Ligando cada setor ao distrito pelo código do próprio setor
setor_distrito <- setores |>
  select(
    codigo_setor,
    codigo_distrito
  ) |>
  arrange(codigo_setor)


################################################################################
##### V. Áreas de ponderação
################################################################################

# Ordenando os distritos de cada AP pelo número de setores
ap_distrito_relacoes <- ap_distritos |>
  group_by(codigo_area_ponderacao) |>
  mutate(
    # Total de setores da AP e fração de cada distrito
    n_setores_ap   = sum(n_setores),
    fracao_setores = n_setores / n_setores_ap,
    # Posição do distrito na AP (1 = maioria de setores)
    posto          = min_rank(desc(n_setores)),
    n_distritos_ap = n()
  ) |>
  ungroup()

# Identificando APs com empate na maioria de setores
empates_ap <- ap_distrito_relacoes |>
  filter(posto == 1) |>
  count(codigo_area_ponderacao) |>
  filter(n > 1)

# Interrompendo se houver empate, porque a regra não decide a atribuição
if (nrow(empates_ap) > 0) {
  stop("APs com empate na maioria de setores: ", paste(empates_ap$codigo_area_ponderacao, collapse = ", "))
}

# Marcando o distrito atribuído a cada AP (opção 1: maioria de setores)
ap_distrito_relacoes <- ap_distrito_relacoes |>
  mutate(
    atribuido = posto == 1
  ) |>
  select(
    codigo_area_ponderacao,
    codigo_distrito,
    nome_distrito,
    n_setores,
    n_setores_ap,
    fracao_setores,
    atribuido
  ) |>
  arrange(codigo_area_ponderacao, desc(n_setores))

# Descrevendo os distritos não atribuídos de cada AP mista
ap_outros_distritos <- ap_distrito_relacoes |>
  filter(!atribuido) |>
  group_by(codigo_area_ponderacao) |>
  summarise(
    outros_distritos = paste0(
      nome_distrito, " (", n_setores, if_else(n_setores == 1, " setor)", " setores)"),
      collapse = "; "
    ),
    .groups          = "drop"
  )

# Criando a tabela com um distrito por AP e a nota das APs mistas
ap_distrito <- ap_distrito_relacoes |>
  filter(atribuido) |>
  left_join(ap_outros_distritos, by = "codigo_area_ponderacao") |>
  mutate(
    # AP com setores em mais de um distrito
    ap_mista = !is.na(outros_distritos),
    # Nota descritiva da atribuição
    nota     = if_else(
      ap_mista,
      paste0(
        "AP atribuída a ", nome_distrito, " (", n_setores, " de ", n_setores_ap,
        " setores); fora dela: ", outros_distritos, "."
      ),
      NA_character_
    )
  ) |>
  select(
    codigo_area_ponderacao,
    codigo_distrito,
    nome_distrito,
    n_setores_distrito = n_setores,
    n_setores_ap,
    fracao_setores,
    ap_mista,
    outros_distritos,
    nota
  ) |>
  arrange(codigo_area_ponderacao)


################################################################################
##### VI. Distritos da OD e do IBGE
################################################################################

# Projetando os distritos do IBGE no CRS da OD
distritos_projetados <- distritos |>
  st_transform(epsg_projetado) |>
  st_make_valid() |>
  mutate(
    area_ibge_m2 = as.numeric(st_area(geometry))
  )

# Preparando os distritos da OD com nome e área
od_distritos_projetados <- od_distritos_geometria |>
  st_make_valid() |>
  left_join(od_distritos, by = "codigo_distrito_od") |>
  mutate(
    area_od_m2 = as.numeric(st_area(geometry))
  )

# Cruzando as geometrias e medindo cada sobreposição (os códigos valem para
# toda a geometria, o que se declara por st_set_agr)
sobreposicoes <- st_intersection(
  x = od_distritos_projetados |> select(codigo_distrito_od) |> st_set_agr("constant"),
  y = distritos_projetados |> select(codigo_distrito) |> st_set_agr("constant")
) |>
  mutate(
    area_intersecao_m2 = as.numeric(st_area(geometry))
  ) |>
  st_drop_geometry() |>
  as_tibble()

# Escolhendo, para cada distrito da OD, o distrito do IBGE com maior sobreposição
od_distrito_ibge <- sobreposicoes |>
  group_by(codigo_distrito_od) |>
  slice_max(area_intersecao_m2, n = 1, with_ties = FALSE) |>
  ungroup() |>
  # Acrescentando nomes e áreas
  left_join(
    od_distritos_projetados |>
      st_drop_geometry() |>
      select(codigo_distrito_od, nome_distrito_od = nome_distri, area_od_m2),
    by = "codigo_distrito_od"
  ) |>
  left_join(
    distritos_projetados |>
      st_drop_geometry() |>
      select(codigo_distrito, nome_distrito, area_ibge_m2),
    by = "codigo_distrito"
  ) |>
  mutate(
    # Fração da área do distrito da OD coberta pelo distrito do IBGE
    fracao_area_od   = area_intersecao_m2 / area_od_m2,
    # Fração da área do distrito do IBGE coberta pelo distrito da OD
    fracao_area_ibge = area_intersecao_m2 / area_ibge_m2
  ) |>
  select(
    codigo_distrito_od,
    nome_distrito_od,
    codigo_distrito,
    nome_distrito,
    area_od_m2,
    area_ibge_m2,
    area_intersecao_m2,
    fracao_area_od,
    fracao_area_ibge
  ) |>
  arrange(as.integer(codigo_distrito_od))

# Ligando cada zona ao distrito do IBGE pelo distrito oficial da OD
zona_distrito <- od_zonas |>
  transmute(
    codigo_zona_od,
    nome_zona,
    codigo_distrito_od = as.character(num_distrit),
    nome_distrito_od   = nome_distri
  ) |>
  left_join(
    od_distrito_ibge |> select(codigo_distrito_od, codigo_distrito, nome_distrito),
    by = "codigo_distrito_od"
  ) |>
  arrange(as.integer(codigo_zona_od))


################################################################################
##### VII. Comparador
################################################################################

# Mantendo a linha do Município de São Paulo como comparador
comparador_municipio <- municipio |>
  select(
    codigo_municipio,
    nome_municipio,
    area_km2,
    v0001,
    v0002
  )


################################################################################
##### VIII. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_territorio,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando os distritos com geometria (EPSG:4674)
write_parquet(
  x    = distritos,
  sink = file.path(diretorio_territorio, "distritos.parquet")
)

# Gravando a ligação setor → distrito
write_parquet(
  x    = setor_distrito,
  sink = file.path(diretorio_territorio, "setor_distrito.parquet")
)

# Gravando a ligação AP → distrito
write_parquet(
  x    = ap_distrito,
  sink = file.path(diretorio_territorio, "ap_distrito.parquet")
)

# Gravando a lista completa de distritos de cada AP
write_parquet(
  x    = ap_distrito_relacoes,
  sink = file.path(diretorio_territorio, "ap_distrito_relacoes.parquet")
)

# Gravando a correspondência entre distritos da OD e do IBGE
write_parquet(
  x    = od_distrito_ibge,
  sink = file.path(diretorio_territorio, "od_distrito_ibge.parquet")
)

# Gravando a ligação zona → distrito
write_parquet(
  x    = zona_distrito,
  sink = file.path(diretorio_territorio, "zona_distrito.parquet")
)

# Gravando o comparador municipal
write_parquet(
  x    = comparador_municipio,
  sink = file.path(diretorio_territorio, "comparador_municipio.parquet")
)

# Informando o fim do script
message("Ligações territoriais gravadas em ", diretorio_territorio)
