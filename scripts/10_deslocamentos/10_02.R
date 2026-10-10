################################################################################
##################################            ##################################
######## 10.02) PREPARAR OS INDICADORES E AS MATRIZES DOS DESLOCAMENTOS ########
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

# Definindo o diretório dos extratos e das saídas do grupo
diretorio_grupo <- caminho_data("10_deslocamentos")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("10_deslocamentos")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983

# Descrevendo os tipos de viagem da OD
tipos_viagem <- tibble(
  tipo_codigo = 1:4,
  tipo        = c("coletivo", "individual", "a_pe", "bicicleta"),
  rotulo_tipo = c("Coletivo", "Individual", "A pé", "Bicicleta")
)

# Descrevendo os três blocos da OD usados nos indicadores (não se somam)
blocos <- tibble(
  bloco        = c("origem", "ct_residencia", "ct_destino"),
  prefixo      = c("origem", "casa_trabalho_residencia", "casa_trabalho_destino"),
  rotulo_bloco = c(
    "Todas as viagens com origem no distrito",
    "Ida ao trabalho de quem mora no distrito",
    "Ida ao trabalho de quem trabalha no distrito"
  ),
  campo_registros = c(
    "origem_viagens_n_registros",
    "casa_trabalho_residencia_viagens_n_registros",
    "casa_trabalho_destino_viagens_n_registros"
  )
)

# Acrescentando o bloco de destino, usado nas chegadas por motivo, aos blocos
# que recebem avisos de amostra menor
blocos_avisos <- bind_rows(
  blocos,
  tibble(
    bloco           = "destino",
    prefixo         = "destino",
    rotulo_bloco    = "Viagens com destino no distrito",
    campo_registros = "destino_viagens_n_registros"
  )
)

# Definindo os motivos no destino usados nas chegadas: trabalho (indústria,
# comércio e serviços), escola/educação e residência (volta para casa)
motivos_trabalho  <- 1:3
motivo_escola     <- 4
motivo_residencia <- 8


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desmembrar uma lista de zonas de destino (ou de
# residência) por zona, mantendo os registros e as estimativas de cada par
desmembrar_pares <- function(zonas, campo, nome_zona_par) {

  # Desmembrando a lista e renomeando a zona do par
  zonas |>
    select(codigo_zona_od, all_of(campo)) |>
    unnest(all_of(campo)) |>
    select(
      codigo_zona_od,
      zona_par = codigo_categoria,
      n_registros,
      estimativa
    ) |>
    rename_with(
      .fn   = ~ nome_zona_par,
      .cols = zona_par
    )
}

# Criando uma função para somar os campos numéricos das zonas de cada grupo; os
# campos vazios (zona sem registros no bloco) ficam fora da soma
somar_zonas <- function(zonas_agrupadas) {

  # Somando os registros, as estimativas e os pesos
  zonas_agrupadas |>
    summarise(
      n_zonas = n(),
      across(
        .cols = where(is.numeric),
        .fns  = ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

# Criando uma função para somar os dois sentidos de cada par (ida e volta numa
# linha só) e separar os fluxos dentro da própria unidade
somar_sentidos <- function(pares, origem, destino) {

  # Ordenando o par pelos códigos e somando os dois sentidos
  pares |>
    mutate(
      unidade_a = pmin(.data[[origem]], .data[[destino]]),
      unidade_b = pmax(.data[[origem]], .data[[destino]])
    ) |>
    group_by(unidade_a, unidade_b) |>
    summarise(
      n_registros = sum(n_registros),
      viagens     = sum(estimativa),
      .groups     = "drop"
    ) |>
    mutate(
      interno = unidade_a == unidade_b
    )
}

# Criando uma função para agregar nas unidades uma matriz de pares de zonas
# (zona_a → zona_b), mantendo o sentido
agregar_unidades <- function(pares, zona_a, zona_b, unidade_a, unidade_b) {

  # Montando a ligação zona → unidade com os nomes de cada ponta
  ligacao <- zona_unidade |>
    select(codigo_zona_od, codigo_unidade)

  # Ligando as duas zonas às unidades e somando os pares
  pares |>
    left_join(
      ligacao |> rename_with(~ c(zona_a, unidade_a)),
      by = zona_a
    ) |>
    left_join(
      ligacao |> rename_with(~ c(zona_b, unidade_b)),
      by = zona_b
    ) |>
    group_by(across(all_of(c(unidade_a, unidade_b)))) |>
    summarise(
      n_registros = sum(n_registros),
      estimativa  = sum(estimativa),
      .groups     = "drop"
    )
}

# Criando uma função para montar os fluxos de um motivo (ida e volta somadas)
# nas duas escalas: zona para zona e unidades agregadas
montar_fluxos <- function(pares, zona_a, zona_b, motivo) {

  # Somando os dois sentidos entre zonas, ordenadas pelo número
  fluxos_zonas <- pares |>
    mutate(
      numero_a = as.integer(.data[[zona_a]]),
      numero_b = as.integer(.data[[zona_b]])
    ) |>
    somar_sentidos("numero_a", "numero_b") |>
    mutate(
      unidade_a = as.character(unidade_a),
      unidade_b = as.character(unidade_b),
      escala    = "zonas"
    )

  # Somando os dois sentidos entre unidades
  fluxos_unidades <- pares |>
    agregar_unidades(zona_a, zona_b, "unidade_1", "unidade_2") |>
    somar_sentidos("unidade_1", "unidade_2") |>
    mutate(
      escala = "unidades"
    )

  # Juntando as duas escalas com o motivo
  bind_rows(fluxos_zonas, fluxos_unidades) |>
    mutate(
      motivo = motivo
    ) |>
    rename(fluxo = viagens) |>
    select(
      motivo,
      escala,
      unidade_a,
      unidade_b,
      interno,
      n_registros,
      fluxo
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo as viagens e a matriz residência → trabalho das 527 zonas
viagens <- read_parquet(file.path(diretorio_grupo, "viagens.parquet"))
pessoas <- read_parquet(file.path(diretorio_grupo, "pessoas.parquet"))

# Lendo as geometrias das zonas e dos municípios da OD
zonas_geometria      <- ler_geoparquet(file.path(diretorio_grupo, "zonas_geometria.parquet"))
municipios_geometria <- ler_geoparquet(file.path(diretorio_grupo, "municipios_geometria.parquet"))

# Lendo a ligação zona → distrito da capital
zona_distrito <- read_parquet(file.path(diretorio_territorio, "zona_distrito.parquet"))

# Lendo os distritos com geometria
distritos <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet")) |>
  st_transform(epsg_projetado)


################################################################################
##### V. Unidades dos fluxos
################################################################################

# Atribuindo cada zona à unidade agregada: o distrito do IBGE na capital e o
# município nos demais municípios da RMSP
zona_unidade <- viagens |>
  select(codigo_zona_od, codigo_municipio, nome_municipio) |>
  left_join(
    zona_distrito |> select(codigo_zona_od, codigo_distrito, nome_distrito),
    by = "codigo_zona_od"
  ) |>
  mutate(
    tipo_unidade   = if_else(codigo_municipio == codigo_municipio_sp, "distrito", "municipio"),
    codigo_unidade = if_else(tipo_unidade == "distrito", codigo_distrito, codigo_municipio),
    nome_unidade   = if_else(tipo_unidade == "distrito", nome_distrito, nome_municipio)
  ) |>
  select(
    codigo_zona_od,
    codigo_municipio,
    tipo_unidade,
    codigo_unidade,
    nome_unidade
  )

# Obtendo um ponto interno de cada distrito da capital e de cada município
unidades <- bind_rows(
  distritos |>
    transmute(
      codigo_unidade = codigo_distrito,
      nome_unidade   = nome_rotulo,
      tipo_unidade   = "distrito"
    ),
  municipios_geometria |>
    filter(codigo_municipio != codigo_municipio_sp) |>
    transmute(
      codigo_unidade = codigo_municipio,
      nome_unidade   = nome_municipio,
      tipo_unidade   = "municipio"
    )
) |>
  # Declarando os atributos constantes na geometria, para o ponto interno
  st_set_agr("constant") |>
  st_point_on_surface()
unidades <- unidades |>
  mutate(
    x = st_coordinates(unidades)[, 1],
    y = st_coordinates(unidades)[, 2]
  ) |>
  st_drop_geometry() |>
  as_tibble()

# Obtendo um ponto interno de cada zona
zonas_pontos <- zonas_geometria |>
  st_set_agr("constant") |>
  st_point_on_surface()
zonas_pontos <- zonas_pontos |>
  mutate(
    x = st_coordinates(zonas_pontos)[, 1],
    y = st_coordinates(zonas_pontos)[, 2]
  ) |>
  st_drop_geometry() |>
  as_tibble() |>
  left_join(zona_unidade, by = "codigo_zona_od")


################################################################################
##### VI. Matrizes e fluxos: todas as viagens, trabalho e escola
################################################################################

# Desmembrando a matriz OD de todas as viagens (zona de origem → zona de destino)
pares_zonas <- desmembrar_pares(viagens, "origem_zona_d_distribuicao", "codigo_zona_destino") |>
  rename(codigo_zona_origem = codigo_zona_od)

# Agregando a matriz OD nas unidades (sentido origem → destino)
matriz_unidades <- pares_zonas |>
  agregar_unidades("codigo_zona_origem", "codigo_zona_destino", "unidade_origem", "unidade_destino")

# Desmembrando a matriz residência → trabalho (pela zona de trabalho, com a
# zona de residência de cada trabalhador)
pares_trabalho <- desmembrar_pares(pessoas, "trabalho_zona_residencia_distribuicao", "codigo_zona_residencia") |>
  rename(codigo_zona_trabalho = codigo_zona_od)

# Desmembrando a matriz residência → escola (pela zona de residência, com a
# zona da escola); o código 0 (quem não estuda) e as escolas fora das 527
# zonas ficam de fora
pares_escola <- desmembrar_pares(pessoas, "zona_esc_distribuicao", "codigo_zona_escola") |>
  rename(codigo_zona_residencia = codigo_zona_od) |>
  filter(as.integer(codigo_zona_escola) %in% 1:527)

# Agregando a matriz residência → trabalho nas unidades, com as duas leituras:
# para onde vão os moradores de cada unidade e de onde vêm os trabalhadores
matriz_residencia_trabalho <- pares_trabalho |>
  agregar_unidades("codigo_zona_residencia", "codigo_zona_trabalho", "unidade_residencia", "unidade_trabalho") |>
  group_by(unidade_residencia) |>
  mutate(pct_dos_moradores = 100 * estimativa / sum(estimativa)) |>
  group_by(unidade_trabalho) |>
  mutate(pct_dos_trabalhadores = 100 * estimativa / sum(estimativa)) |>
  ungroup()

# Agregando a matriz residência → escola nas unidades, com as duas leituras
matriz_residencia_escola <- pares_escola |>
  agregar_unidades("codigo_zona_residencia", "codigo_zona_escola", "unidade_residencia", "unidade_escola") |>
  group_by(unidade_residencia) |>
  mutate(pct_dos_moradores = 100 * estimativa / sum(estimativa)) |>
  group_by(unidade_escola) |>
  mutate(pct_dos_estudantes = 100 * estimativa / sum(estimativa)) |>
  ungroup()

# Montando os fluxos (ida e volta somadas) dos três motivos nas duas escalas:
# viagens de um dia útil, trabalhadores e estudantes
fluxos <- bind_rows(
  montar_fluxos(pares_zonas, "codigo_zona_origem", "codigo_zona_destino", "todas"),
  montar_fluxos(pares_trabalho, "codigo_zona_residencia", "codigo_zona_trabalho", "trabalho"),
  montar_fluxos(pares_escola, "codigo_zona_residencia", "codigo_zona_escola", "escola")
)


################################################################################
##### VII. Indicadores por distrito e do município
################################################################################

# Ligando as zonas da capital aos distritos
viagens_capital <- viagens |>
  inner_join(
    zona_distrito |> select(codigo_zona_od, codigo_distrito),
    by = "codigo_zona_od"
  ) |>
  select(-where(is.list))

# Somando as zonas por distrito e para a capital
totais <- bind_rows(
  viagens_capital |>
    group_by(codigo_distrito) |>
    somar_zonas(),
  viagens_capital |>
    somar_zonas() |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Calculando as viagens internas a cada distrito (origem e destino no mesmo
# distrito); na capital, a soma das viagens internas aos distritos
internas <- matriz_unidades |>
  filter(unidade_origem %in% distritos$codigo_distrito) |>
  group_by(codigo_distrito = unidade_origem) |>
  summarise(
    viagens_internas = sum(estimativa[unidade_destino == unidade_origem]),
    viagens_matriz   = sum(estimativa),
    .groups          = "drop"
  )
internas <- bind_rows(
  internas,
  internas |>
    summarise(across(where(is.numeric), sum)) |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Somando os moradores (estimativa da OD) por distrito e para a capital
moradores <- pessoas |>
  inner_join(
    zona_distrito |> select(codigo_zona_od, codigo_distrito),
    by = "codigo_zona_od"
  )
moradores <- bind_rows(
  moradores |>
    group_by(codigo_distrito) |>
    summarise(moradores = sum(pessoas_estimativa_total, na.rm = TRUE), .groups = "drop"),
  moradores |>
    summarise(moradores = sum(pessoas_estimativa_total, na.rm = TRUE)) |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Calculando os indicadores em formato largo
deslocamentos_largo <- totais |>
  left_join(internas, by = "codigo_distrito") |>
  left_join(moradores, by = "codigo_distrito") |>
  mutate(
    # Chegadas por motivo no destino: trabalho, escola e todas menos a volta
    # para casa
    chegadas_trabalho            = rowSums(pick(sprintf("destino_motivo_d_cat_codigo_%d_estimativa", motivos_trabalho))),
    chegadas_escola              = .data[[sprintf("destino_motivo_d_cat_codigo_%d_estimativa", motivo_escola)]],
    chegadas_sem_volta           = destino_motivo_d_peso_observado -
      .data[[sprintf("destino_motivo_d_cat_codigo_%d_estimativa", motivo_residencia)]],
    chegadas_trabalho_por_morador = chegadas_trabalho / moradores,
    chegadas_escola_por_morador   = chegadas_escola / moradores,
    pct_chegadas_trabalho         = 100 * chegadas_trabalho / chegadas_sem_volta,
    pct_chegadas_escola           = 100 * chegadas_escola / chegadas_sem_volta
  ) |>
  mutate(
    viagens_origem       = origem_viagens_estimativa_total,
    pct_coletivo         = 100 * origem_tipvg_cat_codigo_1_estimativa / origem_tipvg_peso_observado,
    pct_individual       = 100 * origem_tipvg_cat_codigo_2_estimativa / origem_tipvg_peso_observado,
    pct_a_pe             = 100 * origem_tipvg_cat_codigo_3_estimativa / origem_tipvg_peso_observado,
    pct_bicicleta        = 100 * origem_tipvg_cat_codigo_4_estimativa / origem_tipvg_peso_observado,
    duracao_media        = origem_duracao_soma / origem_duracao_peso_observado,
    pct_viagens_internas = 100 * viagens_internas / viagens_matriz,
    nivel                = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito")
  ) |>
  left_join(
    st_drop_geometry(distritos) |> select(codigo_distrito, nome_distrito, nome_rotulo),
    by = "codigo_distrito"
  ) |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Calculando os avisos de cada bloco: amostra menor e sem valor (o município
# não recebe nenhum)
avisos <- deslocamentos_largo |>
  select(codigo_distrito, nivel, all_of(blocos_avisos$campo_registros)) |>
  pivot_longer(
    cols      = all_of(blocos_avisos$campo_registros),
    names_to  = "campo_registros",
    values_to = "registros"
  ) |>
  left_join(blocos_avisos |> select(bloco, campo_registros), by = "campo_registros") |>
  mutate(
    amostra_menor = nivel == "distrito" & registros < minimo_registros_od,
    sem_valor     = nivel == "distrito" & registros < piso_registros_od
  ) |>
  select(codigo_distrito, bloco, registros, amostra_menor, sem_valor)

# Descrevendo os indicadores do grupo, com o bloco de cada um
indicadores <- tribble(
  ~indicador,             ~bloco,    ~rotulo,                                   ~unidade,                     ~comparacao, ~casas_decimais, ~descricao,
  "viagens_origem",       "origem",  "Viagens com origem no distrito",          "viagens por dia útil",       "relativa",  0L,             "Soma de origem_viagens_estimativa_total das zonas do distrito (FE_VIA).",
  "pct_coletivo",         "origem",  "Viagens coletivas",                       "% das viagens",              "pontos",    1L,             "Tipo de viagem coletivo sobre o peso observado do tipo, viagens com origem no distrito.",
  "pct_individual",       "origem",  "Viagens individuais motorizadas",         "% das viagens",              "pontos",    1L,             "Tipo de viagem individual sobre o peso observado do tipo, viagens com origem no distrito.",
  "pct_a_pe",             "origem",  "Viagens a pé",                            "% das viagens",              "pontos",    1L,             "Tipo de viagem a pé sobre o peso observado do tipo, viagens com origem no distrito.",
  "pct_bicicleta",        "origem",  "Viagens de bicicleta",                    "% das viagens",              "pontos",    1L,             "Tipo de viagem bicicleta sobre o peso observado do tipo, viagens com origem no distrito.",
  "duracao_media",        "origem",  "Duração média das viagens",               "minutos",                    "relativa",  1L,             "Soma ponderada da duração sobre o peso observado, viagens com origem no distrito.",
  "pct_viagens_internas", "origem",  "Viagens internas ao distrito",            "% das viagens",              "pontos",    1L,             "Viagens com origem e destino no distrito sobre as viagens com origem no distrito; na capital, a soma das viagens internas aos distritos.",
  "moradores",            "destino", "Moradores (estimativa da OD)",            "moradores",                  "relativa",  0L,             "Soma de pessoas_estimativa_total das zonas do distrito (FE_PESS).",
  "chegadas_trabalho",    "destino", "Viagens que chegam para trabalhar",       "viagens por dia útil",       "relativa",  0L,             "Viagens com destino no distrito e motivo no destino trabalho (indústria, comércio e serviços).",
  "chegadas_escola",      "destino", "Viagens que chegam para estudar",         "viagens por dia útil",       "relativa",  0L,             "Viagens com destino no distrito e motivo no destino escola/educação.",
  "chegadas_trabalho_por_morador", "destino", "Viagens que chegam para trabalhar, por morador", "viagens por dia útil por morador", "relativa", 2L, "Viagens que chegam ao distrito para trabalhar sobre os moradores do distrito, ambos da OD.",
  "chegadas_escola_por_morador",   "destino", "Viagens que chegam para estudar, por morador",   "viagens por dia útil por morador", "relativa", 2L, "Viagens que chegam ao distrito para estudar sobre os moradores do distrito, ambos da OD.",
  "pct_chegadas_trabalho", "destino", "Trabalho entre as viagens que chegam",   "% das viagens que chegam, sem as voltas para casa", "pontos", 1L, "Viagens que chegam para trabalhar sobre as viagens com destino no distrito, sem as de motivo residência.",
  "pct_chegadas_escola",   "destino", "Escola entre as viagens que chegam",     "% das viagens que chegam, sem as voltas para casa", "pontos", 1L, "Viagens que chegam para estudar sobre as viagens com destino no distrito, sem as de motivo residência."
) |>
  mutate(
    ordem = row_number()
  )

# Passando os indicadores para o formato longo, com os avisos do bloco e sem
# valor abaixo do piso
deslocamentos <- deslocamentos_largo |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    all_of(indicadores$indicador)
  ) |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  ) |>
  left_join(indicadores |> select(indicador, bloco), by = "indicador") |>
  left_join(avisos, by = c("codigo_distrito", "bloco")) |>
  mutate(
    valor = if_else(
      sem_valor & !indicador %in% c("viagens_origem", "moradores", "chegadas_trabalho", "chegadas_escola"),
      NA_real_,
      valor
    )
  )

# Montando os tipos de viagem dos três blocos, por distrito e para a capital
tipos <- blocos |>
  select(bloco, prefixo, rotulo_bloco) |>
  pmap(function(bloco, prefixo, rotulo_bloco) {

    # Calculando a proporção de cada tipo sobre o peso observado do bloco
    deslocamentos_largo |>
      select(
        nivel_territorial = nivel,
        codigo_distrito,
        nome_rotulo,
        peso_observado = all_of(paste0(prefixo, "_tipvg_peso_observado")),
        all_of(sprintf("%s_tipvg_cat_codigo_%d_estimativa", prefixo, 1:4))
      ) |>
      pivot_longer(
        cols         = ends_with("_estimativa"),
        names_to     = "tipo_codigo",
        names_pattern = ".*_cat_codigo_(\\d)_estimativa",
        values_to    = "viagens"
      ) |>
      mutate(
        tipo_codigo  = as.integer(tipo_codigo),
        pct          = 100 * viagens / peso_observado,
        bloco        = bloco,
        rotulo_bloco = rotulo_bloco
      )
  }) |>
  list_rbind() |>
  left_join(tipos_viagem, by = "tipo_codigo") |>
  left_join(avisos, by = c("codigo_distrito", "bloco")) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_rotulo,
    bloco,
    rotulo_bloco,
    registros,
    amostra_menor,
    sem_valor,
    tipo_codigo,
    tipo,
    rotulo_tipo,
    viagens,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, bloco, tipo_codigo)


################################################################################
##### VIII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(deslocamentos_largo, nivel == "distrito")

# Somando os tipos de viagem por zona e bloco (campo vazio conta zero)
soma_tipos <- viagens |>
  select(-where(is.list)) |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |>
  mutate(
    origem        = rowSums(pick(sprintf("origem_tipvg_cat_codigo_%d_estimativa", 1:4))),
    ct_residencia = rowSums(pick(sprintf("casa_trabalho_residencia_tipvg_cat_codigo_%d_estimativa", 1:4))),
    ct_destino    = rowSums(pick(sprintf("casa_trabalho_destino_tipvg_cat_codigo_%d_estimativa", 1:4))),
    motivos       = rowSums(pick(sprintf("destino_motivo_d_cat_codigo_%d_estimativa", 1:11)))
  )

# Somando a matriz residência → escola completa (com quem não estuda e as
# escolas fora das 527 zonas)
soma_escola <- desmembrar_pares(pessoas, "zona_esc_distribuicao", "codigo_zona_escola") |>
  pull(estimativa) |>
  sum()

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "527 zonas da OD na RMSP, 343 na capital",
    "Zonas da capital ligadas a um distrito",
    "134 unidades de fluxo (96 distritos e 38 municípios), todas com ponto",
    "Zonas de destino da matriz OD entre 1 e 527",
    "Matriz OD soma o peso observado do destino e o total de viagens com origem",
    "Matriz OD agregada nas unidades preserva o total de viagens",
    "Matriz residência → trabalho soma o total de trabalhadores da RMSP",
    "Matriz residência → escola completa soma o peso observado da zona da escola",
    "Fluxos dos três motivos preservam o total de cada matriz nas duas escalas",
    "Motivos no destino somam o peso observado em cada zona",
    "Tipos de viagem somam o peso observado em cada zona e bloco",
    "Proporções entre 0 e 100 e tipos somando 100% em cada bloco com viagens",
    "Duração média dos distritos entre 5 e 120 minutos"
  ),
  ok          = c(
    nrow(viagens) == 527 && sum(viagens$codigo_municipio == codigo_municipio_sp) == 343,
    sum(zona_unidade$tipo_unidade == "distrito") == 343 && all(!is.na(zona_unidade$codigo_unidade)),
    nrow(unidades) == 134 && all(zona_unidade$codigo_unidade %in% unidades$codigo_unidade),
    all(as.integer(pares_zonas$codigo_zona_destino) %in% 1:527),
    abs(sum(pares_zonas$estimativa) - sum(viagens$origem_zona_d_peso_observado, na.rm = TRUE)) < 1 &&
      abs(sum(pares_zonas$estimativa) - sum(viagens$origem_viagens_estimativa_total, na.rm = TRUE)) < 1,
    abs(sum(matriz_unidades$estimativa) - sum(pares_zonas$estimativa)) < 1,
    abs(sum(matriz_residencia_trabalho$estimativa) - sum(pessoas$trabalho_pessoas_estimativa_total, na.rm = TRUE)) < 1,
    abs(soma_escola - sum(pessoas$zona_esc_peso_observado, na.rm = TRUE)) < 1,
    fluxos |>
      group_by(motivo, escala) |>
      summarise(total = sum(fluxo), .groups = "drop") |>
      left_join(
        tibble(
          motivo     = c("todas", "trabalho", "escola"),
          esperado   = c(sum(pares_zonas$estimativa), sum(pares_trabalho$estimativa), sum(pares_escola$estimativa))
        ),
        by = "motivo"
      ) |>
      summarise(ok = all(abs(total - esperado) < 1)) |>
      pull(ok),
    all(abs(soma_tipos$motivos - soma_tipos$destino_motivo_d_peso_observado) < 1e-6),
    all(abs(soma_tipos$origem - soma_tipos$origem_tipvg_peso_observado) < 1e-6) &&
      all(abs(soma_tipos$ct_residencia - soma_tipos$casa_trabalho_residencia_tipvg_peso_observado) < 1e-6) &&
      all(abs(soma_tipos$ct_destino - soma_tipos$casa_trabalho_destino_tipvg_peso_observado) < 1e-6),
    all(tipos$pct[!is.na(tipos$pct)] >= 0 & tipos$pct[!is.na(tipos$pct)] <= 100) &&
      tipos |>
        filter(!is.na(pct)) |>
        group_by(codigo_distrito, bloco) |>
        summarise(s = sum(pct), .groups = "drop") |>
        pull(s) |>
        (\(x) all(abs(x - 100) < 1e-6))(),
    all(between(largo_distritos$duracao_media, 5, 120))
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Mostrando os avisos por bloco
message(paste(
  avisos |>
    group_by(bloco) |>
    summarise(
      texto = paste0(
        first(bloco), ": mínimo de ", min(registros[codigo_distrito != codigo_municipio_sp]),
        " registros; ", sum(amostra_menor), " com aviso; sem valor: ",
        paste(deslocamentos_largo$nome_rotulo[match(codigo_distrito[sem_valor], deslocamentos_largo$codigo_distrito)], collapse = ", ")
      )
    ) |>
    pull(texto),
  collapse = "\n"
))

# Interrompendo se alguma conferência falhar
if (!all(conferencias$ok)) {
  stop("Conferências com falha: ", paste(conferencias$conferencia[!conferencias$ok], collapse = "; "))
}


################################################################################
##### IX. Exportação
################################################################################

# Criando os diretórios de dados e de resultados do grupo
dir.create(
  path         = diretorio_grupo,
  recursive    = TRUE,
  showWarnings = FALSE
)
dir.create(
  path         = diretorio_resultados,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando os indicadores, a descrição e os tipos de viagem
write_parquet(
  x    = deslocamentos,
  sink = file.path(diretorio_grupo, "deslocamentos.parquet")
)
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)
write_parquet(
  x    = tipos,
  sink = file.path(diretorio_grupo, "tipos.parquet")
)

# Gravando as unidades e os pontos das zonas
write_parquet(
  x    = unidades,
  sink = file.path(diretorio_grupo, "unidades.parquet")
)
write_parquet(
  x    = zonas_pontos,
  sink = file.path(diretorio_grupo, "zonas_pontos.parquet")
)

# Gravando as matrizes e os fluxos dos três motivos
write_parquet(
  x    = matriz_unidades,
  sink = file.path(diretorio_grupo, "matriz_viagens_unidades.parquet")
)
write_parquet(
  x    = fluxos,
  sink = file.path(diretorio_grupo, "fluxos.parquet")
)
write_parquet(
  x    = matriz_residencia_trabalho,
  sink = file.path(diretorio_grupo, "matriz_residencia_trabalho.parquet")
)

# Montando os nomes das unidades para as tabelas
nomes_unidades <- unidades |>
  select(codigo_unidade, nome_unidade, tipo_unidade)

# Gravando a tabela para leitura
write_xlsx(
  x    = list(
    indicadores = deslocamentos_largo |>
      select(
        codigo_distrito,
        nome_distrito,
        n_zonas,
        all_of(blocos_avisos$campo_registros),
        all_of(indicadores$indicador)
      ) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ),
    tipos_viagem = tipos |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_rotulo, rotulo_bloco, registros, rotulo_tipo, viagens, pct),
    viagens_unidades = matriz_unidades |>
      left_join(nomes_unidades |> rename_with(~ paste0(.x, "_origem")), by = c("unidade_origem" = "codigo_unidade_origem")) |>
      left_join(nomes_unidades |> rename_with(~ paste0(.x, "_destino")), by = c("unidade_destino" = "codigo_unidade_destino")) |>
      select(unidade_origem, nome_unidade_origem, unidade_destino, nome_unidade_destino, n_registros, viagens = estimativa) |>
      arrange(desc(viagens)),
    viagens_unidades_larga = matriz_unidades |>
      left_join(nomes_unidades |> select(unidade_origem = codigo_unidade, origem = nome_unidade), by = "unidade_origem") |>
      left_join(nomes_unidades |> select(unidade_destino = codigo_unidade, destino = nome_unidade), by = "unidade_destino") |>
      arrange(unidade_destino) |>
      select(unidade_origem, origem, destino, estimativa) |>
      pivot_wider(names_from = destino, values_from = estimativa, values_fill = 0) |>
      arrange(unidade_origem),
    viagens_zonas = pares_zonas |>
      rename(viagens = estimativa) |>
      arrange(as.integer(codigo_zona_origem), as.integer(codigo_zona_destino)),
    residencia_trabalho = matriz_residencia_trabalho |>
      left_join(nomes_unidades |> select(unidade_residencia = codigo_unidade, residencia = nome_unidade), by = "unidade_residencia") |>
      left_join(nomes_unidades |> select(unidade_trabalho = codigo_unidade, trabalho = nome_unidade), by = "unidade_trabalho") |>
      select(unidade_residencia, residencia, unidade_trabalho, trabalho, n_registros, trabalhadores = estimativa,
             pct_dos_moradores, pct_dos_trabalhadores) |>
      arrange(desc(trabalhadores)),
    residencia_escola = matriz_residencia_escola |>
      left_join(nomes_unidades |> select(unidade_residencia = codigo_unidade, residencia = nome_unidade), by = "unidade_residencia") |>
      left_join(nomes_unidades |> select(unidade_escola = codigo_unidade, escola = nome_unidade), by = "unidade_escola") |>
      select(unidade_residencia, residencia, unidade_escola, escola, n_registros, estudantes = estimativa,
             pct_dos_moradores, pct_dos_estudantes) |>
      arrange(desc(estudantes)),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: Metrô, Pesquisa Origem e Destino 2023, tabelas de viagens e de pessoas por zona; viagens de um dia útil (FE_VIA) e trabalhadores (FE_PESS).",
        "Unidades dos fluxos: as zonas da capital agregadas nos 96 distritos do IBGE e as dos demais 38 municípios da RMSP agregadas no município.",
        "Matriz de viagens pelo bloco de origem (zona de destino das viagens com origem em cada zona); os blocos de origem, destino e casa → trabalho não se somam.",
        "Residência → trabalho pelo bloco da zona do primeiro trabalho: trabalhadores residentes na RMSP com local fixo de trabalho.",
        "Residência → escola pela zona da escola de cada morador: estudantes com escola numa das 527 zonas (quem não estuda e as escolas fora da malha ficam de fora).",
        "Chegadas por motivo pelo bloco de destino (motivo no destino); a participação exclui as viagens de volta para casa.",
        paste0("Distritos com menos de ", minimo_registros_od, " registros no bloco recebem aviso de amostra menor; com menos de ",
               piso_registros_od, ", ficam sem valor nos indicadores do bloco."),
        "Recortes por destino ou local de trabalho não são estratos do desenho amostral da OD; estimativas sem margem de erro calculada."
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_deslocamentos.xlsx")
)

# Informando o fim do script
message("Indicadores e matrizes dos deslocamentos gravados em ", diretorio_grupo)
