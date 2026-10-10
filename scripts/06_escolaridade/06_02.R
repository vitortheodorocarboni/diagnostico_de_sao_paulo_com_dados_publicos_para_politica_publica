################################################################################
##################################            ##################################
################ 06.02) PREPARAR OS INDICADORES DA ESCOLARIDADE ################
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
diretorio_grupo <- caminho_data("06_escolaridade")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("06_escolaridade")

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Descrevendo os níveis de instrução de quem tem 25 anos ou mais
niveis_instrucao <- tibble(
  nivel_codigo = 1:5,
  nivel        = c("sem_instrucao", "fundamental", "medio", "superior", "nao_determinado"),
  rotulo_nivel = c(
    "Sem instrução e fundamental incompleto",
    "Fundamental completo e médio incompleto",
    "Médio completo e superior incompleto",
    "Superior completo",
    "Não determinado"
  ),
  campo        = sprintf("p0770_25_mais_cat_codigo_%d_estimativa", 1:5)
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar as estimativas das APs de cada grupo. As
# estimativas já vêm ponderadas pelo IBGE; somam-se as estimativas e as
# proporções são recalculadas a partir das somas, nunca pela média das
# proporções das APs.
somar_aps <- function(aps_agrupadas) {

  # Somando os registros, as estimativas e os pesos
  aps_agrupadas |>
    summarise(
      n_aps = n(),
      across(
        .cols = c(pessoas_n_registros, pessoas_estimativa_total, starts_with("p07")),
        .fns  = sum
      ),
      .groups = "drop"
    )
}

# Criando uma função para somar os campos publicados dos setores; as células
# com sigilo ficam fora da soma, sem imputação
somar_setores <- function(setores_agrupados) {

  # Somando os campos de cada setor
  setores_agrupados |>
    summarise(
      across(
        .cols = matches("^v0[0-9]+$"),
        .fns  = ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo as APs da capital
amostra <- read_parquet(file.path(diretorio_grupo, "amostra.parquet"))

# Lendo os setores da capital
setores <- read_parquet(file.path(diretorio_grupo, "setores.parquet"))

# Lendo a linha do município
municipio <- read_parquet(file.path(diretorio_grupo, "municipio.parquet"))

# Lendo as ausências dos campos do universo
ausencias <- read_parquet(file.path(diretorio_grupo, "ausencias.parquet"))

# Lendo a ligação AP → distrito
ap_distrito <- read_parquet(file.path(diretorio_territorio, "ap_distrito.parquet"))

# Lendo a ligação setor → distrito
setor_distrito <- read_parquet(file.path(diretorio_territorio, "setor_distrito.parquet"))

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
  ~indicador,             ~grupo,       ~rotulo,                                          ~unidade,                               ~comparacao, ~casas_decimais, ~descricao,
  "pct_sem_instrucao",    "instrucao",  "Sem instrução ou fundamental incompleto",        "% das pessoas de 25 anos ou mais",     "pontos",    1L,             "Estimativa de P0770 = 1 sobre o peso observado de quem tem 25 anos ou mais (amostra).",
  "pct_fundamental",      "instrucao",  "Fundamental completo e médio incompleto",        "% das pessoas de 25 anos ou mais",     "pontos",    1L,             "Estimativa de P0770 = 2 sobre o peso observado de quem tem 25 anos ou mais (amostra).",
  "pct_medio",            "instrucao",  "Médio completo e superior incompleto",           "% das pessoas de 25 anos ou mais",     "pontos",    1L,             "Estimativa de P0770 = 3 sobre o peso observado de quem tem 25 anos ou mais (amostra).",
  "pct_superior",         "instrucao",  "Superior completo",                              "% das pessoas de 25 anos ou mais",     "pontos",    1L,             "Estimativa de P0770 = 4 sobre o peso observado de quem tem 25 anos ou mais (amostra).",
  "pct_nao_determinado",  "instrucao",  "Instrução não determinada",                      "% das pessoas de 25 anos ou mais",     "pontos",    1L,             "Estimativa de P0770 = 5 sobre o peso observado de quem tem 25 anos ou mais (amostra).",
  "pct_medio_ou_mais",    "instrucao",  "Médio completo ou mais",                         "% das pessoas de 25 anos ou mais",     "pontos",    1L,             "Soma de P0770 = 3 e 4 sobre o peso observado de quem tem 25 anos ou mais (amostra).",
  "pessoas_25_mais",      "instrucao",  "Pessoas de 25 anos ou mais (estimativa)",        "pessoas",                              "relativa",  0L,             "Peso observado de P0770 entre quem tem 25 anos ou mais (amostra).",
  "pct_adequacao",        "frequencia", "Frequência escolar adequada à idade",            "% dos estudantes",                     "pontos",    1L,             "Estimativa de P0780 = adequado sobre adequado e não adequado (amostra); o não aplicável fica fora.",
  "taxa_alfabetizacao",   "universo",   "Taxa de alfabetização de 15 anos ou mais",       "% das pessoas de 15 anos ou mais",     "pontos",    1L,             "Pessoas alfabetizadas (v00748 a v00760) sobre as pessoas de 15 anos ou mais (v00644 a v00656), somas publicadas do universo.",
  "registros_amostra",    "qualidade",  "Registros de pessoas na amostra",                "registros",                            "relativa",  0L,             "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor."
) |>
  mutate(
    ordem = row_number()
  )


################################################################################
##### VI. Indicadores por distrito e do município
################################################################################

# Ligando as APs aos distritos pela regra da maioria de setores
amostra_distritos <- amostra |>
  inner_join(
    ap_distrito |> select(codigo_area_ponderacao, codigo_distrito),
    by = "codigo_area_ponderacao"
  )

# Somando as APs por distrito e para o município
totais_amostra <- bind_rows(
  amostra_distritos |>
    group_by(codigo_distrito) |>
    somar_aps(),
  amostra_distritos |>
    somar_aps() |>
    mutate(codigo_distrito = municipio$codigo_municipio)
)

# Somando os setores por distrito e para o município
setores_distritos <- setores |>
  inner_join(setor_distrito, by = "codigo_setor")
totais_universo <- bind_rows(
  setores_distritos |>
    group_by(codigo_distrito) |>
    somar_setores(),
  setores_distritos |>
    somar_setores() |>
    mutate(codigo_distrito = municipio$codigo_municipio)
) |>
  mutate(
    pessoas_15_mais = rowSums(pick(sprintf("v%05d", 644:656))),
    alfabetizadas   = rowSums(pick(sprintf("v%05d", 748:760)))
  )

# Calculando os indicadores em formato largo
escolaridade_largo <- totais_amostra |>
  left_join(
    totais_universo |> select(codigo_distrito, pessoas_15_mais, alfabetizadas),
    by = "codigo_distrito"
  ) |>
  mutate(
    # Base dos níveis de instrução: peso observado de quem tem 25 anos ou mais
    base_instrucao      = p0770_25_mais_peso_observado,
    pct_sem_instrucao   = 100 * p0770_25_mais_cat_codigo_1_estimativa / base_instrucao,
    pct_fundamental     = 100 * p0770_25_mais_cat_codigo_2_estimativa / base_instrucao,
    pct_medio           = 100 * p0770_25_mais_cat_codigo_3_estimativa / base_instrucao,
    pct_superior        = 100 * p0770_25_mais_cat_codigo_4_estimativa / base_instrucao,
    pct_nao_determinado = 100 * p0770_25_mais_cat_codigo_5_estimativa / base_instrucao,
    pct_medio_ou_mais   = pct_medio + pct_superior,
    pessoas_25_mais     = base_instrucao,
    # Adequação idade-série entre os estudantes (o não aplicável fica fora)
    pct_adequacao       = 100 * p0780_cat_codigo_1_estimativa /
      (p0780_cat_codigo_1_estimativa + p0780_cat_codigo_2_estimativa),
    # Alfabetização de 15 anos ou mais, pelo universo
    taxa_alfabetizacao  = 100 * alfabetizadas / pessoas_15_mais,
    # Registros e aviso de amostra menor (o município não recebe aviso)
    registros_amostra   = pessoas_n_registros,
    nivel               = if_else(codigo_distrito == municipio$codigo_municipio, "municipio", "distrito"),
    amostra_menor       = nivel == "distrito" & registros_amostra < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  )

# Mantendo as colunas de saída
escolaridade_largo <- escolaridade_largo |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    n_aps,
    amostra_menor,
    all_of(indicadores$indicador),
    pessoas_estimativa_total,
    p0770_25_mais_peso_ausente,
    p0770_25_mais_peso_ignorado
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
escolaridade <- escolaridade_largo |>
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

# Criando a tabela da composição por nível de instrução (estimativa e proporção)
composicao <- totais_amostra |>
  select(
    codigo_distrito,
    p0770_25_mais_peso_observado,
    all_of(niveis_instrucao$campo)
  ) |>
  pivot_longer(
    cols      = all_of(niveis_instrucao$campo),
    names_to  = "campo",
    values_to = "pessoas"
  ) |>
  left_join(niveis_instrucao, by = "campo") |>
  mutate(
    pct = 100 * pessoas / p0770_25_mais_peso_observado
  ) |>
  left_join(
    escolaridade_largo |> select(codigo_distrito, nivel, nome_distrito, nome_rotulo, amostra_menor),
    by = "codigo_distrito"
  ) |>
  rename(nivel_territorial = nivel.y, nivel = nivel.x) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    nivel_codigo,
    nivel,
    rotulo_nivel,
    pessoas,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, nivel_codigo)


################################################################################
##### VII. Conferências
################################################################################

# Obtendo os campos da tabela municipal que também estão nos setores
campos_comuns <- setdiff(
  intersect(names(municipio), names(setores)),
  "codigo_municipio"
)

# Somando os setores nesses campos
soma_setores <- setores |>
  summarise(
    across(
      .cols = all_of(campos_comuns),
      .fns  = ~ sum(.x, na.rm = TRUE)
    )
  )

# Separando distritos e município
largo_distritos <- filter(escolaridade_largo, nivel == "distrito")
largo_municipio <- filter(escolaridade_largo, nivel == "municipio")

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Peso observado de P0770 igual à soma das cinco categorias em cada AP",
    "Sem peso ausente nem ignorado em P0770 (25 anos ou mais)",
    "Níveis de instrução somam 100% em cada distrito",
    "Soma dos setores igual à tabela municipal nos campos de alfabetização",
    "Proporções entre 0 e 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)",
    "Células ausentes do universo são só do símbolo X (sigilo)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(abs(amostra$p0770_25_mais_peso_observado - rowSums(amostra[niveis_instrucao$campo])) < 1e-6),
    sum(amostra$p0770_25_mais_peso_ausente) == 0 && sum(amostra$p0770_25_mais_peso_ignorado) == 0,
    composicao |> group_by(codigo_distrito) |> summarise(s = sum(pct)) |> pull(s) |> (\(x) all(abs(x - 100) < 1e-6))(),
    all(abs(unlist(soma_setores[campos_comuns]) - unlist(municipio[campos_comuns])) < 1e-6),
    escolaridade |>
      filter(str_starts(indicador, "pct_") | indicador == "taxa_alfabetizacao") |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 13,
    all(ausencias$ausencia_original == "X")
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Mostrando os distritos com amostra menor
message(
  "Distritos com amostra menor: ",
  paste0(largo_distritos$nome_rotulo[largo_distritos$amostra_menor], " (",
         largo_distritos$registros_amostra[largo_distritos$amostra_menor], ")", collapse = ", ")
)

# Interrompendo se alguma conferência falhar
if (!all(conferencias$ok)) {
  stop("Conferências com falha: ", paste(conferencias$conferencia[!conferencias$ok], collapse = "; "))
}


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
  x    = escolaridade,
  sink = file.path(diretorio_grupo, "escolaridade.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a composição por nível de instrução
write_parquet(
  x    = composicao,
  sink = file.path(diretorio_grupo, "composicao.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = escolaridade_largo |>
      select(-nivel, -nome_rotulo, -pessoas_estimativa_total, -p0770_25_mais_peso_ausente, -p0770_25_mais_peso_ignorado) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                  = codigo_distrito,
        `Distrito`                = nome_distrito,
        `APs no distrito`         = n_aps,
        `Amostra menor (aviso)`   = amostra_menor
      ),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado) e resultados do universo por setor (alfabetização).",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_escolaridade.xlsx")
)

# Informando o fim do script
message("Indicadores da escolaridade gravados em ", diretorio_grupo)
