################################################################################
##################################            ##################################
############### 08.02) PREPARAR OS INDICADORES DA RELIGIOSIDADE ################
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
diretorio_grupo <- caminho_data("08_religiosidade")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("08_religiosidade")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Descrevendo as categorias de religião ou culto (P0411) e os seis grandes
# grupos de apresentação
categorias <- tibble(
  codigo_categoria = as.character(1:33),
  rotulo_categoria = c(
    "Católica apostólica romana", "Católica apostólica brasileira", "Católica ortodoxa",
    "Evangélica de missão", "Evangélica de origem pentecostal", "Igreja evangélica indígena",
    "Evangélica não determinada", "Outras religiosidades cristãs",
    "Igreja de Jesus Cristo dos Santos dos Últimos Dias", "Testemunhas de Jeová",
    "Espiritualista", "Espírita", "Umbanda", "Candomblé", "Outras religiosidades afro-brasileiras",
    "Judaísmo", "Hinduísmo", "Budismo", "Igreja Messiânica Mundial",
    "Outras novas religiões orientais", "Outras religiões orientais", "Islamismo",
    "Tradições esotéricas", "Tradições indígenas", "Religiões ayahuasqueiras", "LBV",
    "Sem religião", "Ateu", "Agnóstico", "Múltiplo pertencimento",
    "Não determinada", "Não sabe", "Sem declaração"
  ),
  grupo_codigo     = case_when(
    as.integer(codigo_categoria) %in% 1:3   ~ 1L,
    as.integer(codigo_categoria) %in% 4:7   ~ 2L,
    as.integer(codigo_categoria) == 12L     ~ 3L,
    as.integer(codigo_categoria) %in% 13:15 ~ 4L,
    as.integer(codigo_categoria) %in% 27:29 ~ 5L,
    TRUE                                    ~ 6L
  )
)
grupos <- tibble(
  grupo_codigo = 1:6,
  grupo        = c("catolica", "evangelica", "espirita", "afro", "sem_religiao", "outras"),
  rotulo_grupo = c(
    "Católica",
    "Evangélica",
    "Espírita",
    "Umbanda, candomblé e outras afro-brasileiras",
    "Sem religião, ateu ou agnóstico",
    "Outras ou não declarada"
  )
)
categorias <- left_join(categorias, grupos, by = "grupo_codigo")


################################################################################
##### III. Dados
################################################################################

# Lendo as APs da capital
amostra <- read_parquet(file.path(diretorio_grupo, "amostra.parquet"))

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
##### IV. Descrição dos indicadores
################################################################################

# Descrevendo os indicadores do grupo, na ordem de apresentação
indicadores <- tribble(
  ~indicador,           ~grupo,      ~rotulo,                                      ~unidade,                          ~comparacao, ~casas_decimais, ~descricao,
  "pct_catolica",       "religiao",  "Católica",                                   "% das pessoas de 10 anos ou mais", "pontos",    1L,             "Soma das estimativas de P0411 = 1 a 3 sobre a soma das categorias declaradas (amostra).",
  "pct_evangelica",     "religiao",  "Evangélica",                                 "% das pessoas de 10 anos ou mais", "pontos",    1L,             "Soma das estimativas de P0411 = 4 a 7 sobre a soma das categorias declaradas (amostra).",
  "pct_espirita",       "religiao",  "Espírita",                                   "% das pessoas de 10 anos ou mais", "pontos",    1L,             "Estimativa de P0411 = 12 sobre a soma das categorias declaradas (amostra).",
  "pct_afro",           "religiao",  "Umbanda, candomblé e outras afro-brasileiras", "% das pessoas de 10 anos ou mais", "pontos",  1L,             "Soma das estimativas de P0411 = 13 a 15 sobre a soma das categorias declaradas (amostra).",
  "pct_sem_religiao",   "religiao",  "Sem religião, ateu ou agnóstico",            "% das pessoas de 10 anos ou mais", "pontos",    1L,             "Soma das estimativas de P0411 = 27 a 29 sobre a soma das categorias declaradas (amostra).",
  "pct_outras",         "religiao",  "Outras ou não declarada",                    "% das pessoas de 10 anos ou mais", "pontos",    1L,             "Soma das demais categorias de P0411 (8 a 11, 16 a 26 e 30 a 33) sobre a soma das categorias declaradas (amostra).",
  "pessoas_10_mais",    "religiao",  "Pessoas de 10 anos ou mais (estimativa)",    "pessoas",                          "relativa",  0L,             "Soma das estimativas das categorias de P0411 entre quem tem 10 anos ou mais (amostra).",
  "registros_amostra",  "qualidade", "Registros de pessoas na amostra",            "registros",                        "relativa",  0L,             "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor."
) |>
  mutate(
    ordem = row_number()
  )


################################################################################
##### V. Estimativas por categoria, distrito e município
################################################################################

# Desmembrando a lista de categorias de cada AP e completando com zero as
# categorias ausentes da lista (estimativa zero, pois o peso observado é positivo)
categorias_ap <- amostra |>
  select(codigo_area_ponderacao, p0411_10_mais_distribuicao) |>
  unnest(p0411_10_mais_distribuicao) |>
  select(
    codigo_area_ponderacao,
    codigo_categoria,
    n_registros,
    estimativa
  ) |>
  complete(
    codigo_area_ponderacao,
    codigo_categoria = categorias$codigo_categoria,
    fill             = list(n_registros = 0L, estimativa = 0)
  )

# Ligando as APs aos distritos pela regra da maioria de setores
amostra_distritos <- amostra |>
  inner_join(
    ap_distrito |> select(codigo_area_ponderacao, codigo_distrito),
    by = "codigo_area_ponderacao"
  )
categorias_distritos <- categorias_ap |>
  inner_join(
    ap_distrito |> select(codigo_area_ponderacao, codigo_distrito),
    by = "codigo_area_ponderacao"
  )

# Somando as categorias por distrito e para o município
categorias_totais <- bind_rows(
  categorias_distritos |>
    group_by(codigo_distrito, codigo_categoria) |>
    summarise(
      estimativa   = sum(estimativa),
      n_registros  = sum(n_registros),
      .groups      = "drop"
    ),
  categorias_distritos |>
    group_by(codigo_categoria) |>
    summarise(
      estimativa   = sum(estimativa),
      n_registros  = sum(n_registros),
      .groups      = "drop"
    ) |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Somando os registros e as estimativas totais por distrito e para o município
totais_territorio <- bind_rows(
  amostra_distritos |>
    group_by(codigo_distrito) |>
    summarise(
      n_aps                    = n(),
      pessoas_n_registros      = sum(pessoas_n_registros),
      pessoas_estimativa_total = sum(pessoas_estimativa_total),
      .groups                  = "drop"
    ),
  amostra_distritos |>
    summarise(
      n_aps                    = n(),
      pessoas_n_registros      = sum(pessoas_n_registros),
      pessoas_estimativa_total = sum(pessoas_estimativa_total)
    ) |>
    mutate(codigo_distrito = codigo_municipio_sp)
) |>
  mutate(
    nivel         = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor = nivel == "distrito" & pessoas_n_registros < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  )

# Calculando as proporções das categorias sobre a soma das categorias declaradas
categorias_pct <- categorias_totais |>
  group_by(codigo_distrito) |>
  mutate(
    pessoas_10_mais = sum(estimativa),
    pct             = 100 * estimativa / pessoas_10_mais
  ) |>
  ungroup() |>
  left_join(categorias, by = "codigo_categoria") |>
  left_join(
    totais_territorio |>
      select(codigo_distrito, nivel_territorial = nivel, nome_distrito, nome_rotulo, amostra_menor),
    by = "codigo_distrito"
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    codigo_categoria,
    rotulo_categoria,
    grupo_codigo,
    grupo,
    rotulo_grupo,
    n_registros,
    estimativa,
    pessoas_10_mais,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, as.integer(codigo_categoria))


################################################################################
##### VI. Indicadores por distrito e do município
################################################################################

# Somando as categorias nos seis grupos
composicao <- categorias_pct |>
  group_by(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    grupo_codigo,
    grupo,
    rotulo_grupo
  ) |>
  summarise(
    pessoas = sum(estimativa),
    pct     = sum(pct),
    .groups = "drop"
  ) |>
  arrange(nivel_territorial, codigo_distrito, grupo_codigo)

# Montando os indicadores em formato largo
religiao_largo <- composicao |>
  select(codigo_distrito, grupo, pct) |>
  pivot_wider(
    names_from   = grupo,
    values_from  = pct,
    names_prefix = "pct_"
  ) |>
  left_join(
    categorias_pct |>
      distinct(codigo_distrito, pessoas_10_mais),
    by = "codigo_distrito"
  ) |>
  left_join(totais_territorio, by = "codigo_distrito") |>
  mutate(
    registros_amostra = pessoas_n_registros
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
religiao <- religiao_largo |>
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
largo_distritos <- filter(religiao_largo, nivel == "distrito")

# Comparando, em cada AP, a soma das categorias com o peso observado mais o
# ignorado (as categorias publicadas incluem o peso ignorado)
conferencia_ap <- categorias_ap |>
  group_by(codigo_area_ponderacao) |>
  summarise(
    soma_categorias = sum(estimativa),
    registros       = sum(n_registros),
    .groups         = "drop"
  ) |>
  left_join(amostra, by = "codigo_area_ponderacao")

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Códigos de categoria entre 1 e 33",
    "Soma das categorias igual ao peso observado mais o ignorado em cada AP",
    "Sem peso ausente em P0411 (10 anos ou mais)",
    "Registros das categorias não passam dos registros de pessoas da AP",
    "Estimativa das categorias não passa do total de pessoas da AP",
    "Seis grupos somam 100% em cada distrito e no município",
    "Proporções entre 0 e 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(as.integer(unnest(amostra, p0411_10_mais_distribuicao)$codigo_categoria) %in% 1:33),
    all(abs(conferencia_ap$soma_categorias -
              (conferencia_ap$p0411_10_mais_peso_observado + conferencia_ap$p0411_10_mais_peso_ignorado)) < 0.01),
    sum(amostra$p0411_10_mais_peso_ausente) == 0,
    all(conferencia_ap$registros <= conferencia_ap$pessoas_n_registros),
    all(conferencia_ap$soma_categorias <= conferencia_ap$pessoas_estimativa_total + 0.5),
    composicao |>
      group_by(codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    religiao |>
      filter(str_starts(indicador, "pct_")) |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 13
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Mostrando o peso ignorado sobre a soma das categorias na capital
message(
  "Peso ignorado sobre a soma das categorias: ",
  round(100 * sum(amostra$p0411_10_mais_peso_ignorado) / sum(conferencia_ap$soma_categorias), 2), "%"
)

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
  x    = religiao,
  sink = file.path(diretorio_grupo, "religiao.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a composição nos seis grupos
write_parquet(
  x    = composicao,
  sink = file.path(diretorio_grupo, "composicao.parquet")
)

# Gravando as 33 categorias por distrito e município
write_parquet(
  x    = categorias_pct,
  sink = file.path(diretorio_grupo, "categorias.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = religiao_largo |>
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
    categorias  = categorias_pct |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, codigo_categoria, rotulo_categoria, rotulo_grupo, n_registros, estimativa, pct),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), religião ou culto (P0411) de quem tem 10 anos ou mais.",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        "A religião é autodeclarada. As proporções usam a soma das categorias declaradas, que inclui o peso ignorado (cerca de 0,2%).",
        "Grupos: católica (1 a 3); evangélica (4 a 7); espírita (12); umbanda, candomblé e outras afro-brasileiras (13 a 15); sem religião, ateu ou agnóstico (27 a 29); outras ou não declarada (as demais, incluindo 31 a 33).",
        "Categorias raras aparecem em poucas APs e são pouco confiáveis em distritos pequenos.",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_religiosidade.xlsx")
)

# Informando o fim do script
message("Indicadores da religiosidade gravados em ", diretorio_grupo)
