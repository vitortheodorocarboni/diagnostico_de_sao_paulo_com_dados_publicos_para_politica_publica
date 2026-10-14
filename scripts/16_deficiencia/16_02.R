################################################################################
##################################            ##################################
################# 16.02) PREPARAR OS INDICADORES DA DEFICIÊNCIA ################
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
diretorio_grupo <- caminho_data("16_deficiencia")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("16_deficiencia")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c("P0420", "P0430", "P0440", "P0450", "P0460", "P0470", "P1200")

# Descrevendo os cinco tipos de deficiência, na ordem do questionário
tipos <- tibble(
  tipo_codigo = 1:5,
  variavel    = c("p0420", "p0430", "p0440", "p0450", "p0460"),
  indicador   = c("pct_visual", "pct_auditiva", "pct_motora", "pct_pegar", "pct_mental"),
  rotulo_tipo = c("Visual", "Auditiva", "Motora", "Pegar objetos", "Mental ou intelectual")
)

# Descrevendo os quatro graus de dificuldade (códigos 1 a 4 de P0420 a P0460)
graus <- tibble(
  grau_codigo = 1:4,
  rotulo_grau = c("Não consegue de modo algum", "Muita dificuldade", "Alguma dificuldade", "Nenhuma dificuldade")
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar as estimativas das APs de cada grupo. As
# estimativas já vêm ponderadas pelo IBGE; somam-se as estimativas e os pesos,
# e os indicadores são recalculados a partir das somas.
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

# Criando uma função para somar os campos publicados dos setores de cada grupo,
# sem imputar as células com sigilo (valores ausentes entram como zero)
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
  ~indicador,          ~grupo,        ~base,      ~rotulo,                                        ~unidade,                               ~comparacao, ~casas_decimais, ~descricao,
  "pct_deficiencia",   "deficiencia", "amostra",  "Pessoas com deficiência",                      "% das pessoas de 2 anos ou mais",      "pontos",    1L,              "P0470 código 1 sobre o peso observado de P0470: quem não consegue de modo algum ou tem muita dificuldade em ao menos um dos cinco tipos (indicadora do IBGE).",
  "pct_visual",        "tipo",        "amostra",  "Deficiência visual",                           "% das pessoas de 2 anos ou mais",      "pontos",    1L,              "P0420 códigos 1 e 2 (não consegue de modo algum ou muita dificuldade para enxergar, mesmo com óculos ou lentes) sobre o peso observado de P0420.",
  "pct_auditiva",      "tipo",        "amostra",  "Deficiência auditiva",                         "% das pessoas de 2 anos ou mais",      "pontos",    1L,              "P0430 códigos 1 e 2 (para ouvir, mesmo com aparelho auditivo) sobre o peso observado de P0430.",
  "pct_motora",        "tipo",        "amostra",  "Deficiência motora",                           "% das pessoas de 2 anos ou mais",      "pontos",    1L,              "P0440 códigos 1 e 2 (para andar ou subir degraus) sobre o peso observado de P0440.",
  "pct_pegar",         "tipo",        "amostra",  "Dificuldade para pegar objetos",               "% das pessoas de 2 anos ou mais",      "pontos",    1L,              "P0450 códigos 1 e 2 (para pegar pequenos objetos ou abrir e fechar recipientes) sobre o peso observado de P0450.",
  "pct_mental",        "tipo",        "amostra",  "Deficiência mental ou intelectual",            "% das pessoas de 2 anos ou mais",      "pontos",    1L,              "P0460 códigos 1 e 2 (para as atividades habituais, por limitação nas funções mentais) sobre o peso observado de P0460.",
  "pct_autismo",       "autismo",     "amostra",  "Pessoas com diagnóstico de autismo",           "% dos moradores",                      "pontos",    1L,              "P1200 código 1 (diagnóstico de autismo por profissional de saúde) sobre o peso observado de P1200 (todas as pessoas).",
  "pct_60_mais",       "idade",       "universo", "Moradores de 60 anos ou mais",                 "% dos moradores",                      "pontos",    1L,              "Grupos de 60 a 69 e 70 anos ou mais (v01040 e v01041) sobre a soma publicada dos 11 grupos de idade (universo), como no grupo 04.",
  "registros_amostra", "qualidade",   "amostra",  "Registros de pessoas na amostra",              "registros",                            "relativa",  0L,              "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor."
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

# Somando as APs por distrito e para o município
totais_amostra <- bind_rows(
  amostra_distritos |>
    group_by(codigo_distrito) |>
    somar_aps(),
  amostra_distritos |>
    somar_aps() |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Somando os setores por distrito e para o município
totais_universo <- setores |>
  inner_join(setor_distrito, by = "codigo_setor") |>
  (\(x) bind_rows(
    x |>
      group_by(codigo_distrito) |>
      somar_setores(),
    x |>
      somar_setores() |>
      mutate(codigo_distrito = codigo_municipio_sp)
  ))()

# Calculando a proporção de cada grau de dificuldade, por tipo, em cada
# distrito e no município (sobre o peso observado do tipo)
composicao_graus <- tipos |>
  cross_join(graus) |>
  mutate(
    valores = map2(
      variavel,
      grau_codigo,
      \(variavel_tipo, codigo) tibble(
        codigo_distrito = totais_amostra$codigo_distrito,
        pessoas         = totais_amostra[[sprintf("%s_cat_codigo_%d_estimativa", variavel_tipo, codigo)]],
        pct             = 100 * pessoas / totais_amostra[[paste0(variavel_tipo, "_peso_observado")]]
      )
    )
  ) |>
  unnest(valores)

# Calculando os indicadores em formato largo
deficiencia_largo <- totais_amostra |>
  left_join(
    composicao_graus |>
      filter(grau_codigo %in% 1:2) |>
      group_by(codigo_distrito, indicador) |>
      summarise(pct = sum(pct), .groups = "drop") |>
      pivot_wider(names_from = indicador, values_from = pct),
    by = "codigo_distrito"
  ) |>
  inner_join(totais_universo, by = "codigo_distrito") |>
  mutate(
    # Pessoas com deficiência sobre o peso observado de P0470 (2 anos ou mais)
    pct_deficiencia   = 100 * p0470_cat_codigo_1_estimativa / p0470_peso_observado,
    # Diagnóstico de autismo sobre o peso observado de P1200 (todas as pessoas)
    pct_autismo       = 100 * p1200_cat_codigo_1_estimativa / p1200_peso_observado,
    # Universo: 60 anos ou mais sobre a soma publicada dos 11 grupos de idade
    total_idades      = v01031 + v01032 + v01033 + v01034 + v01035 + v01036 + v01037 + v01038 + v01039 + v01040 + v01041,
    pct_60_mais       = 100 * (v01040 + v01041) / total_idades,
    # Registros e aviso de amostra menor (o município não recebe aviso)
    registros_amostra = pessoas_n_registros,
    nivel             = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor     = nivel == "distrito" & registros_amostra < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
deficiencia <- deficiencia_largo |>
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

# Criando a tabela dos graus de dificuldade por tipo
graus_tipos <- composicao_graus |>
  inner_join(
    deficiencia_largo |>
      select(
        nivel_territorial = nivel,
        codigo_distrito,
        nome_distrito,
        nome_rotulo,
        amostra_menor
      ),
    by = "codigo_distrito"
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    tipo_codigo,
    rotulo_tipo,
    grau_codigo,
    rotulo_grau,
    pessoas,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, tipo_codigo, grau_codigo)


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(deficiencia_largo, nivel == "distrito")
largo_municipio <- filter(deficiencia_largo, nivel == "municipio")

# Calculando, por AP, a diferença entre a soma dos quatro graus e o peso
# observado de cada tipo
diferenca_graus <- map_dbl(
  tipos$variavel,
  \(variavel_tipo) max(abs(
    rowSums(as.matrix(amostra[sprintf("%s_cat_codigo_%d_estimativa", variavel_tipo, 1:4)])) -
      amostra[[paste0(variavel_tipo, "_peso_observado")]]
  ))
)

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Universos de aplicação das sete variáveis conferidos",
    "Quatro graus somam o peso observado de cada tipo em cada AP",
    "Com e sem deficiência somam o peso observado de P0470 em cada AP",
    "Com e sem diagnóstico de autismo somam o peso observado de P1200 em cada AP",
    "Deficiência entre o maior tipo e a soma dos cinco tipos em cada distrito",
    "Graus somam 100% em cada tipo e distrito",
    "Nenhuma proporção fora de 0 a 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)",
    "Soma dos setores igual à tabela municipal (v0001 e 11 grupos de idade)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    all(diferenca_graus < 1e-6),
    all(abs(amostra$p0470_cat_codigo_1_estimativa + amostra$p0470_cat_codigo_2_estimativa - amostra$p0470_peso_observado) < 1e-6),
    all(abs(amostra$p1200_cat_codigo_1_estimativa + amostra$p1200_cat_codigo_2_estimativa - amostra$p1200_peso_observado) < 1e-6),
    with(
      largo_distritos,
      all(
        pct_deficiencia >= pmax(pct_visual, pct_auditiva, pct_motora, pct_pegar, pct_mental) - 1e-6 &
          pct_deficiencia <= pct_visual + pct_auditiva + pct_motora + pct_pegar + pct_mental + 1e-6
      )
    ),
    graus_tipos |>
      group_by(codigo_distrito, tipo_codigo) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    deficiencia |>
      filter(str_starts(indicador, "pct_")) |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 13,
    all(map_lgl(
      names(municipio) |> str_subset("^v0[0-9]+$"),
      \(campo) largo_municipio[[campo]] == municipio[[campo]]
    ))
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

# Informando os valores do município e a correlação com a idade
message(
  "Município: deficiência ", round(largo_municipio$pct_deficiencia, 2),
  "%; autismo ", round(largo_municipio$pct_autismo, 2),
  "%; 60 anos ou mais ", round(largo_municipio$pct_60_mais, 2),
  "%. Correlação entre deficiência e 60 anos ou mais nos distritos: ",
  round(cor(largo_distritos$pct_deficiencia, largo_distritos$pct_60_mais), 3)
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
  x    = deficiencia,
  sink = file.path(diretorio_grupo, "deficiencia.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando os graus de dificuldade por tipo
write_parquet(
  x    = graus_tipos,
  sink = file.path(diretorio_grupo, "graus.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = deficiencia_largo |>
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
    graus       = graus_tipos |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, tipo_codigo, rotulo_tipo, grau_codigo, rotulo_grau, pessoas, pct),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), P0420 a P0470 e P1200; e universo por setor (grupos de idade, v01031 a v01041).",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        "Pessoa com deficiência: quem não consegue de modo algum ou tem muita dificuldade em ao menos um dos cinco tipos (indicadora P0470 do IBGE), entre as pessoas de 2 anos ou mais; quem tem alguma dificuldade não entra.",
        "A deficiência cresce com a idade, mas o tratado não a cruza com a idade, e não há padronização possível; o % de 60 anos ou mais (universo) vai ao lado para a leitura. Nos distritos, a deficiência é maior onde há menos idosos (correlação negativa).",
        "O diagnóstico de autismo, pergunta nova em 2022, depende do acesso a profissional de saúde; é raro, e nos distritos com amostra menor resulta de poucas dezenas de registros.",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido."),
        "No universo, os totais por distrito somam só os valores publicados por setor (células com sigilo fora, sem imputação); na capital, a soma dos setores coincide com a tabela municipal."
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_deficiencia.xlsx")
)

# Informando o fim do script
message("Indicadores da deficiência gravados em ", diretorio_grupo)
