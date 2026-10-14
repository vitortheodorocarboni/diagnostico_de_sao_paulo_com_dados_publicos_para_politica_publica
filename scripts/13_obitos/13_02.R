################################################################################
##################################            ##################################
################## 13.02) PREPARAR OS INDICADORES DOS ÓBITOS ###################
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
diretorio_grupo <- caminho_data("13_obitos")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("13_obitos")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c("D0340", "M0150", "M0160", "M0170", "M0171", "P0180")

# Definindo a duração do período dos óbitos, de janeiro de 2019 a julho de
# 2022, em anos
anos_periodo <- 3 + 7 / 12

# Definindo os códigos das 17 faixas de idade (0 a 4 até 80 ou mais), comuns à
# idade ao falecer (M0170) e à idade dos moradores (P0180)
faixas_idade <- sprintf("%02d", 1:17)

# Descrevendo as cinco faixas largas da idade ao falecer
faixas_largas <- tibble(
  faixa_codigo  = 1:5,
  rotulo_faixa  = c("0 a 14 anos", "15 a 29 anos", "30 a 59 anos", "60 a 79 anos", "80 anos ou mais"),
  campos        = list(
    sprintf("m0170_cat_codigo_%02d_estimativa", 1:3),
    sprintf("m0170_cat_codigo_%02d_estimativa", 4:6),
    sprintf("m0170_cat_codigo_%02d_estimativa", 7:12),
    sprintf("m0170_cat_codigo_%02d_estimativa", 13:16),
    "m0170_cat_codigo_17_estimativa"
  )
)

# Definindo o valor de referência do município medido em 2026-10-12 (etapa 10
# do plano), em %
referencia_domicilios_obito <- 5.0


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar as estimativas das APs de cada grupo. As
# estimativas já vêm ponderadas pelo IBGE; somam-se as estimativas, as somas
# ponderadas e os pesos, e os indicadores são recalculados a partir das somas.
somar_aps <- function(aps_agrupadas) {

  # Somando os registros, as estimativas, as somas ponderadas e os pesos
  aps_agrupadas |>
    summarise(
      n_aps = n(),
      across(
        .cols = c(where(is.numeric), -m0171_media),
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
  ~indicador,                   ~grupo,        ~base,      ~rotulo,                                        ~unidade,                                   ~comparacao, ~casas_decimais, ~descricao,
  "rmp",                        "mortalidade", "amostra",  "Razão de mortalidade padronizada pela idade",  "óbitos observados por óbito esperado",     "relativa",  2L,              "Óbitos estimados do distrito (M0170, 17 faixas de idade) sobre os esperados se cada faixa de idade dos moradores (P0180) tivesse a taxa do município; 1 é igual ao município (padronização indireta).",
  "pct_domicilios_obito",       "mortalidade", "universo", "Domicílios com morador falecido",              "% dos domicílios ocupados",                "pontos",    1L,              "Domicílios com pessoa falecida de janeiro de 2019 a julho de 2022 que morava no domicílio (v01224) sobre os domicílios com e sem falecido (v01224 e v01225, universo).",
  "pct_obitos_menos_60",        "mortalidade", "amostra",  "Óbitos antes dos 60 anos",                     "% dos óbitos com idade informada",         "pontos",    1L,              "Falecidos com 0 a 59 anos (M0170 códigos 01 a 12) sobre os falecidos com idade informada (01 a 17).",
  "obitos_mil_ano",             "mortalidade", "amostra",  "Taxa bruta de mortalidade",                    "óbitos por mil moradores ao ano",          "relativa",  1L,              "Falecidos estimados (peso observado de M0150) sobre os moradores (pessoas_estimativa_total), divididos pelos 3,58 anos do período e multiplicados por mil; não descontam o efeito da idade.",
  "idade_media_obito",          "mortalidade", "amostra",  "Idade média ao falecer",                       "anos",                                     "relativa",  1L,              "Média de M0171 ponderada pelos falecidos: soma ponderada sobre o peso observado.",
  "pct_obitos_2020_2021",       "periodo",     "amostra",  "Óbitos em 2020 e 2021",                        "% dos óbitos",                             "pontos",    1L,              "Falecidos em 2020 e 2021 (M0150 códigos 2 e 3) sobre os de 2019 a 2022; 2022 só até julho e captação menor nos anos mais antigos.",
  "pct_obitos_homens",          "perfil",      "amostra",  "Homens entre os falecidos",                    "% dos óbitos",                             "pontos",    1L,              "Falecidos do sexo masculino (M0160 código 1) sobre os falecidos por sexo.",
  "pct_domicilios_obito_amostra", "qualidade", "amostra",  "Domicílios com morador falecido (amostra)",    "% dos domicílios ocupados",                "pontos",    1L,              "D0340 código 1 sobre os códigos 1 e 2 (amostra); conferência do indicador do universo.",
  "obitos_estimados",           "qualidade",   "amostra",  "Óbitos estimados de 2019 a julho de 2022",     "óbitos",                                   "relativa",  0L,              "Peso observado de M0150 (falecidos estimados).",
  "obitos_esperados",           "qualidade",   "amostra",  "Óbitos esperados pela idade",                  "óbitos",                                   "relativa",  0L,              "Soma, nas 17 faixas, dos moradores do distrito vezes a taxa do município na mesma faixa.",
  "registros_obitos",           "qualidade",   "amostra",  "Registros de falecidos na amostra",            "registros",                                "relativa",  0L,              "Soma de mortalidade_n_registros das APs do distrito; abaixo do mínimo, aviso; abaixo do piso, sem valor nos indicadores da amostra."
) |>
  mutate(
    ordem = row_number()
  )

# Listando os indicadores da amostra que ficam sem valor abaixo do piso
indicadores_com_piso <- c(
  "rmp",
  "pct_obitos_menos_60",
  "obitos_mil_ano",
  "idade_media_obito",
  "pct_obitos_2020_2021",
  "pct_obitos_homens"
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

# Separando as matrizes de óbitos e de moradores nas 17 faixas de idade
obitos_idade    <- as.matrix(totais_amostra[paste0("m0170_cat_codigo_", faixas_idade, "_estimativa")])
moradores_idade <- as.matrix(totais_amostra[paste0("p0180_cat_codigo_", faixas_idade, "_estimativa")])

# Localizando a linha do município
linha_municipio <- which(totais_amostra$codigo_distrito == codigo_municipio_sp)

# Calculando as taxas do município em cada faixa de idade
taxas_municipio <- obitos_idade[linha_municipio, ] / moradores_idade[linha_municipio, ]

# Calculando os indicadores em formato largo
obitos_largo <- totais_amostra |>
  mutate(
    # --- Padronização indireta pela idade
    obitos_idade_informada = rowSums(obitos_idade),
    obitos_esperados       = as.vector(moradores_idade %*% taxas_municipio),
    rmp                    = obitos_idade_informada / obitos_esperados,
    # --- Demais indicadores da amostra
    obitos_estimados       = m0150_peso_observado,
    obitos_mil_ano         = 1000 * obitos_estimados / pessoas_estimativa_total / anos_periodo,
    pct_obitos_menos_60    = 100 * rowSums(obitos_idade[, 1:12]) / obitos_idade_informada,
    idade_media_obito      = m0171_soma / m0171_peso_observado,
    pct_obitos_2020_2021   = 100 * (m0150_cat_codigo_2_estimativa + m0150_cat_codigo_3_estimativa) / m0150_peso_observado,
    pct_obitos_homens      = 100 * m0160_cat_codigo_1_estimativa / m0160_peso_observado,
    pct_domicilios_obito_amostra = 100 * d0340_cat_codigo_1_estimativa / d0340_peso_observado,
    registros_obitos       = mortalidade_n_registros,
    # --- Aviso e piso pelos registros de falecidos (o município não recebe)
    nivel                  = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor          = nivel == "distrito" & registros_obitos < minimo_registros_obitos,
    sem_valor              = nivel == "distrito" & registros_obitos < piso_registros_obitos
  ) |>
  mutate(
    across(
      .cols = all_of(indicadores_com_piso),
      .fns  = ~ if_else(sem_valor, NA_real_, .x)
    )
  ) |>
  inner_join(totais_universo, by = "codigo_distrito") |>
  mutate(
    # --- Universo: domicílios com morador falecido
    pct_domicilios_obito = 100 * v01224 / (v01224 + v01225)
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
obitos <- obitos_largo |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    sem_valor,
    all_of(indicadores$indicador)
  ) |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  )

# Criando a tabela da idade ao falecer em cinco faixas largas
idade_obito <- faixas_largas |>
  mutate(
    # Somando as faixas de M0170 de cada faixa larga, em cada distrito
    valores = map(
      campos,
      \(campos_faixa) tibble(
        codigo_distrito = obitos_largo$codigo_distrito,
        obitos          = rowSums(as.matrix(obitos_largo[campos_faixa]))
      )
    )
  ) |>
  select(-campos) |>
  unnest(valores) |>
  inner_join(
    obitos_largo |>
      select(
        nivel_territorial = nivel,
        codigo_distrito,
        nome_distrito,
        nome_rotulo,
        amostra_menor,
        sem_valor,
        obitos_idade_informada
      ),
    by = "codigo_distrito"
  ) |>
  mutate(
    pct = 100 * obitos / obitos_idade_informada
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    sem_valor,
    faixa_codigo,
    rotulo_faixa,
    obitos,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, faixa_codigo)


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(obitos_largo, nivel == "distrito")
largo_municipio <- filter(obitos_largo, nivel == "municipio")

# Obtendo os campos da tabela municipal que também estão nos setores
campos_comuns <- setdiff(
  intersect(names(municipio), names(setores)),
  c("codigo_municipio")
)

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores da amostra e do universo",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Universos de aplicação das seis variáveis conferidos",
    "Falecidos estimados iguais a mortalidade_estimativa_total em cada AP",
    "Falecidos por ano, sexo e idade somam o peso observado em cada AP",
    "Média publicada da AP igual a soma sobre peso observado (M0171)",
    "Óbitos esperados somam os observados no município (RMP do município igual a 1)",
    "Faixas largas da idade ao falecer somam 100% em cada distrito",
    "Soma dos setores igual à tabela municipal (v01224 e v01225)",
    "Células ausentes do universo são só do símbolo X (sigilo)",
    "Domicílios com falecido: universo igual à referência e perto da amostra no município",
    "Aviso e piso pelos registros de falecidos: 22 com aviso e 4 sem valor"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    all(abs(amostra$m0150_peso_observado - amostra$mortalidade_estimativa_total) < 1e-6),
    all(abs(amostra$m0150_peso_observado - rowSums(as.matrix(amostra[sprintf("m0150_cat_codigo_%d_estimativa", 1:4)]))) < 1e-6) &&
      all(abs(amostra$m0160_peso_observado - rowSums(as.matrix(amostra[sprintf("m0160_cat_codigo_%d_estimativa", 1:2)]))) < 1e-6) &&
      all(abs(amostra$m0170_peso_observado - rowSums(as.matrix(amostra[sprintf("m0170_cat_codigo_%02d_estimativa", c(1:17, 99))]))) < 1e-6),
    all(abs(amostra$m0171_soma / amostra$m0171_peso_observado - amostra$m0171_media) < 1e-6, na.rm = TRUE),
    abs(largo_municipio$rmp - 1) < 1e-9 &&
      abs(sum(largo_distritos$obitos_esperados) - largo_municipio$obitos_esperados) < 1e-3,
    idade_obito |>
      group_by(codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    all(abs(unlist(totais_universo[totais_universo$codigo_distrito == codigo_municipio_sp, campos_comuns]) -
              unlist(municipio[campos_comuns])) < 1e-6),
    all(ausencias$ausencia_original == "X"),
    abs(largo_municipio$pct_domicilios_obito - referencia_domicilios_obito) < 0.05 &&
      abs(largo_municipio$pct_domicilios_obito - largo_municipio$pct_domicilios_obito_amostra) < 0.1,
    sum(largo_distritos$amostra_menor) == 22 && sum(largo_distritos$sem_valor) == 4
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Mostrando os distritos sem valor
message(
  "Distritos sem valor (menos de ", piso_registros_obitos, " registros de falecidos): ",
  paste0(largo_distritos$nome_rotulo[largo_distritos$sem_valor], " (",
         largo_distritos$registros_obitos[largo_distritos$sem_valor], ")", collapse = ", ")
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
  x    = obitos,
  sink = file.path(diretorio_grupo, "obitos.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a idade ao falecer em faixas largas
write_parquet(
  x    = idade_obito,
  sink = file.path(diretorio_grupo, "idade_obito.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores  = obitos_largo |>
      select(
        codigo_distrito,
        nome_distrito,
        n_aps,
        amostra_menor,
        sem_valor,
        all_of(indicadores$indicador)
      ) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, "; ", indicadores$base, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                = codigo_distrito,
        `Distrito`              = nome_distrito,
        `APs no distrito`       = n_aps,
        `Amostra menor (aviso)` = amostra_menor,
        `Sem valor (piso)`      = sem_valor
      ),
    idade_obito  = idade_obito |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, sem_valor, faixa_codigo, rotulo_faixa, obitos, pct),
    taxas_idade  = tibble(
      faixa_idade         = faixas_idade,
      obitos_municipio    = obitos_idade[linha_municipio, ],
      moradores_municipio = moradores_idade[linha_municipio, ],
      taxa_por_mil_ano    = 1000 * taxas_municipio / anos_periodo
    ),
    descricao    = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas        = tibble(
      nota = c(
        "Fontes: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), registros de falecidos (M0150 a M0171), D0340 e P0180; e resultados do universo por setor censitário, domicílios com falecido (v01224 e v01225).",
        "Dados da amostra de acesso controlado: uso interno, não publicar sem autorização. O indicador de domicílios com falecido do universo é público.",
        "Óbitos de moradores de janeiro de 2019 a julho de 2022 informados pelos domicílios: domicílios desfeitos não informam e a captação cai com a antiguidade do óbito (50 mil em 2019 contra 43 mil em só sete meses de 2022, na estimativa da capital); os totais ficam abaixo dos registros civis e não medem o excesso de mortes da pandemia.",
        "Os campos de sexo, idade e período ao falecer do universo não foram usados: o sigilo das células pequenas por setor os deixa viesados (80% dos óbitos aparecem com 70 anos ou mais).",
        "A razão de mortalidade padronizada (RMP) compara os óbitos do distrito com os esperados pela idade dos moradores (17 faixas, taxas do município); acima de 1, morre-se mais do que a idade explica.",
        paste0("Distritos com menos de ", minimo_registros_obitos, " registros de falecidos recebem aviso; com menos de ", piso_registros_obitos, ", ficam sem valor nos indicadores da amostra (Sé, Barra Funda, Brás e Pari)."),
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada."
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_obitos.xlsx")
)

# Informando o fim do script
message("Indicadores dos óbitos gravados em ", diretorio_grupo)
