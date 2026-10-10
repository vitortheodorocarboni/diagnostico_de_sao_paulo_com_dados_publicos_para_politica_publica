################################################################################
##################################            ##################################
################ 05.02) PREPARAR OS INDICADORES DE COR OU RAÇA #################
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

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Definindo o diretório dos extratos e das saídas do grupo
diretorio_grupo <- caminho_data("05_raca_cor")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("05_raca_cor")

# Descrevendo as cinco categorias de cor ou raça, na ordem dos campos do IBGE
cores <- c("branca", "preta", "amarela", "parda", "indigena")

# Descrevendo os rótulos das cores
rotulos_cores <- c(
  branca   = "Branca",
  preta    = "Preta",
  amarela  = "Amarela",
  parda    = "Parda",
  indigena = "Indígena"
)

# Descrevendo os campos de cada conjunto (cinco campos, um por cor, na ordem de
# cores acima): moradores, responsável pelo domicílio e quatro faixas de idade
campos_conjuntos <- list(
  moradores   = sprintf("v%05d", 1317:1321),
  responsavel = sprintf("v%05d", 1332:1336),
  idade_0_14  = sprintf("v%05d", 1372:1376),
  idade_15_29 = sprintf("v%05d", 1377:1381),
  idade_30_59 = sprintf("v%05d", 1382:1386),
  idade_60_mais = sprintf("v%05d", 1387:1391)
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar os campos publicados de cada grupo de setores;
# as células com sigilo ficam fora da soma, sem imputação
somar_campos <- function(setores_agrupados) {

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

# Criando uma função para calcular a proporção de pretos e pardos de um
# conjunto de cinco campos (preta e parda sobre a soma publicada das cinco cores)
proporcao_negra <- function(totais, campos) {

  # Obtendo os campos de cada cor
  preta <- totais[[campos[2]]]
  parda <- totais[[campos[4]]]
  total <- rowSums(totais[campos])

  # Calculando pretos e pardos sobre a soma publicada das cinco cores
  100 * (preta + parda) / total
}


################################################################################
##### IV. Dados
################################################################################

# Lendo os setores da capital
setores <- read_parquet(file.path(diretorio_grupo, "setores.parquet"))

# Lendo a linha do município
municipio <- read_parquet(file.path(diretorio_grupo, "municipio.parquet"))

# Lendo as ausências dos campos do grupo
ausencias <- read_parquet(file.path(diretorio_grupo, "ausencias.parquet"))

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
  ~indicador,              ~grupo,       ~rotulo,                                           ~unidade,               ~comparacao, ~casas_decimais, ~descricao,
  "pct_branca",            "cores",      "Moradores que se declaram brancos",               "% dos moradores",      "pontos",    1L,             "Cor ou raça branca (v01317) sobre a soma publicada das cinco cores (v01317 a v01321).",
  "pct_preta",             "cores",      "Moradores que se declaram pretos",                "% dos moradores",      "pontos",    1L,             "Cor ou raça preta (v01318) sobre a soma publicada das cinco cores.",
  "pct_amarela",           "cores",      "Moradores que se declaram amarelos",              "% dos moradores",      "pontos",    1L,             "Cor ou raça amarela (v01319) sobre a soma publicada das cinco cores.",
  "pct_parda",             "cores",      "Moradores que se declaram pardos",                "% dos moradores",      "pontos",    1L,             "Cor ou raça parda (v01320) sobre a soma publicada das cinco cores.",
  "pct_indigena",          "cores",      "Moradores que se declaram indígenas",             "% dos moradores",      "pontos",    2L,             "Cor ou raça indígena (v01321) sobre a soma publicada das cinco cores; subestimada pelo sigilo.",
  "pct_negra",             "negra",      "Pretos e pardos",                                 "% dos moradores",      "pontos",    1L,             "Soma de preta e parda (v01318 + v01320) sobre a soma publicada das cinco cores.",
  "pct_negra_0_14",        "negra",      "Pretos e pardos de 0 a 14 anos",                  "% dos moradores de 0 a 14 anos",      "pontos", 1L,        "Preta e parda entre os moradores de 0 a 14 anos (v01373 + v01375), sobre as cinco cores nessa faixa (v01372 a v01376).",
  "pct_negra_15_29",       "negra",      "Pretos e pardos de 15 a 29 anos",                 "% dos moradores de 15 a 29 anos",     "pontos", 1L,        "Preta e parda entre os moradores de 15 a 29 anos (v01378 + v01380), sobre as cinco cores nessa faixa (v01377 a v01381).",
  "pct_negra_30_59",       "negra",      "Pretos e pardos de 30 a 59 anos",                 "% dos moradores de 30 a 59 anos",     "pontos", 1L,        "Preta e parda entre os moradores de 30 a 59 anos (v01383 + v01385), sobre as cinco cores nessa faixa (v01382 a v01386).",
  "pct_negra_60_mais",     "negra",      "Pretos e pardos de 60 anos ou mais",              "% dos moradores de 60 anos ou mais",  "pontos", 1L,        "Preta e parda entre os moradores de 60 anos ou mais (v01388 + v01390), sobre as cinco cores nessa faixa (v01387 a v01391).",
  "pct_negra_responsavel", "negra",      "Responsáveis pelo domicílio pretos e pardos",     "% dos responsáveis",   "pontos",    1L,             "Preta e parda entre as pessoas responsáveis pelo domicílio (v01333 + v01335), sobre as cinco cores (v01332 a v01336).",
  "cobertura_cor",         "qualidade",  "Cobertura da cor ou raça",                        "% dos moradores",      "pontos",    1L,             "Moradores com cor ou raça publicada (v01317 a v01321) sobre o total de pessoas (v0001, sem sigilo)."
) |>
  mutate(
    ordem = row_number()
  )


################################################################################
##### VI. Indicadores por distrito e do município
################################################################################

# Ligando os setores aos distritos
setores_distritos <- setores |>
  inner_join(setor_distrito, by = "codigo_setor")

# Somando os campos por distrito e para o município
totais <- bind_rows(
  setores_distritos |>
    group_by(codigo_distrito) |>
    somar_campos() |>
    left_join(distritos, by = "codigo_distrito") |>
    mutate(nivel = "distrito"),
  setores_distritos |>
    somar_campos() |>
    mutate(
      codigo_distrito = municipio$codigo_municipio,
      nome_distrito   = "Município de São Paulo",
      nome_rotulo     = "Município de São Paulo",
      nivel           = "municipio"
    )
)

# Calculando os indicadores em formato largo
total_cores <- rowSums(totais[campos_conjuntos$moradores])
cor_largo <- totais |>
  mutate(
    pct_branca            = 100 * v01317 / total_cores,
    pct_preta             = 100 * v01318 / total_cores,
    pct_amarela           = 100 * v01319 / total_cores,
    pct_parda             = 100 * v01320 / total_cores,
    pct_indigena          = 100 * v01321 / total_cores,
    pct_negra             = proporcao_negra(totais, campos_conjuntos$moradores),
    pct_negra_0_14        = proporcao_negra(totais, campos_conjuntos$idade_0_14),
    pct_negra_15_29       = proporcao_negra(totais, campos_conjuntos$idade_15_29),
    pct_negra_30_59       = proporcao_negra(totais, campos_conjuntos$idade_30_59),
    pct_negra_60_mais     = proporcao_negra(totais, campos_conjuntos$idade_60_mais),
    pct_negra_responsavel = proporcao_negra(totais, campos_conjuntos$responsavel),
    cobertura_cor         = 100 * total_cores / v0001
  ) |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    all_of(indicadores$indicador)
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
cor <- cor_largo |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  )

# Criando a tabela da composição por cor ou raça: moradores e proporção
composicao <- totais |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    all_of(campos_conjuntos$moradores)
  ) |>
  pivot_longer(
    cols      = all_of(campos_conjuntos$moradores),
    names_to  = "campo",
    values_to = "moradores"
  ) |>
  mutate(
    cor = cores[match(campo, campos_conjuntos$moradores)]
  ) |>
  group_by(codigo_distrito) |>
  mutate(
    pct = 100 * moradores / sum(moradores)
  ) |>
  ungroup() |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    cor,
    moradores,
    pct
  ) |>
  arrange(nivel, codigo_distrito, match(cor, cores))

# Contando os setores sem cor ou raça publicada em alguma categoria, por
# distrito, para a tabela de revisão
setores_com_sigilo <- setores_distritos |>
  group_by(codigo_distrito) |>
  summarise(
    setores_com_sigilo = sum(if_any(all_of(campos_conjuntos$moradores), is.na)),
    .groups            = "drop"
  )


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

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "Distritos com indicadores",
    "Setores ligados",
    "Soma dos setores igual à tabela municipal em todos os campos comuns",
    "Moradores somados nos distritos iguais ao município (v0001)",
    "As cinco cores somam 100% em cada distrito",
    "Pretos e pardos iguais a preta mais parda em cada distrito",
    "Proporções entre 0 e 100",
    "Células ausentes são só do símbolo X (sigilo)"
  ),
  ok          = c(
    nrow(filter(cor_largo, nivel == "distrito")) == 96,
    nrow(setores_distritos) == nrow(setores),
    all(abs(unlist(soma_setores[campos_comuns]) - unlist(municipio[campos_comuns])) < 1e-6),
    sum(filter(totais, nivel == "distrito")$v0001) == filter(totais, nivel == "municipio")$v0001,
    composicao |> group_by(codigo_distrito) |> summarise(s = sum(pct)) |> pull(s) |> (\(x) all(abs(x - 100) < 1e-6))(),
    all(abs(cor_largo$pct_negra - cor_largo$pct_preta - cor_largo$pct_parda) < 1e-6),
    all(cor$valor >= 0 & cor$valor <= 100),
    all(ausencias$ausencia_original == "X")
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Mostrando a cobertura
message(
  "Cobertura da cor ou raça no município: ", round(filter(cor_largo, nivel == "municipio")$cobertura_cor, 2),
  "%; mínima entre os distritos: ", round(min(filter(cor_largo, nivel == "distrito")$cobertura_cor), 2), "%"
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
  x    = cor,
  sink = file.path(diretorio_grupo, "cor.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a composição por cor ou raça
write_parquet(
  x    = composicao,
  sink = file.path(diretorio_grupo, "composicao.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores         = cor_largo |>
      left_join(setores_com_sigilo, by = "codigo_distrito") |>
      mutate(
        setores_com_sigilo = if_else(nivel == "distrito", setores_com_sigilo, NA_integer_)
      ) |>
      select(-nivel, -nome_rotulo) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                                         = codigo_distrito,
        `Distrito`                                       = nome_distrito,
        `Setores com sigilo em alguma cor (moradores)`   = setores_com_sigilo
      ),
    moradores_por_cor   = composicao |>
      select(nivel, codigo_distrito, nome_distrito, cor, moradores) |>
      mutate(cor = factor(rotulos_cores[cor], levels = rotulos_cores)) |>
      pivot_wider(names_from = cor, values_from = moradores) |>
      select(-nivel),
    descricao           = indicadores |> select(-ordem, -casas_decimais, -comparacao)
  ),
  path = file.path(diretorio_resultados, "tabela_raca_cor.xlsx")
)

# Informando o fim do script
message("Indicadores de cor ou raça gravados em ", diretorio_grupo)
