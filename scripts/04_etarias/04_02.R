################################################################################
##################################            ##################################
################### 04.02) PREPARAR OS INDICADORES DE IDADE ####################
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
diretorio_grupo <- caminho_data("04_etarias")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("04_etarias")

# Descrevendo os 11 grupos de idade do IBGE e os campos de cada um (campo do
# total de moradores, do sexo masculino e do sexo feminino); a largura do grupo
# em anos serve às pirâmides (o grupo de 70 anos ou mais é aberto)
grupos_idade <- tibble(
  grupo           = 1:11,
  rotulo_grupo    = c(
    "0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29",
    "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 ou mais"
  ),
  largura_anos    = c(rep(5L, 6), rep(10L, 4), NA_integer_),
  campo_total     = sprintf("v%05d", 1031:1041),
  campo_homens    = sprintf("v%05d", 1009:1019),
  campo_mulheres  = sprintf("v%05d", 1020:1030)
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
  ~indicador,           ~grupo,       ~rotulo,                                  ~unidade,                                       ~comparacao, ~casas_decimais, ~descricao,
  "pct_0_14",           "faixas",     "Moradores de 0 a 14 anos",               "% dos moradores",                              "pontos",    1L,             "Grupos de 0 a 4, 5 a 9 e 10 a 14 anos (v01031 a v01033) sobre a soma publicada dos 11 grupos de idade.",
  "pct_15_29",          "faixas",     "Moradores de 15 a 29 anos",              "% dos moradores",                              "pontos",    1L,             "Grupos de 15 a 19, 20 a 24 e 25 a 29 anos (v01034 a v01036) sobre a soma publicada dos 11 grupos.",
  "pct_30_59",          "faixas",     "Moradores de 30 a 59 anos",              "% dos moradores",                              "pontos",    1L,             "Grupos de 30 a 39, 40 a 49 e 50 a 59 anos (v01037 a v01039) sobre a soma publicada dos 11 grupos.",
  "pct_60_mais",        "faixas",     "Moradores de 60 anos ou mais",           "% dos moradores",                              "pontos",    1L,             "Grupos de 60 a 69 e 70 anos ou mais (v01040 e v01041) sobre a soma publicada dos 11 grupos.",
  "pct_70_mais",        "faixas",     "Moradores de 70 anos ou mais",           "% dos moradores",                              "pontos",    1L,             "Grupo de 70 anos ou mais (v01041) sobre a soma publicada dos 11 grupos.",
  "envelhecimento",     "indices",    "Índice de envelhecimento",               "pessoas de 60 anos ou mais por 100 de 0 a 14 anos", "relativa", 1L,            "Moradores de 60 anos ou mais por 100 moradores de 0 a 14 anos (o padrão do IBGE usa 65 anos ou mais, que os grupos publicados não permitem).",
  "dependencia",        "indices",    "Razão de dependência",                   "dependentes por 100 pessoas de 15 a 59 anos",  "relativa",  1L,             "Moradores de 0 a 14 anos e de 60 anos ou mais por 100 moradores de 15 a 59 anos.",
  "cobertura_idade",    "qualidade",  "Cobertura da idade",                     "% dos moradores",                              "pontos",    1L,             "Moradores com faixa de idade publicada (v01031 a v01041) sobre o total de pessoas (v0001, sem sigilo).",
  "cobertura_piramide", "qualidade",  "Cobertura de sexo e idade",              "% dos moradores",                              "pontos",    1L,             "Moradores com sexo e faixa de idade publicados (v01009 a v01030) sobre o total de pessoas (v0001)."
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
) |>
  # Calculando os totais dos 11 grupos de idade e da pirâmide
  mutate(
    total_idades   = rowSums(pick(all_of(grupos_idade$campo_total))),
    total_piramide = rowSums(pick(all_of(c(grupos_idade$campo_homens, grupos_idade$campo_mulheres))))
  )

# Calculando os indicadores em formato largo
idade_largo <- totais |>
  mutate(
    # Moradores nas quatro faixas largas (somas exatas dos grupos do IBGE)
    n_0_14             = v01031 + v01032 + v01033,
    n_15_29            = v01034 + v01035 + v01036,
    n_30_59            = v01037 + v01038 + v01039,
    n_60_mais          = v01040 + v01041,
    pct_0_14           = 100 * n_0_14 / total_idades,
    pct_15_29          = 100 * n_15_29 / total_idades,
    pct_30_59          = 100 * n_30_59 / total_idades,
    pct_60_mais        = 100 * n_60_mais / total_idades,
    pct_70_mais        = 100 * v01041 / total_idades,
    # Índices
    envelhecimento     = 100 * n_60_mais / n_0_14,
    dependencia        = 100 * (n_0_14 + n_60_mais) / (n_15_29 + n_30_59),
    # Qualidade
    cobertura_idade    = 100 * total_idades / v0001,
    cobertura_piramide = 100 * total_piramide / v0001
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
idade <- idade_largo |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  )

# Criando a tabela dos 11 grupos de idade: moradores e proporção sobre a soma
# publicada dos grupos
grupos <- totais |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    total_idades,
    all_of(grupos_idade$campo_total)
  ) |>
  pivot_longer(
    cols      = all_of(grupos_idade$campo_total),
    names_to  = "campo_total",
    values_to = "moradores"
  ) |>
  left_join(grupos_idade |> select(grupo, rotulo_grupo, largura_anos, campo_total), by = "campo_total") |>
  mutate(
    pct = 100 * moradores / total_idades
  ) |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    grupo,
    rotulo_grupo,
    largura_anos,
    moradores,
    pct
  ) |>
  arrange(nivel, codigo_distrito, grupo)

# Criando a tabela da pirâmide: moradores por sexo e grupo de idade, com a
# proporção sobre a soma publicada de sexo e idade
piramide <- totais |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    total_piramide,
    all_of(c(grupos_idade$campo_homens, grupos_idade$campo_mulheres))
  ) |>
  pivot_longer(
    cols      = all_of(c(grupos_idade$campo_homens, grupos_idade$campo_mulheres)),
    names_to  = "campo",
    values_to = "moradores"
  ) |>
  mutate(
    sexo = if_else(campo %in% grupos_idade$campo_homens, "Homens", "Mulheres")
  ) |>
  left_join(
    bind_rows(
      grupos_idade |> transmute(campo = campo_homens, grupo, rotulo_grupo, largura_anos),
      grupos_idade |> transmute(campo = campo_mulheres, grupo, rotulo_grupo, largura_anos)
    ),
    by = "campo"
  ) |>
  mutate(
    pct = 100 * moradores / total_piramide
  ) |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    sexo,
    grupo,
    rotulo_grupo,
    largura_anos,
    moradores,
    pct
  ) |>
  arrange(nivel, codigo_distrito, sexo, grupo)

# Contando os setores sem idade publicada por distrito, para a tabela de revisão
setores_sem_idade <- setores_distritos |>
  group_by(codigo_distrito) |>
  summarise(
    setores_sem_idade = sum(is.na(v01031) | is.na(v01041)),
    .groups           = "drop"
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
    "Quatro faixas largas somam 100% em cada distrito",
    "Os 11 grupos de idade somam 100% em cada distrito",
    "A pirâmide soma 100% em cada distrito",
    "Índices positivos e proporções entre 0 e 100",
    "Células ausentes são só do símbolo X (sigilo)"
  ),
  ok          = c(
    nrow(filter(idade_largo, nivel == "distrito")) == 96,
    nrow(setores_distritos) == nrow(setores),
    all(abs(unlist(soma_setores[campos_comuns]) - unlist(municipio[campos_comuns])) < 1e-6),
    sum(filter(totais, nivel == "distrito")$v0001) == filter(totais, nivel == "municipio")$v0001,
    all(abs(idade_largo$pct_0_14 + idade_largo$pct_15_29 + idade_largo$pct_30_59 + idade_largo$pct_60_mais - 100) < 1e-6),
    grupos |> group_by(codigo_distrito) |> summarise(s = sum(pct)) |> pull(s) |> (\(x) all(abs(x - 100) < 1e-6))(),
    piramide |> group_by(codigo_distrito) |> summarise(s = sum(pct)) |> pull(s) |> (\(x) all(abs(x - 100) < 1e-6))(),
    all(idade$valor[idade$indicador %in% c("envelhecimento", "dependencia")] > 0) &&
      all(idade$valor[!idade$indicador %in% c("envelhecimento", "dependencia")] >= 0 &
            idade$valor[!idade$indicador %in% c("envelhecimento", "dependencia")] <= 100),
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
  "Cobertura da idade no município: ", round(filter(idade_largo, nivel == "municipio")$cobertura_idade, 2),
  "%; mínima entre os distritos: ", round(min(filter(idade_largo, nivel == "distrito")$cobertura_idade), 2), "%. ",
  "Cobertura de sexo e idade: ", round(filter(idade_largo, nivel == "municipio")$cobertura_piramide, 2),
  "% no município; mínima: ", round(min(filter(idade_largo, nivel == "distrito")$cobertura_piramide), 2), "%"
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
  x    = idade,
  sink = file.path(diretorio_grupo, "idade.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando os 11 grupos de idade
write_parquet(
  x    = grupos,
  sink = file.path(diretorio_grupo, "grupos.parquet")
)

# Gravando a pirâmide
write_parquet(
  x    = piramide,
  sink = file.path(diretorio_grupo, "piramide.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores      = idade_largo |>
      left_join(setores_sem_idade, by = "codigo_distrito") |>
      mutate(
        setores_sem_idade = if_else(nivel == "distrito", setores_sem_idade, NA_integer_)
      ) |>
      select(-nivel, -nome_rotulo) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                               = codigo_distrito,
        `Distrito`                             = nome_distrito,
        `Setores sem idade publicada (sigilo)` = setores_sem_idade
      ),
    grupos_pct       = grupos |>
      select(nivel, codigo_distrito, nome_distrito, rotulo_grupo, pct) |>
      mutate(rotulo_grupo = factor(rotulo_grupo, levels = grupos_idade$rotulo_grupo)) |>
      pivot_wider(names_from = rotulo_grupo, values_from = pct, names_glue = "{rotulo_grupo} anos (%)") |>
      select(-nivel),
    grupos_moradores = grupos |>
      select(nivel, codigo_distrito, nome_distrito, rotulo_grupo, moradores) |>
      mutate(rotulo_grupo = factor(rotulo_grupo, levels = grupos_idade$rotulo_grupo)) |>
      pivot_wider(names_from = rotulo_grupo, values_from = moradores, names_glue = "{rotulo_grupo} anos") |>
      select(-nivel),
    descricao        = indicadores |> select(-ordem, -casas_decimais, -comparacao)
  ),
  path = file.path(diretorio_resultados, "tabela_etarias.xlsx")
)

# Informando o fim do script
message("Indicadores de idade gravados em ", diretorio_grupo)
