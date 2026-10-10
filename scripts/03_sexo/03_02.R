################################################################################
##################################            ##################################
#################### 03.02) PREPARAR OS INDICADORES DE SEXO ####################
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
diretorio_grupo <- caminho_data("03_sexo")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("03_sexo")


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar os campos publicados e calcular os indicadores.
# Cada razão usa a soma dos valores publicados; as células com sigilo ficam
# fora da soma, sem imputação.
calcular_indicadores <- function(setores_agrupados) {

  # Somando os campos de cada setor
  totais <- setores_agrupados |>
    summarise(
      across(
        .cols = matches("^v0[0-9]+$"),
        .fns  = ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  # Calculando os indicadores
  totais |>
    mutate(
      # Totais por sexo
      moradores_homens   = v01007,
      moradores_mulheres = v01008,
      # Homens por faixa de idade (v01009 a v01019) e mulheres (v01020 a v01030)
      homens_0_14        = v01009 + v01010 + v01011,
      homens_15_29       = v01012 + v01013 + v01014,
      homens_30_59       = v01015 + v01016 + v01017,
      homens_60_mais     = v01018 + v01019,
      mulheres_0_14      = v01020 + v01021 + v01022,
      mulheres_15_29     = v01023 + v01024 + v01025,
      mulheres_30_59     = v01026 + v01027 + v01028,
      mulheres_60_mais   = v01029 + v01030,
      # Proporção de mulheres e razão de sexo (homens por 100 mulheres)
      pct_mulheres       = 100 * v01008 / (v01007 + v01008),
      razao_sexo         = 100 * v01007 / v01008,
      razao_0_14         = 100 * homens_0_14 / mulheres_0_14,
      razao_15_29        = 100 * homens_15_29 / mulheres_15_29,
      razao_30_59        = 100 * homens_30_59 / mulheres_30_59,
      razao_60_mais      = 100 * homens_60_mais / mulheres_60_mais,
      # Qualidade: moradores com sexo publicado sobre o total de pessoas, e
      # moradores com sexo e idade publicados sobre os com sexo publicado
      cobertura_sexo     = 100 * (v01007 + v01008) / v0001,
      cobertura_faixas   = 100 * (homens_0_14 + homens_15_29 + homens_30_59 + homens_60_mais +
                                    mulheres_0_14 + mulheres_15_29 + mulheres_30_59 + mulheres_60_mais) /
        (v01007 + v01008)
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
  ~indicador,           ~grupo,      ~rotulo,                                  ~unidade,                         ~comparacao, ~casas_decimais, ~descricao,
  "moradores_homens",   "sexo",      "Moradores do sexo masculino",            "moradores",                      "relativa",  0L,             "Soma publicada de pessoas do sexo masculino (v01007).",
  "moradores_mulheres", "sexo",      "Moradores do sexo feminino",             "moradores",                      "relativa",  0L,             "Soma publicada de pessoas do sexo feminino (v01008).",
  "pct_mulheres",       "sexo",      "Mulheres entre os moradores",            "% dos moradores",                "pontos",    1L,             "Mulheres sobre a soma publicada de homens e mulheres (v01008 / (v01007 + v01008)).",
  "razao_sexo",         "razao",     "Razão de sexo",                          "homens por 100 mulheres",        "relativa",  1L,             "Homens (v01007) por 100 mulheres (v01008); 100 indica paridade.",
  "razao_0_14",         "razao",     "Razão de sexo, 0 a 14 anos",             "homens por 100 mulheres",        "relativa",  1L,             "Soma publicada das faixas de 0 a 4, 5 a 9 e 10 a 14 anos (v01009 a v01011 e v01020 a v01022).",
  "razao_15_29",        "razao",     "Razão de sexo, 15 a 29 anos",            "homens por 100 mulheres",        "relativa",  1L,             "Soma publicada das faixas de 15 a 19, 20 a 24 e 25 a 29 anos (v01012 a v01014 e v01023 a v01025).",
  "razao_30_59",        "razao",     "Razão de sexo, 30 a 59 anos",            "homens por 100 mulheres",        "relativa",  1L,             "Soma publicada das faixas de 30 a 39, 40 a 49 e 50 a 59 anos (v01015 a v01017 e v01026 a v01028).",
  "razao_60_mais",      "razao",     "Razão de sexo, 60 anos ou mais",         "homens por 100 mulheres",        "relativa",  1L,             "Soma publicada das faixas de 60 a 69 e 70 anos ou mais (v01018, v01019, v01029 e v01030).",
  "cobertura_sexo",     "qualidade", "Cobertura do sexo",                      "% dos moradores",                "pontos",    1L,             "Moradores com sexo publicado (v01007 + v01008) sobre o total de pessoas (v0001, sem sigilo).",
  "cobertura_faixas",   "qualidade", "Cobertura de sexo e idade",              "% dos moradores com sexo",       "pontos",    1L,             "Moradores com sexo e faixa de idade publicados sobre os com sexo publicado."
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

# Calculando os indicadores por distrito
indicadores_distritos <- setores_distritos |>
  group_by(codigo_distrito) |>
  calcular_indicadores() |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nivel = "distrito"
  )

# Calculando os indicadores do município pela soma dos setores
indicadores_municipio <- setores_distritos |>
  calcular_indicadores() |>
  mutate(
    codigo_distrito = municipio$codigo_municipio,
    nome_distrito   = "Município de São Paulo",
    nome_rotulo     = "Município de São Paulo",
    nivel           = "municipio"
  )

# Juntando distritos e município em formato largo
sexo_largo <- bind_rows(
  indicadores_distritos,
  indicadores_municipio
) |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    all_of(indicadores$indicador)
  ) |>
  arrange(nivel, codigo_distrito)

# Passando para o formato longo
sexo <- sexo_largo |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  )

# Contando os setores sem valor de sexo por distrito, para a tabela de revisão
setores_sem_sexo <- setores_distritos |>
  group_by(codigo_distrito) |>
  summarise(
    setores_sem_sexo = sum(is.na(v01007) | is.na(v01008)),
    .groups          = "drop"
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
    "Homens e mulheres publicados não passam do total de pessoas, em cada distrito",
    "Razões de sexo positivas e proporções entre 0 e 100",
    "Células ausentes são só do símbolo X (sigilo)"
  ),
  ok          = c(
    nrow(indicadores_distritos) == 96,
    nrow(setores_distritos) == nrow(setores),
    all(abs(unlist(soma_setores[campos_comuns]) - unlist(municipio[campos_comuns])) < 1e-6),
    sum(indicadores_distritos$v0001) == indicadores_municipio$v0001,
    all(indicadores_distritos$v01007 + indicadores_distritos$v01008 <= indicadores_distritos$v0001),
    all(sexo$valor[str_detect(sexo$indicador, "^razao")] > 0) &&
      all(sexo$valor[sexo$indicador %in% c("pct_mulheres", "cobertura_sexo", "cobertura_faixas")] >= 0 &
            sexo$valor[sexo$indicador %in% c("pct_mulheres", "cobertura_sexo", "cobertura_faixas")] <= 100),
    all(ausencias$ausencia_original == "X")
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencia = conferencias$conferencia),
  collapse = "\n"
))

# Mostrando a cobertura
message(
  "Cobertura do sexo no município: ", round(indicadores_municipio$cobertura_sexo, 2),
  "%; mínima entre os distritos: ", round(min(indicadores_distritos$cobertura_sexo), 2), "%. ",
  "Cobertura de sexo e idade: ", round(indicadores_municipio$cobertura_faixas, 2),
  "% no município; mínima: ", round(min(indicadores_distritos$cobertura_faixas), 2), "%"
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
  x    = sexo,
  sink = file.path(diretorio_grupo, "sexo.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = sexo_largo |>
      left_join(setores_sem_sexo, by = "codigo_distrito") |>
      mutate(
        setores_sem_sexo = if_else(nivel == "distrito", setores_sem_sexo, NA_integer_)
      ) |>
      select(-nivel, -nome_rotulo) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                             = codigo_distrito,
        `Distrito`                           = nome_distrito,
        `Setores sem sexo publicado (sigilo)` = setores_sem_sexo
      ),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao)
  ),
  path = file.path(diretorio_resultados, "tabela_sexo.xlsx")
)

# Informando o fim do script
message("Indicadores de sexo gravados em ", diretorio_grupo)
