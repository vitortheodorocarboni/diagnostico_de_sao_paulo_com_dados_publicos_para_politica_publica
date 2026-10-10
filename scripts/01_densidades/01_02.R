################################################################################
##################################            ##################################
################# 01.02) PREPARAR OS INDICADORES DE DENSIDADE ##################
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
diretorio_grupo <- caminho_data("01_densidades")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("01_densidades")


################################################################################
##### III. Funções
################################################################################

# Criando uma função para calcular os indicadores a partir de setores agrupados
calcular_indicadores <- function(setores_agrupados) {

  # Somando os totais e calculando as razões a partir das somas
  setores_agrupados |>
    summarise(
      # Totais (a área urbana vem antes da total, porque summarise() passa a
      # usar a soma assim que area_km2 é redefinida)
      moradores                        = sum(v0001),
      domicilios_ocupados              = sum(v0007),
      area_urbana_km2                  = sum(area_km2[situacao == "Urbana"]),
      area_km2                         = sum(area_km2),
      moradores_urbanos                = sum(v0001[situacao == "Urbana"]),
      # Moradores em domicílios particulares ocupados, recompostos pela média
      # do setor (v0005, com duas casas decimais) vezes os domicílios ocupados
      moradores_em_domicilios_ocupados = sum(v0005 * v0007),
      .groups                          = "drop"
    ) |>
    mutate(
      # Densidades sobre a área total e sobre a área dos setores urbanos
      densidade_demografica   = moradores / area_km2,
      densidade_urbana        = if_else(area_urbana_km2 > 0, moradores_urbanos / area_urbana_km2, NA_real_),
      densidade_domiciliar    = domicilios_ocupados / area_km2,
      # Média de moradores por domicílio particular ocupado
      moradores_por_domicilio = moradores_em_domicilios_ocupados / domicilios_ocupados
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
  ~indicador,                         ~rotulo,                                        ~unidade,                             ~casas_decimais, ~descricao,
  "moradores",                        "Moradores",                                    "moradores",                          0,               "Total de pessoas (v0001).",
  "domicilios_ocupados",              "Domicílios particulares ocupados",             "domicílios",                         0,               "Total de domicílios particulares ocupados (v0007).",
  "area_km2",                         "Área total",                                   "km²",                                1,               "Soma da área dos setores (inclui parques, represas e zona rural).",
  "area_urbana_km2",                  "Área dos setores urbanos",                     "km²",                                1,               "Soma da área dos setores de situação urbana.",
  "moradores_urbanos",                "Moradores dos setores urbanos",                "moradores",                          0,               "Total de pessoas nos setores de situação urbana.",
  "moradores_em_domicilios_ocupados", "Moradores em domicílios ocupados",             "moradores",                          0,               "Soma, por setor, da média de moradores (v0005) vezes os domicílios ocupados (v0007).",
  "densidade_demografica",            "Densidade demográfica",                        "moradores por km²",                  0,               "Moradores / área total.",
  "densidade_urbana",                 "Densidade na área urbana",                     "moradores por km² urbano",           0,               "Moradores dos setores urbanos / área dos setores urbanos.",
  "densidade_domiciliar",             "Densidade domiciliar",                         "domicílios ocupados por km²",        0,               "Domicílios particulares ocupados / área total.",
  "moradores_por_domicilio",          "Moradores por domicílio",                      "moradores por domicílio ocupado",    2,               "Moradores em domicílios ocupados / domicílios particulares ocupados."
) |>
  mutate(
    ordem = row_number()
  )


################################################################################
##### VI. Indicadores por distrito e do município
################################################################################

# Interrompendo se houver células ausentes, que este grupo não trata
if (nrow(ausencias) > 0) {
  stop(
    "Há ", nrow(ausencias), " células ausentes nos campos do grupo; ",
    "revise a regra de soma antes de seguir."
  )
}

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
densidades_largo <- bind_rows(
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
densidades <- densidades_largo |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  )


################################################################################
##### VII. Conferências
################################################################################

# Montando as conferências contra a tabela municipal do IBGE
conferencias <- tibble(
  conferencia = c(
    "Distritos com indicadores",
    "Setores ligados",
    "Moradores iguais à tabela municipal",
    "Domicílios ocupados iguais à tabela municipal",
    "Área igual à tabela municipal (0,1 km²)",
    "Média de moradores igual à tabela municipal (1 casa decimal)"
  ),
  ok          = c(
    nrow(indicadores_distritos) == 96,
    nrow(setores_distritos) == nrow(setores),
    indicadores_municipio$moradores == municipio$v0001,
    indicadores_municipio$domicilios_ocupados == municipio$v0007,
    abs(indicadores_municipio$area_km2 - municipio$area_km2) < 0.1,
    round(indicadores_municipio$moradores_por_domicilio, 1) == municipio$v0005
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
  x    = densidades,
  sink = file.path(diretorio_grupo, "densidades.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    densidades   = densidades_largo |>
      select(-nivel, -nome_rotulo) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`   = codigo_distrito,
        `Distrito` = nome_distrito
      ),
    indicadores  = indicadores |> select(-ordem)
  ),
  path = file.path(diretorio_resultados, "tabela_densidades.xlsx")
)

# Informando o fim do script
message("Indicadores de densidade gravados em ", diretorio_grupo)
