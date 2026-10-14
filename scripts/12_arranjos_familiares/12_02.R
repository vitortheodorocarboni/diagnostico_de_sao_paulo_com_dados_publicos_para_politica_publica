################################################################################
##################################            ##################################
############ 12.02) PREPARAR OS INDICADORES DOS ARRANJOS FAMILIARES ############
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
diretorio_grupo <- caminho_data("12_arranjos_familiares")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("12_arranjos_familiares")

# Definindo os valores de referência do município medidos em 2026-10-12
# (etapa 10 do plano), em %
referencia_mesmo_sexo         <- 1.0
referencia_mulher_sem_conjuge <- 14.9

# Descrevendo as categorias das duas composições, com os campos do universo
categorias_composicao <- tribble(
  ~tabela,      ~categoria_codigo, ~rotulo_categoria,                     ~campo,
  "especie",    1L,                "Unipessoal",                          "v01209",
  "especie",    2L,                "Nuclear",                             "v01210",
  "especie",    3L,                "Estendida",                           "v01211",
  "especie",    4L,                "Composta",                            "v01212",
  "composicao", 1L,                "Casal sem filhos",                    "v01191",
  "composicao", 2L,                "Casal com filhos de ambos",           "v01192",
  "composicao", 3L,                "Casal com filho de só um",            "v01193",
  "composicao", 4L,                "Responsável sem cônjuge com filhos",  "v01194",
  "composicao", 5L,                "Outros",                              "v01195"
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar os campos publicados e calcular os indicadores.
# Cada proporção é a soma dos valores publicados do numerador sobre a soma dos
# valores publicados do denominador do próprio bloco, sem imputar as células
# com sigilo (valores ausentes entram como zero na soma, não como estimativa).
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

  # Calculando os indicadores em pontos percentuais (0 a 100)
  totais |>
    mutate(
      # --- Espécie de unidade doméstica (base: soma das quatro espécies)
      base_especie                = v01209 + v01210 + v01211 + v01212,
      pct_unipessoal              = 100 * v01209 / base_especie,
      pct_nuclear                 = 100 * v01210 / base_especie,
      pct_estendida               = 100 * v01211 / base_especie,
      pct_composta                = 100 * v01212 / base_especie,
      # --- Composição (base: soma das cinco composições)
      base_composicao             = v01191 + v01192 + v01193 + v01194 + v01195,
      pct_casal_sem_filhos        = 100 * v01191 / base_composicao,
      pct_casal_com_filhos        = 100 * v01192 / base_composicao,
      pct_casal_filho_de_um       = 100 * v01193 / base_composicao,
      pct_sem_conjuge_com_filhos  = 100 * v01194 / base_composicao,
      pct_outra_composicao        = 100 * v01195 / base_composicao,
      pct_mulher_sem_conjuge      = 100 * v01204 / base_composicao,
      pct_mulher_entre_sem_conjuge = 100 * v01204 / v01194,
      # --- Responsável pelo domicílio (base: responsáveis por sexo e por idade)
      pct_responsavel_mulher      = 100 * v01063 / (v01062 + v01063),
      pct_responsavel_60_mais     = 100 * v01068 / (v01064 + v01065 + v01066 + v01067 + v01068),
      # --- Cônjuges (base: cônjuges; e domicílios com cônjuge)
      pct_conjuges_mesmo_sexo     = 100 * v01044 / (v01043 + v01044),
      pct_domicilios_mesmo_sexo   = 100 * v01177 / (v01176 + v01177 + v01178),
      # --- Parentesco (base: filhos do casal; e moradores)
      pct_filhos_25_mais          = 100 * (v01089 + v01090 + v01091 + v01092 + v01093 + v01094) /
                                      (v01084 + v01085 + v01086 + v01087 + v01088 + v01089 +
                                         v01090 + v01091 + v01092 + v01093 + v01094),
      pct_netos                   = 100 * v01051 / v01006,
      pct_pais_sogros             = 100 * (v01049 + v01050) / v01006,
      # --- Qualidade: bases das proporções
      domicilios_especie          = base_especie,
      moradores                   = v01006
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
  ~indicador,                     ~grupo,         ~base_sigilo,      ~rotulo,                                                   ~unidade,                                   ~descricao,
  "pct_unipessoal",               "especie",      "sigilo_universo", "Domicílios unipessoais",                                  "% dos domicílios particulares ocupados",   "Espécie de unidade doméstica unipessoal (v01209) sobre a soma das quatro espécies (v01209 a v01212).",
  "pct_nuclear",                  "especie",      "sigilo_universo", "Domicílios nucleares",                                    "% dos domicílios particulares ocupados",   "Unidade doméstica nuclear (v01210): responsável com cônjuge e/ou filhos, sem outros parentes.",
  "pct_estendida",                "especie",      "sigilo_universo", "Domicílios estendidos",                                   "% dos domicílios particulares ocupados",   "Unidade doméstica estendida (v01211): com outros parentes do responsável.",
  "pct_composta",                 "especie",      "sigilo_universo", "Domicílios compostos",                                    "% dos domicílios particulares ocupados",   "Unidade doméstica composta (v01212): com pessoas sem parentesco com o responsável.",
  "pct_casal_sem_filhos",         "composicao",   "sigilo_universo", "Casal sem filhos",                                        "% dos domicílios particulares ocupados",   "Responsável e cônjuge(s) sem filhos (v01191) sobre a soma das cinco composições (v01191 a v01195).",
  "pct_casal_com_filhos",         "composicao",   "sigilo_universo", "Casal com filhos de ambos",                               "% dos domicílios particulares ocupados",   "Responsável e cônjuge com filhos só de ambos (v01192).",
  "pct_casal_filho_de_um",        "composicao",   "sigilo_universo", "Casal com filho de só um",                                "% dos domicílios particulares ocupados",   "Responsável e cônjuge(s) com pelo menos um filho só do responsável ou só do cônjuge (v01193).",
  "pct_sem_conjuge_com_filhos",   "composicao",   "sigilo_universo", "Responsável sem cônjuge com filhos",                      "% dos domicílios particulares ocupados",   "Responsável sem cônjuge com filho(s) e/ou enteado(s) (v01194).",
  "pct_outra_composicao",         "composicao",   "sigilo_universo", "Outras composições",                                      "% dos domicílios particulares ocupados",   "Outros tipos de composição (v01195), inclusive quem mora sozinho.",
  "pct_mulher_sem_conjuge",       "composicao",   "sigilo_universo", "Mulher responsável sem cônjuge com filhos",               "% dos domicílios particulares ocupados",   "Responsável mulher sem cônjuge com filho(s) e/ou enteado(s) (v01204) sobre a soma das cinco composições.",
  "pct_mulher_entre_sem_conjuge", "composicao",   "sigilo_universo", "Mulheres entre os responsáveis sem cônjuge com filhos",   "% dos domicílios sem cônjuge com filhos",  "v01204 sobre v01194.",
  "pct_responsavel_mulher",       "responsavel",  "sigilo_universo", "Responsável mulher",                                      "% dos responsáveis",                       "Responsável pelo domicílio do sexo feminino (v01063) sobre os responsáveis por sexo (v01062 e v01063).",
  "pct_responsavel_60_mais",      "responsavel",  "sigilo_universo", "Responsável com 60 anos ou mais",                         "% dos responsáveis",                       "Responsável com 60 anos ou mais (v01068) sobre os responsáveis por idade (v01064 a v01068).",
  "pct_conjuges_mesmo_sexo",      "conjuges",     "sigilo_pessoas",  "Cônjuges do mesmo sexo",                                  "% dos cônjuges",                           "Cônjuge ou companheiro(a) do mesmo sexo do responsável (v01044) sobre os cônjuges (v01043 e v01044).",
  "pct_domicilios_mesmo_sexo",    "conjuges",     "sigilo_universo", "Domicílios com cônjuge do mesmo sexo",                    "% dos domicílios com cônjuge",             "Domicílio com cônjuge do mesmo sexo e nenhum de sexo diferente (v01177) sobre os domicílios com cônjuge (v01176 a v01178).",
  "pct_filhos_25_mais",           "parentesco",   "sigilo_pessoas",  "Filhos do casal com 25 anos ou mais",                     "% dos filhos do responsável e do cônjuge", "Filhos do responsável e do cônjuge com 25 anos ou mais (v01089 a v01094) sobre todos os filhos do casal (v01084 a v01094); não inclui filhos só do responsável nem enteados.",
  "pct_netos",                    "parentesco",   "sigilo_pessoas",  "Netos do responsável",                                    "% dos moradores",                          "Neto(a) do responsável (v01051) sobre os moradores (v01006).",
  "pct_pais_sogros",              "parentesco",   "sigilo_pessoas",  "Pais ou sogros do responsável",                           "% dos moradores",                          "Pai, mãe, padrasto, madrasta (v01049) ou sogro(a) (v01050) sobre os moradores (v01006).",
  "domicilios_especie",           "qualidade",    "sigilo_universo", "Domicílios com espécie publicada",                        "domicílios",                               "Soma publicada das quatro espécies de unidade doméstica (v01209 a v01212).",
  "moradores",                    "qualidade",    "sigilo_pessoas",  "Moradores",                                               "pessoas",                                  "Quantidade de moradores (v01006)."
) |>
  mutate(
    # Casas decimais na apresentação e forma de comparar com o município
    casas_decimais = if_else(unidade %in% c("domicílios", "pessoas"), 0L, 1L),
    comparacao     = if_else(unidade %in% c("domicílios", "pessoas"), "relativa", "pontos"),
    ordem          = row_number()
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
arranjos_largo <- bind_rows(
  indicadores_distritos,
  indicadores_municipio
) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
arranjos <- arranjos_largo |>
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
  )

# Criando a tabela das duas composições (espécie e composição), com a
# proporção de cada categoria sobre a soma publicada da própria tabela
composicoes <- arranjos_largo |>
  select(
    nivel_territorial = nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    all_of(unique(categorias_composicao$campo))
  ) |>
  pivot_longer(
    cols      = all_of(unique(categorias_composicao$campo)),
    names_to  = "campo",
    values_to = "domicilios"
  ) |>
  inner_join(categorias_composicao, by = "campo") |>
  group_by(tabela, codigo_distrito) |>
  mutate(
    pct = 100 * domicilios / sum(domicilios)
  ) |>
  ungroup() |>
  select(
    tabela,
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    categoria_codigo,
    rotulo_categoria,
    domicilios,
    pct
  ) |>
  arrange(tabela, nivel_territorial, codigo_distrito, categoria_codigo)

# Contando as células ausentes (sigilo) por distrito, para a tabela de revisão
ausencias_distrito <- ausencias |>
  inner_join(setor_distrito, by = "codigo_setor") |>
  count(codigo_distrito, name = "celulas_com_sigilo")


################################################################################
##### VII. Conferências
################################################################################

# Obtendo os campos da tabela municipal que também estão nos setores
campos_comuns <- setdiff(
  intersect(names(municipio), names(setores)),
  c("codigo_municipio")
)

# Somando os setores nesses campos
soma_setores <- setores |>
  summarise(
    across(
      .cols = all_of(campos_comuns),
      .fns  = ~ sum(.x, na.rm = TRUE)
    )
  )

# Calculando a soma dos domicílios por presença de cônjuge no município
base_conjuge_municipio <- with(
  indicadores_municipio,
  v01176 + v01177 + v01178 + v01179
)

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "Distritos com indicadores",
    "Setores ligados",
    "Soma dos setores igual à tabela municipal em todos os campos do grupo",
    "Espécies e composições somam 100% em cada distrito",
    "As três classificações de domicílios (espécie, composição, cônjuge) somam o mesmo no município (até 0,5%)",
    "Células ausentes são só do símbolo X (sigilo)",
    "Nenhuma proporção fora de 0 a 100",
    "Município igual aos valores de referência da etapa 10 (mesmo sexo e mulher sem cônjuge)"
  ),
  ok          = c(
    nrow(indicadores_distritos) == 96,
    nrow(setores_distritos) == nrow(setores),
    length(campos_comuns) == sum(str_detect(names(setores), "^v0")) &&
      all(abs(unlist(soma_setores[campos_comuns]) - unlist(municipio[campos_comuns])) < 1e-6),
    composicoes |>
      group_by(tabela, codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    abs(indicadores_municipio$base_composicao / indicadores_municipio$base_especie - 1) < 0.005 &&
      abs(base_conjuge_municipio / indicadores_municipio$base_especie - 1) < 0.005,
    all(ausencias$ausencia_original == "X"),
    arranjos |>
      filter(str_starts(indicador, "pct_")) |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    abs(indicadores_municipio$pct_conjuges_mesmo_sexo - referencia_mesmo_sexo) < 0.05 &&
      abs(indicadores_municipio$pct_mulher_sem_conjuge - referencia_mulher_sem_conjuge) < 0.05
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
  x    = arranjos,
  sink = file.path(diretorio_grupo, "arranjos.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando as composições
write_parquet(
  x    = composicoes,
  sink = file.path(diretorio_grupo, "composicoes.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = arranjos_largo |>
      left_join(ausencias_distrito, by = "codigo_distrito") |>
      mutate(celulas_com_sigilo = if_else(nivel == "distrito", replace_na(celulas_com_sigilo, 0L), NA_integer_)) |>
      select(
        codigo_distrito,
        nome_distrito,
        all_of(indicadores$indicador),
        celulas_com_sigilo
      ) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                                   = codigo_distrito,
        `Distrito`                                 = nome_distrito,
        `Células com sigilo (setores do distrito)` = celulas_com_sigilo
      ),
    composicoes = composicoes |>
      mutate(pct = round(pct, 3)) |>
      select(tabela, nivel_territorial, codigo_distrito, nome_distrito, categoria_codigo, rotulo_categoria, domicilios, pct),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, resultados do universo por setor censitário, tema parentesco (dados públicos).",
        "Os totais por distrito somam só os valores publicados por setor; as células com sigilo ficam fora, sem imputação (na capital, a soma dos setores coincide com a tabela municipal).",
        "Proporções sobre a soma publicada das categorias de cada bloco; as bases em domicílios (espécie, composição, responsável) e em pessoas (cônjuges, filhos, netos, pais ou sogros) não se misturam.",
        "Domicílios particulares ocupados; as unidades em domicílio coletivo ficam fora das espécies e das composições."
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_arranjos_familiares.xlsx")
)

# Informando o fim do script
message("Indicadores dos arranjos familiares gravados em ", diretorio_grupo)
