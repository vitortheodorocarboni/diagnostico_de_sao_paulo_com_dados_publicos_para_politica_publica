################################################################################
##################################            ##################################
################ 17.02) PREPARAR OS INDICADORES DOS UNIPESSOAIS ################
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
diretorio_grupo <- caminho_data("17_unipessoais")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("17_unipessoais")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo os valores de referência do município medidos em 2026-10-13 na
# etapa 17a, em %: unipessoais na amostra e na espécie unipessoal do universo
referencia_unipessoal_amostra  <- 21.45
referencia_unipessoal_universo <- 21.85

# Descrevendo os cinco recortes da amostra: os campos do total de domicílios
# particulares ocupados e dos unipessoais, pela característica do responsável
recortes <- tribble(
  ~recorte,    ~rotulo_recorte,        ~campo_total,                  ~campo_unipessoal,
  "sexo",      "Sexo",                 "d0170_dpo",                   "d0170_dpo_unipessoal",
  "idade",     "Idade",                "d0180_dpo",                   "d0180_dpo_unipessoal",
  "cor_raca",  "Cor ou raça",          "p0210_responsavel_dpo",       "p0210_responsavel_dpo_unipessoal",
  "instrucao", "Instrução",            "p0770_responsavel_dpo",       "p0770_responsavel_dpo_unipessoal",
  "renda",     "Renda per capita",     "d0360_classe_salario_minimo", "d0360_classe_salario_minimo_unipessoal"
)

# Descrevendo os grupos de cada recorte, com as categorias do tratado que cada
# um reúne (os códigos de ignorado e de não determinado ficam fora)
grupos <- tribble(
  ~recorte,    ~grupo_codigo, ~rotulo_grupo,                              ~categorias,
  "sexo",      1L,            "Homens",                                   list(c("1")),
  "sexo",      2L,            "Mulheres",                                 list(c("2")),
  "idade",     1L,            "Até 39 anos",                              list(sprintf("%02d", 1:8)),
  "idade",     2L,            "40 a 59 anos",                             list(sprintf("%02d", 9:12)),
  "idade",     3L,            "60 a 79 anos",                             list(sprintf("%02d", 13:16)),
  "idade",     4L,            "80 anos ou mais",                          list(c("17")),
  "cor_raca",  1L,            "Branca",                                   list(c("1")),
  "cor_raca",  2L,            "Preta ou parda",                           list(c("2", "4")),
  "cor_raca",  3L,            "Amarela ou indígena",                      list(c("3", "5")),
  "instrucao", 1L,            "Sem instrução ou fundamental incompleto",  list(c("1")),
  "instrucao", 2L,            "Fundamental completo ou médio incompleto", list(c("2")),
  "instrucao", 3L,            "Médio completo ou superior incompleto",    list(c("3")),
  "instrucao", 4L,            "Superior completo",                        list(c("4")),
  "renda",     1L,            "Até 1/2 salário mínimo",                   list(sprintf("%02d", 1:2)),
  "renda",     2L,            "Mais de 1/2 a 2 salários mínimos",         list(sprintf("%02d", 3:4)),
  "renda",     3L,            "Mais de 2 a 5 salários mínimos",           list(sprintf("%02d", 5:7)),
  "renda",     4L,            "Mais de 5 salários mínimos",               list(sprintf("%02d", 8:10))
) |>
  # Desfazendo o aninhamento das categorias de cada grupo
  mutate(
    categorias = map(categorias, unlist)
  ) |>
  left_join(recortes, by = "recorte")


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar as estimativas das APs de cada grupo. As
# estimativas já vêm ponderadas pelo IBGE; somam-se as estimativas, os pesos e
# os registros, e os indicadores são recalculados a partir das somas.
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

# Criando uma função para somar, em cada linha de totais, as estimativas das
# categorias de um campo
somar_categorias <- function(totais, campo, categorias) {
  rowSums(as.matrix(totais[sprintf("%s_cat_codigo_%s_estimativa", campo, categorias)]))
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
  ~indicador,               ~grupo,    ~base,      ~rotulo,                                  ~unidade,                                   ~comparacao, ~casas_decimais, ~descricao,
  "pct_unipessoal",         "taxa",    "amostra",  "Domicílios unipessoais",                 "% dos domicílios particulares ocupados",   "pontos",    1L,              "Domicílios particulares ocupados com um morador (D0150 igual a 1) sobre todos os domicílios particulares ocupados (D0130 01 e 05), na amostra.",
  "pct_unipessoal_universo", "taxa",   "universo", "Domicílios unipessoais (universo)",      "% dos domicílios particulares ocupados",   "pontos",    1L,              "Espécie de unidade doméstica unipessoal (v01209) sobre a soma das quatro espécies (v01209 a v01212); o mesmo indicador do grupo 12.",
  "pct_mulheres",           "perfil",  "amostra",  "Mulheres entre quem mora só",            "% dos domicílios unipessoais",             "pontos",    1L,              "Sexo feminino do responsável (D0170 código 2) entre os domicílios unipessoais com sexo informado; o responsável é o único morador.",
  "pct_mulheres_universo",  "perfil",  "universo", "Mulheres entre quem mora só (universo)", "% dos domicílios unipessoais",             "pontos",    1L,              "Unipessoais com responsável do sexo feminino (v01215) sobre os de sexo masculino e feminino (v01214 e v01215).",
  "pct_60_mais",            "perfil",  "amostra",  "60 anos ou mais entre quem mora só",     "% dos domicílios unipessoais",             "pontos",    1L,              "Responsável de 60 anos ou mais (D0180 códigos 13 a 17) entre os unipessoais com idade informada.",
  "pct_preta_parda",        "perfil",  "amostra",  "Pretos e pardos entre quem mora só",     "% dos domicílios unipessoais",             "pontos",    1L,              "Responsável de cor ou raça preta ou parda (P0210 códigos 2 e 4) entre os unipessoais com cor ou raça informada.",
  "pct_superior",           "perfil",  "amostra",  "Superior completo entre quem mora só",   "% dos domicílios unipessoais",             "pontos",    1L,              "Responsável com superior completo (P0770 código 4, todas as idades) entre os unipessoais com instrução determinada.",
  "pct_renda_ate_meio",     "perfil",  "amostra",  "Renda até 1/2 salário mínimo entre quem mora só", "% dos domicílios unipessoais",    "pontos",    1L,              "Renda domiciliar per capita (do próprio morador) de até 1/2 salário mínimo (R$ 606; D0360 classes 01 e 02) entre os unipessoais com renda informada.",
  "registros_unipessoais",  "qualidade", "amostra", "Registros de domicílios unipessoais",    "registros",                                "relativa",  0L,              "Registros de domicílios unipessoais na amostra das APs do distrito; abaixo do mínimo, o distrito recebe aviso; abaixo do piso, fica sem valor nos indicadores de perfil."
) |>
  mutate(
    ordem = row_number()
  )

# Listando os indicadores de perfil da amostra, que ficam sem valor abaixo do
# piso (a taxa geral e a taxa por sexo não recebem o piso)
indicadores_com_piso <- c(
  "pct_mulheres",
  "pct_60_mais",
  "pct_preta_parda",
  "pct_superior",
  "pct_renda_ate_meio"
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

# Descrevendo cada distrito e o município: registros de unipessoais, aviso e
# piso (o município não recebe aviso nem piso)
territorio <- totais_amostra |>
  transmute(
    codigo_distrito,
    n_aps,
    nivel                 = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    registros_unipessoais = rowSums(as.matrix(totais_amostra[sprintf("d0170_dpo_unipessoal_cat_codigo_%s_n_registros", c("1", "2", "9"))])),
    amostra_menor         = nivel == "distrito" & registros_unipessoais < minimo_registros_unipessoais,
    sem_valor             = nivel == "distrito" & registros_unipessoais < piso_registros_unipessoais,
    domicilios_dpo        = totais_amostra$d0170_dpo_peso_observado + totais_amostra$d0170_dpo_peso_ignorado + totais_amostra$d0170_dpo_peso_ausente,
    domicilios_unipessoal = totais_amostra$d0170_dpo_unipessoal_peso_observado + totais_amostra$d0170_dpo_unipessoal_peso_ignorado + totais_amostra$d0170_dpo_unipessoal_peso_ausente
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  )

# Somando, em cada grupo de cada recorte, os domicílios, os unipessoais e os
# registros de unipessoais, por distrito e no município
grupos_longo <- grupos |>
  mutate(
    valores = pmap(
      .l = list(campo_total, campo_unipessoal, categorias),
      .f = \(total, unipessoal, categorias_grupo) tibble(
        codigo_distrito       = totais_amostra$codigo_distrito,
        domicilios            = somar_categorias(totais_amostra, total, categorias_grupo),
        unipessoais           = somar_categorias(totais_amostra, unipessoal, categorias_grupo),
        registros_unipessoais = rowSums(as.matrix(totais_amostra[sprintf("%s_cat_codigo_%s_n_registros", unipessoal, categorias_grupo)]))
      )
    )
  ) |>
  select(-categorias, -campo_total, -campo_unipessoal) |>
  unnest(valores) |>
  rename(registros_grupo = registros_unipessoais) |>
  inner_join(
    territorio |> select(nivel, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor, sem_valor),
    by = "codigo_distrito"
  )

# Calculando o perfil de quem mora só (composição de cada recorte entre os
# unipessoais com a característica informada), sem valor abaixo do piso
perfil <- grupos_longo |>
  group_by(codigo_distrito, recorte) |>
  mutate(
    valor = if_else(sem_valor, NA_real_, 100 * unipessoais / sum(unipessoais))
  ) |>
  ungroup() |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    sem_valor,
    recorte,
    rotulo_recorte,
    grupo_codigo,
    rotulo_grupo,
    unipessoais,
    registros_grupo,
    valor
  ) |>
  arrange(nivel, codigo_distrito, recorte, grupo_codigo)

# Calculando a taxa de unipessoais em cada grupo (unipessoais entre os
# domicílios do grupo); só a taxa por sexo, com muitos registros, escapa do piso
taxas <- grupos_longo |>
  mutate(
    valor = if_else(
      sem_valor & recorte != "sexo" | domicilios <= 0,
      NA_real_,
      100 * unipessoais / domicilios
    )
  ) |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    sem_valor,
    recorte,
    rotulo_recorte,
    grupo_codigo,
    rotulo_grupo,
    domicilios,
    unipessoais,
    registros_grupo,
    valor
  ) |>
  arrange(nivel, codigo_distrito, recorte, grupo_codigo)

# Reunindo os indicadores em formato largo: taxa geral, perfil e universo
unipessoais_largo <- territorio |>
  left_join(
    perfil |>
      filter(
        (recorte == "sexo" & grupo_codigo == 2L) |
          (recorte == "idade" & grupo_codigo %in% 3:4) |
          (recorte == "cor_raca" & grupo_codigo == 2L) |
          (recorte == "instrucao" & grupo_codigo == 4L) |
          (recorte == "renda" & grupo_codigo == 1L)
      ) |>
      mutate(
        indicador = case_when(
          recorte == "sexo"      ~ "pct_mulheres",
          recorte == "idade"     ~ "pct_60_mais",
          recorte == "cor_raca"  ~ "pct_preta_parda",
          recorte == "instrucao" ~ "pct_superior",
          recorte == "renda"     ~ "pct_renda_ate_meio"
        )
      ) |>
      group_by(codigo_distrito, indicador) |>
      summarise(valor = sum(valor), .groups = "drop") |>
      pivot_wider(names_from = indicador, values_from = valor),
    by = "codigo_distrito"
  ) |>
  left_join(totais_universo, by = "codigo_distrito") |>
  mutate(
    # --- Taxa geral na amostra, sem piso
    pct_unipessoal           = 100 * domicilios_unipessoal / domicilios_dpo,
    # --- Universo: espécie unipessoal e sexo do responsável nos unipessoais
    pct_unipessoal_universo  = 100 * v01209 / (v01209 + v01210 + v01211 + v01212),
    pct_mulheres_universo    = 100 * v01215 / (v01214 + v01215)
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
unipessoais <- unipessoais_largo |>
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


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(unipessoais_largo, nivel == "distrito")
largo_municipio <- filter(unipessoais_largo, nivel == "municipio")

# Calculando, por AP, a maior diferença entre a soma das categorias e a soma dos
# pesos observado, ignorado e ausente de cada campo (total e unipessoal)
diferenca_pesos <- map_dbl(
  c(recortes$campo_total, recortes$campo_unipessoal),
  \(campo) {
    categorias <- names(amostra) |>
      str_subset(paste0("^", campo, "_cat_codigo_.*_estimativa$"))
    max(abs(
      rowSums(as.matrix(amostra[categorias])) -
        amostra[[paste0(campo, "_peso_observado")]] - amostra[[paste0(campo, "_peso_ignorado")]] - amostra[[paste0(campo, "_peso_ausente")]]
    ))
  }
)

# Calculando o total de unipessoais estimado em cada recorte, no município, que
# deve ser o mesmo nos cinco
totais_recortes <- map_dbl(
  recortes$campo_unipessoal,
  \(campo) sum(amostra_distritos[[paste0(campo, "_peso_observado")]] + amostra_distritos[[paste0(campo, "_peso_ignorado")]] + amostra_distritos[[paste0(campo, "_peso_ausente")]])
)

# Calculando o total de domicílios estimado em cada recorte, no município
totais_dpo <- map_dbl(
  recortes$campo_total,
  \(campo) sum(amostra_distritos[[paste0(campo, "_peso_observado")]] + amostra_distritos[[paste0(campo, "_peso_ignorado")]] + amostra_distritos[[paste0(campo, "_peso_ausente")]])
)

# Calculando os registros de unipessoais de cada recorte, que devem ser os mesmos
registros_recortes <- map_dbl(
  recortes$campo_unipessoal,
  \(campo) sum(as.matrix(amostra_distritos[str_subset(names(amostra_distritos), paste0("^", campo, "_cat_codigo_.*_n_registros$"))]))
)

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Categorias somam os pesos de cada campo, total e unipessoal, em cada AP",
    "Os cinco recortes têm o mesmo total de unipessoais estimados",
    "Os cinco recortes têm o mesmo total de domicílios estimados",
    "Os cinco recortes têm os mesmos registros de unipessoais",
    "Unipessoais não superam os domicílios em nenhum grupo",
    "Taxas e perfis dentro de 0 a 100",
    "Perfil soma 100% em cada recorte e distrito com valor",
    "Unipessoais da amostra no município próximos aos 21,45% medidos na etapa 17a",
    "Município: amostra e universo diferem menos de 1 ponto percentual na taxa de unipessoais",
    "Taxa de unipessoais: correlação entre os distritos, amostra e universo, de pelo menos 0,9",
    "Mulheres entre os unipessoais: município na amostra e no universo diferem menos de 2 pontos",
    "Universo: soma dos setores igual à tabela municipal (v01209 a v01212, v01214 e v01215)",
    "Universo: unipessoais por sexo (v01214 e v01215) somam a espécie unipessoal (v01209) com diferença de até 0,5%",
    "15 distritos com amostra menor e 3 sem valor (decisão de 2026-10-13)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    all(diferenca_pesos < 1e-6),
    all(abs(totais_recortes - totais_recortes[1]) < 1e-6 * totais_recortes[1]),
    all(abs(totais_dpo - totais_dpo[1]) < 1e-6 * totais_dpo[1]),
    all(registros_recortes == registros_recortes[1]),
    all(grupos_longo$unipessoais <= grupos_longo$domicilios + 1e-6),
    all(c(perfil$valor, taxas$valor) >= 0 & c(perfil$valor, taxas$valor) <= 100, na.rm = TRUE),
    perfil |>
      filter(!sem_valor) |>
      group_by(codigo_distrito, recorte) |>
      summarise(s = sum(valor), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    abs(largo_municipio$pct_unipessoal - referencia_unipessoal_amostra) < 0.05,
    abs(largo_municipio$pct_unipessoal - largo_municipio$pct_unipessoal_universo) < 1,
    cor(largo_distritos$pct_unipessoal, largo_distritos$pct_unipessoal_universo) >= 0.9,
    abs(largo_municipio$pct_mulheres - largo_municipio$pct_mulheres_universo) < 2,
    all(map_lgl(
      c("v01209", "v01210", "v01211", "v01212", "v01214", "v01215"),
      \(campo) largo_municipio[[campo]] == municipio[[campo]]
    )),
    abs(largo_municipio$v01209 - largo_municipio$v01214 - largo_municipio$v01215) < 0.005 * largo_municipio$v01209,
    sum(largo_distritos$amostra_menor) == 15 && sum(largo_distritos$sem_valor) == 3
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

# Informando os valores do município e a correlação entre as fontes
message(
  "Município: unipessoais ", round(largo_municipio$pct_unipessoal, 2),
  "% na amostra e ", round(largo_municipio$pct_unipessoal_universo, 2),
  "% no universo; mulheres entre os unipessoais ", round(largo_municipio$pct_mulheres, 1),
  "% (amostra) e ", round(largo_municipio$pct_mulheres_universo, 1),
  "% (universo); correlação da taxa entre os distritos: ",
  round(cor(largo_distritos$pct_unipessoal, largo_distritos$pct_unipessoal_universo), 3)
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
  x    = unipessoais,
  sink = file.path(diretorio_grupo, "unipessoais.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando o perfil de quem mora só
write_parquet(
  x    = perfil,
  sink = file.path(diretorio_grupo, "perfil.parquet")
)

# Gravando a taxa de unipessoais por grupo
write_parquet(
  x    = taxas,
  sink = file.path(diretorio_grupo, "taxas.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = unipessoais_largo |>
      select(
        codigo_distrito,
        nome_distrito,
        n_aps,
        amostra_menor,
        sem_valor,
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
        `Amostra menor (aviso)` = amostra_menor,
        `Sem valor (piso)`      = sem_valor
      ),
    perfil      = perfil |>
      mutate(valor = round(valor, 3)) |>
      select(nivel, codigo_distrito, nome_distrito, rotulo_recorte, rotulo_grupo, unipessoais, registros_grupo, valor),
    taxas       = taxas |>
      mutate(valor = round(valor, 3)) |>
      select(nivel, codigo_distrito, nome_distrito, rotulo_recorte, rotulo_grupo, domicilios, unipessoais, registros_grupo, valor),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), D0150, D0170, D0180, D0360, P0210 e P0770 do responsável; e universo por setor (espécie de unidade doméstica, v01209 a v01215).",
        "Dados de acesso controlado: uso interno, não publicar sem autorização. Os indicadores do universo (taxa de unipessoais e % de mulheres) são públicos.",
        "Domicílio unipessoal: domicílio particular ocupado (permanente ou improvisado, D0130 01 e 05) com um morador, que é a pessoa responsável. O perfil é o do responsável; a taxa é a dos unipessoais entre os domicílios da característica do responsável.",
        "Grupos: idade em 4 faixas; cor ou raça em 3 (branca, preta ou parda, amarela ou indígena); instrução nos 4 níveis do P0770 (todas as idades do responsável); renda per capita em 4 classes de salário mínimo (R$ 1.212 em 2022). Ignorados e instrução não determinada ficam fora.",
        "Aviso de amostra menor: menos de 200 registros de domicílios unipessoais no distrito; sem valor (perfil e taxas, exceto a taxa geral e a por sexo): menos de 100 registros. Os grupos pequenos de cada recorte têm menos registros que o distrito; a coluna de registros do grupo acompanha cada valor.",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        "No universo, os totais por distrito somam só os valores publicados por setor (células com sigilo fora, sem imputação); na capital, a soma dos setores coincide com a tabela municipal."
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_unipessoais.xlsx")
)

# Informando o fim do script
message("Indicadores dos unipessoais gravados em ", diretorio_grupo)
