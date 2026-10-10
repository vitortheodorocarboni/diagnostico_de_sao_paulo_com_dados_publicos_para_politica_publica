################################################################################
##################################            ##################################
############### 09.02) PREPARAR OS INDICADORES DOS TRABALHADORES ###############
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
diretorio_grupo <- caminho_data("09_trabalhadores")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("09_trabalhadores")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o código da linha de residência fora da capital na matriz
codigo_fora_capital <- "fora_capital"


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar os campos numéricos das zonas de cada grupo; as
# zonas sem trabalhadores têm os campos do bloco vazios e ficam fora da soma
somar_zonas <- function(zonas_agrupadas) {

  # Somando os registros, as estimativas e os pesos
  zonas_agrupadas |>
    summarise(
      n_zonas = n(),
      across(
        .cols = c(
          starts_with("pessoas_"),
          starts_with("trabalho_") & where(is.numeric)
        ),
        .fns  = ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

# Criando uma função para desmembrar uma lista de categorias por zona, ligando
# a zona de trabalho ao distrito
desmembrar_lista <- function(zonas, campo, zona_distrito) {

  # Desmembrando a lista e ligando a zona de trabalho ao distrito
  zonas |>
    select(codigo_zona_od, all_of(campo)) |>
    unnest(all_of(campo)) |>
    select(
      codigo_zona_od,
      codigo_categoria,
      n_registros,
      estimativa
    ) |>
    inner_join(
      zona_distrito |> select(codigo_zona_od, codigo_distrito),
      by = "codigo_zona_od"
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo as zonas da capital
pessoas <- read_parquet(file.path(diretorio_grupo, "pessoas.parquet"))

# Lendo a ligação zona → distrito
zona_distrito <- read_parquet(file.path(diretorio_territorio, "zona_distrito.parquet"))

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
  ~indicador,                   ~grupo,      ~rotulo,                                              ~unidade,                     ~comparacao, ~casas_decimais, ~descricao,
  "trabalhadores",              "volume",    "Trabalhadores com local fixo no distrito",           "trabalhadores (estimativa)", "relativa",  0L,             "Soma de trabalho_pessoas_estimativa_total das zonas do distrito (FE_PESS): residentes na RMSP com primeiro trabalho de local fixo no distrito.",
  "moradores",                  "volume",    "Moradores (estimativa da OD)",                       "moradores",                  "relativa",  0L,             "Soma de pessoas_estimativa_total das zonas do distrito (FE_PESS).",
  "trabalhadores_por_morador",  "volume",    "Trabalhadores por morador",                          "trabalhadores com local fixo para cada morador", "relativa",  2L,             "Trabalhadores com local fixo no distrito sobre os moradores do distrito, ambos da OD.",
  "pct_mulheres",               "perfil",    "Mulheres entre os trabalhadores",                    "% dos trabalhadores",        "pontos",    1L,             "Sexo feminino sobre o peso observado (masculino e feminino; o não respondeu fica fora).",
  "idade_media",                "perfil",    "Idade média dos trabalhadores",                      "anos",                       "relativa",  1L,             "Média da idade em anos completos, ponderada pela estimativa de cada idade.",
  "pct_negra",                  "perfil",    "Pretos e pardos entre os trabalhadores",             "% dos trabalhadores",        "pontos",    1L,             "Cor ou raça preta e parda sobre o peso observado (a sem declaração fica fora).",
  "pct_superior",               "perfil",    "Superior completo entre os trabalhadores",           "% dos trabalhadores",        "pontos",    1L,             "Grau de instrução superior completo sobre o peso observado.",
  "pct_renda_ate_1000",         "perfil",    "Renda familiar per capita até R$ 1.000",             "% dos trabalhadores",        "pontos",    1L,             "Classes de renda familiar per capita até R$ 500 e de R$ 500 a R$ 1.000 sobre o peso observado; renda zero na primeira classe.",
  "pct_mora_distrito",          "residencia", "Trabalhadores que moram no próprio distrito",       "% dos trabalhadores",        "pontos",    1L,             "Estimativa dos trabalhadores com zona de residência no mesmo distrito sobre o peso observado da zona de residência; no município, a soma dos distritos.",
  "pct_mora_fora_capital",      "residencia", "Trabalhadores que moram fora da capital",           "% dos trabalhadores",        "pontos",    1L,             "Estimativa dos trabalhadores com zona de residência em outro município da RMSP sobre o peso observado da zona de residência.",
  "registros_trabalhadores",    "qualidade", "Registros de trabalhadores na OD",                   "registros",                  "relativa",  0L,             "Soma de trabalho_pessoas_n_registros das zonas do distrito; abaixo do mínimo, aviso de amostra menor; abaixo do piso, sem valor.",
  "registros_moradores",        "qualidade", "Registros de moradores na OD",                       "registros",                  "relativa",  0L,             "Soma de pessoas_n_registros das zonas do distrito."
) |>
  mutate(
    ordem = row_number()
  )

# Listando os indicadores calculados, que ficam sem valor abaixo do piso
indicadores_calculados <- c(
  "trabalhadores_por_morador",
  "pct_mulheres",
  "idade_media",
  "pct_negra",
  "pct_superior",
  "pct_renda_ate_1000",
  "pct_mora_distrito",
  "pct_mora_fora_capital"
)


################################################################################
##### VI. Indicadores por distrito e do município
################################################################################

# Ligando as zonas aos distritos pelo distrito oficial da OD
pessoas_distritos <- pessoas |>
  inner_join(
    zona_distrito |> select(codigo_zona_od, codigo_distrito),
    by = "codigo_zona_od"
  )

# Somando as zonas por distrito e para o município
totais_zonas <- bind_rows(
  pessoas_distritos |>
    group_by(codigo_distrito) |>
    somar_zonas(),
  pessoas_distritos |>
    somar_zonas() |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Calculando a idade média por distrito e para o município
idades <- desmembrar_lista(pessoas, "trabalho_idade_distribuicao", zona_distrito) |>
  mutate(idade = as.integer(codigo_categoria))
idade_media <- bind_rows(
  idades |>
    group_by(codigo_distrito) |>
    summarise(
      idade_media = sum(idade * estimativa) / sum(estimativa),
      soma_idade  = sum(estimativa),
      .groups     = "drop"
    ),
  idades |>
    summarise(
      idade_media = sum(idade * estimativa) / sum(estimativa),
      soma_idade  = sum(estimativa)
    ) |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Montando a matriz residência → trabalho por distrito: a zona de residência
# vira distrito na capital e "fora_capital" nos demais municípios da RMSP
matriz <- desmembrar_lista(pessoas, "trabalho_zona_residencia_distribuicao", zona_distrito) |>
  rename(
    codigo_distrito_trabalho = codigo_distrito,
    codigo_zona_residencia   = codigo_categoria
  ) |>
  left_join(
    zona_distrito |>
      select(codigo_zona_residencia = codigo_zona_od, codigo_distrito_residencia = codigo_distrito),
    by = "codigo_zona_residencia"
  ) |>
  mutate(
    codigo_distrito_residencia = replace_na(codigo_distrito_residencia, codigo_fora_capital)
  ) |>
  group_by(codigo_distrito_trabalho, codigo_distrito_residencia) |>
  summarise(
    n_registros = sum(n_registros),
    estimativa  = sum(estimativa),
    .groups     = "drop"
  )

# Calculando quem mora no próprio distrito e fora da capital
residencia <- matriz |>
  group_by(codigo_distrito = codigo_distrito_trabalho) |>
  summarise(
    soma_matriz         = sum(estimativa),
    mora_distrito       = sum(estimativa[codigo_distrito_residencia == codigo_distrito_trabalho]),
    mora_fora_capital   = sum(estimativa[codigo_distrito_residencia == codigo_fora_capital]),
    .groups             = "drop"
  )
residencia <- bind_rows(
  residencia,
  residencia |>
    summarise(across(where(is.numeric), sum)) |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Calculando os indicadores em formato largo
trabalhadores_largo <- totais_zonas |>
  left_join(idade_media, by = "codigo_distrito") |>
  left_join(residencia, by = "codigo_distrito") |>
  mutate(
    trabalhadores             = trabalho_pessoas_estimativa_total,
    moradores                 = pessoas_estimativa_total,
    trabalhadores_por_morador = trabalhadores / moradores,
    pct_mulheres              = 100 * trabalho_sexo_cat_codigo_2_estimativa / trabalho_sexo_peso_observado,
    pct_negra                 = 100 * (trabalho_raca_cat_codigo_2_estimativa + trabalho_raca_cat_codigo_4_estimativa) /
      trabalho_raca_peso_observado,
    pct_superior              = 100 * trabalho_grau_ins_cat_codigo_5_estimativa / trabalho_grau_ins_peso_observado,
    pct_renda_ate_1000        = 100 * (trabalho_renda_familiar_per_capita_classe_cat_codigo_01_estimativa +
                                         trabalho_renda_familiar_per_capita_classe_cat_codigo_02_estimativa) /
      trabalho_renda_familiar_per_capita_classe_peso_observado,
    pct_mora_distrito         = 100 * mora_distrito / trabalho_zona_residencia_peso_observado,
    pct_mora_fora_capital     = 100 * mora_fora_capital / trabalho_zona_residencia_peso_observado,
    registros_trabalhadores   = trabalho_pessoas_n_registros,
    registros_moradores       = pessoas_n_registros,
    # Aviso de amostra menor e piso sem valor (o município não recebe nenhum)
    nivel                     = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor             = nivel == "distrito" & registros_trabalhadores < minimo_registros_od,
    sem_valor                 = nivel == "distrito" & registros_trabalhadores < piso_registros_od
  ) |>
  # Deixando sem valor os indicadores calculados dos distritos abaixo do piso
  mutate(
    across(
      .cols = all_of(indicadores_calculados),
      .fns  = ~ if_else(sem_valor, NA_real_, .x)
    )
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
trabalhadores <- trabalhadores_largo |>
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

# Juntando os nomes à matriz residência → trabalho
matriz <- matriz |>
  left_join(
    distritos |> select(codigo_distrito_trabalho = codigo_distrito, nome_distrito_trabalho = nome_distrito),
    by = "codigo_distrito_trabalho"
  ) |>
  left_join(
    distritos |> select(codigo_distrito_residencia = codigo_distrito, nome_distrito_residencia = nome_distrito),
    by = "codigo_distrito_residencia"
  ) |>
  mutate(
    nome_distrito_residencia = replace_na(nome_distrito_residencia, "Outros municípios da RMSP")
  ) |>
  select(
    codigo_distrito_trabalho,
    nome_distrito_trabalho,
    codigo_distrito_residencia,
    nome_distrito_residencia,
    n_registros,
    estimativa
  ) |>
  arrange(codigo_distrito_trabalho, desc(estimativa))


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(trabalhadores_largo, nivel == "distrito")
largo_municipio <- filter(trabalhadores_largo, nivel == "municipio")

# Somando as categorias de cada bloco por zona (categoria vazia conta zero)
soma_zona <- pessoas |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |>
  mutate(
    sexo  = trabalho_sexo_cat_codigo_1_estimativa + trabalho_sexo_cat_codigo_2_estimativa,
    raca  = rowSums(pick(sprintf("trabalho_raca_cat_codigo_%d_estimativa", 1:5))),
    grau  = rowSums(pick(sprintf("trabalho_grau_ins_cat_codigo_%d_estimativa", 1:5))),
    renda = rowSums(pick(sprintf("trabalho_renda_familiar_per_capita_classe_cat_codigo_%02d_estimativa", 1:21)))
  ) |>
  filter(trabalho_pessoas_n_registros > 0)

# Somando os registros da matriz por zona de trabalho
registros_matriz <- desmembrar_lista(pessoas, "trabalho_zona_residencia_distribuicao", zona_distrito) |>
  group_by(codigo_zona_od) |>
  summarise(registros = sum(n_registros), .groups = "drop") |>
  left_join(pessoas |> select(codigo_zona_od, trabalho_pessoas_n_registros), by = "codigo_zona_od")

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "343 zonas da OD na capital",
    "Todas as zonas ligadas a um distrito",
    "Distritos com indicadores",
    "Total de trabalhadores igual nas idades, na matriz, na instrução e na renda",
    "Sexo e cor ou raça: peso observado mais ignorado igual ao total de trabalhadores",
    "Categorias somam o peso observado em cada zona (sexo, cor ou raça, instrução, renda)",
    "Registros da matriz iguais aos registros de trabalhadores em cada zona",
    "Zonas de residência da matriz entre 1 e 527",
    "Proporções entre 0 e 100",
    paste0("Distritos com menos de ", minimo_registros_od, " registros de trabalhadores: 21"),
    paste0("Distritos sem valor (menos de ", piso_registros_od, " registros): só Marsilac")
  ),
  ok          = c(
    nrow(pessoas) == 343,
    nrow(pessoas_distritos) == nrow(pessoas),
    nrow(largo_distritos) == 96,
    all(abs(c(largo_municipio$soma_idade, largo_municipio$soma_matriz,
              largo_municipio$trabalho_grau_ins_peso_observado,
              largo_municipio$trabalho_renda_familiar_per_capita_classe_peso_observado) -
              largo_municipio$trabalhadores) < 0.5),
    abs(largo_municipio$trabalho_sexo_peso_observado + largo_municipio$trabalho_sexo_peso_ignorado - largo_municipio$trabalhadores) < 0.5 &&
      abs(largo_municipio$trabalho_raca_peso_observado + largo_municipio$trabalho_raca_peso_ignorado - largo_municipio$trabalhadores) < 0.5,
    all(abs(soma_zona$sexo - soma_zona$trabalho_sexo_peso_observado) < 1e-6) &&
      all(abs(soma_zona$raca - soma_zona$trabalho_raca_peso_observado) < 1e-6) &&
      all(abs(soma_zona$grau - soma_zona$trabalho_grau_ins_peso_observado) < 1e-6) &&
      all(abs(soma_zona$renda - soma_zona$trabalho_renda_familiar_per_capita_classe_peso_observado) < 1e-6),
    all(registros_matriz$registros == registros_matriz$trabalho_pessoas_n_registros),
    all(as.integer(unnest(pessoas, trabalho_zona_residencia_distribuicao)$codigo_categoria) %in% 1:527),
    trabalhadores |>
      filter(str_starts(indicador, "pct_"), !is.na(valor)) |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 21,
    identical(largo_distritos$nome_rotulo[largo_distritos$sem_valor], "Marsilac")
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
         largo_distritos$registros_trabalhadores[largo_distritos$amostra_menor], ")", collapse = ", ")
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
  x    = trabalhadores,
  sink = file.path(diretorio_grupo, "trabalhadores.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a matriz residência → trabalho por distrito
write_parquet(
  x    = matriz,
  sink = file.path(diretorio_grupo, "matriz_residencia_trabalho.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = trabalhadores_largo |>
      select(
        codigo_distrito,
        nome_distrito,
        n_zonas,
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
        `Zonas OD no distrito`  = n_zonas,
        `Amostra menor (aviso)` = amostra_menor,
        `Sem valor (piso)`      = sem_valor
      ),
    matriz      = matriz,
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: Metrô, Pesquisa Origem e Destino 2023, tabela de pessoas por zona, bloco por zona do primeiro trabalho (trabalho_*).",
        "Trabalhadores residentes na RMSP com primeiro trabalho de local fixo (ou em casa) na zona; cada pessoa uma vez; fator FE_PESS.",
        "Zona → distrito pelo distrito oficial da OD, traduzido para o código do IBGE por sobreposição de área.",
        "Recortes por local de trabalho não são estratos do desenho amostral da OD; estimativas sem margem de erro calculada.",
        paste0("Distritos com menos de ", minimo_registros_od, " registros de trabalhadores recebem aviso de amostra menor; com menos de ",
               piso_registros_od, ", ficam sem valor nos indicadores calculados."),
        "No município, a proporção que mora no próprio distrito soma os trabalhadores que moram no distrito em que trabalham."
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_trabalhadores.xlsx")
)

# Informando o fim do script
message("Indicadores dos trabalhadores gravados em ", diretorio_grupo)
