################################################################################
##################################            ##################################
################## 14.02) PREPARAR OS INDICADORES DA MIGRAÇÃO ##################
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
diretorio_grupo <- caminho_data("14_migracao")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("14_migracao")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c("P0480", "P0490", "P0520", "P0550")

# Definindo os valores de referência do município medidos em 2026-10-12
# (etapa 10 do plano), em %
referencia_outro_municipio <- 31.3
referencia_exterior        <- 1.8

# Descrevendo as origens dos moradores: o local de nascimento (P0480) e, para
# os nascidos em outro município do Brasil, a UF de nascimento (P0490)
# agrupada pelo primeiro dígito do código de UF do IBGE (grande região)
origens <- tibble(
  origem_codigo = 1:8,
  indicador     = c(
    "pct_nascidos_municipio",
    "pct_outro_municipio_sp",
    "pct_nordeste",
    "pct_demais_sudeste",
    "pct_sul",
    "pct_norte_centro_oeste",
    "pct_uf_nao_especificada",
    "pct_exterior"
  ),
  rotulo_origem = c(
    "Município de São Paulo",
    "Outro município de SP",
    "Nordeste",
    "Demais Sudeste",
    "Sul",
    "Norte e Centro-Oeste",
    "UF não especificada",
    "Exterior"
  ),
  campos        = list(
    "p0480_cat_codigo_1_estimativa",
    "p0490_cat_codigo_35_estimativa",
    sprintf("p0490_cat_codigo_%d_estimativa", 21:29),
    sprintf("p0490_cat_codigo_%d_estimativa", 31:33),
    sprintf("p0490_cat_codigo_%d_estimativa", 41:43),
    sprintf("p0490_cat_codigo_%d_estimativa", c(11:17, 50:53)),
    sprintf("p0490_cat_codigo_%d_estimativa", c(88, 99)),
    "p0480_cat_codigo_3_estimativa"
  )
)


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
        .cols = c(where(is.numeric), -p0550_media),
        .fns  = sum
      ),
      .groups = "drop"
    )
}


################################################################################
##### IV. Dados
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
##### V. Descrição dos indicadores
################################################################################

# Descrevendo os indicadores do grupo, na ordem de apresentação
indicadores <- tribble(
  ~indicador,                ~grupo,          ~rotulo,                                          ~unidade,                                 ~comparacao, ~casas_decimais, ~descricao,
  "pct_nascidos_municipio",  "origem",        "Nascidos no município",                          "% dos moradores",                        "pontos",    1L,              "P0480 código 1 sobre o peso observado de P0480 (todas as pessoas).",
  "pct_outro_municipio_sp",  "origem",        "Nascidos em outro município de SP",              "% dos moradores",                        "pontos",    1L,              "P0490 código 35 (São Paulo) sobre o peso observado de P0480; P0490 vale para os nascidos em outro município do Brasil.",
  "pct_nordeste",            "origem",        "Nascidos no Nordeste",                           "% dos moradores",                        "pontos",    1L,              "P0490 códigos 21 a 29 sobre o peso observado de P0480.",
  "pct_demais_sudeste",      "origem",        "Nascidos no Sudeste fora de SP",                 "% dos moradores",                        "pontos",    1L,              "P0490 códigos 31 a 33 (Minas Gerais, Espírito Santo, Rio de Janeiro) sobre o peso observado de P0480.",
  "pct_sul",                 "origem",        "Nascidos no Sul",                                "% dos moradores",                        "pontos",    1L,              "P0490 códigos 41 a 43 sobre o peso observado de P0480.",
  "pct_norte_centro_oeste",  "origem",        "Nascidos no Norte ou no Centro-Oeste",           "% dos moradores",                        "pontos",    1L,              "P0490 códigos 11 a 17 e 50 a 53 sobre o peso observado de P0480.",
  "pct_uf_nao_especificada", "origem",        "Nascidos no Brasil com UF não especificada",     "% dos moradores",                        "pontos",    2L,              "P0490 códigos 88 e 99 sobre o peso observado de P0480.",
  "pct_exterior",            "origem",        "Nascidos no exterior",                           "% dos moradores",                        "pontos",    1L,              "P0480 código 3 sobre o peso observado de P0480.",
  "pct_outro_municipio",     "origem",        "Nascidos fora do município",                     "% dos moradores",                        "pontos",    1L,              "P0480 códigos 2 e 3 sobre o peso observado de P0480.",
  "pct_estrangeiros",        "nacionalidade", "Estrangeiros",                                   "% dos moradores",                        "pontos",    1L,              "P0520 código 3 sobre o peso observado de P0520.",
  "pct_naturalizados",       "nacionalidade", "Naturalizados brasileiros",                      "% dos moradores",                        "pontos",    2L,              "P0520 código 2 sobre o peso observado de P0520.",
  "tempo_moradia",           "tempo",         "Tempo médio de moradia no município",            "anos, entre quem nasceu ou já morou fora", "relativa",  1L,              "Média de P0550 ponderada por pessoa (soma ponderada sobre o peso observado); só para quem nasceu fora do município ou já morou fora dele.",
  "registros_amostra",       "qualidade",     "Registros de pessoas na amostra",                "registros",                              "relativa",  0L,              "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor."
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

# Somando as estimativas de cada origem, em cada distrito e no município
estimativas_origens <- origens |>
  mutate(
    valores = map(
      campos,
      \(campos_origem) tibble(
        codigo_distrito = totais_amostra$codigo_distrito,
        moradores       = rowSums(as.matrix(totais_amostra[campos_origem]))
      )
    )
  ) |>
  select(-campos) |>
  unnest(valores)

# Calculando os indicadores em formato largo
migracao_largo <- totais_amostra |>
  left_join(
    estimativas_origens |>
      select(codigo_distrito, indicador, moradores) |>
      pivot_wider(names_from = indicador, values_from = moradores, names_prefix = "n_"),
    by = "codigo_distrito"
  ) |>
  mutate(
    # Proporções das origens sobre o peso observado de P0480 (todas as pessoas)
    across(
      .cols  = starts_with("n_pct_"),
      .fns   = ~ 100 * .x / p0480_peso_observado,
      .names = "{str_remove(.col, '^n_')}"
    ),
    pct_outro_municipio = 100 * (p0480_cat_codigo_2_estimativa + p0480_cat_codigo_3_estimativa) / p0480_peso_observado,
    # Nacionalidade sobre o peso observado de P0520
    pct_estrangeiros    = 100 * p0520_cat_codigo_3_estimativa / p0520_peso_observado,
    pct_naturalizados   = 100 * p0520_cat_codigo_2_estimativa / p0520_peso_observado,
    # Média ponderada por pessoa: soma ponderada sobre o peso observado
    tempo_moradia       = p0550_soma / p0550_peso_observado,
    # Registros e aviso de amostra menor (o município não recebe aviso)
    registros_amostra   = pessoas_n_registros,
    nivel               = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor       = nivel == "distrito" & registros_amostra < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
migracao <- migracao_largo |>
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

# Criando a tabela da composição por origem
composicao <- estimativas_origens |>
  inner_join(
    migracao_largo |>
      select(
        nivel_territorial = nivel,
        codigo_distrito,
        nome_distrito,
        nome_rotulo,
        amostra_menor,
        p0480_peso_observado
      ),
    by = "codigo_distrito"
  ) |>
  mutate(
    pct = 100 * moradores / p0480_peso_observado
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    origem_codigo,
    rotulo_origem,
    moradores,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, origem_codigo)


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(migracao_largo, nivel == "distrito")
largo_municipio <- filter(migracao_largo, nivel == "municipio")

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Universos de aplicação das quatro variáveis conferidos",
    "Sem peso ignorado em P0480 na capital",
    "UFs de nascimento somam os nascidos em outro município do Brasil em cada AP",
    "Média publicada da AP igual a soma sobre peso observado (P0550)",
    "As oito origens somam 100% em cada distrito",
    "Nenhuma proporção fora de 0 a 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)",
    "Município igual aos valores de referência da etapa 10 (fora do município e exterior)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    sum(amostra$p0480_peso_ignorado) == 0,
    all(abs(
      rowSums(as.matrix(amostra[grep("^p0490_cat_codigo_", names(amostra), value = TRUE)])) -
        amostra$p0480_cat_codigo_2_estimativa
    ) < 1e-6),
    all(abs(amostra$p0550_soma / amostra$p0550_peso_observado - amostra$p0550_media) < 1e-6),
    composicao |>
      group_by(codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    migracao |>
      filter(str_starts(indicador, "pct_")) |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 13,
    abs(largo_municipio$pct_outro_municipio - referencia_outro_municipio) < 0.05 &&
      abs(largo_municipio$pct_exterior - referencia_exterior) < 0.05
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
  x    = migracao,
  sink = file.path(diretorio_grupo, "migracao.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a composição por origem
write_parquet(
  x    = composicao,
  sink = file.path(diretorio_grupo, "composicao.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = migracao_largo |>
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
    origens     = composicao |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, origem_codigo, rotulo_origem, moradores, pct),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), P0480, P0490, P0520 e P0550.",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        "A região de nascimento agrupa P0490 pelo primeiro dígito do código de UF do IBGE (1 Norte, 2 Nordeste, 3 Sudeste, 4 Sul, 5 Centro-Oeste; 35 é São Paulo); os códigos 88 e 99 ficam como UF não especificada.",
        "Ficam de fora a migração recente (residência há 5 anos, P0600, com universo não identificado) e os países de nascimento (P0510, sem rótulo no dicionário do Dados).",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_migracao.xlsx")
)

# Informando o fim do script
message("Indicadores da migração gravados em ", diretorio_grupo)
