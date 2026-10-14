################################################################################
##################################            ##################################
############## 14.09) PREPARAR OS INDICADORES DA MIGRAÇÃO RECENTE ##############
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
codigos_amostra <- c("P0180", "P0510", "P0500", "P0600", "P0610")

# Definindo quantos países entram nas figuras (os mais frequentes da capital) e
# quantos municípios de nascimento
n_paises     <- 10
n_municipios <- 12

# Definindo os códigos de país e de município que não são destinos: ignorado e
# não sabe, além da ausência (quem não se aplica)
codigos_sem_destino <- c("ausente", "9999999999", "8000999000", "9999999", "8888888")

# Descrevendo as origens de quem morava em outro município ou país há 5 anos:
# a UF de moradia há 5 anos (P0610), agrupada pelo primeiro dígito do código de
# UF do IBGE, e o exterior (P0600 código 3)
origens <- tibble(
  origem_codigo = 1:7,
  indicador     = c(
    "pct_recente_sp",
    "pct_recente_nordeste",
    "pct_recente_demais_sudeste",
    "pct_recente_sul",
    "pct_recente_norte_centro_oeste",
    "pct_recente_uf_nao_especificada",
    "pct_recente_exterior"
  ),
  rotulo_origem = c(
    "Outro município de SP",
    "Nordeste",
    "Demais Sudeste",
    "Sul",
    "Norte e Centro-Oeste",
    "UF não especificada",
    "Exterior"
  ),
  campos        = list(
    "p0610_cat_codigo_35_estimativa",
    sprintf("p0610_cat_codigo_%d_estimativa", 21:29),
    sprintf("p0610_cat_codigo_%d_estimativa", 31:33),
    sprintf("p0610_cat_codigo_%d_estimativa", 41:43),
    sprintf("p0610_cat_codigo_%d_estimativa", c(11:17, 50:53)),
    sprintf("p0610_cat_codigo_%d_estimativa", c(88, 99)),
    "p0600_cat_codigo_3_estimativa"
  )
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar as estimativas das APs de cada grupo. As
# estimativas já vêm ponderadas pelo IBGE; somam-se as estimativas e os pesos, e
# os indicadores são recalculados a partir das somas.
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

# Criando uma função para escrever em caixa de título o nome de um descritor do
# IBGE, que vem em caixa alta e sem acento (as preposições ficam em minúsculas)
formatar_nome <- function(x) {
  x |>
    str_to_title() |>
    str_replace_all("\\b(De|Do|Da|Dos|Das|E)\\b", str_to_lower)
}

# Criando uma função para somar uma variável em lista (por AP) por distrito e
# para o município, devolvendo a estimativa e os registros de cada categoria
somar_lista <- function(amostra_distritos, campo) {

  # Abrindo a lista de categorias de cada AP
  categorias <- amostra_distritos |>
    select(codigo_distrito, lista = all_of(campo)) |>
    unnest(lista) |>
    filter(!codigo_categoria %in% codigos_sem_destino)

  # Somando por distrito e para o município
  bind_rows(
    categorias |>
      group_by(codigo_distrito, codigo_categoria),
    categorias |>
      mutate(codigo_distrito = codigo_municipio_sp) |>
      group_by(codigo_distrito, codigo_categoria)
  ) |>
    summarise(
      estimativa  = sum(estimativa),
      n_registros = sum(n_registros),
      .groups     = "drop"
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo as APs da capital
amostra <- read_parquet(file.path(diretorio_grupo, "recente_amostra.parquet"))

# Lendo os rótulos dos países e dos municípios
rotulos <- read_parquet(file.path(diretorio_grupo, "rotulos.parquet"))

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
  ~indicador,                       ~grupo,    ~rotulo,                                         ~unidade,                                      ~comparacao, ~casas_decimais, ~descricao,
  "pct_migrou_5anos",               "recente", "Moravam em outro município ou país há 5 anos",  "% das pessoas de 5 anos ou mais",             "pontos",    1L,              "P0600 códigos 2 e 3 (outro município do Brasil ou outro país) sobre as pessoas de 5 anos ou mais (total de pessoas menos as de 0 a 4 anos, P0180 código 01).",
  "pct_exterior_5anos",             "recente", "Moravam em outro país há 5 anos",               "% das pessoas de 5 anos ou mais",             "pontos",    2L,              "P0600 código 3 (outro país) sobre as pessoas de 5 anos ou mais.",
  "pct_recente_sp",                 "origem",  "Vindos de outro município de SP",               "% de quem morava fora há 5 anos",             "pontos",    1L,              "P0610 código 35 (São Paulo) sobre P0600 códigos 2 e 3.",
  "pct_recente_nordeste",           "origem",  "Vindos do Nordeste",                            "% de quem morava fora há 5 anos",             "pontos",    1L,              "P0610 códigos 21 a 29 sobre P0600 códigos 2 e 3.",
  "pct_recente_demais_sudeste",     "origem",  "Vindos do Sudeste fora de SP",                  "% de quem morava fora há 5 anos",             "pontos",    1L,              "P0610 códigos 31 a 33 sobre P0600 códigos 2 e 3.",
  "pct_recente_sul",                "origem",  "Vindos do Sul",                                 "% de quem morava fora há 5 anos",             "pontos",    1L,              "P0610 códigos 41 a 43 sobre P0600 códigos 2 e 3.",
  "pct_recente_norte_centro_oeste", "origem",  "Vindos do Norte ou do Centro-Oeste",            "% de quem morava fora há 5 anos",             "pontos",    1L,              "P0610 códigos 11 a 17 e 50 a 53 sobre P0600 códigos 2 e 3.",
  "pct_recente_uf_nao_especificada", "origem", "Vindos do Brasil com UF não especificada",      "% de quem morava fora há 5 anos",             "pontos",    1L,              "P0610 códigos 88 e 99 sobre P0600 códigos 2 e 3.",
  "pct_recente_exterior",           "origem",  "Vindos do exterior",                            "% de quem morava fora há 5 anos",             "pontos",    1L,              "P0600 código 3 sobre P0600 códigos 2 e 3.",
  "registros_amostra",              "qualidade", "Registros de pessoas na amostra",             "registros",                                   "relativa",  0L,              "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor."
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

# Somando as APs por distrito e para o município (só os campos de contagem, sem
# as listas)
totais_amostra <- bind_rows(
  amostra_distritos |>
    select(-ends_with("_distribuicao")) |>
    group_by(codigo_distrito) |>
    somar_aps(),
  amostra_distritos |>
    select(-ends_with("_distribuicao")) |>
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
        pessoas         = rowSums(as.matrix(totais_amostra[campos_origem]))
      )
    )
  ) |>
  select(-campos) |>
  unnest(valores)

# Calculando os indicadores em formato largo
recente_largo <- totais_amostra |>
  left_join(
    estimativas_origens |>
      select(codigo_distrito, indicador, pessoas) |>
      pivot_wider(names_from = indicador, values_from = pessoas, names_prefix = "n_"),
    by = "codigo_distrito"
  ) |>
  mutate(
    # População de 5 anos ou mais e quem veio de fora há 5 anos
    pessoas_5_mais                  = pessoas_estimativa_total - p0180_cat_codigo_01_estimativa,
    migrantes_5anos                 = p0600_cat_codigo_2_estimativa + p0600_cat_codigo_3_estimativa,
    pct_migrou_5anos                = 100 * migrantes_5anos / pessoas_5_mais,
    pct_exterior_5anos              = 100 * p0600_cat_codigo_3_estimativa / pessoas_5_mais,
    # Origem de quem morava em outro município ou país há 5 anos
    across(
      .cols  = starts_with("n_pct_recente_"),
      .fns   = ~ 100 * .x / migrantes_5anos,
      .names = "{str_remove(.col, '^n_')}"
    ),
    # Registros e aviso de amostra menor (o município não recebe aviso)
    registros_amostra               = pessoas_n_registros,
    nivel                           = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor                   = nivel == "distrito" & registros_amostra < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
recente <- recente_largo |>
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

# Criando a tabela da composição por origem de quem veio de fora
origem_recente <- estimativas_origens |>
  inner_join(
    recente_largo |>
      select(
        nivel_territorial = nivel,
        codigo_distrito,
        nome_distrito,
        nome_rotulo,
        amostra_menor,
        migrantes_5anos
      ),
    by = "codigo_distrito"
  ) |>
  mutate(
    pct = 100 * pessoas / migrantes_5anos
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    origem_codigo,
    rotulo_origem,
    pessoas,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, origem_codigo)

# Obtendo as informações de cada distrito e do município
territorio <- recente_largo |>
  select(
    nivel_territorial = nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor
  )

# Somando os países de nascimento por distrito e para o município, com o rótulo
paises_todos <- somar_lista(amostra_distritos, "p0510_distribuicao") |>
  left_join(
    rotulos |>
      filter(codigo_original == "P0510") |>
      select(codigo_categoria, rotulo_pais = descricao_categoria),
    by = "codigo_categoria"
  ) |>
  group_by(codigo_distrito) |>
  mutate(
    pct = 100 * estimativa / sum(estimativa)
  ) |>
  ungroup()

# Escolhendo os países mais frequentes da capital, pela estimativa
paises_principais <- paises_todos |>
  filter(codigo_distrito == codigo_municipio_sp) |>
  arrange(desc(estimativa)) |>
  slice_head(n = n_paises) |>
  mutate(
    posicao = row_number()
  ) |>
  select(codigo_categoria, posicao)

# Montando a tabela dos países: os principais da capital em cada distrito, mais
# "Outros países" (o resto), em % dos nascidos no exterior
paises <- paises_todos |>
  left_join(paises_principais, by = "codigo_categoria") |>
  mutate(
    codigo_categoria = if_else(is.na(posicao), "outros", codigo_categoria),
    rotulo_pais      = if_else(is.na(posicao), "Outros países", formatar_nome(rotulo_pais)),
    posicao          = coalesce(posicao, n_paises + 1L)
  ) |>
  group_by(codigo_distrito, codigo_categoria, rotulo_pais, posicao) |>
  summarise(
    estimativa  = sum(estimativa),
    n_registros = sum(n_registros),
    pct         = sum(pct),
    .groups     = "drop"
  ) |>
  inner_join(territorio, by = "codigo_distrito") |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    posicao,
    codigo_pais = codigo_categoria,
    rotulo_pais,
    estimativa,
    n_registros,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, posicao)

# Montando a tabela dos municípios de nascimento mais frequentes da capital (só
# o município), em % dos nascidos em outro município do Brasil
municipios_nascimento <- somar_lista(amostra_distritos, "p0500_distribuicao") |>
  filter(codigo_distrito == codigo_municipio_sp) |>
  left_join(
    rotulos |>
      filter(codigo_original == "P0500") |>
      select(codigo_categoria, rotulo_municipio = descricao_categoria),
    by = "codigo_categoria"
  ) |>
  mutate(
    pct = 100 * estimativa / sum(estimativa)
  ) |>
  arrange(desc(estimativa)) |>
  slice_head(n = n_municipios) |>
  mutate(
    posicao = row_number()
  ) |>
  select(
    posicao,
    codigo_municipio = codigo_categoria,
    rotulo_municipio,
    estimativa,
    n_registros,
    pct
  )


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(recente_largo, nivel == "distrito")
largo_municipio <- filter(recente_largo, nivel == "municipio")

# Calculando, por AP, as UFs de origem e o total de quem veio de outro município
ufs_origem <- rowSums(as.matrix(amostra[grep("^p0610_cat_codigo_", names(amostra), value = TRUE)]))

# Calculando, por distrito, a soma de países e de municípios com destino
soma_paises <- paises |>
  group_by(codigo_distrito) |>
  summarise(s = sum(estimativa), .groups = "drop")
soma_municipios_capital <- sum(unlist(map(amostra_distritos$p0500_distribuicao, \(x) sum(x$estimativa[!x$codigo_categoria %in% codigos_sem_destino]))))

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Universos de aplicação das cinco variáveis conferidos (P0560 a P0630 incluídos)",
    "Locais de moradia há 5 anos (códigos 1 a 3) somam o peso observado de P0600 em cada AP",
    "UFs de origem (P0610) somam os que vieram de outro município do Brasil (P0600 código 2) em cada AP",
    "As sete origens somam 100% em cada distrito",
    "Nenhuma proporção fora de 0 a 100",
    "Países: a soma por distrito é o peso observado de P0510 (sem ignorado nem não sabe)",
    "Municípios de nascimento: soma da capital igual ao peso observado de P0500",
    "Os países e municípios principais têm rótulo",
    "13 distritos com amostra menor (decisão de 2026-10-10)",
    "Município: 369.797 pessoas morando fora há 5 anos (3,4% das de 5 anos ou mais), medido na etapa 17c"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    all(abs(
      amostra$p0600_cat_codigo_1_estimativa + amostra$p0600_cat_codigo_2_estimativa + amostra$p0600_cat_codigo_3_estimativa -
        amostra$p0600_peso_observado
    ) < 1e-6),
    all(abs(ufs_origem - amostra$p0600_cat_codigo_2_estimativa) < 1e-6),
    origem_recente |>
      group_by(codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    recente |>
      filter(str_starts(indicador, "pct_")) |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    all(abs(
      soma_paises$s - c(
        totais_amostra$p0510_peso_observado[match(soma_paises$codigo_distrito, totais_amostra$codigo_distrito)]
      )
    ) < 1e-6),
    abs(soma_municipios_capital - sum(totais_amostra$p0500_peso_observado[totais_amostra$codigo_distrito != codigo_municipio_sp])) < 1e-6,
    !anyNA(paises_principais$codigo_categoria) &&
      !anyNA(filter(paises, nivel_territorial == "municipio", codigo_pais != "outros")$rotulo_pais) &&
      !anyNA(municipios_nascimento$rotulo_municipio),
    sum(largo_distritos$amostra_menor) == 13,
    abs(largo_municipio$migrantes_5anos - 369797.5) < 1 && abs(largo_municipio$pct_migrou_5anos - 3.41) < 0.005
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

# Informando os valores do município
message(
  "Município: moravam fora há 5 anos ", round(largo_municipio$pct_migrou_5anos, 2),
  "%; no exterior ", round(largo_municipio$pct_exterior_5anos, 2),
  "%; vindos do Nordeste ", round(largo_municipio$pct_recente_nordeste, 1),
  "% e do exterior ", round(largo_municipio$pct_recente_exterior, 1), "% de quem veio de fora"
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
  x    = recente,
  sink = file.path(diretorio_grupo, "recente.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores_recente.parquet")
)

# Gravando a origem de quem veio de fora
write_parquet(
  x    = origem_recente,
  sink = file.path(diretorio_grupo, "origem_recente.parquet")
)

# Gravando os países de nascimento
write_parquet(
  x    = paises,
  sink = file.path(diretorio_grupo, "paises.parquet")
)

# Gravando os municípios de nascimento
write_parquet(
  x    = municipios_nascimento,
  sink = file.path(diretorio_grupo, "municipios_nascimento.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = recente_largo |>
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
    origens     = origem_recente |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, origem_codigo, rotulo_origem, pessoas, pct),
    paises      = paises |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, posicao, rotulo_pais, estimativa, n_registros, pct),
    municipios  = municipios_nascimento |>
      mutate(pct = round(pct, 3)),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), P0180, P0500, P0510, P0600 e P0610; rótulos dos países e municípios dos descritores do IBGE, pela tabela de rótulos do Dados.",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        "Moravam fora há 5 anos: P0600 códigos 2 (outro município do Brasil) e 3 (outro país), entre as pessoas de 5 anos ou mais. A pergunta se aplica a quem mora há menos de 6 anos no município (P0550); o código 1 (neste município) inclui quem chegou há 5 a 6 anos e já morava aqui há 5 anos, e não conta como migrante recente.",
        "A região de origem agrupa P0610 pelo primeiro dígito do código de UF do IBGE (1 Norte, 2 Nordeste, 3 Sudeste, 4 Sul, 5 Centro-Oeste; 35 é São Paulo); os códigos 88 e 99 ficam como UF não especificada.",
        "Países e municípios de nascimento: só os mais frequentes da capital aparecem; o ignorado e o não sabe ficam fora do cálculo. Categorias raras vêm de poucos registros por distrito.",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_migracao_recente.xlsx")
)

# Informando o fim do script
message("Indicadores da migração recente gravados em ", diretorio_grupo)
