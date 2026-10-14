################################################################################
##################################            ##################################
############ 15.10) PREPARAR OS INDICADORES DE ATIVIDADE E OCUPAÇÃO ############
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
diretorio_grupo <- caminho_data("15_trabalho_moradores")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("15_trabalho_moradores")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c("P0970", "P0980", "P1140")

# Definindo quantas ocupações e quantos municípios de trabalho entram nas
# figuras (os mais frequentes da capital)
n_ocupacoes <- 10
n_destinos  <- 10

# Definindo o código de ocupação sem rótulo no descritor do IBGE (4,5% dos
# ocupados na capital); a equivalência com "mal definidas" não é certificada
codigo_ocupacao_sem_rotulo <- "0000"

# Definindo os códigos que não são destino de trabalho: ignorado e não sabe
codigos_sem_destino <- c("9999999", "8888888")

# Agrupando as seções da atividade (CNAE-Domiciliar, letras A a V do IBGE) em
# sete grupos, na ordem de apresentação
grupos_setor <- tribble(
  ~grupo_setor, ~rotulo_setor,                                   ~secoes,
  1L,           "Comércio",                                      "G",
  2L,           "Informação, finanças e serviços profissionais", c("J", "K", "L", "M", "N"),
  3L,           "Indústria e construção",                        c("B", "C", "D", "E", "F"),
  4L,           "Transporte, alojamento e alimentação",          c("H", "I"),
  5L,           "Educação, saúde e administração pública",       c("O", "P", "Q"),
  6L,           "Serviços domésticos",                           "T",
  7L,           "Outros e mal definidas",                        c("A", "R", "S", "U", "V")
) |>
  unnest(secoes) |>
  rename(secao = secoes)


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

# Criando uma função para somar uma variável em lista (por AP) por distrito e
# para o município, devolvendo a estimativa e os registros de cada categoria
# (a ausência, que é quem não se aplica, e os códigos de excluir ficam fora)
somar_lista <- function(amostra_distritos, campo, excluir = character()) {

  # Abrindo a lista de categorias de cada AP
  categorias <- amostra_distritos |>
    select(codigo_distrito, lista = all_of(campo)) |>
    unnest(lista) |>
    filter(!codigo_categoria %in% c("ausente", excluir))

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
amostra <- read_parquet(file.path(diretorio_grupo, "atividade_amostra.parquet"))

# Lendo os rótulos das ocupações, das atividades e dos municípios
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
  ~indicador,                ~grupo,      ~rotulo,                                      ~unidade,                    ~comparacao, ~casas_decimais, ~descricao,
  "pct_comercio",            "atividade", "Ocupados no comércio",                       "% dos ocupados",            "pontos",    1L,              "Atividade do trabalho principal na seção G da CNAE-Domiciliar (comércio e reparação de veículos), sobre os ocupados de 10 anos ou mais com atividade (peso observado de P0980).",
  "pct_servicos_domesticos", "atividade", "Ocupados em serviços domésticos",            "% dos ocupados",            "pontos",    1L,              "Atividade na seção T (serviços domésticos), sobre os ocupados de 10 anos ou mais com atividade.",
  "pct_mal_definidas",       "atividade", "Atividade mal definida ou não especificada", "% dos ocupados",            "pontos",    1L,              "Atividade na seção V (mal definidas ou não especificadas, biscate), sobre os ocupados de 10 anos ou mais com atividade.",
  "pct_ocupacoes_principais", "ocupacao", "Nas dez ocupações mais frequentes",          "% dos ocupados",            "pontos",    1L,              "Ocupados nas dez ocupações (famílias, P0970) mais frequentes da capital, sobre os ocupados de 10 anos ou mais com ocupação; o código 0000 fica fora das dez.",
  "registros_amostra",       "qualidade", "Registros de pessoas na amostra",            "registros",                 "relativa",  0L,              "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor."
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

# Descrevendo cada distrito e o município, com o aviso de amostra menor (o
# município não recebe aviso)
territorio <- totais_amostra |>
  transmute(
    codigo_distrito,
    n_aps,
    registros_amostra = pessoas_n_registros,
    nivel             = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor     = nivel == "distrito" & registros_amostra < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  )

# Preparando as tabelas de rótulos de cada variável
rotulos_atividade <- rotulos |>
  filter(codigo_original == "P0980") |>
  select(codigo_categoria, secao = grupo_codigo, rotulo_secao = grupo_rotulo)
rotulos_ocupacao <- rotulos |>
  filter(codigo_original == "P0970") |>
  select(codigo_categoria, rotulo_ocupacao = descricao_categoria)
rotulos_destino <- rotulos |>
  filter(codigo_original == "P1140") |>
  select(codigo_categoria, rotulo_destino = descricao_categoria)

# Somando as atividades por seção e por grupo de setor, em cada distrito e no
# município
atividade_grupos <- somar_lista(amostra_distritos, "p0980_distribuicao") |>
  left_join(rotulos_atividade, by = "codigo_categoria") |>
  left_join(grupos_setor, by = "secao") |>
  group_by(codigo_distrito, grupo_setor, rotulo_setor) |>
  summarise(
    estimativa  = sum(estimativa),
    n_registros = sum(n_registros),
    .groups     = "drop"
  ) |>
  group_by(codigo_distrito) |>
  mutate(
    pct = 100 * estimativa / sum(estimativa)
  ) |>
  ungroup() |>
  inner_join(
    territorio |> select(nivel_territorial = nivel, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor),
    by = "codigo_distrito"
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    grupo_setor,
    rotulo_setor,
    estimativa,
    n_registros,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, grupo_setor)

# Somando as ocupações e escolhendo as dez mais frequentes da capital, sem o
# código sem rótulo
ocupacoes_todas <- somar_lista(amostra_distritos, "p0970_distribuicao") |>
  left_join(rotulos_ocupacao, by = "codigo_categoria") |>
  group_by(codigo_distrito) |>
  mutate(
    pct = 100 * estimativa / sum(estimativa)
  ) |>
  ungroup()
ocupacoes_principais <- ocupacoes_todas |>
  filter(codigo_distrito == codigo_municipio_sp, codigo_categoria != codigo_ocupacao_sem_rotulo) |>
  arrange(desc(estimativa)) |>
  slice_head(n = n_ocupacoes) |>
  mutate(
    posicao = row_number()
  ) |>
  select(codigo_categoria, posicao)

# Montando a tabela das ocupações: as dez principais da capital, o código sem
# rótulo e as demais, em % dos ocupados, em cada distrito e no município
ocupacoes <- ocupacoes_todas |>
  left_join(ocupacoes_principais, by = "codigo_categoria") |>
  mutate(
    # As dez principais mantêm a posição; o código sem rótulo e as demais vão
    # para as posições seguintes
    posicao         = case_when(
      !is.na(posicao)                                  ~ posicao,
      codigo_categoria == codigo_ocupacao_sem_rotulo   ~ n_ocupacoes + 1L,
      TRUE                                             ~ n_ocupacoes + 2L
    ),
    codigo_categoria = case_when(
      posicao <= n_ocupacoes + 1L ~ codigo_categoria,
      TRUE                        ~ "demais"
    ),
    rotulo_ocupacao = case_when(
      posicao <= n_ocupacoes                 ~ str_to_sentence(rotulo_ocupacao),
      posicao == n_ocupacoes + 1L            ~ "Código 0000 (sem rótulo)",
      TRUE                                   ~ "Demais ocupações"
    )
  ) |>
  group_by(codigo_distrito, posicao, codigo_categoria, rotulo_ocupacao) |>
  summarise(
    estimativa  = sum(estimativa),
    n_registros = sum(n_registros),
    pct         = sum(pct),
    .groups     = "drop"
  ) |>
  inner_join(
    territorio |> select(nivel_territorial = nivel, codigo_distrito, nome_distrito, nome_rotulo, amostra_menor),
    by = "codigo_distrito"
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    posicao,
    codigo_ocupacao = codigo_categoria,
    rotulo_ocupacao,
    estimativa,
    n_registros,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, posicao)

# Montando a tabela dos municípios de trabalho mais frequentes da capital (só o
# município), em % de quem trabalha em outro município
destinos <- somar_lista(amostra_distritos, "p1140_distribuicao", excluir = codigos_sem_destino) |>
  filter(codigo_distrito == codigo_municipio_sp) |>
  left_join(rotulos_destino, by = "codigo_categoria") |>
  mutate(
    pct = 100 * estimativa / sum(estimativa)
  ) |>
  arrange(desc(estimativa)) |>
  slice_head(n = n_destinos) |>
  mutate(
    posicao = row_number()
  ) |>
  select(
    posicao,
    codigo_municipio = codigo_categoria,
    rotulo_destino,
    estimativa,
    n_registros,
    pct
  )

# Calculando os indicadores em formato largo
atividade_largo <- territorio |>
  left_join(
    atividade_grupos |>
      filter(rotulo_setor %in% c("Comércio", "Serviços domésticos")) |>
      mutate(
        indicador = if_else(rotulo_setor == "Comércio", "pct_comercio", "pct_servicos_domesticos")
      ) |>
      select(codigo_distrito, indicador, pct) |>
      pivot_wider(names_from = indicador, values_from = pct),
    by = "codigo_distrito"
  ) |>
  left_join(
    somar_lista(amostra_distritos, "p0980_distribuicao") |>
      left_join(rotulos_atividade, by = "codigo_categoria") |>
      group_by(codigo_distrito) |>
      summarise(
        pct_mal_definidas = 100 * sum(estimativa[secao == "V"]) / sum(estimativa),
        .groups           = "drop"
      ),
    by = "codigo_distrito"
  ) |>
  left_join(
    ocupacoes |>
      filter(posicao <= n_ocupacoes) |>
      group_by(codigo_distrito) |>
      summarise(pct_ocupacoes_principais = sum(pct), .groups = "drop"),
    by = "codigo_distrito"
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
atividade <- atividade_largo |>
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


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(atividade_largo, nivel == "distrito")
largo_municipio <- filter(atividade_largo, nivel == "municipio")

# Calculando os pesos observados por distrito e para o município
pesos <- totais_amostra |>
  select(codigo_distrito, p0970_peso_observado, p0980_peso_observado, p1140_peso_observado)

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Universos de aplicação das três variáveis conferidos",
    "Todas as seções de atividade têm grupo de setor",
    "Os sete grupos de setor somam 100% em cada distrito",
    "Atividades por grupo somam o peso observado de P0980 em cada distrito",
    "Ocupações somam o peso observado de P0970 em cada distrito",
    "Ocupações somam 100% em cada distrito",
    "Municípios de trabalho: a soma da capital sem ignorado nem não sabe não passa do peso observado de P1140",
    "As dez ocupações e os dez municípios principais têm rótulo",
    "Nenhuma proporção fora de 0 a 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    !anyNA(distinct(left_join(rotulos_atividade, grupos_setor, by = "secao"), codigo_categoria, grupo_setor)$grupo_setor),
    atividade_grupos |>
      group_by(codigo_distrito) |>
      summarise(s = sum(pct), n = n(), .groups = "drop") |>
      (\(x) all(abs(x$s - 100) < 1e-6) && all(x$n == 7L))(),
    atividade_grupos |>
      group_by(codigo_distrito) |>
      summarise(s = sum(estimativa), .groups = "drop") |>
      left_join(pesos, by = "codigo_distrito") |>
      (\(x) all(abs(x$s - x$p0980_peso_observado) < 1e-6))(),
    ocupacoes |>
      group_by(codigo_distrito) |>
      summarise(s = sum(estimativa), .groups = "drop") |>
      left_join(pesos, by = "codigo_distrito") |>
      (\(x) all(abs(x$s - x$p0970_peso_observado) < 1e-6))(),
    ocupacoes |>
      group_by(codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      (\(x) all(abs(x$s - 100) < 1e-6))(),
    sum(somar_lista(amostra_distritos, "p1140_distribuicao", excluir = codigos_sem_destino) |>
      filter(codigo_distrito == codigo_municipio_sp) |>
      pull(estimativa)) <= pesos$p1140_peso_observado[pesos$codigo_distrito == codigo_municipio_sp] + 1e-6,
    !anyNA(filter(ocupacoes, nivel_territorial == "municipio", posicao <= n_ocupacoes)$rotulo_ocupacao) &&
      !anyNA(destinos$rotulo_destino),
    atividade |>
      filter(str_starts(indicador, "pct_")) |>
      summarise(ok = all(valor >= 0 & valor <= 100, na.rm = TRUE)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 13
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
  "Município: comércio ", round(largo_municipio$pct_comercio, 1),
  "%; serviços domésticos ", round(largo_municipio$pct_servicos_domesticos, 1),
  "%; mal definidas ", round(largo_municipio$pct_mal_definidas, 1),
  "%; dez ocupações principais ", round(largo_municipio$pct_ocupacoes_principais, 1), "%"
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
  x    = atividade,
  sink = file.path(diretorio_grupo, "atividade.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores_atividade.parquet")
)

# Gravando a composição por grupo de setor
write_parquet(
  x    = atividade_grupos,
  sink = file.path(diretorio_grupo, "setores.parquet")
)

# Gravando as ocupações
write_parquet(
  x    = ocupacoes,
  sink = file.path(diretorio_grupo, "ocupacoes.parquet")
)

# Gravando os municípios de trabalho
write_parquet(
  x    = destinos,
  sink = file.path(diretorio_grupo, "destinos.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = atividade_largo |>
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
    setores     = atividade_grupos |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, grupo_setor, rotulo_setor, estimativa, n_registros, pct),
    ocupacoes   = ocupacoes |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, posicao, rotulo_ocupacao, estimativa, n_registros, pct),
    destinos    = destinos |>
      mutate(pct = round(pct, 3)),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), P0970, P0980 e P1140; rótulos dos descritores do IBGE, pela tabela de rótulos do Dados.",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        "Grupos de setor pelas seções da CNAE-Domiciliar: comércio (G); informação, finanças e serviços profissionais (J a N); indústria e construção (B a F); transporte, alojamento e alimentação (H e I); educação, saúde e administração pública (O a Q); serviços domésticos (T); outros e mal definidas (A, R, S, U e V).",
        "Ocupações pela família ocupacional (P0970, quatro dígitos). O código 0000 (4,5% dos ocupados da capital) não tem rótulo no descritor do IBGE; o descritor tem K000 (mal definidas), sem equivalência certificada, e por isso o 0000 aparece à parte.",
        "Município de trabalho (P1140) só para quem trabalha em outro município; o ignorado e o não sabe ficam fora do cálculo.",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_atividade_ocupacao.xlsx")
)

# Informando o fim do script
message("Indicadores de atividade e ocupação gravados em ", diretorio_grupo)
