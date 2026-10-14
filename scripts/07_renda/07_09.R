################################################################################
##################################            ##################################
############# 07.09) PREPARAR OS INDICADORES DA EXTENSÃO DA RENDA ##############
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
diretorio_grupo <- caminho_data("07_renda")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("07_renda")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Definindo as variáveis da amostra usadas na extensão
codigos_amostra <- c("P1080", "P1090", "P1100", "P1110", "F0260")

# Definindo o salário mínimo de 2022 (R$ por mês), como no restante do grupo
salario_minimo <- 1212


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar as estimativas das APs de cada grupo. As
# estimativas já vêm ponderadas pelo IBGE; somam-se as estimativas, as somas
# ponderadas e os pesos, e os indicadores são recalculados a partir das somas.
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


################################################################################
##### IV. Dados
################################################################################

# Lendo as APs da capital
amostra <- read_parquet(file.path(diretorio_grupo, "extensao_amostra.parquet"))

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
  ~indicador,                 ~grupo,      ~rotulo,                                              ~unidade,                                        ~comparacao, ~casas_decimais, ~descricao,
  "renda_trabalho_media",     "trabalho",  "Rendimento médio do trabalho",                       "R$ por mês, por ocupado com rendimento",        "relativa",  0L,              "Soma ponderada de P1080 (rendimento bruto mensal habitual de todos os trabalhos) sobre o peso observado, dos ocupados de 10 anos ou mais com rendimento (P1070 código 1).",
  "pct_outras_fontes",        "outras",    "Com aposentadoria, pensão, benefício ou aluguel",    "% das pessoas de 10 anos ou mais",              "pontos",    1L,              "P1090 código 1 (rendimento de aposentadoria, pensão, Bolsa Família, BPC ou aluguel) sobre o peso observado de P1090.",
  "renda_outras_fontes_media", "outras",   "Rendimento médio de aposentadoria, pensão, benefícios e aluguel", "R$ por mês, por pessoa de 10 anos ou mais", "relativa", 0L,             "Soma ponderada de P1100 sobre o peso observado, de todas as pessoas de 10 anos ou mais, com ou sem esse rendimento (quem não recebe entra com zero).",
  "renda_todas_fontes_media", "todas",     "Rendimento médio de todas as fontes",                "R$ por mês, por pessoa de 10 anos ou mais",     "relativa",  0L,              "Soma ponderada de P1110 sobre o peso observado, de todas as pessoas de 10 anos ou mais, com ou sem rendimento (quem não recebe entra com zero).",
  "renda_familiar_pc_media",  "familia",   "Rendimento familiar per capita médio",               "R$ por mês, por pessoa da família, média das famílias", "relativa", 0L,            "Soma ponderada de F0260 (rendimento da família dividido pelo número de integrantes) pelo peso das famílias sobre o peso observado, famílias de 2 pessoas ou mais com valor informado.",
  "pct_renda_outras_fontes",  "todas",     "Parcela da renda que não vem do trabalho",           "% do rendimento de todas as fontes",            "pontos",    1L,              "Soma ponderada de P1100 sobre a soma ponderada de P1110: P1110 é a soma do rendimento do trabalho (P1080) e do de aposentadoria, pensão, benefícios e aluguel (P1100), conferido AP a AP.",
  "registros_amostra",        "qualidade", "Registros de pessoas na amostra",                    "registros",                                     "relativa",  0L,              "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor."
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

# Descrevendo cada distrito e o município, com o aviso de amostra menor (o
# município não recebe aviso)
territorio <- totais_amostra |>
  transmute(
    codigo_distrito,
    nivel         = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor = nivel == "distrito" & pessoas_n_registros < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  )

# Calculando os indicadores em formato largo
extensao_largo <- totais_amostra |>
  inner_join(territorio, by = "codigo_distrito") |>
  mutate(
    # Rendimento do trabalho: média ponderada dos ocupados com rendimento
    renda_trabalho_media      = p1080_soma / p1080_peso_observado,
    # Existência de rendimento de outras fontes sobre o peso observado de P1090
    pct_outras_fontes         = 100 * p1090_cat_codigo_1_estimativa / p1090_peso_observado,
    # Médias sobre todas as pessoas de 10 anos ou mais
    renda_outras_fontes_media = p1100_soma / p1100_peso_observado,
    renda_todas_fontes_media  = p1110_soma / p1110_peso_observado,
    # Rendimento familiar per capita: média das famílias
    renda_familiar_pc_media   = f0260_soma / f0260_peso_observado,
    # Parcela da renda total que não vem do trabalho
    pct_renda_outras_fontes   = 100 * p1100_soma / p1110_soma,
    registros_amostra         = pessoas_n_registros
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
extensao <- extensao_largo |>
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
largo_distritos <- filter(extensao_largo, nivel == "distrito")
largo_municipio <- filter(extensao_largo, nivel == "municipio")

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Universos de aplicação das cinco variáveis conferidos",
    "Com e sem rendimento de outras fontes somam o peso observado de P1090 em cada AP",
    "P1110 é a soma de P1080 e P1100 em cada AP (tolerância de R$ 0,01 por pessoa)",
    "P1100 e P1110 têm o mesmo peso observado em cada AP",
    "Rendimento médio de todas as fontes não é menor que o de outras fontes em cada distrito",
    "Rendimentos médios positivos e abaixo de R$ 30.000 em cada distrito (inclui o familiar per capita)",
    "Nenhuma proporção fora de 0 a 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    all(abs(amostra$p1090_cat_codigo_1_estimativa + amostra$p1090_cat_codigo_2_estimativa - amostra$p1090_peso_observado) < 1e-6),
    all(abs((amostra$p1080_soma + amostra$p1100_soma - amostra$p1110_soma) / amostra$p1110_peso_observado) < 0.01),
    all(abs(amostra$p1100_peso_observado - amostra$p1110_peso_observado) < 1e-6),
    all(largo_distritos$renda_todas_fontes_media >= largo_distritos$renda_outras_fontes_media - 1e-6),
    all(largo_distritos$renda_trabalho_media > 0 & largo_distritos$renda_todas_fontes_media > 0 &
          largo_distritos$renda_trabalho_media < 30000 & largo_distritos$renda_todas_fontes_media < 30000 &
          largo_distritos$renda_familiar_pc_media > 0 & largo_distritos$renda_familiar_pc_media < 30000),
    extensao |>
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
  "Município: rendimento do trabalho R$ ", round(largo_municipio$renda_trabalho_media),
  "; com outras fontes ", round(largo_municipio$pct_outras_fontes, 1),
  "%; todas as fontes R$ ", round(largo_municipio$renda_todas_fontes_media),
  "; parcela fora do trabalho ", round(largo_municipio$pct_renda_outras_fontes, 1),
  "%; familiar per capita R$ ", round(largo_municipio$renda_familiar_pc_media)
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
  x    = extensao,
  sink = file.path(diretorio_grupo, "extensao.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores_extensao.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = extensao_largo |>
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
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), P1080, P1090, P1100, P1110 e F0260.",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        paste0("Valores nominais de 2022 (salário mínimo de R$ ", format(salario_minimo, big.mark = ".", decimal.mark = ","), "); médias ponderadas, sem padronização por idade."),
        "O rendimento do trabalho (P1080) é a média dos ocupados com rendimento; as médias de outras fontes (P1100) e de todas as fontes (P1110) são sobre todas as pessoas de 10 anos ou mais, com ou sem rendimento, e por isso não são comparáveis à primeira.",
        "P1110 é a soma de P1080 e P1100 em cada AP; a parcela da renda que não vem do trabalho usa essa identidade.",
        "O rendimento familiar per capita (F0260) é a média das famílias (peso F0111), não das pessoas, e cobre as famílias de 2 pessoas ou mais (o arquivo de famílias do IBGE não traz os domicílios unipessoais); universo verificado no microdado em 2026-10-13: todas as famílias, menos 31 registros de conviventes secundárias sem valor. Difere do rendimento domiciliar per capita do grupo, que é média por pessoa e inclui os unipessoais.",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_renda_extensao.xlsx")
)

# Informando o fim do script
message("Indicadores da extensão da renda gravados em ", diretorio_grupo)
