################################################################################
##################################            ##################################
################## 11.02) PREPARAR OS INDICADORES DA MORADIA ###################
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
diretorio_grupo <- caminho_data("11_moradia")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("11_moradia")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c("D0190", "D0210", "D0240", "D0320", "D0330")

# Definindo os valores de referência do município medidos em 2026-10-12
# (etapa 10 do plano), em %
referencia_alugado     <- 28.4
referencia_3_banheiros <- 10.0

# Descrevendo as cinco condições de ocupação, com os códigos de D0190
condicoes_ocupacao <- tibble(
  condicao_codigo = 1:5,
  rotulo_condicao = c(
    "Próprio quitado",
    "Próprio em pagamento",
    "Alugado",
    "Cedido",
    "Outra condição"
  ),
  campos          = list(
    "d0190_cat_codigo_1_estimativa",
    "d0190_cat_codigo_2_estimativa",
    "d0190_cat_codigo_3_estimativa",
    sprintf("d0190_cat_codigo_%d_estimativa", 4:6),
    "d0190_cat_codigo_7_estimativa"
  )
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar as estimativas das APs de cada grupo. As
# estimativas já vêm ponderadas pelo IBGE; somam-se as estimativas, as somas
# ponderadas e os pesos, e os indicadores são recalculados a partir das somas,
# nunca pela média das médias ou das proporções das APs.
somar_aps <- function(aps_agrupadas) {

  # Somando os registros, as estimativas, as somas ponderadas e os pesos
  aps_agrupadas |>
    summarise(
      n_aps = n(),
      across(
        .cols = c(where(is.numeric), -d0240_media),
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


################################################################################
##### IV. Dados
################################################################################

# Lendo as APs da capital
amostra <- read_parquet(file.path(diretorio_grupo, "amostra.parquet"))

# Lendo os setores da capital
setores <- read_parquet(file.path(diretorio_grupo, "setores.parquet"))

# Lendo a linha do município
municipio <- read_parquet(file.path(diretorio_grupo, "municipio.parquet"))

# Lendo as ausências dos campos do universo
ausencias <- read_parquet(file.path(diretorio_grupo, "ausencias.parquet"))

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
  ~indicador,                     ~grupo,       ~base,      ~rotulo,                                          ~unidade,                                       ~comparacao, ~casas_decimais, ~descricao,
  "pct_proprio_quitado",          "ocupacao",   "amostra",  "Domicílios próprios quitados",                   "% dos domicílios ocupados",                    "pontos",    1L,              "D0190 código 1 (próprio de algum morador, já pago, herdado ou ganho) sobre o peso observado de D0190 (domicílios particulares permanentes ocupados, amostra).",
  "pct_financiando",              "ocupacao",   "amostra",  "Domicílios próprios em pagamento",               "% dos domicílios ocupados",                    "pontos",    1L,              "D0190 código 2 (próprio de algum morador, ainda pagando) sobre o peso observado de D0190 (amostra).",
  "pct_alugado",                  "ocupacao",   "amostra",  "Domicílios alugados",                            "% dos domicílios ocupados",                    "pontos",    1L,              "D0190 código 3 (alugado) sobre o peso observado de D0190 (amostra).",
  "pct_cedido",                   "ocupacao",   "amostra",  "Domicílios cedidos",                             "% dos domicílios ocupados",                    "pontos",    1L,              "D0190 códigos 4 a 6 (cedido ou emprestado por empregador, por familiar ou de outra forma) sobre o peso observado de D0190 (amostra).",
  "pct_outra_condicao",           "ocupacao",   "amostra",  "Domicílios em outra condição de ocupação",       "% dos domicílios ocupados",                    "pontos",    1L,              "D0190 código 7 (outra condição) sobre o peso observado de D0190 (amostra).",
  "pct_sem_internet",             "materiais",  "amostra",  "Domicílios sem acesso à internet",               "% dos domicílios ocupados",                    "pontos",    1L,              "D0330 código 2 (não) sobre o peso observado de D0330 (amostra).",
  "pct_sem_maquina",              "materiais",  "amostra",  "Domicílios sem máquina de lavar roupa",          "% dos domicílios ocupados",                    "pontos",    1L,              "D0320 código 2 (não) sobre o peso observado de D0320 (amostra).",
  "moradores_dormitorio",         "materiais",  "amostra",  "Moradores por dormitório",                       "moradores por cômodo usado como dormitório",   "relativa",  2L,              "Média de D0240 (moradores por cômodo usado como dormitório, calculado em cada domicílio) ponderada pelos domicílios: soma ponderada sobre o peso observado (amostra). Só a média existe no tratado, sem a distribuição.",
  "pct_paredes_sem_revestimento", "materiais",  "amostra",  "Domicílios sem paredes de alvenaria revestida",  "% dos domicílios ocupados",                    "pontos",    1L,              "D0210 códigos 2 a 7 (alvenaria sem revestimento, taipa sem revestimento, madeira, outro material ou sem parede) sobre o peso observado de D0210 (amostra).",
  "pct_sem_banheiro_exclusivo",   "banheiros",  "universo", "Domicílios sem banheiro de uso exclusivo",       "% dos domicílios ocupados",                    "pontos",    2L,              "Apenas banheiro de uso comum, apenas sanitário ou buraco e sem banheiro nem sanitário (v00236 a v00238) sobre a soma publicada das sete categorias (v00232 a v00238, universo).",
  "pct_1_banheiro",               "banheiros",  "universo", "Domicílios com 1 banheiro",                      "% dos domicílios ocupados",                    "pontos",    1L,              "Um banheiro de uso exclusivo com chuveiro e vaso sanitário (v00232) sobre a soma publicada das sete categorias (universo).",
  "pct_2_banheiros",              "banheiros",  "universo", "Domicílios com 2 banheiros",                     "% dos domicílios ocupados",                    "pontos",    1L,              "Dois banheiros de uso exclusivo (v00233) sobre a soma publicada das sete categorias (universo).",
  "pct_3_mais_banheiros",         "banheiros",  "universo", "Domicílios com 3 banheiros ou mais",             "% dos domicílios ocupados",                    "pontos",    1L,              "Três banheiros (v00234) e quatro ou mais (v00235) de uso exclusivo sobre a soma publicada das sete categorias (universo).",
  "domicilios_amostra",           "qualidade",  "amostra",  "Domicílios ocupados (estimativa da amostra)",    "domicílios",                                   "relativa",  0L,              "Peso observado de D0190 (domicílios particulares permanentes ocupados), base das proporções da amostra.",
  "domicilios_universo",          "qualidade",  "universo", "Domicílios ocupados com banheiro publicado",     "domicílios",                                   "relativa",  0L,              "Soma publicada de v00232 a v00238, base das proporções de banheiros; fica abaixo de v00001 pelas células com sigilo.",
  "registros_amostra",            "qualidade",  "amostra",  "Registros de pessoas na amostra",                "registros",                                    "relativa",  0L,              "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor.",
  "registros_domicilios",         "qualidade",  "amostra",  "Registros de domicílios na amostra",             "registros",                                    "relativa",  0L,              "Soma de domicilios_n_registros das APs do distrito (base das proporções da amostra)."
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

# Ligando os setores aos distritos
setores_distritos <- setores |>
  inner_join(setor_distrito, by = "codigo_setor")

# Somando os setores por distrito e para o município
totais_universo <- bind_rows(
  setores_distritos |>
    group_by(codigo_distrito) |>
    somar_setores(),
  setores_distritos |>
    somar_setores() |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Calculando os indicadores em formato largo (proporções de 0 a 100)
moradia_largo <- totais_amostra |>
  inner_join(totais_universo, by = "codigo_distrito") |>
  mutate(
    # --- Condição de ocupação (base: peso observado de D0190)
    pct_proprio_quitado          = 100 * d0190_cat_codigo_1_estimativa / d0190_peso_observado,
    pct_financiando              = 100 * d0190_cat_codigo_2_estimativa / d0190_peso_observado,
    pct_alugado                  = 100 * d0190_cat_codigo_3_estimativa / d0190_peso_observado,
    pct_cedido                   = 100 * (d0190_cat_codigo_4_estimativa + d0190_cat_codigo_5_estimativa +
                                            d0190_cat_codigo_6_estimativa) / d0190_peso_observado,
    pct_outra_condicao           = 100 * d0190_cat_codigo_7_estimativa / d0190_peso_observado,
    # --- Condições materiais (base: peso observado de cada variável)
    pct_sem_internet             = 100 * d0330_cat_codigo_2_estimativa / d0330_peso_observado,
    pct_sem_maquina              = 100 * d0320_cat_codigo_2_estimativa / d0320_peso_observado,
    # Média ponderada por domicílio: soma ponderada sobre o peso observado
    moradores_dormitorio         = d0240_soma / d0240_peso_observado,
    pct_paredes_sem_revestimento = 100 * (d0210_peso_observado - d0210_cat_codigo_1_estimativa) / d0210_peso_observado,
    # --- Banheiros (base: soma publicada das sete categorias do universo)
    domicilios_universo          = v00232 + v00233 + v00234 + v00235 + v00236 + v00237 + v00238,
    pct_sem_banheiro_exclusivo   = 100 * (v00236 + v00237 + v00238) / domicilios_universo,
    pct_1_banheiro               = 100 * v00232 / domicilios_universo,
    pct_2_banheiros              = 100 * v00233 / domicilios_universo,
    pct_3_mais_banheiros         = 100 * (v00234 + v00235) / domicilios_universo,
    # --- Qualidade e aviso de amostra menor (o município não recebe aviso)
    domicilios_amostra           = d0190_peso_observado,
    registros_amostra            = pessoas_n_registros,
    registros_domicilios         = domicilios_n_registros,
    nivel                        = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor                = nivel == "distrito" & registros_amostra < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
moradia <- moradia_largo |>
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

# Criando a tabela da composição por condição de ocupação
ocupacao <- condicoes_ocupacao |>
  mutate(
    # Somando as categorias de D0190 de cada condição, em cada distrito
    valores = map(
      campos,
      \(campos_condicao) tibble(
        codigo_distrito = moradia_largo$codigo_distrito,
        domicilios      = rowSums(as.matrix(moradia_largo[campos_condicao]))
      )
    )
  ) |>
  select(-campos) |>
  unnest(valores) |>
  inner_join(
    moradia_largo |>
      select(
        nivel_territorial = nivel,
        codigo_distrito,
        nome_distrito,
        nome_rotulo,
        amostra_menor,
        domicilios_amostra
      ),
    by = "codigo_distrito"
  ) |>
  mutate(
    pct = 100 * domicilios / domicilios_amostra
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    condicao_codigo,
    rotulo_condicao,
    domicilios,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, condicao_codigo)

# Contando as células ausentes (sigilo) por distrito, para a tabela de revisão
ausencias_distrito <- ausencias |>
  inner_join(setor_distrito, by = "codigo_setor") |>
  count(codigo_distrito, name = "celulas_com_sigilo")


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(moradia_largo, nivel == "distrito")
largo_municipio <- filter(moradia_largo, nivel == "municipio")

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

# Criando uma função para conferir se o peso observado de uma variável
# categórica é igual à soma das suas categorias em cada AP
conferir_categorias <- function(prefixo, codigos) {

  # Comparando o peso observado com a soma das estimativas das categorias
  all(abs(
    amostra[[paste0(prefixo, "_peso_observado")]] -
      rowSums(as.matrix(amostra[sprintf("%s_cat_codigo_%d_estimativa", prefixo, codigos)]))
  ) < 1e-6)
}

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores da amostra e do universo",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Universos de aplicação das cinco variáveis conferidos",
    "Peso observado igual à soma das categorias em cada AP (D0190, D0210, D0320, D0330)",
    "Média publicada da AP igual a soma sobre peso observado (D0240)",
    "Condições de ocupação somam 100% em cada distrito",
    "Setores ligados",
    "Soma dos setores igual à tabela municipal nos campos de banheiro",
    "Categorias de banheiro somam 100% em cada distrito",
    "Células ausentes do universo são só do símbolo X (sigilo)",
    "Nenhuma proporção fora de 0 a 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)",
    "Município igual aos valores de referência da etapa 10 (alugados e 3 banheiros ou mais)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    conferir_categorias("d0190", 1:7) && conferir_categorias("d0210", 1:7) &&
      conferir_categorias("d0320", 1:2) && conferir_categorias("d0330", 1:2),
    all(abs(amostra$d0240_soma / amostra$d0240_peso_observado - amostra$d0240_media) < 1e-6),
    ocupacao |>
      group_by(nivel_territorial, codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    nrow(setores_distritos) == nrow(setores),
    all(abs(unlist(soma_setores[campos_comuns]) - unlist(municipio[campos_comuns])) < 1e-6),
    all(abs(
      moradia_largo$pct_sem_banheiro_exclusivo + moradia_largo$pct_1_banheiro +
        moradia_largo$pct_2_banheiros + moradia_largo$pct_3_mais_banheiros - 100
    ) < 1e-6),
    all(ausencias$ausencia_original == "X"),
    moradia |>
      filter(str_starts(indicador, "pct_")) |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 13,
    abs(largo_municipio$pct_alugado - referencia_alugado) < 0.05 &&
      abs(largo_municipio$pct_3_mais_banheiros - referencia_3_banheiros) < 0.05
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Mostrando a cobertura dos banheiros publicados no município
message(
  "Domicílios com banheiro publicado sobre v00001 no município: ",
  round(100 * largo_municipio$domicilios_universo / largo_municipio$v00001, 2), "%; mínima entre os distritos: ",
  round(100 * min(largo_distritos$domicilios_universo / largo_distritos$v00001), 2), "%"
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
  x    = moradia,
  sink = file.path(diretorio_grupo, "moradia.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a composição por condição de ocupação
write_parquet(
  x    = ocupacao,
  sink = file.path(diretorio_grupo, "ocupacao.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = moradia_largo |>
      left_join(ausencias_distrito, by = "codigo_distrito") |>
      mutate(celulas_com_sigilo = if_else(nivel == "distrito", replace_na(celulas_com_sigilo, 0L), NA_integer_)) |>
      select(
        codigo_distrito,
        nome_distrito,
        n_aps,
        amostra_menor,
        all_of(indicadores$indicador),
        celulas_com_sigilo
      ) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, "; ", indicadores$base, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                                   = codigo_distrito,
        `Distrito`                                 = nome_distrito,
        `APs no distrito`                          = n_aps,
        `Amostra menor (aviso)`                    = amostra_menor,
        `Células com sigilo (setores do distrito)` = celulas_com_sigilo
      ),
    ocupacao    = ocupacao |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, condicao_codigo, rotulo_condicao, domicilios, pct),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fontes: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), D0190, D0210, D0240, D0320 e D0330; e resultados do universo por setor censitário, banheiros (v00232 a v00238).",
        "Dados da amostra de acesso controlado: uso interno, não publicar sem autorização. Os indicadores de banheiros vêm do universo e são públicos.",
        "O Censo 2022 não pergunta o valor do aluguel; só a condição de ocupação. O rendimento de aluguel recebido (P1100) vem somado a aposentadoria, pensão, Bolsa Família e BPC.",
        "Proporções da amostra sobre o peso observado de cada variável (domicílios particulares permanentes ocupados); o branco (não se aplica) e o ignorado ficam fora.",
        "Banheiros: os totais por distrito somam só os valores publicados por setor; as células com sigilo ficam fora, sem imputação (na capital, a soma dos setores coincide com a tabela municipal).",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_moradia.xlsx")
)

# Informando o fim do script
message("Indicadores da moradia gravados em ", diretorio_grupo)
