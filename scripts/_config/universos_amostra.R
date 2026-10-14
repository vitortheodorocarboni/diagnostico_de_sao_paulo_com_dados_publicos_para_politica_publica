################################################################################
##################################            ##################################
############################# UNIVERSOS DA AMOSTRA #############################
##################################            ##################################
################################################################################

# Universo de aplicação (a quem a pergunta se aplica) das variáveis da amostra
# do Censo usadas na ampliação temática (etapas 10 a 19 do plano). O layout do
# IBGE não traz os universos; eles foram deduzidos em 2026-10-12 comparando,
# no Município de São Paulo, o peso observado de cada variável (mais o peso
# ignorado) com a população de referência. Nas variáveis conferidas, a razão
# ficou entre 0,9988 e 1,0016.
#
# Regras para os grupos:
#   - o denominador de uma proporção é o peso observado da própria variável
#     (exclui o branco, que é o "não se aplica", e o ignorado);
#   - variáveis com situação "nao_identificado" não entram em indicadores (hoje,
#     nenhuma: P0560 a P0630 foram conferidas na etapa 17c);
#   - conferir_universos() roda no script NN_02 de cada grupo, na capital
#     inteira, antes de calcular os indicadores.


################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("tidyverse")

# Carregando bibliotecas
library(tidyverse)


################################################################################
##### II. Populações de referência
################################################################################

# Definindo os domicílios particulares permanentes ocupados (tipos 11 a 16)
ref_domicilios_ocupados <- paste0("d0200_cat_codigo_", 11:16, "_estimativa", collapse = ";")

# Definindo o total de pessoas da AP
ref_pessoas <- "pessoas_estimativa_total"

# Definindo os ocupados de 10 anos ou mais
ref_ocupados_10 <- "p0960_cat_codigo_1_estimativa"

# Definindo quem trabalha fora de casa, no município ou em outro município
ref_trabalha_fora <- "p1120_cat_codigo_2_estimativa;p1120_cat_codigo_3_estimativa"

# Definindo quem se desloca ao trabalho, com o tempo informado ou ignorado
ref_se_desloca <- paste0("p1180_cat_codigo_", c(1:7, 9), "_estimativa", collapse = ";")


################################################################################
##### III. Universos de aplicação
################################################################################

# Criando a tabela de universos (referência: campos somados e subtraídos,
# separados por ponto e vírgula)
universos_amostra <- tribble(
  ~codigo, ~tipo,        ~peso,   ~universo,                                                                     ~referencia_soma,                                  ~referencia_subtrai,                  ~situacao,          ~nota,
  "D0190", "categorica", "D0111", "Domicílios particulares permanentes ocupados",                                ref_domicilios_ocupados,                           NA,                                   "conferido",        NA,
  "D0210", "categorica", "D0111", "Domicílios particulares permanentes ocupados",                                ref_domicilios_ocupados,                           NA,                                   "conferido",        NA,
  "D0240", "numerica",   "D0111", "Domicílios particulares permanentes ocupados",                                ref_domicilios_ocupados,                           NA,                                   "conferido",        "Só a média de moradores por dormitório; não há distribuição no tratado.",
  "D0270", "categorica", "D0111", "Domicílios particulares permanentes ocupados",                                ref_domicilios_ocupados,                           NA,                                   "conferido",        NA,
  "D0320", "categorica", "D0111", "Domicílios particulares permanentes ocupados",                                ref_domicilios_ocupados,                           NA,                                   "conferido",        NA,
  "D0330", "categorica", "D0111", "Domicílios particulares permanentes ocupados",                                ref_domicilios_ocupados,                           NA,                                   "conferido",        NA,
  "D0340", "categorica", "D0111", "Domicílios particulares permanentes ocupados",                                ref_domicilios_ocupados,                           NA,                                   "conferido",        "Código 9 (ignorado) fora do peso observado.",
  "M0150", "categorica", "M0111", "Moradores falecidos de janeiro de 2019 a julho de 2022",                    "mortalidade_estimativa_total",                    NA,                                   "conferido",        "Ano do óbito (2022 só até julho).",
  "M0160", "categorica", "M0111", "Moradores falecidos de janeiro de 2019 a julho de 2022",                    "mortalidade_estimativa_total",                    NA,                                   "conferido",        NA,
  "M0170", "categorica", "M0111", "Moradores falecidos de janeiro de 2019 a julho de 2022",                    "mortalidade_estimativa_total",                    NA,                                   "conferido",        "Código 99 (idade ignorada) dentro do peso observado; excluí-lo nas faixas de idade.",
  "M0171", "numerica",   "M0111", "Moradores falecidos com idade informada",                                   "mortalidade_estimativa_total",                    "m0170_cat_codigo_99_estimativa",     "conferido",        NA,
  "P0180", "categorica", "P0111", "Todas as pessoas",                                                            ref_pessoas,                                       NA,                                   "conferido",        "Código 99 (ignorado) fora do peso observado.",
  "P0420", "categorica", "P0111", "Pessoas de 2 anos ou mais",                                                  ref_pessoas,                                       "p0470_cat_codigo_9_estimativa",      "conferido",        NA,
  "P0430", "categorica", "P0111", "Pessoas de 2 anos ou mais",                                                   ref_pessoas,                                       "p0470_cat_codigo_9_estimativa",      "conferido",        NA,
  "P0440", "categorica", "P0111", "Pessoas de 2 anos ou mais",                                                   ref_pessoas,                                       "p0470_cat_codigo_9_estimativa",      "conferido",        NA,
  "P0450", "categorica", "P0111", "Pessoas de 2 anos ou mais",                                                   ref_pessoas,                                       "p0470_cat_codigo_9_estimativa",      "conferido",        NA,
  "P0460", "categorica", "P0111", "Pessoas de 2 anos ou mais",                                                   ref_pessoas,                                       "p0470_cat_codigo_9_estimativa",      "conferido",        NA,
  "P0470", "categorica", "P0111", "Pessoas de 2 anos ou mais",                                                   ref_pessoas,                                       NA,                                   "conferido",        "Menores de 2 anos no código 9, fora do peso observado.",
  "P1200", "categorica", "P0111", "Todas as pessoas",                                                            ref_pessoas,                                       NA,                                   "conferido",        NA,
  "P0480", "categorica", "P0111", "Todas as pessoas",                                                            ref_pessoas,                                       NA,                                   "conferido",        NA,
  "P0490", "categorica", "P0111", "Nascidos em outro município do Brasil",                                       "p0480_cat_codigo_2_estimativa",                   NA,                                   "conferido",        "Para a população total, multiplicar pela parcela de P0480 código 2.",
  "P0500", "lista",      "P0111", "Nascidos em outro município do Brasil",                                       "p0480_cat_codigo_2_estimativa",                   NA,                                   "conferido",        "Municípios sem rótulo no dicionário do Dados.",
  "P0510", "lista",      "P0111", "Nascidos no exterior",                                                        "p0480_cat_codigo_3_estimativa",                   NA,                                   "conferido",        "Países sem rótulo no dicionário do Dados.",
  "P0520", "categorica", "P0111", "Todas as pessoas",                                                            ref_pessoas,                                       NA,                                   "conferido",        NA,
  "P0530", "categorica", "P0111", "Nascidos no município",                                                       "p0480_cat_codigo_1_estimativa",                   NA,                                   "conferido",        NA,
  "P0540", "numerica",   "P0111", "Nascidos no exterior",                                                        "p0480_cat_codigo_3_estimativa",                   NA,                                   "conferido",        "Ano de fixação de residência no Brasil.",
  "P0550", "numerica",   "P0111", "Nascidos fora do município ou que já moraram fora dele",                      "p0480_cat_codigo_2_estimativa;p0480_cat_codigo_3_estimativa;p0530_cat_codigo_1_estimativa", NA,          "conferido",        NA,
  "P0560", "categorica", "P0111", "Pessoas que moram sem interrupção há menos de 10 anos no município (P0550 abaixo de 10)", NA,                                                NA,                                   "conferido",        "Universo do Dicionário de Variáveis do IBGE, conferido no Dados registro a registro (aba universos_ibge); total da capital em referencias_fixas.",
  "P0570", "categorica", "P0111", "Moradia anterior em estado e município do Brasil (P0560 código 1)",                          "p0560_cat_codigo_1_estimativa",                   NA,                                   "conferido",        "Unidade da Federação de moradia anterior, sem rótulo no dicionário do Dados.",
  "P0580", "lista",      "P0111", "Moradia anterior em estado e município do Brasil (P0560 código 1)",                          "p0560_cat_codigo_1_estimativa",                   NA,                                   "conferido",        "Municípios rotulados pela tabela de rótulos do Dados.",
  "P0590", "lista",      "P0111", "Moradia anterior em país estrangeiro (P0560 código 2)",                                      "p0560_cat_codigo_2_estimativa",                   NA,                                   "conferido",        "Países rotulados pela tabela de rótulos do Dados.",
  "P0600", "categorica", "P0111", "Pessoas de 5 anos ou mais que moram sem interrupção há menos de 6 anos no município (P0550 abaixo de 6)", NA,                                  NA,                                   "conferido",        "Universo do Dicionário de Variáveis do IBGE, conferido no Dados registro a registro (aba universos_ibge); total da capital em referencias_fixas. Código 1: neste município há 5 anos (inclui quem chegou há 5 a 6 anos).",
  "P0610", "categorica", "P0111", "Moradia há 5 anos em outro município do Brasil (P0600 código 2)",                             "p0600_cat_codigo_2_estimativa",                   NA,                                   "conferido",        "Unidade da Federação de moradia há 5 anos, sem rótulo no dicionário do Dados.",
  "P0620", "lista",      "P0111", "Moradia há 5 anos em outro município do Brasil (P0600 código 2)",                             "p0600_cat_codigo_2_estimativa",                   NA,                                   "conferido",        "Municípios rotulados pela tabela de rótulos do Dados.",
  "P0630", "lista",      "P0111", "Moradia há 5 anos em outro país (P0600 código 3)",                                            "p0600_cat_codigo_3_estimativa",                   NA,                                   "conferido",        "Países rotulados pela tabela de rótulos do Dados.",
  "P0650", "categorica", "P0111", "Todas as pessoas",                                                            ref_pessoas,                                       NA,                                   "conferido",        NA,
  "P0660", "categorica", "P0111", "Pessoas que frequentam escola ou creche",                                     "p0650_cat_codigo_1_estimativa",                   NA,                                   "conferido",        NA,
  "P0700", "categorica", "P0111", "Pessoas que não frequentam, mas já frequentaram",                             "p0650_cat_codigo_2_estimativa",                   NA,                                   "conferido",        NA,
  "P0750", "lista",      "P0111", "Pessoas com superior completo",                                               "p0770_cat_codigo_4_estimativa",                   NA,                                   "conferido",        "Áreas de curso sem rótulo no dicionário do Dados.",
  "P0790", "numerica",   "P0111", "Todas as pessoas",                                                            ref_pessoas,                                       NA,                                   "conferido",        NA,
  "P0910", "categorica", "P0111", "Todas as pessoas",                                                            ref_pessoas,                                       NA,                                   "conferido",        "Indicadora de 10 anos ou mais.",
  "P0920", "categorica", "P0111", "Todas as pessoas",                                                            ref_pessoas,                                       NA,                                   "conferido",        "Indicadora de 14 anos ou mais.",
  "P0930", "categorica", "P0111", "Pessoas de 14 anos ou mais",                                                  "p0920_cat_codigo_1_estimativa",                   NA,                                   "conferido",        "Código 1: na força de trabalho.",
  "P0950", "categorica", "P0111", "Pessoas de 14 anos ou mais na força de trabalho",                             "p0930_cat_codigo_1_estimativa",                   NA,                                   "conferido",        "Código 0: desocupada; a desocupação é a proporção do código 0.",
  "P0940", "categorica", "P0111", "Ocupados de 14 anos ou mais",                                                 "p0950_cat_codigo_1_estimativa",                   NA,                                   "conferido",        NA,
  "P0960", "categorica", "P0111", "Pessoas de 10 anos ou mais",                                                  "p0910_cat_codigo_1_estimativa",                   NA,                                   "conferido",        NA,
  "P0970", "lista",      "P0111", "Ocupados de 10 anos ou mais",                                                 ref_ocupados_10,                                   NA,                                   "conferido",        "Ocupações sem rótulo no dicionário do Dados.",
  "P0980", "lista",      "P0111", "Ocupados de 10 anos ou mais",                                                 ref_ocupados_10,                                   NA,                                   "conferido",        "Atividades sem rótulo no dicionário do Dados.",
  "P0990", "categorica", "P0111", "Ocupados de 10 anos ou mais",                                                 ref_ocupados_10,                                   NA,                                   "conferido",        NA,
  "P1000", "categorica", "P0111", "Empregados, exceto militares e estatutários (P0990 01, 03, 05 e 06)",         "p0990_cat_codigo_01_estimativa;p0990_cat_codigo_03_estimativa;p0990_cat_codigo_05_estimativa;p0990_cat_codigo_06_estimativa", NA, "conferido", "Razão de 0,9988 na capital.",
  "P1010", "categorica", "P0111", "Empregadores e conta própria (P0990 07 e 08)",                                "p0990_cat_codigo_07_estimativa;p0990_cat_codigo_08_estimativa", NA,                     "conferido",        "Razão de 0,9993 na capital.",
  "P1030", "categorica", "P0111", "Ocupados de 10 anos ou mais",                                                 ref_ocupados_10,                                   NA,                                   "conferido",        NA,
  "P1040", "categorica", "P0111", "Ocupados de 10 anos ou mais",                                                 ref_ocupados_10,                                   NA,                                   "conferido",        NA,
  "P1050", "categorica", "P0111", "Pessoas de 14 anos ou mais não ocupadas",                                     "p0920_cat_codigo_1_estimativa",                   "p0950_cat_codigo_1_estimativa",      "conferido",        NA,
  "P1060", "categorica", "P0111", "Pessoas que procuraram trabalho em 30 dias",                                  "p1050_cat_codigo_1_estimativa",                   NA,                                   "conferido",        NA,
  "P1070", "categorica", "P0111", "Ocupados de 10 anos ou mais, exceto não remunerados",                         ref_ocupados_10,                                   "p0990_cat_codigo_09_estimativa",     "conferido",        NA,
  "P1080", "numerica",   "P0111", "Ocupados com rendimento em dinheiro, produtos ou mercadorias",                "p1070_cat_codigo_1_estimativa",                   NA,                                   "conferido",        NA,
  "P1090", "categorica", "P0111", "Pessoas de 10 anos ou mais",                                                  "p0910_cat_codigo_1_estimativa",                   NA,                                   "conferido",        "Junta aposentadoria, pensão, Bolsa Família, BPC e aluguel.",
  "P1100", "numerica",   "P0111", "Pessoas de 10 anos ou mais",                                                  "p0910_cat_codigo_1_estimativa",                   NA,                                   "conferido",        "Média sobre todas as pessoas de 10 anos ou mais, com ou sem esse rendimento.",
  "P1110", "numerica",   "P0111", "Pessoas de 10 anos ou mais",                                                  "p0910_cat_codigo_1_estimativa",                   NA,                                   "conferido",        "Média sobre todas as pessoas de 10 anos ou mais, com ou sem rendimento.",
  "F0260", "numerica",   "F0111", "Famílias de 2 pessoas ou mais (o arquivo de famílias do IBGE não traz os domicílios unipessoais)", "f0150_peso_observado",                  NA,                                   "conferido",        "Universo verificado no microdado em 2026-10-13: todas as famílias de SP, menos 31 registros de conviventes secundárias sem valor (150 famílias expandidas na capital). Média por família, não por pessoa.",
  "P1120", "categorica", "P0111", "Ocupados de 10 anos ou mais",                                                 ref_ocupados_10,                                   NA,                                   "conferido",        "Código 9 (ignorado, 4,6% na capital) fora do peso observado.",
  "P1140", "lista",      "P0111", "Ocupados que trabalham em outro município (P1120 código 3)",                  "p1120_cat_codigo_3_estimativa",                   NA,                                   "conferido",        "Municípios sem rótulo no dicionário do Dados.",
  "P1160", "categorica", "P0111", "Ocupados que trabalham fora de casa, no município ou em outro (P1120 2 e 3)",  ref_trabalha_fora,                                 NA,                                   "conferido",        NA,
  "P1180", "categorica", "P0111", "Ocupados que trabalham fora de casa, no município ou em outro (P1120 2 e 3)",  ref_trabalha_fora,                                 NA,                                   "conferido",        "Código 0: não se desloca; código 9 fora do peso observado.",
  "P1170", "categorica", "P0111", "Ocupados que se deslocam ao trabalho (P1180 1 a 7 e 9)",                      ref_se_desloca,                                    NA,                                   "conferido",        "Código 99 (ignorado) fora do peso observado.",
  "P1190", "numerica",   "P0111", "Ocupados que se deslocam ao trabalho com tempo informado (P1180 1 a 7)",      paste0("p1180_cat_codigo_", 1:7, "_estimativa", collapse = ";"), NA,                      "conferido",        "A média de 56 minutos na capital fica acima da obtida pelos pontos médios das classes de P1180 (52 minutos)."
)

# Definindo os totais de referência da capital das variáveis cujo universo não
# se confere por campos agregados (medidos no microdado, no Dados, em
# 2026-10-13, na aba universos_ibge do dicionário): P0560, pessoas que moram há
# menos de 10 anos no município, e P0600, pessoas de 5 anos ou mais que moram
# há menos de 6 anos
referencias_fixas <- c(
  P0560 = 930163.3,
  P0600 = 632365.5
)


################################################################################
##### IV. Funções
################################################################################

# Criando uma função para separar os campos de uma referência
separar_campos <- function(texto) {

  # Devolvendo vazio para referência ausente
  if (is.na(texto)) {
    return(character())
  }

  # Separando os campos pelo ponto e vírgula
  strsplit(texto, ";", fixed = TRUE)[[1]]
}

# Criando uma função para listar os campos que a conferência dos universos lê
campos_universos <- function(codigos) {

  # Selecionando os universos pedidos
  universos <- universos_amostra |>
    filter(codigo %in% codigos)

  # Identificando os códigos que não estão na tabela
  faltantes <- setdiff(codigos, universos$codigo)

  # Interrompendo se algum código não estiver na tabela
  if (length(faltantes) > 0) {
    stop("Variáveis sem universo registrado: ", paste(faltantes, collapse = ", "))
  }

  # Definindo os prefixos dos campos no tratado
  prefixos <- str_to_lower(universos$codigo)

  # Montando os pesos observado e ignorado (este não existe nas numéricas)
  campos_pesos <- c(
    paste0(prefixos, "_peso_observado"),
    paste0(prefixos[universos$tipo != "numerica"], "_peso_ignorado")
  )

  # Juntando os campos das referências
  campos_referencias <- c(universos$referencia_soma, universos$referencia_subtrai) |>
    map(separar_campos) |>
    unlist()

  # Devolvendo os campos sem repetição
  unique(c(campos_pesos, campos_referencias))
}

# Criando uma função para conferir os universos na capital inteira
conferir_universos <- function(amostra, codigos, tolerancia = 0.005) {

  # Selecionando os universos pedidos
  universos <- universos_amostra |>
    filter(codigo %in% codigos)

  # Identificando as variáveis sem universo identificado
  sem_universo <- universos$codigo[universos$situacao != "conferido"]

  # Interrompendo se alguma variável não tiver universo identificado
  if (length(sem_universo) > 0) {
    stop("Variáveis sem universo identificado: ", paste(sem_universo, collapse = ", "))
  }

  # Somando os campos de todas as APs
  totais <- amostra |>
    summarise(across(all_of(campos_universos(codigos)), \(x) sum(x, na.rm = TRUE)))

  # Criando uma função interna para somar os campos de uma referência
  somar_referencia <- function(texto) {

    # Somando os totais dos campos (zero para referência ausente)
    sum(unlist(totais[separar_campos(texto)]))
  }

  # Calculando a razão entre o peso respondido e a população de referência
  conferencia <- universos |>
    mutate(
      prefixo    = str_to_lower(codigo),
      # Peso observado mais o ignorado (as numéricas não têm ignorado)
      respondido = map2_dbl(
        prefixo,
        tipo,
        \(p, t) totais[[paste0(p, "_peso_observado")]] +
          if (t == "numerica") 0 else totais[[paste0(p, "_peso_ignorado")]]
      ),
      # Usando o total fixo da capital quando a referência não é um campo agregado
      referencia = if_else(
        is.na(referencia_soma),
        unname(referencias_fixas[codigo]),
        map_dbl(referencia_soma, somar_referencia) - map_dbl(referencia_subtrai, somar_referencia)
      ),
      razao      = respondido / referencia,
      aprovado   = abs(razao - 1) <= tolerancia
    ) |>
    select(codigo, universo, respondido, referencia, razao, aprovado)

  # Interrompendo se algum universo divergir da referência
  if (!all(conferencia$aprovado)) {

    # Separando as variáveis reprovadas
    reprovados <- conferencia |>
      filter(!aprovado)
    stop(
      "Universo divergente da referência: ",
      paste0(reprovados$codigo, " (razão ", round(reprovados$razao, 4), ")", collapse = ", ")
    )
  }

  # Informando o resultado
  message("Universos conferidos: ", nrow(conferencia), " variáveis, razão entre ",
          round(min(conferencia$razao), 4), " e ", round(max(conferencia$razao), 4))

  # Devolvendo a conferência
  conferencia
}
