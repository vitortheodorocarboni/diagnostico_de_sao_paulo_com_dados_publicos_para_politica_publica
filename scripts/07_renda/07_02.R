################################################################################
##################################            ##################################
################### 07.02) PREPARAR OS INDICADORES DA RENDA ####################
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
diretorio_grupo <- caminho_data("07_renda")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("07_renda")

# Definindo o código IBGE do Município de São Paulo
codigo_municipio_sp <- "3550308"

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Definindo o salário mínimo de 2022, referência das classes (R$ por mês)
salario_minimo <- 1212

# Descrevendo as dez classes de salário mínimo per capita e as cinco faixas
classes_renda <- tibble(
  classe_codigo = 1:10,
  classe        = sprintf("%02d", 1:10),
  rotulo_classe = c(
    "Até 1/4", "1/4 a 1/2", "1/2 a 1", "1 a 2", "2 a 3",
    "3 a 4", "4 a 5", "5 a 10", "10 a 20", "Mais de 20"
  ),
  faixa_codigo  = c(1, 1, 2, 3, 4, 4, 4, 5, 5, 5),
  rotulo_faixa  = c(
    "Até 1/2 salário mínimo", "Até 1/2 salário mínimo", "Mais de 1/2 a 1", "Mais de 1 a 2",
    "Mais de 2 a 5", "Mais de 2 a 5", "Mais de 2 a 5",
    "Mais de 5", "Mais de 5", "Mais de 5"
  ),
  campo         = sprintf("d0360_classe_salario_minimo_cat_codigo_%02d_estimativa", 1:10)
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
        .cols = c(
          pessoas_n_registros,
          pessoas_estimativa_total,
          domicilios_n_registros,
          domicilios_estimativa_total,
          d0360_moradores_n_observado,
          d0360_moradores_peso_observado,
          d0360_moradores_soma,
          starts_with("d0360_classe_salario_minimo")
        ),
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
  ~indicador,             ~grupo,      ~rotulo,                                                 ~unidade,                                  ~comparacao, ~casas_decimais, ~descricao,
  "renda_media",          "renda",     "Renda domiciliar per capita média",                     "R$ por mês, por pessoa",                  "relativa",  0L,             "Soma ponderada de D0360 pelo peso das pessoas sobre o peso observado, moradores de domicílios particulares permanentes ocupados, sem pensionistas e empregados domésticos e seus parentes (amostra).",
  "renda_media_sm",       "renda",     "Renda domiciliar per capita média",                     "salários mínimos (R$ 1.212) por pessoa",  "relativa",  2L,             "Renda domiciliar per capita média dividida pelo salário mínimo de 2022 (R$ 1.212).",
  "pct_ate_meio_sm",      "classes",   "Domicílios até 1/2 salário mínimo per capita",          "% dos domicílios",                        "pontos",    1L,             "Estimativa das classes D0360 01 e 02 sobre o peso observado das dez classes (amostra); renda zero na primeira classe.",
  "pct_mais_5_sm",        "classes",   "Domicílios com mais de 5 salários mínimos per capita",  "% dos domicílios",                        "pontos",    1L,             "Estimativa das classes D0360 08, 09 e 10 sobre o peso observado das dez classes (amostra).",
  "domicilios_classes",   "classes",   "Domicílios com renda declarada (estimativa)",           "domicílios",                              "relativa",  0L,             "Peso observado das classes de salário mínimo (domicílios particulares ocupados, amostra).",
  "registros_amostra",    "qualidade", "Registros de pessoas na amostra",                       "registros",                               "relativa",  0L,             "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor.",
  "registros_domicilios", "qualidade", "Registros de domicílios na amostra",                    "registros",                               "relativa",  0L,             "Soma de domicilios_n_registros das APs do distrito (base das classes de salário mínimo)."
) |>
  mutate(
    ordem = row_number()
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

# Calculando os indicadores em formato largo
renda_largo <- totais_amostra |>
  mutate(
    # Média ponderada por pessoa: soma ponderada sobre o peso observado
    renda_media          = d0360_moradores_soma / d0360_moradores_peso_observado,
    renda_media_sm       = renda_media / salario_minimo,
    # Base das classes: peso observado das dez classes (domicílios)
    domicilios_classes   = d0360_classe_salario_minimo_peso_observado,
    pct_ate_meio_sm      = 100 * (d0360_classe_salario_minimo_cat_codigo_01_estimativa +
                                    d0360_classe_salario_minimo_cat_codigo_02_estimativa) / domicilios_classes,
    pct_mais_5_sm        = 100 * (d0360_classe_salario_minimo_cat_codigo_08_estimativa +
                                    d0360_classe_salario_minimo_cat_codigo_09_estimativa +
                                    d0360_classe_salario_minimo_cat_codigo_10_estimativa) / domicilios_classes,
    # Registros e aviso de amostra menor (o município não recebe aviso)
    registros_amostra    = pessoas_n_registros,
    registros_domicilios = domicilios_n_registros,
    nivel                = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor        = nivel == "distrito" & registros_amostra < minimo_registros_amostra
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
renda <- renda_largo |>
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

# Criando a tabela da composição por classe de salário mínimo
composicao <- renda_largo |>
  select(
    nivel_territorial = nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    domicilios_classes,
    all_of(classes_renda$campo)
  ) |>
  pivot_longer(
    cols      = all_of(classes_renda$campo),
    names_to  = "campo",
    values_to = "domicilios"
  ) |>
  left_join(classes_renda, by = "campo") |>
  mutate(
    pct = 100 * domicilios / domicilios_classes
  ) |>
  select(
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    classe_codigo,
    classe,
    rotulo_classe,
    faixa_codigo,
    rotulo_faixa,
    domicilios,
    pct
  ) |>
  arrange(nivel_territorial, codigo_distrito, classe_codigo)


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(renda_largo, nivel == "distrito")
largo_municipio <- filter(renda_largo, nivel == "municipio")

# Calculando a cobertura dos moradores com renda sobre o total de pessoas
cobertura_moradores <- largo_municipio$d0360_moradores_peso_observado / largo_municipio$pessoas_estimativa_total
message("Moradores com renda per capita sobre o total de pessoas: ", round(100 * cobertura_moradores, 2), "%")

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Peso observado das classes igual à soma das dez categorias em cada AP",
    "Sem peso ausente nem ignorado nas classes e na renda dos moradores",
    "Média publicada da AP igual a soma sobre peso observado (moradores)",
    "Classes somam 100% em cada distrito",
    "Peso observado dos moradores não passa do total de pessoas na AP",
    "Peso observado das classes não passa do total de domicílios na AP",
    "Renda média positiva e proporções entre 0 e 100",
    "13 distritos com amostra menor (decisão de 2026-10-10)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(abs(amostra$d0360_classe_salario_minimo_peso_observado - rowSums(amostra[classes_renda$campo])) < 1e-6),
    sum(amostra$d0360_classe_salario_minimo_peso_ausente) == 0 &&
      sum(amostra$d0360_classe_salario_minimo_peso_ignorado) == 0 &&
      sum(amostra$d0360_moradores_peso_ausente) == 0,
    all(abs(amostra$d0360_moradores_soma / amostra$d0360_moradores_peso_observado - amostra$d0360_moradores_media) < 1e-6),
    composicao |>
      group_by(nivel_territorial, codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    all(amostra$d0360_moradores_peso_observado <= amostra$pessoas_estimativa_total + 0.5),
    all(amostra$d0360_classe_salario_minimo_peso_observado <= amostra$domicilios_estimativa_total + 0.5),
    all(renda_largo$renda_media > 0) &&
      renda |>
        filter(indicador %in% c("pct_ate_meio_sm", "pct_mais_5_sm")) |>
        summarise(ok = all(valor >= 0 & valor <= 100)) |>
        pull(ok),
    sum(largo_distritos$amostra_menor) == 13
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
         largo_distritos$registros_amostra[largo_distritos$amostra_menor], ")", collapse = ", ")
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
  x    = renda,
  sink = file.path(diretorio_grupo, "renda.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a composição por classe de salário mínimo
write_parquet(
  x    = composicao,
  sink = file.path(diretorio_grupo, "composicao.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores = renda_largo |>
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
    classes     = composicao |>
      mutate(pct = round(pct, 3)) |>
      select(nivel_territorial, codigo_distrito, nome_distrito, classe_codigo, rotulo_classe, rotulo_faixa, domicilios, pct),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fonte: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), rendimento domiciliar per capita (D0360).",
        "Dados de acesso controlado: uso interno, não publicar sem autorização.",
        "A renda média pesa pessoas (moradores de domicílios particulares permanentes ocupados, sem pensionistas e empregados domésticos e seus parentes); as classes de salário mínimo pesam domicílios particulares ocupados. As duas bases não são diretamente comparáveis.",
        "Valores nominais; salário mínimo de 2022 de R$ 1.212. Renda zero entra na primeira classe (até 1/4 de salário mínimo).",
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_renda.xlsx")
)

# Informando o fim do script
message("Indicadores da renda gravados em ", diretorio_grupo)
