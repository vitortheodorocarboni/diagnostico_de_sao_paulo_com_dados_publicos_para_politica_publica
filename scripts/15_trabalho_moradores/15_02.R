################################################################################
##################################            ##################################
########### 15.02) PREPARAR OS INDICADORES DO TRABALHO DOS MORADORES ###########
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

# Definindo o total de moradores da capital (v0001), para conferir as estimativas
moradores_capital <- 11451999

# Definindo as variáveis da amostra usadas no grupo
codigos_amostra <- c("P0930", "P0950", "P0940", "P1000", "P0990", "P1040", "P1120", "P1170", "P1180", "P1190", "P1080")

# Definindo os valores de referência do município medidos em 2026-10-12
# (etapa 10 do plano)
referencia_desocupacao <- 6.1
referencia_tempo       <- 56.4

# Descrevendo as categorias das três composições, com os códigos da amostra
categorias_composicao <- tribble(
  ~tabela,      ~categoria_codigo, ~rotulo_categoria,                          ~campos,
  "transporte", 1L,                "Coletivo",                                 sprintf("p1170_cat_codigo_%02d_estimativa", 7:13),
  "transporte", 2L,                "Individual motorizado",                    sprintf("p1170_cat_codigo_%02d_estimativa", 3:6),
  "transporte", 3L,                "A pé",                                     "p1170_cat_codigo_01_estimativa",
  "transporte", 4L,                "Bicicleta",                                "p1170_cat_codigo_02_estimativa",
  "transporte", 5L,                "Outros",                                   "p1170_cat_codigo_14_estimativa",
  "posicao",    1L,                "Conta própria",                            "p0990_cat_codigo_08_estimativa",
  "posicao",    2L,                "Empregado do setor privado",               "p0990_cat_codigo_03_estimativa",
  "posicao",    3L,                "Setor público e militares",                sprintf("p0990_cat_codigo_%02d_estimativa", c(2, 4, 5, 6)),
  "posicao",    4L,                "Trabalhador doméstico",                    "p0990_cat_codigo_01_estimativa",
  "posicao",    5L,                "Empregador",                               "p0990_cat_codigo_07_estimativa",
  "posicao",    6L,                "Não remunerado",                           "p0990_cat_codigo_09_estimativa",
  "grupos",     1L,                "Dirigentes e profissionais das ciências",  sprintf("p1040_cat_codigo_%02d_estimativa", 1:2),
  "grupos",     2L,                "Técnicos e apoio administrativo",          sprintf("p1040_cat_codigo_%02d_estimativa", 3:4),
  "grupos",     3L,                "Serviços e comércio",                      "p1040_cat_codigo_05_estimativa",
  "grupos",     4L,                "Operários, artesãos e operadores",         sprintf("p1040_cat_codigo_%02d_estimativa", 6:8),
  "grupos",     5L,                "Ocupações elementares",                    "p1040_cat_codigo_09_estimativa",
  "grupos",     6L,                "Outros",                                   sprintf("p1040_cat_codigo_%02d_estimativa", 10:11)
)

# Definindo a base (peso observado) de cada composição
bases_composicao <- c(
  transporte = "p1170_peso_observado",
  posicao    = "p0990_peso_observado",
  grupos     = "p1040_peso_observado"
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
        .cols = c(where(is.numeric), -p1190_media, -p1080_media),
        .fns  = sum
      ),
      .groups = "drop"
    )
}

# Criando uma função para somar as viagens das zonas de cada grupo; categoria
# vazia numa zona é zero viagem (14 zonas da capital não têm registro casa →
# trabalho)
somar_zonas <- function(zonas_agrupadas) {

  # Somando os registros, as estimativas e os pesos
  zonas_agrupadas |>
    summarise(
      across(
        .cols = where(is.numeric),
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

# Lendo as viagens da OD por zona
viagens <- read_parquet(file.path(diretorio_grupo, "viagens.parquet"))

# Lendo a ligação AP → distrito
ap_distrito <- read_parquet(file.path(diretorio_territorio, "ap_distrito.parquet"))

# Lendo a ligação zona OD → distrito
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
  ~indicador,                     ~grupo,         ~base,     ~rotulo,                                              ~unidade,                                    ~comparacao, ~casas_decimais, ~descricao,
  "pct_mais_1h",                  "deslocamento", "censo",   "Mais de 1 hora até o trabalho",                      "% de quem se desloca ao trabalho",          "pontos",    1L,              "P1180 códigos 5 a 7 (mais de uma hora) sobre os códigos 1 a 7 (ocupados que trabalham fora de casa e se deslocam, com tempo informado).",
  "tempo_medio_deslocamento",     "deslocamento", "censo",   "Tempo médio até o trabalho",                         "minutos",                                   "relativa",  0L,              "Média de P1190 ponderada por pessoa (soma ponderada sobre o peso observado), entre quem se desloca ao trabalho com tempo informado.",
  "pct_desocupacao",              "mercado",      "censo",   "Desocupação",                                        "% da força de trabalho",                    "pontos",    1L,              "P0950 código 0 (desocupada) sobre o peso observado de P0950 (pessoas de 14 anos ou mais na força de trabalho).",
  "pct_informalidade",            "mercado",      "censo",   "Ocupados sem contribuir para a previdência",         "% dos ocupados de 14 anos ou mais",         "pontos",    1L,              "P0940 código 0 sobre o peso observado de P0940.",
  "pct_em_casa",                  "local",        "censo",   "Trabalham em casa",                                  "% dos ocupados",                            "pontos",    1L,              "P1120 código 1 (em casa ou na propriedade) sobre o peso observado de P1120 (ocupados de 10 anos ou mais; o ignorado fica fora).",
  "pct_participacao",             "mercado",      "censo",   "Participação na força de trabalho",                  "% das pessoas de 14 anos ou mais",          "pontos",    1L,              "P0930 código 1 sobre o peso observado de P0930.",
  "pct_sem_carteira",             "mercado",      "censo",   "Empregados sem carteira assinada",                   "% dos empregados",                          "pontos",    1L,              "P1000 código 2 sobre o peso observado de P1000 (empregados, exceto militares e estatutários).",
  "pct_conta_propria",            "posicao",      "censo",   "Conta própria",                                      "% dos ocupados",                            "pontos",    1L,              "P0990 código 08 sobre o peso observado de P0990.",
  "pct_setor_publico",            "posicao",      "censo",   "Setor público e militares",                          "% dos ocupados",                            "pontos",    1L,              "P0990 códigos 02, 04, 05 e 06 sobre o peso observado de P0990.",
  "pct_dirigentes_profissionais", "grupos",       "censo",   "Dirigentes e profissionais das ciências",            "% dos ocupados",                            "pontos",    1L,              "P1040 códigos 01 e 02 sobre o peso observado de P1040.",
  "pct_elementares",              "grupos",       "censo",   "Ocupações elementares",                              "% dos ocupados",                            "pontos",    1L,              "P1040 código 09 sobre o peso observado de P1040.",
  "pct_outro_municipio",          "local",        "censo",   "Trabalham em outro município",                       "% dos ocupados",                            "pontos",    1L,              "P1120 código 3 sobre o peso observado de P1120.",
  "pct_coletivo",                 "transporte",   "censo",   "Transporte coletivo até o trabalho",                 "% de quem se desloca ao trabalho",          "pontos",    1L,              "P1170 códigos 07 a 13 (van, ônibus, BRT, trem ou metrô, caminhão adaptado, embarcações) sobre o peso observado de P1170.",
  "pct_individual",               "transporte",   "censo",   "Transporte individual motorizado até o trabalho",    "% de quem se desloca ao trabalho",          "pontos",    1L,              "P1170 códigos 03 a 06 (motocicleta, mototáxi, automóvel, táxi) sobre o peso observado de P1170.",
  "pct_a_pe",                     "transporte",   "censo",   "A pé até o trabalho",                                "% de quem se desloca ao trabalho",          "pontos",    1L,              "P1170 código 01 sobre o peso observado de P1170.",
  "rendimento_trabalho",          "rendimento",   "censo",   "Rendimento médio do trabalho",                       "R$ por mês",                                "relativa",  0L,              "Média de P1080 ponderada por pessoa (soma ponderada sobre o peso observado), ocupados com rendimento em dinheiro, produtos ou mercadorias; valores nominais de 2022.",
  "od_pct_coletivo",              "od",           "od",      "Viagens casa → trabalho coletivas (OD)",             "% das viagens casa → trabalho",             "pontos",    1L,              "Tipo de viagem coletivo sobre o peso observado do tipo, viagens casa → trabalho por zona de residência (OD 2023, FE_VIA).",
  "od_pct_individual",            "od",           "od",      "Viagens casa → trabalho individuais (OD)",           "% das viagens casa → trabalho",             "pontos",    1L,              "Tipo de viagem individual motorizado, viagens casa → trabalho por zona de residência (OD 2023).",
  "od_pct_a_pe",                  "od",           "od",      "Viagens casa → trabalho a pé (OD)",                  "% das viagens casa → trabalho",             "pontos",    1L,              "Tipo de viagem a pé, viagens casa → trabalho por zona de residência (OD 2023).",
  "registros_amostra",            "qualidade",    "censo",   "Registros de pessoas na amostra",                    "registros",                                 "relativa",  0L,              "Soma de pessoas_n_registros das APs do distrito; abaixo do mínimo, o distrito recebe aviso de amostra menor.",
  "od_registros",                 "qualidade",    "od",      "Registros de viagens casa → trabalho na OD",         "registros",                                 "relativa",  0L,              "Soma de casa_trabalho_residencia_viagens_n_registros das zonas do distrito; abaixo de 100, aviso; abaixo de 30, sem valor nos indicadores da OD."
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

# Ligando as zonas da capital aos distritos
viagens_distritos <- viagens |>
  inner_join(
    zona_distrito |> select(codigo_zona_od, codigo_distrito),
    by = "codigo_zona_od"
  )

# Somando as zonas por distrito e para o município
totais_od <- bind_rows(
  viagens_distritos |>
    group_by(codigo_distrito) |>
    somar_zonas(),
  viagens_distritos |>
    select(-codigo_distrito) |>
    somar_zonas() |>
    mutate(codigo_distrito = codigo_municipio_sp)
)

# Calculando os indicadores em formato largo
trabalho_largo <- totais_amostra |>
  inner_join(totais_od, by = "codigo_distrito") |>
  mutate(
    # --- Deslocamento (base: quem se desloca com tempo informado)
    pct_mais_1h                  = 100 * (p1180_cat_codigo_5_estimativa + p1180_cat_codigo_6_estimativa +
                                            p1180_cat_codigo_7_estimativa) /
                                     (p1180_peso_observado - p1180_cat_codigo_0_estimativa),
    tempo_medio_deslocamento     = p1190_soma / p1190_peso_observado,
    # --- Mercado de trabalho (base: peso observado de cada variável)
    pct_desocupacao              = 100 * p0950_cat_codigo_0_estimativa / p0950_peso_observado,
    pct_informalidade            = 100 * p0940_cat_codigo_0_estimativa / p0940_peso_observado,
    pct_participacao             = 100 * p0930_cat_codigo_1_estimativa / p0930_peso_observado,
    pct_sem_carteira             = 100 * p1000_cat_codigo_2_estimativa / p1000_peso_observado,
    # --- Local de trabalho
    pct_em_casa                  = 100 * p1120_cat_codigo_1_estimativa / p1120_peso_observado,
    pct_outro_municipio          = 100 * p1120_cat_codigo_3_estimativa / p1120_peso_observado,
    # --- Posição na ocupação e grupos ocupacionais
    pct_conta_propria            = 100 * p0990_cat_codigo_08_estimativa / p0990_peso_observado,
    pct_setor_publico            = 100 * (p0990_cat_codigo_02_estimativa + p0990_cat_codigo_04_estimativa +
                                            p0990_cat_codigo_05_estimativa + p0990_cat_codigo_06_estimativa) / p0990_peso_observado,
    pct_dirigentes_profissionais = 100 * (p1040_cat_codigo_01_estimativa + p1040_cat_codigo_02_estimativa) / p1040_peso_observado,
    pct_elementares              = 100 * p1040_cat_codigo_09_estimativa / p1040_peso_observado,
    # --- Meio de transporte
    pct_coletivo                 = 100 * rowSums(across(all_of(sprintf("p1170_cat_codigo_%02d_estimativa", 7:13)))) / p1170_peso_observado,
    pct_individual               = 100 * rowSums(across(all_of(sprintf("p1170_cat_codigo_%02d_estimativa", 3:6)))) / p1170_peso_observado,
    pct_a_pe                     = 100 * p1170_cat_codigo_01_estimativa / p1170_peso_observado,
    # --- Rendimento do trabalho
    rendimento_trabalho          = p1080_soma / p1080_peso_observado,
    # --- OD: tipo das viagens casa → trabalho por residência
    od_registros                 = casa_trabalho_residencia_viagens_n_registros,
    od_pct_coletivo              = 100 * casa_trabalho_residencia_tipvg_cat_codigo_1_estimativa / casa_trabalho_residencia_tipvg_peso_observado,
    od_pct_individual            = 100 * casa_trabalho_residencia_tipvg_cat_codigo_2_estimativa / casa_trabalho_residencia_tipvg_peso_observado,
    od_pct_a_pe                  = 100 * casa_trabalho_residencia_tipvg_cat_codigo_3_estimativa / casa_trabalho_residencia_tipvg_peso_observado,
    # --- Registros, aviso e piso (o município não recebe aviso)
    registros_amostra            = pessoas_n_registros,
    nivel                        = if_else(codigo_distrito == codigo_municipio_sp, "municipio", "distrito"),
    amostra_menor                = nivel == "distrito" & registros_amostra < minimo_registros_amostra,
    od_amostra_menor             = nivel == "distrito" & od_registros < minimo_registros_od,
    od_sem_valor                 = nivel == "distrito" & od_registros < piso_registros_od
  ) |>
  mutate(
    across(
      .cols = c(od_pct_coletivo, od_pct_individual, od_pct_a_pe),
      .fns  = ~ if_else(od_sem_valor, NA_real_, .x)
    )
  ) |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nome_distrito = replace_na(nome_distrito, "Município de São Paulo"),
    nome_rotulo   = replace_na(nome_rotulo, "Município de São Paulo")
  ) |>
  arrange(nivel, codigo_distrito)

# Passando os indicadores para o formato longo
trabalho <- trabalho_largo |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    od_amostra_menor,
    od_sem_valor,
    all_of(indicadores$indicador)
  ) |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  )

# Criando a tabela das três composições, com a proporção de cada categoria
# sobre o peso observado da própria variável
composicoes <- categorias_composicao |>
  mutate(
    # Somando os campos de cada categoria e lendo a base, em cada distrito
    valores = map2(
      campos,
      tabela,
      \(campos_categoria, tabela_atual) tibble(
        codigo_distrito = trabalho_largo$codigo_distrito,
        ocupados        = rowSums(as.matrix(trabalho_largo[campos_categoria])),
        base            = trabalho_largo[[bases_composicao[[tabela_atual]]]]
      )
    )
  ) |>
  select(-campos) |>
  unnest(valores) |>
  inner_join(
    trabalho_largo |>
      select(
        nivel_territorial = nivel,
        codigo_distrito,
        nome_distrito,
        nome_rotulo,
        amostra_menor
      ),
    by = "codigo_distrito"
  ) |>
  mutate(
    pct = 100 * ocupados / base
  ) |>
  select(
    tabela,
    nivel_territorial,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    amostra_menor,
    categoria_codigo,
    rotulo_categoria,
    ocupados,
    pct
  ) |>
  arrange(tabela, nivel_territorial, codigo_distrito, categoria_codigo)


################################################################################
##### VII. Conferências
################################################################################

# Separando distritos e município
largo_distritos <- filter(trabalho_largo, nivel == "distrito")
largo_municipio <- filter(trabalho_largo, nivel == "municipio")

# Criando uma função para conferir se o peso observado de uma variável
# categórica é igual à soma das suas categorias em cada AP
conferir_categorias <- function(prefixo, codigos, formato = "%d") {

  # Comparando o peso observado com a soma das estimativas das categorias
  all(abs(
    amostra[[paste0(prefixo, "_peso_observado")]] -
      rowSums(as.matrix(amostra[paste0(prefixo, "_cat_codigo_", sprintf(formato, codigos), "_estimativa")]))
  ) < 1e-6)
}

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "332 APs na amostra da capital",
    "Todas as APs ligadas a um distrito",
    "Distritos com indicadores do Censo e da OD",
    "Estimativa total de pessoas igual a v0001 da capital (11.451.999)",
    "Universos de aplicação das onze variáveis conferidos",
    "Peso observado igual à soma das categorias em cada AP (P0990, P1040, P1120, P1170, P1180)",
    "Médias publicadas da AP iguais a soma sobre peso observado (P1190 e P1080)",
    "As três composições somam 100% em cada distrito",
    "343 zonas da OD ligadas aos distritos da capital",
    "Tipo das viagens casa → trabalho soma o peso observado em cada zona",
    "Nenhuma proporção fora de 0 a 100",
    "13 distritos com amostra menor no Censo (decisão de 2026-10-10)",
    "Município igual aos valores de referência da etapa 10 (desocupação e tempo médio)"
  ),
  ok          = c(
    nrow(amostra) == 332,
    nrow(amostra_distritos) == nrow(amostra),
    nrow(largo_distritos) == 96,
    abs(sum(largo_distritos$pessoas_estimativa_total) - moradores_capital) < 0.5,
    all(universos$aprovado) && nrow(universos) == length(codigos_amostra),
    conferir_categorias("p0990", 1:9, "%02d") && conferir_categorias("p1040", 1:11, "%02d") &&
      conferir_categorias("p1120", 1:5) && conferir_categorias("p1170", 1:14, "%02d") &&
      conferir_categorias("p1180", 0:7),
    all(abs(amostra$p1190_soma / amostra$p1190_peso_observado - amostra$p1190_media) < 1e-6) &&
      all(abs(amostra$p1080_soma / amostra$p1080_peso_observado - amostra$p1080_media) < 1e-6),
    composicoes |>
      group_by(tabela, codigo_distrito) |>
      summarise(s = sum(pct), .groups = "drop") |>
      pull(s) |>
      (\(x) all(abs(x - 100) < 1e-6))(),
    nrow(viagens_distritos) == 343,
    all(abs(
      coalesce(viagens$casa_trabalho_residencia_tipvg_peso_observado, 0) -
        rowSums(as.matrix(viagens[sprintf("casa_trabalho_residencia_tipvg_cat_codigo_%d_estimativa", 1:4)]), na.rm = TRUE)
    ) < 1e-3),
    trabalho |>
      filter(str_detect(indicador, "pct_"), !is.na(valor)) |>
      summarise(ok = all(valor >= 0 & valor <= 100)) |>
      pull(ok),
    sum(largo_distritos$amostra_menor) == 13,
    abs(largo_municipio$pct_desocupacao - referencia_desocupacao) < 0.05 &&
      abs(largo_municipio$tempo_medio_deslocamento - referencia_tempo) < 0.05
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Mostrando os avisos da OD
message(
  "OD casa → trabalho por residência: ", sum(largo_distritos$od_amostra_menor), " distritos com aviso; sem valor: ",
  paste0(largo_distritos$nome_rotulo[largo_distritos$od_sem_valor], " (", largo_distritos$od_registros[largo_distritos$od_sem_valor], ")", collapse = ", ")
)

# Mostrando a correlação entre o Censo e a OD no transporte coletivo
message(
  "Correlação Censo × OD no transporte coletivo casa → trabalho: ",
  round(cor(largo_distritos$pct_coletivo, largo_distritos$od_pct_coletivo, use = "complete.obs"), 3)
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
  x    = trabalho,
  sink = file.path(diretorio_grupo, "trabalho.parquet")
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
    indicadores = trabalho_largo |>
      select(
        codigo_distrito,
        nome_distrito,
        n_aps,
        amostra_menor,
        od_amostra_menor,
        od_sem_valor,
        all_of(indicadores$indicador)
      ) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                    = codigo_distrito,
        `Distrito`                  = nome_distrito,
        `APs no distrito`           = n_aps,
        `Amostra menor (Censo)`     = amostra_menor,
        `Amostra menor (OD, aviso)` = od_amostra_menor,
        `Sem valor (OD, piso)`      = od_sem_valor
      ),
    composicoes = composicoes |>
      mutate(pct = round(pct, 3)) |>
      select(tabela, nivel_territorial, codigo_distrito, nome_distrito, categoria_codigo, rotulo_categoria, ocupados, pct),
    descricao   = indicadores |> select(-ordem, -casas_decimais, -comparacao),
    notas       = tibble(
      nota = c(
        "Fontes: IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado), P0930 a P1190; e Metrô, Pesquisa Origem e Destino 2023, viagens casa → trabalho por zona de residência.",
        "Dados do Censo de acesso controlado: uso interno, não publicar sem autorização.",
        "Cada indicador do Censo usa o universo de aplicação da própria pergunta (pessoas de 14 anos ou mais, força de trabalho, ocupados, empregados ou quem se desloca), conferido em scripts/_config/universos_amostra.R.",
        "O tempo médio em minutos (P1190) dá 56 minutos no município, acima dos cerca de 52 obtidos pelos pontos médios das classes de P1180.",
        "Censo × OD: o Censo informa o meio de transporte de quem trabalha fora de casa e se desloca, na semana de referência, por pessoa; a OD informa as viagens casa → trabalho de um dia útil, por viagem, pela zona de residência. A OD não tem a duração dessas viagens nos tratados.",
        paste0("OD: distritos com menos de ", minimo_registros_od, " registros no bloco casa → trabalho por residência recebem aviso; com menos de ", piso_registros_od, ", ficam sem valor."),
        "AP → distrito pela maioria de setores; a AP 3550308001, atribuída à Sé, contém 12 setores e 3.222 moradores da República.",
        "Estimativas amostrais sem margem de erro calculada; a precisão local não foi validada.",
        paste0("Distritos com menos de ", minimo_registros_amostra, " registros de pessoas na amostra do Censo recebem aviso de amostra menor; nenhum é suprimido.")
      )
    )
  ),
  path = file.path(diretorio_resultados, "tabela_trabalho_moradores.xlsx")
)

# Informando o fim do script
message("Indicadores do trabalho dos moradores gravados em ", diretorio_grupo)
