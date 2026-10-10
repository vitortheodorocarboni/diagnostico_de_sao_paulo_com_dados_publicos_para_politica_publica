################################################################################
##################################            ##################################
################ 02.02) PREPARAR OS INDICADORES DOS DOMICÍLIOS #################
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

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Definindo o diretório dos extratos e das saídas do grupo
diretorio_grupo <- caminho_data("02_domicilios")

# Definindo o diretório do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do grupo
diretorio_resultados <- caminho_resultados("02_domicilios")


################################################################################
##### III. Funções
################################################################################

# Criando uma função para somar os campos publicados e calcular os indicadores.
# Cada proporção é a soma dos valores publicados do numerador sobre a soma dos
# valores publicados do denominador do próprio bloco, sem imputar as células
# com sigilo (valores ausentes entram como zero na soma, não como estimativa).
calcular_indicadores <- function(setores_agrupados) {

  # Somando os campos de cada setor
  totais <- setores_agrupados |>
    summarise(
      across(
        .cols  = matches("^v0[0-9]+$"),
        .fns   = ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  # Calculando os indicadores em pontos percentuais (0 a 100)
  totais |>
    mutate(
      # --- Ocupação (base: domicílios particulares, v0003, sem sigilo)
      vagos                   = 100 * v0009 / v0003,
      uso_ocasional           = 100 * v0008 / v0003,
      # --- Tipo (base: soma publicada dos seis tipos de domicílio particular
      # permanente ocupado)
      base_tipo               = v00047 + v00048 + v00049 + v00050 + v00051 + v00052,
      casa                    = 100 * v00047 / base_tipo,
      casa_vila               = 100 * v00048 / base_tipo,
      apartamento             = 100 * v00049 / base_tipo,
      comodos                 = 100 * v00050 / base_tipo,
      outros_tipos            = 100 * (v00051 + v00052) / base_tipo,
      # --- Saneamento (base: soma publicada das categorias de cada tema)
      base_agua               = v00111 + v00112 + v00113 + v00114 + v00115 + v00116 + v00117 + v00118,
      agua_rede               = 100 * v00111 / base_agua,
      base_esgoto             = v00309 + v00310 + v00311 + v00312 + v00313 + v00314 + v00315 + v00316,
      esgoto_rede             = 100 * (v00309 + v00310) / base_esgoto,
      sem_esgoto_rede         = 100 - esgoto_rede,
      base_lixo               = v00397 + v00398 + v00399 + v00400 + v00401 + v00402,
      lixo_coletado           = 100 * (v00397 + v00398) / base_lixo,
      base_banheiro           = v00494 + v00495,
      banheiro_exclusivo      = 100 * v00494 / base_banheiro,
      # --- Entorno (base: domicílios com resposta sim ou não; o não declarado
      # fica fora do denominador)
      via_pavimentada         = 100 * v05006 / (v05006 + v05007),
      bueiro                  = 100 * v05009 / (v05009 + v05010),
      iluminacao_publica      = 100 * v05012 / (v05012 + v05013),
      ponto_onibus            = 100 * v05015 / (v05015 + v05016),
      via_ciclo               = 100 * v05018 / (v05018 + v05019),
      calcada                 = 100 * v05021 / (v05021 + v05022),
      obstaculo_calcada       = 100 * v05024 / (v05024 + v05025),
      rampa                   = 100 * v05027 / (v05027 + v05028),
      base_arvores            = v05030 + v05031 + v05032 + v05033,
      sem_arvores             = 100 * v05030 / base_arvores,
      com_arvores             = 100 - sem_arvores,
      arvores_5_ou_mais       = 100 * v05033 / base_arvores,
      # --- Parcela de não declarado em cada característica do entorno
      nd_via_pavimentada      = 100 * v05008 / v05000,
      nd_bueiro               = 100 * v05011 / v05000,
      nd_iluminacao_publica   = 100 * v05014 / v05000,
      nd_ponto_onibus         = 100 * v05017 / v05000,
      nd_via_ciclo            = 100 * v05020 / v05000,
      nd_calcada              = 100 * v05023 / v05000,
      nd_obstaculo_calcada    = 100 * v05026 / v05000,
      nd_rampa                = 100 * v05029 / v05000,
      nd_arvores              = 100 * v05034 / v05000,
      # --- Qualidade: domicílios ocupados com valor publicado em relação ao
      # total de domicílios particulares ocupados (v0007, sem sigilo)
      cobertura_ocupados      = 100 * (v00001 + v00002) / v0007,
      domicilios_entorno      = v05000,
      domicilios_ocupados     = v0007
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo os setores da capital
setores <- read_parquet(file.path(diretorio_grupo, "setores.parquet"))

# Lendo a linha do município
municipio <- read_parquet(file.path(diretorio_grupo, "municipio.parquet"))

# Lendo as ausências dos campos do grupo
ausencias <- read_parquet(file.path(diretorio_grupo, "ausencias.parquet"))

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
  ~indicador,             ~grupo,                ~rotulo,                                    ~unidade,                                       ~descricao,
  "vagos",                "ocupacao_e_tipo",     "Domicílios vagos",                         "% dos domicílios particulares",                "Domicílios particulares permanentes vagos (v0009) sobre os domicílios particulares (v0003).",
  "uso_ocasional",        "ocupacao_e_tipo",     "Domicílios de uso ocasional",              "% dos domicílios particulares",                "Domicílios particulares de uso ocasional (v0008) sobre os domicílios particulares (v0003).",
  "casa",                 "ocupacao_e_tipo",     "Domicílios em casas",                      "% dos domicílios ocupados",                    "Tipo de espécie casa (v00047) sobre a soma publicada dos tipos (v00047 a v00052).",
  "casa_vila",            "ocupacao_e_tipo",     "Domicílios em casas de vila ou condomínio", "% dos domicílios ocupados",                   "Tipo de espécie casa de vila ou em condomínio (v00048) sobre a soma publicada dos tipos.",
  "apartamento",          "ocupacao_e_tipo",     "Domicílios em apartamentos",               "% dos domicílios ocupados",                    "Tipo de espécie apartamento (v00049) sobre a soma publicada dos tipos.",
  "comodos",              "ocupacao_e_tipo",     "Domicílios em cômodos ou cortiços",        "% dos domicílios ocupados",                    "Habitação em casa de cômodos ou cortiço (v00050) sobre a soma publicada dos tipos.",
  "outros_tipos",         "ocupacao_e_tipo",     "Outros tipos de domicílio",                "% dos domicílios ocupados",                    "Habitação indígena (v00051) e estrutura residencial degradada ou inacabada (v00052) sobre a soma publicada dos tipos.",
  "agua_rede",            "saneamento",          "Água da rede geral",                       "% dos domicílios ocupados",                    "Utiliza rede geral de distribuição (v00111) sobre a soma publicada das formas de abastecimento (v00111 a v00118).",
  "esgoto_rede",          "saneamento",          "Esgoto em rede",                           "% dos domicílios ocupados",                    "Rede geral ou pluvial (v00309) e fossa séptica ligada à rede (v00310) sobre a soma publicada das destinações do esgoto (v00309 a v00316).",
  "sem_esgoto_rede",      "saneamento",          "Sem esgoto em rede",                       "% dos domicílios ocupados",                    "Complemento de esgoto em rede: fossa não ligada à rede, fossa rudimentar, vala, rio, outra forma ou sem banheiro.",
  "lixo_coletado",        "saneamento",          "Lixo coletado",                            "% dos domicílios ocupados",                    "Lixo coletado no domicílio (v00397) ou depositado em caçamba (v00398) sobre a soma publicada dos destinos (v00397 a v00402).",
  "banheiro_exclusivo",   "saneamento",          "Banheiro de uso exclusivo",                "% dos domicílios ocupados",                    "Com banheiro de uso exclusivo com chuveiro e vaso sanitário (v00494) sobre v00494 + v00495.",
  "via_pavimentada",      "entorno",             "Via pavimentada na face",                  "% dos domicílios do entorno",                  "Domicílios em face com via pavimentada (v05006) sobre sim e não (v05006 + v05007).",
  "bueiro",               "entorno",             "Bueiro na face",                           "% dos domicílios do entorno",                  "Domicílios em face com bueiro (v05009) sobre sim e não (v05009 + v05010).",
  "iluminacao_publica",   "entorno",             "Iluminação pública na face",               "% dos domicílios do entorno",                  "Domicílios em face com iluminação pública (v05012) sobre sim e não (v05012 + v05013).",
  "ponto_onibus",         "entorno",             "Ponto de ônibus na face",                  "% dos domicílios do entorno",                  "Domicílios em face com ponto de ônibus (v05015) sobre sim e não (v05015 + v05016).",
  "via_ciclo",            "entorno",             "Via sinalizada para bicicleta na face",    "% dos domicílios do entorno",                  "Domicílios em face com via sinalizada para bicicleta (v05018) sobre sim e não (v05018 + v05019).",
  "calcada",              "entorno",             "Calçada na face",                          "% dos domicílios do entorno",                  "Domicílios em face com calçada (v05021) sobre sim e não (v05021 + v05022).",
  "obstaculo_calcada",    "entorno",             "Obstáculo na calçada",                     "% dos domicílios do entorno",                  "Domicílios em face com obstáculo na calçada (v05024) sobre sim e não (v05024 + v05025).",
  "rampa",                "entorno",             "Rampa para cadeirante na face",            "% dos domicílios do entorno",                  "Domicílios em face com rampa para cadeirante (v05027) sobre sim e não (v05027 + v05028).",
  "sem_arvores",          "entorno",             "Face sem árvores",                         "% dos domicílios do entorno",                  "Domicílios em face sem árvores (v05030) sobre as quatro classes de arborização (v05030 a v05033); o saltado fica fora.",
  "com_arvores",          "entorno",             "Face com árvores",                         "% dos domicílios do entorno",                  "Complemento de face sem árvores (uma árvore ou mais).",
  "arvores_5_ou_mais",    "entorno",             "Face com 5 árvores ou mais",               "% dos domicílios do entorno",                  "Domicílios em face com 5 ou mais árvores (v05033) sobre as quatro classes de arborização.",
  "nd_via_pavimentada",   "nao_declarado",       "Via pavimentada: não declarado",           "% dos domicílios do entorno",                  "Não declarado (v05008) sobre os domicílios em setores com entorno (v05000).",
  "nd_bueiro",            "nao_declarado",       "Bueiro: não declarado",                    "% dos domicílios do entorno",                  "Não declarado (v05011) sobre v05000.",
  "nd_iluminacao_publica","nao_declarado",       "Iluminação pública: não declarado",        "% dos domicílios do entorno",                  "Não declarado (v05014) sobre v05000.",
  "nd_ponto_onibus",      "nao_declarado",       "Ponto de ônibus: não declarado",           "% dos domicílios do entorno",                  "Não declarado (v05017) sobre v05000.",
  "nd_via_ciclo",         "nao_declarado",       "Via para bicicleta: não declarado",        "% dos domicílios do entorno",                  "Não declarado (v05020) sobre v05000.",
  "nd_calcada",           "nao_declarado",       "Calçada: não declarado",                   "% dos domicílios do entorno",                  "Não declarado (v05023) sobre v05000.",
  "nd_obstaculo_calcada", "nao_declarado",       "Obstáculo na calçada: não declarado",      "% dos domicílios do entorno",                  "Não declarado (v05026) sobre v05000.",
  "nd_rampa",             "nao_declarado",       "Rampa: não declarado",                     "% dos domicílios do entorno",                  "Não declarado (v05029) sobre v05000.",
  "nd_arvores",           "nao_declarado",       "Arborização: saltado",                     "% dos domicílios do entorno",                  "Saltado (v05034) sobre v05000.",
  "cobertura_ocupados",   "qualidade",           "Cobertura dos domicílios ocupados",        "% dos domicílios particulares ocupados",       "Domicílios com valor publicado (v00001 + v00002) sobre os domicílios particulares ocupados (v0007, sem sigilo).",
  "domicilios_entorno",   "qualidade",           "Domicílios nos setores com entorno",       "domicílios",                                   "Domicílios em setores com entorno pesquisado (v05000), base das proporções do entorno.",
  "domicilios_ocupados",  "qualidade",           "Domicílios particulares ocupados",         "domicílios",                                   "Total de domicílios particulares ocupados (v0007)."
) |>
  mutate(
    # Casas decimais na apresentação e forma de comparar com o município
    casas_decimais = if_else(unidade == "domicílios", 0L, 1L),
    comparacao     = if_else(unidade == "domicílios", "relativa", "pontos"),
    ordem          = row_number()
  )


################################################################################
##### VI. Indicadores por distrito e do município
################################################################################

# Ligando os setores aos distritos
setores_distritos <- setores |>
  inner_join(setor_distrito, by = "codigo_setor")

# Calculando os indicadores por distrito
indicadores_distritos <- setores_distritos |>
  group_by(codigo_distrito) |>
  calcular_indicadores() |>
  left_join(distritos, by = "codigo_distrito") |>
  mutate(
    nivel = "distrito"
  )

# Calculando os indicadores do município pela soma dos setores
indicadores_municipio <- setores_distritos |>
  calcular_indicadores() |>
  mutate(
    codigo_distrito = municipio$codigo_municipio,
    nome_distrito   = "Município de São Paulo",
    nome_rotulo     = "Município de São Paulo",
    nivel           = "municipio"
  )

# Juntando distritos e município em formato largo
dominios_largo <- bind_rows(
  indicadores_distritos,
  indicadores_municipio
) |>
  select(
    nivel,
    codigo_distrito,
    nome_distrito,
    nome_rotulo,
    all_of(indicadores$indicador)
  ) |>
  arrange(nivel, codigo_distrito)

# Passando para o formato longo
dominios <- dominios_largo |>
  pivot_longer(
    cols      = all_of(indicadores$indicador),
    names_to  = "indicador",
    values_to = "valor"
  )

# Contando as células ausentes (sigilo) por distrito e campo, para a tabela
# de revisão
ausencias_distrito <- ausencias |>
  inner_join(setor_distrito, by = "codigo_setor") |>
  count(codigo_distrito, name = "celulas_com_sigilo")


################################################################################
##### VII. Conferências
################################################################################

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

# Montando as conferências
conferencias <- tibble(
  conferencia = c(
    "Distritos com indicadores",
    "Setores ligados",
    "Soma dos setores igual à tabela municipal em todos os campos comuns",
    "Soma publicada dos tipos de domicílio até 0,5% abaixo de v00001 (sigilo)",
    "Domicílios ocupados somados nos distritos iguais ao município (v0007)",
    "Tipos de domicílio somam 100% em cada distrito",
    "Nenhum indicador de proporção fora de 0 a 100",
    "Células ausentes são só do símbolo X (sigilo)"
  ),
  ok          = c(
    nrow(indicadores_distritos) == 96,
    nrow(setores_distritos) == nrow(setores),
    all(abs(unlist(soma_setores[campos_comuns]) - unlist(municipio[campos_comuns])) < 1e-6),
    abs(indicadores_municipio$base_tipo / sum(setores$v00001, na.rm = TRUE) - 1) < 0.005,
    sum(indicadores_distritos$domicilios_ocupados) == indicadores_municipio$domicilios_ocupados,
    all(abs(
      indicadores_distritos$casa + indicadores_distritos$casa_vila + indicadores_distritos$apartamento +
        indicadores_distritos$comodos + indicadores_distritos$outros_tipos - 100
    ) < 1e-6),
    dominios |>
      filter(!(indicador %in% c("domicilios_entorno", "domicilios_ocupados"))) |>
      summarise(ok = all(valor >= 0 & valor <= 100, na.rm = TRUE)) |>
      pull(ok),
    all(ausencias$ausencia_original == "X")
  )
)

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia),
  collapse = "\n"
))

# Mostrando a cobertura do município
message(
  "Cobertura dos domicílios ocupados no município: ",
  round(indicadores_municipio$cobertura_ocupados, 2), "%; mínima entre os distritos: ",
  round(min(indicadores_distritos$cobertura_ocupados), 2), "%"
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
  x    = dominios,
  sink = file.path(diretorio_grupo, "dominios.parquet")
)

# Gravando a descrição dos indicadores
write_parquet(
  x    = indicadores,
  sink = file.path(diretorio_grupo, "indicadores.parquet")
)

# Gravando a tabela para leitura, com os rótulos dos indicadores nas colunas
write_xlsx(
  x    = list(
    indicadores  = dominios_largo |>
      left_join(ausencias_distrito, by = "codigo_distrito") |>
      mutate(celulas_com_sigilo = if_else(nivel == "distrito", replace_na(celulas_com_sigilo, 0L), NA_integer_)) |>
      select(-nivel, -nome_rotulo) |>
      rename_with(
        .fn   = ~ paste0(indicadores$rotulo, " (", indicadores$unidade, ")")[match(.x, indicadores$indicador)],
        .cols = all_of(indicadores$indicador)
      ) |>
      rename(
        `Código`                    = codigo_distrito,
        `Distrito`                  = nome_distrito,
        `Células com sigilo (setores do distrito)` = celulas_com_sigilo
      ),
    descricao    = indicadores |> select(-ordem, -casas_decimais, -comparacao)
  ),
  path = file.path(diretorio_resultados, "tabela_domicilios.xlsx")
)

# Informando o fim do script
message("Indicadores dos domicílios gravados em ", diretorio_grupo)
