################################################################################
##################################            ##################################
############ 00.03) CONFERIR E VISUALIZAR AS LIGAÇÕES TERRITORIAIS #############
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("colorspace")
# install.packages("geoarrow")
# install.packages("ggrepel")
# install.packages("sf")
# install.packages("tidyverse")
# install.packages("writexl")

# Carregando bibliotecas
library(arrow)
library(colorspace)
library(geoarrow)
library(ggrepel)
library(sf)
library(tidyverse)
library(writexl)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Carregando a identidade visual
source(file.path("scripts", "_config", "identidade_visual.R"), encoding = "UTF-8")

# Definindo o diretório dos dados do território
diretorio_territorio <- caminho_data("00_territorio")

# Definindo o diretório de resultados do território
diretorio_resultados <- caminho_resultados("00_territorio")

# Definindo o CRS projetado dos mapas
epsg_projetado <- 31983


################################################################################
##### III. Funções
################################################################################

# Criando uma função para colorir os distritos de modo que vizinhos tenham
# cores diferentes (coloração gulosa, dos distritos com mais vizinhos aos com
# menos)
colorir_vizinhos <- function(poligonos, n_cores) {

  # Listando os vizinhos de cada polígono, sem ele mesmo
  vizinhos <- st_intersects(poligonos) |>
    imap(function(x, i) setdiff(x, i))

  # Ordenando os polígonos pelo número de vizinhos
  ordem <- order(lengths(vizinhos), decreasing = TRUE)

  # Criando o vetor de cores
  cores <- integer(length(vizinhos))

  # Atribuindo a cada polígono a primeira cor livre entre os vizinhos
  for (i in ordem) {

    # Listando as cores livres
    livres <- setdiff(seq_len(n_cores), cores[vizinhos[[i]]])

    # Interrompendo se faltar cor
    if (length(livres) == 0) {
      stop("A coloração precisou de mais de ", n_cores, " cores.")
    }

    # Usando a primeira cor livre
    cores[i] <- min(livres)
  }

  # Devolvendo as cores
  cores
}

# Criando uma função para obter um ponto de rótulo dentro de cada polígono
pontos_rotulo <- function(poligonos) {

  # Calculando um ponto sobre a superfície de cada polígono
  pontos <- st_point_on_surface(st_set_agr(poligonos, "constant"))

  # Devolvendo os atributos com as coordenadas
  pontos |>
    st_drop_geometry() |>
    bind_cols(as_tibble(st_coordinates(pontos)))
}


################################################################################
##### IV. Dados
################################################################################

# Lendo os setores da capital
setores <- read_parquet(file.path(diretorio_territorio, "setores.parquet"))

# Lendo os distritos com geometria
distritos <- ler_geoparquet(file.path(diretorio_territorio, "distritos.parquet"))

# Lendo a ligação setor → distrito
setor_distrito <- read_parquet(file.path(diretorio_territorio, "setor_distrito.parquet"))

# Lendo a ligação AP → distrito
ap_distrito <- read_parquet(file.path(diretorio_territorio, "ap_distrito.parquet"))

# Lendo a lista completa de distritos de cada AP
ap_distrito_relacoes <- read_parquet(file.path(diretorio_territorio, "ap_distrito_relacoes.parquet"))

# Lendo a geometria das APs
ap_geometria <- ler_geoparquet(file.path(diretorio_territorio, "ap_geometria.parquet"))

# Lendo a correspondência entre distritos da OD e do IBGE
od_distrito_ibge <- read_parquet(file.path(diretorio_territorio, "od_distrito_ibge.parquet"))

# Lendo a ligação zona → distrito
zona_distrito <- read_parquet(file.path(diretorio_territorio, "zona_distrito.parquet"))

# Lendo a geometria das zonas da OD
od_zonas_geometria <- ler_geoparquet(file.path(diretorio_territorio, "od_zonas_geometria.parquet"))

# Lendo o comparador municipal
comparador_municipio <- read_parquet(file.path(diretorio_territorio, "comparador_municipio.parquet"))


################################################################################
##### V. Sobreposição das zonas com o distrito atribuído
################################################################################

# Projetando os distritos no CRS da OD
distritos_projetados <- distritos |>
  st_transform(epsg_projetado) |>
  st_make_valid()

# Juntando cada zona ao distrito atribuído
zonas_projetadas <- od_zonas_geometria |>
  st_make_valid() |>
  inner_join(zona_distrito, by = "codigo_zona_od") |>
  mutate(
    area_zona_m2 = as.numeric(st_area(geometry))
  )

# Medindo a área de cada zona dentro do distrito atribuído
zonas_no_distrito <- st_intersection(
  x = zonas_projetadas |> select(codigo_zona_od, codigo_distrito) |> st_set_agr("constant"),
  y = distritos_projetados |> select(codigo_distrito_geometria = codigo_distrito) |> st_set_agr("constant")
) |>
  # Mantendo só a parte dentro do próprio distrito atribuído
  filter(codigo_distrito == codigo_distrito_geometria) |>
  mutate(
    area_no_distrito_m2 = as.numeric(st_area(geometry))
  ) |>
  st_drop_geometry() |>
  as_tibble() |>
  group_by(codigo_zona_od) |>
  summarise(
    area_no_distrito_m2 = sum(area_no_distrito_m2),
    .groups             = "drop"
  )

# Calculando a fração da área de cada zona no distrito atribuído
zonas_fracao <- zonas_projetadas |>
  st_drop_geometry() |>
  as_tibble() |>
  left_join(zonas_no_distrito, by = "codigo_zona_od") |>
  mutate(
    area_no_distrito_m2 = replace_na(area_no_distrito_m2, 0),
    fracao_no_distrito  = area_no_distrito_m2 / area_zona_m2
  ) |>
  select(
    codigo_zona_od,
    nome_zona,
    codigo_distrito,
    nome_distrito,
    fracao_no_distrito
  )


################################################################################
##### VI. Conferências
################################################################################

# Definindo a atribuição esperada das cinco APs mistas
aps_mistas_esperadas <- tribble(
  ~codigo_area_ponderacao, ~nome_esperado,
  "3550308001",            "Sé",
  "3550308104",            "Butantã",
  "3550308108",            "Jaguara",
  "3550308134",            "Tremembé",
  "3550308217",            "Jardim São Luís"
)

# Comparando as APs mistas obtidas com as esperadas
aps_mistas_comparacao <- ap_distrito |>
  filter(ap_mista) |>
  full_join(aps_mistas_esperadas, by = "codigo_area_ponderacao")

# Contando APs e zonas por distrito
unidades_por_distrito <- distritos |>
  st_drop_geometry() |>
  as_tibble() |>
  select(codigo_distrito, nome_distrito) |>
  left_join(count(ap_distrito, codigo_distrito, name = "n_aps"), by = "codigo_distrito") |>
  left_join(count(zona_distrito, codigo_distrito, name = "n_zonas"), by = "codigo_distrito") |>
  mutate(
    n_aps   = replace_na(n_aps, 0L),
    n_zonas = replace_na(n_zonas, 0L)
  )

# Montando a tabela de conferências
conferencias <- tribble(
  ~conferencia,                                         ~esperado,                     ~obtido,
  "Distritos na geometria",                             "96",                          as.character(nrow(distritos)),
  "Distritos nos setores iguais aos da geometria",      "sim",                         if_else(setequal(setores$codigo_distrito, distritos$codigo_distrito), "sim", "não"),
  "Setores ligados a um distrito",                      "27301",                       as.character(sum(setor_distrito$codigo_distrito %in% distritos$codigo_distrito)),
  "APs com distrito atribuído",                         "332",                         as.character(sum(!is.na(ap_distrito$codigo_distrito))),
  "Setores somados nas APs",                            "27301",                       as.character(sum(ap_distrito_relacoes$n_setores)),
  "APs mistas atribuídas como na referência",           "5",                           as.character(sum(aps_mistas_comparacao$nome_distrito == aps_mistas_comparacao$nome_esperado, na.rm = TRUE)),
  "APs mistas encontradas",                             "5",                           as.character(sum(ap_distrito$ap_mista)),
  "Zonas da OD com distrito",                           "343",                         as.character(sum(!is.na(zona_distrito$codigo_distrito))),
  "Distritos da OD ligados",                            "96",                          as.character(nrow(od_distrito_ibge)),
  "Distritos do IBGE distintos na ligação OD (1:1)",    "96",                          as.character(n_distinct(od_distrito_ibge$codigo_distrito)),
  "Sobreposição mínima sobre a área da OD (> 99%)",     "sim",                         if_else(min(od_distrito_ibge$fracao_area_od) > 0.99, "sim", "não"),
  "Sobreposição mínima sobre a área do IBGE (> 99%)",   "sim",                         if_else(min(od_distrito_ibge$fracao_area_ibge) > 0.99, "sim", "não"),
  "Moradores (v0001) somados nos distritos",            "11451999",                    format(sum(setores$v0001, na.rm = TRUE), scientific = FALSE),
  "Moradores iguais à tabela municipal",                "sim",                         if_else(sum(setores$v0001, na.rm = TRUE) == comparador_municipio$v0001, "sim", "não"),
  "Domicílios (v0002) somados nos distritos",           "4996529",                     format(sum(setores$v0002, na.rm = TRUE), scientific = FALSE),
  "Domicílios iguais à tabela municipal",               "sim",                         if_else(sum(setores$v0002, na.rm = TRUE) == comparador_municipio$v0002, "sim", "não"),
  "Área somada nos distritos (km²)",                    "1521.2",                      format(round(sum(distritos$area_km2), 1), nsmall = 1),
  "Distritos com ao menos uma AP",                      "96",                          as.character(sum(unidades_por_distrito$n_aps > 0)),
  "Distritos com ao menos uma zona da OD",              "96",                          as.character(sum(unidades_por_distrito$n_zonas > 0))
) |>
  mutate(
    ok = esperado == obtido
  )

# Mostrando as conferências
message(paste(
  sprintf("[%s] %s: %s", if_else(conferencias$ok, "ok", "FALHA"), conferencias$conferencia, conferencias$obtido),
  collapse = "\n"
))

# Mostrando a sobreposição das zonas com o distrito atribuído (informativa)
message(
  "Zonas com menos de 95% da área no distrito atribuído: ",
  sum(zonas_fracao$fracao_no_distrito < 0.95),
  " (mínimo: ", formatar_numero(100 * min(zonas_fracao$fracao_no_distrito), 1), "%)"
)


################################################################################
##### VII. Mapas
################################################################################

# Colorindo os distritos para que vizinhos tenham cores diferentes
distritos_cores <- distritos_projetados |>
  mutate(
    cor_distrito = colorir_vizinhos(distritos_projetados, n_cores = length(paleta_insper))
  ) |>
  st_drop_geometry() |>
  as_tibble() |>
  select(codigo_distrito, cor_distrito)

# Definindo as cores claras de preenchimento, uma por classe de vizinhança
cores_preenchimento <- setNames(
  lighten(unname(paleta_insper), amount = 0.45, space = "HCL"),
  seq_along(paleta_insper)
)

# Criando os pontos de rótulo dos distritos
rotulos_distritos <- pontos_rotulo(distritos_projetados |> select(nome_rotulo))

# Desenhando o mapa dos distritos com rótulos
mapa_distritos <- ggplot() +
  geom_sf(
    data      = distritos_projetados,
    fill      = "#ECECEC",
    colour    = cor_fundo,
    linewidth = 0.3
  ) +
  geom_text_repel(
    data               = rotulos_distritos,
    mapping            = aes(x = X, y = Y, label = nome_rotulo),
    family             = familia_fonte,
    size               = 1.7,
    colour             = cor_texto,
    segment.size       = 0.15,
    segment.color      = cor_texto_nota,
    min.segment.length = 0.3,
    max.overlaps       = Inf,
    seed               = 1
  ) +
  elementos_mapa() +
  labs(
    title    = "Os 96 distritos do Município de São Paulo",
    subtitle = "Limites do IBGE, Censo Demográfico 2022",
    caption  = legenda_fonte("censo_universo", largura = 95)
  ) +
  tema_mapa()

# Preparando as APs com o distrito atribuído e a cor do distrito
aps_mapa <- ap_geometria |>
  st_transform(epsg_projetado) |>
  inner_join(ap_distrito, by = "codigo_area_ponderacao") |>
  left_join(distritos_cores, by = "codigo_distrito")

# Criando os rótulos das APs mistas
rotulos_aps <- pontos_rotulo(
  aps_mapa |>
    filter(ap_mista) |>
    mutate(rotulo = paste0(codigo_area_ponderacao, "\n", nome_distrito, " + ", str_remove(outros_distritos, " \\(.*"))) |>
    select(rotulo)
)

# Desenhando o mapa das APs coloridas pelo distrito atribuído
mapa_aps <- ggplot() +
  geom_sf(
    data      = aps_mapa,
    mapping   = aes(fill = factor(cor_distrito)),
    colour    = cor_fundo,
    linewidth = 0.1
  ) +
  geom_sf(
    data      = distritos_projetados,
    fill      = NA,
    colour    = cor_texto_apoio,
    linewidth = 0.3
  ) +
  geom_sf(
    data      = aps_mapa |> filter(ap_mista),
    fill      = NA,
    colour    = cor_texto,
    linewidth = 0.9
  ) +
  geom_label_repel(
    data          = rotulos_aps,
    mapping       = aes(x = X, y = Y, label = rotulo),
    family        = familia_fonte,
    size          = 2.2,
    fontface      = "bold",
    colour        = cor_texto,
    fill          = alpha(cor_fundo, 0.9),
    label.size    = 0,
    segment.size  = 0.3,
    box.padding   = 1.2,
    max.overlaps  = Inf,
    seed          = 1
  ) +
  scale_fill_manual(values = cores_preenchimento, guide = "none") +
  elementos_mapa() +
  labs(
    title    = "Áreas de ponderação pelo distrito atribuído",
    subtitle = paste0(
      "Cada AP tem a cor do distrito com a maioria dos seus setores.\n",
      "Linhas cinza: limites dos distritos. Contorno preto: APs em dois distritos."
    ),
    caption  = legenda_fonte(c("censo_universo", "censo_amostra"), notas = "se_republica", largura = 95)
  ) +
  tema_mapa()

# Preparando as zonas com a cor do distrito atribuído
zonas_mapa <- zonas_projetadas |>
  left_join(distritos_cores, by = "codigo_distrito")

# Desenhando o mapa das zonas coloridas pelo distrito atribuído
mapa_zonas <- ggplot() +
  geom_sf(
    data      = zonas_mapa,
    mapping   = aes(fill = factor(cor_distrito)),
    colour    = cor_fundo,
    linewidth = 0.1
  ) +
  geom_sf(
    data      = distritos_projetados,
    fill      = NA,
    colour    = cor_texto_apoio,
    linewidth = 0.3
  ) +
  scale_fill_manual(values = cores_preenchimento, guide = "none") +
  elementos_mapa() +
  labs(
    title    = "Zonas da OD pelo distrito atribuído",
    subtitle = paste0(
      "Cada zona tem a cor do seu distrito oficial na OD, traduzido para o IBGE.\n",
      "Linhas cinza: limites dos distritos do IBGE."
    ),
    caption  = legenda_fonte(c("od", "censo_universo"), largura = 95)
  ) +
  tema_mapa()


################################################################################
##### VIII. Exportação
################################################################################

# Criando o diretório de resultados
dir.create(
  path         = diretorio_resultados,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando as tabelas de conferência e das ligações para revisão
write_xlsx(
  x    = list(
    conferencias          = conferencias,
    aps_mistas            = ap_distrito |> filter(ap_mista),
    od_ibge               = od_distrito_ibge,
    unidades_por_distrito = unidades_por_distrito,
    zonas_fracao          = zonas_fracao |> arrange(fracao_no_distrito)
  ),
  path = file.path(diretorio_resultados, "ligacoes_territoriais.xlsx")
)

# Gravando o mapa dos distritos
gravar_figura(
  grafico = mapa_distritos,
  arquivo = file.path(diretorio_resultados, "mapa_distritos.png"),
  tamanho = "mapa"
)

# Gravando o mapa das APs
gravar_figura(
  grafico = mapa_aps,
  arquivo = file.path(diretorio_resultados, "mapa_aps_distritos.png"),
  tamanho = "mapa"
)

# Gravando o mapa das zonas
gravar_figura(
  grafico = mapa_zonas,
  arquivo = file.path(diretorio_resultados, "mapa_zonas_distritos.png"),
  tamanho = "mapa"
)

# Interrompendo se alguma conferência obrigatória falhar
if (!all(conferencias$ok)) {
  stop("Conferências com falha: ", paste(conferencias$conferencia[!conferencias$ok], collapse = "; "))
}

# Informando o fim do script
message("Conferências do território aprovadas; saídas em ", diretorio_resultados)
