################################################################################
##################################            ##################################
########################### IDENTIDADE VISUAL INSPER ###########################
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("colorspace")
# install.packages("ggspatial")
# install.packages("ragg")
# install.packages("scales")
# install.packages("stringi")
# install.packages("systemfonts")
# install.packages("tidyverse")

# Carregando bibliotecas (scales é chamado por scales::, porque carregá-lo
# mascararia purrr::discard() nos scripts que usam este arquivo)
library(colorspace)
library(ggspatial)
library(ragg)
library(stringi)
library(systemfonts)
library(tidyverse)


################################################################################
##### II. Caminhos
################################################################################

# Carregando os caminhos do projeto, se ainda não estiverem carregados
if (!exists("caminho_projeto", mode = "function")) {
  source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")
}


################################################################################
##### III. Cores
################################################################################

# Definindo as cores do Manual de Marca e da Educação Executiva do Insper
cores_insper <- c(
  vermelho     = "#E50505",
  branco       = "#FFFFFF",
  preto        = "#000000",
  azul         = "#0E171D",
  cinza_claro  = "#808080",
  cinza_medio  = "#5B5B5B",
  cinza_escuro = "#3F3F3F",
  turquesa     = "#3ACC9F",
  verde        = "#92D053",
  amarelo      = "#FFCC00",
  laranja      = "#F89D49",
  rosa         = "#F47DCD",
  roxo         = "#730D9F"
)

# Definindo as cores de texto, grade e fundo
cor_texto       <- cores_insper[["preto"]]
cor_texto_apoio <- cores_insper[["cinza_escuro"]]
cor_texto_nota  <- cores_insper[["cinza_medio"]]
cor_grade       <- "#E3E3E3"
cor_fundo       <- cores_insper[["branco"]]

# Definindo a cor neutra para "outros", comparador e elementos de apoio
cor_neutra <- cores_insper[["cinza_claro"]]

# Criando a paleta categórica em ordem fixa, com variações tonais das cores do
# manual. As cores claras foram escurecidas (e o roxo, clareado) até passar na
# conferência de daltonismo e contraste sobre fundo branco. O verde ficou de
# fora porque se confunde com o laranja para daltônicos.
paleta_insper <- c(
  vermelho = cores_insper[["vermelho"]],
  turquesa = darken(cores_insper[["turquesa"]], amount = 0.35, space = "HCL"),
  roxo     = lighten(cores_insper[["roxo"]], amount = 0.10, space = "HCL"),
  amarelo  = darken(cores_insper[["amarelo"]], amount = 0.30, space = "HCL"),
  rosa     = darken(cores_insper[["rosa"]], amount = 0.25, space = "HCL"),
  laranja  = darken(cores_insper[["laranja"]], amount = 0.25, space = "HCL")
)

# Definindo o centro neutro das escalas divergentes
cor_centro_divergente <- "#F2F2F2"


################################################################################
##### IV. Fonte
################################################################################

# Definindo os arquivos da Inter copiados para fontes/
arquivos_inter <- c(
  normal  = caminho_projeto("fontes", "inter_normal_400.ttf"),
  negrito = caminho_projeto("fontes", "inter_normal_700.ttf"),
  italico = caminho_projeto("fontes", "inter_italic_400.ttf")
)

# Registrando a Inter, ou usando a Verdana se os arquivos faltarem
if (all(file.exists(arquivos_inter))) {

  # Definindo a família usada nos gráficos
  familia_fonte <- "Inter"

  # Registrando a família só se ela ainda não estiver disponível
  if (!familia_fonte %in% c(registry_fonts()$family, system_fonts()$family)) {
    register_font(
      name       = familia_fonte,
      plain      = arquivos_inter[["normal"]],
      bold       = arquivos_inter[["negrito"]],
      italic     = arquivos_inter[["italico"]],
      bolditalic = arquivos_inter[["negrito"]]
    )
  }
} else {

  # Avisando que a fonte de sistema será usada
  warning("Arquivos da Inter ausentes em fontes/; usando Verdana.")

  # Definindo a fonte de sistema prevista no manual
  familia_fonte <- "Verdana"
}


################################################################################
##### V. Textos padrão das figuras
################################################################################

# Definindo a descrição de cada fonte de dados para os rodapés
fontes_dados <- c(
  censo_universo  = "IBGE, Censo Demográfico 2022, resultados do universo por setor censitário",
  censo_municipio = "IBGE, Censo Demográfico 2022, resultados do universo por município",
  censo_amostra   = "IBGE, Censo Demográfico 2022, amostra por área de ponderação (acesso controlado)",
  od              = "Metrô, Pesquisa Origem e Destino 2023",
  geosampa        = "Prefeitura de São Paulo, GeoSampa"
)

# Definindo as notas padrão das saídas
notas_padrao <- c(
  sigilo_universo   = paste(
    "Os totais por distrito somam só os valores publicados por setor; as células",
    "com sigilo do IBGE ficam fora, sem imputação, e por isso os totais podem ficar",
    "abaixo do número real de domicílios (na capital, a soma dos setores coincide com a",
    "tabela municipal do IBGE)."
  ),
  sigilo_pessoas    = paste(
    "Os totais por distrito somam só os valores publicados por setor; as células",
    "com sigilo do IBGE ficam fora, sem imputação, e por isso os totais podem ficar",
    "abaixo do número real de moradores (na capital, a soma dos setores coincide com a",
    "tabela municipal do IBGE)."
  ),
  se_republica      = paste(
    "A área de ponderação 3550308001 foi atribuída à Sé, mas contém 12 setores",
    "e 3.222 moradores da República."
  ),
  incerteza_amostra = paste(
    "Estimativas amostrais sem margem de erro calculada; a precisão local não",
    "foi validada."
  ),
  od_destino        = paste(
    "Recortes por destino ou local de trabalho não são estratos do desenho",
    "amostral da OD."
  ),
  acesso_controlado = "Dados de acesso controlado: uso interno, não publicar sem autorização.",
  amostra_menor     = paste0(
    "Asterisco (*) ou contorno tracejado: distrito com menos de ",
    format(minimo_registros_amostra, big.mark = ".", decimal.mark = ","),
    " registros de pessoas na amostra; ler com cautela."
  ),
  amostra_menor_od  = paste0(
    "Asterisco (*) ou contorno tracejado: distrito com menos de ",
    minimo_registros_od, " registros de trabalhadores na OD; ler com cautela. ",
    "Sem valor (cinza): menos de ", piso_registros_od, " registros."
  ),
  amostra_menor_obitos = paste0(
    "Asterisco (*) ou contorno tracejado: distrito com menos de ",
    minimo_registros_obitos, " registros de falecidos na amostra; ler com cautela. ",
    "Sem valor (cinza): menos de ", piso_registros_obitos, " registros."
  ),
  amostra_menor_unipessoais = paste0(
    "Asterisco (*) ou contorno tracejado: distrito com menos de ",
    minimo_registros_unipessoais, " registros de domicílios unipessoais na amostra; ler com cautela. ",
    "Sem valor (cinza): menos de ", piso_registros_unipessoais, " registros."
  ),
  perfil_unipessoais = paste(
    "Domicílio unipessoal: domicílio particular ocupado com um morador, que é a pessoa responsável;",
    "o perfil é o de quem mora só (amostra, estimado a partir dos registros dos domicílios unipessoais)."
  ),
  captacao_obitos   = paste(
    "Óbitos de moradores de janeiro de 2019 a julho de 2022 informados pelos",
    "domicílios no Censo: domicílios desfeitos não informam e a captação cai com a",
    "antiguidade do óbito, por isso os totais ficam abaixo dos registros civis e não",
    "medem o excesso de mortes da pandemia."
  )
)

# Definindo o aviso dos distritos com amostra menor nos gráficos por distrito
aviso_amostra_menor <- paste0(
  "Amostra menor (menos de ", format(minimo_registros_amostra, big.mark = ".", decimal.mark = ","),
  " registros de pessoas): ler com cautela"
)

# Definindo o aviso dos distritos com amostra menor da OD nos gráficos por
# distrito
aviso_amostra_menor_od <- paste0(
  "Amostra menor (menos de ", minimo_registros_od,
  " registros de trabalhadores na OD): ler com cautela"
)

# Definindo o aviso dos distritos com poucos registros de falecidos nos
# gráficos por distrito
aviso_amostra_menor_obitos <- paste0(
  "Amostra menor (menos de ", minimo_registros_obitos,
  " registros de falecidos): ler com cautela"
)

# Definindo o aviso dos distritos com poucos registros de domicílios unipessoais
# nos gráficos por distrito
aviso_amostra_menor_unipessoais <- paste0(
  "Amostra menor (menos de ", minimo_registros_unipessoais,
  " registros de domicílios unipessoais): ler com cautela"
)

# Definindo os tamanhos nomeados das figuras, em centímetros
tamanhos_figura <- list(
  grafico = c(largura = 24, altura = 13.5),
  mapa    = c(largura = 20, altura = 22)
)


################################################################################
##### VI. Funções
################################################################################

# Criando uma função para obter n cores categóricas, com a principal primeiro
paleta_categorica <- function(n, principal = "vermelho") {

  # Ordenando a paleta com a cor principal na frente
  paleta <- paleta_insper[c(principal, setdiff(names(paleta_insper), principal))]

  # Interrompendo se houver mais categorias que cores
  if (n > length(paleta)) {
    stop(
      "A paleta categórica tem ", length(paleta), " cores; agrupe as demais ",
      "categorias em 'Outros' (cor_neutra) ou use painéis."
    )
  }

  # Devolvendo as n primeiras cores
  unname(paleta[seq_len(n)])
}

# Criando uma função para obter uma escala sequencial de uma só cor, do claro
# ao escuro
paleta_sequencial <- function(n, cor = "vermelho") {

  # Obtendo o código hexadecimal da cor
  hex <- c(paleta_insper, cores_insper)[[cor]]

  # Interpolando do tom claro ao escuro em espaço Lab
  colorRampPalette(
    colors = c(lighten(hex, amount = 0.9), hex, darken(hex, amount = 0.5)),
    space  = "Lab"
  )(n)
}

# Criando uma função para obter uma escala divergente com centro cinza
paleta_divergente <- function(n, baixo = "turquesa", alto = "vermelho") {

  # Obtendo os códigos hexadecimais dos dois polos
  hex_baixo <- c(paleta_insper, cores_insper)[[baixo]]
  hex_alto  <- c(paleta_insper, cores_insper)[[alto]]

  # Interpolando de um polo ao outro, passando pelo centro neutro
  colorRampPalette(
    colors = c(
      darken(hex_baixo, amount = 0.3),
      hex_baixo,
      cor_centro_divergente,
      hex_alto,
      darken(hex_alto, amount = 0.3)
    ),
    space  = "Lab"
  )(n)
}

# Criando uma função para rótulos numéricos em português
rotulo_numero <- function(accuracy = 1, ...) {

  # Usando ponto no milhar e vírgula decimal
  scales::label_number(
    accuracy     = accuracy,
    big.mark     = ".",
    decimal.mark = ",",
    ...
  )
}

# Criando uma função para rótulos percentuais em português
rotulo_percentual <- function(accuracy = 1, ...) {

  # Usando ponto no milhar e vírgula decimal
  scales::label_percent(
    accuracy     = accuracy,
    big.mark     = ".",
    decimal.mark = ",",
    ...
  )
}

# Criando uma função para formatar números em textos e anotações
formatar_numero <- function(x, digitos = 0) {

  # Arredondando e formatando com ponto no milhar e vírgula decimal
  format(
    x            = round(x, digitos),
    big.mark     = ".",
    decimal.mark = ",",
    nsmall       = digitos,
    trim         = TRUE,
    scientific   = FALSE
  )
}

# Criando escalas categóricas em que a cor acompanha a categoria, não a ordem
escala_fill_categorica <- function(niveis, principal = "vermelho", ...) {

  # Associando cada nível a uma cor fixa
  scale_fill_manual(
    values = setNames(paleta_categorica(length(niveis), principal), niveis),
    ...
  )
}
escala_colour_categorica <- function(niveis, principal = "vermelho", ...) {

  # Associando cada nível a uma cor fixa
  scale_colour_manual(
    values = setNames(paleta_categorica(length(niveis), principal), niveis),
    ...
  )
}

# Criando uma escala de preenchimento sequencial, contínua ou em classes
escala_fill_sequencial <- function(cor = "vermelho", classes = FALSE,
                                   labels = rotulo_numero(), ...) {

  # Obtendo as cores da escala
  cores <- paleta_sequencial(n = 7, cor = cor)

  # Escolhendo entre escala em classes e escala contínua
  if (classes) {
    scale_fill_stepsn(colours = cores, labels = labels, na.value = cor_grade, ...)
  } else {
    scale_fill_gradientn(colours = cores, labels = labels, na.value = cor_grade, ...)
  }
}

# Criando uma escala de preenchimento divergente em torno de um valor de
# referência (por exemplo, o valor do município)
escala_fill_divergente <- function(meio, baixo = "turquesa", alto = "vermelho",
                                   labels = rotulo_numero(), ...) {

  # Criando a escala com o centro neutro no valor de referência
  scale_fill_gradientn(
    colours  = paleta_divergente(n = 7, baixo = baixo, alto = alto),
    labels   = labels,
    na.value = cor_grade,
    rescaler = function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to = to, from = from, mid = meio)
    },
    ...
  )
}

# Criando o tema ggplot2 do projeto
tema_insper <- function(tamanho_base = 10) {

  # Partindo do tema mínimo com a fonte do projeto
  theme_minimal(
    base_size   = tamanho_base,
    base_family = familia_fonte
  ) +
    theme(
      # Fundo branco
      plot.background       = element_rect(fill = cor_fundo, colour = NA),
      panel.background      = element_rect(fill = cor_fundo, colour = NA),
      # Grade discreta, sem linhas secundárias
      panel.grid.major      = element_line(colour = cor_grade, linewidth = 0.3),
      panel.grid.minor      = element_blank(),
      # Eixos em cinza escuro, sem marcas
      axis.text             = element_text(colour = cor_texto_apoio, size = rel(0.85)),
      axis.title            = element_text(colour = cor_texto_apoio, size = rel(0.9)),
      axis.ticks            = element_blank(),
      # Título, subtítulo e rodapé alinhados à esquerda da figura
      plot.title            = element_text(
        colour = cor_texto,
        face   = "bold",
        size   = rel(1.35),
        hjust  = 0,
        margin = margin(b = 4)
      ),
      plot.subtitle         = element_text(
        colour = cor_texto_apoio,
        size   = rel(1),
        hjust  = 0,
        margin = margin(b = 10)
      ),
      plot.caption          = element_text(
        colour     = cor_texto_nota,
        size       = rel(0.75),
        hjust      = 0,
        lineheight = 1.15,
        margin     = margin(t = 10)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      # Legenda acima do gráfico, à esquerda
      legend.position       = "top",
      legend.justification  = "left",
      legend.title          = element_text(colour = cor_texto_apoio, size = rel(0.85)),
      legend.text           = element_text(colour = cor_texto_apoio, size = rel(0.85)),
      # Títulos de painéis em negrito, à esquerda
      strip.text            = element_text(colour = cor_texto, face = "bold", hjust = 0),
      # Margens externas
      plot.margin           = margin(12, 12, 12, 12)
    )
}

# Criando o tema dos mapas, sem eixos nem grade
tema_mapa <- function(tamanho_base = 10) {

  # Partindo do tema do projeto
  tema_insper(tamanho_base = tamanho_base) +
    theme(
      # Retirando eixos e grade (a grade principal é zerada à parte, porque o
      # tema do projeto a define explicitamente)
      axis.text            = element_blank(),
      axis.title           = element_blank(),
      panel.grid           = element_blank(),
      panel.grid.major     = element_blank(),
      # Legenda à direita, no alto
      legend.position      = "right",
      legend.justification = "top"
    )
}

# Criando uma função para acrescentar escala gráfica e norte aos mapas
elementos_mapa <- function(escala = TRUE, norte = TRUE) {

  # Criando a lista de camadas
  camadas <- list()

  # Acrescentando a escala gráfica no canto inferior esquerdo
  if (escala) {
    camadas <- c(camadas, list(
      annotation_scale(
        location    = "bl",
        style       = "ticks",
        line_col    = cor_texto_apoio,
        text_col    = cor_texto_apoio,
        text_family = familia_fonte,
        text_cex    = 0.7
      )
    ))
  }

  # Acrescentando o norte no canto superior esquerdo
  if (norte) {
    camadas <- c(camadas, list(
      annotation_north_arrow(
        location = "tl",
        height   = unit(1.5, "cm"),
        width    = unit(1.0, "cm"),
        style    = north_arrow_orienteering(
          line_col    = cor_texto_apoio,
          fill        = c(cor_fundo, cor_texto_apoio),
          text_col    = cor_texto_apoio,
          text_family = familia_fonte,
          text_size   = 8
        )
      )
    ))
  }

  # Devolvendo as camadas
  camadas
}

# Criando uma função para montar o rodapé com fontes e notas
legenda_fonte <- function(fontes, notas = NULL, largura = 140) {

  # Traduzindo chaves de fontes_dados e mantendo textos livres
  fontes <- ifelse(fontes %in% names(fontes_dados), fontes_dados[fontes], fontes)

  # Traduzindo chaves de notas_padrao e mantendo textos livres
  if (!is.null(notas)) {
    notas <- ifelse(notas %in% names(notas_padrao), notas_padrao[notas], notas)
  }

  # Montando as linhas do rodapé
  linhas <- c(
    paste0("Fonte: ", paste(fontes, collapse = "; "), "."),
    if (!is.null(notas)) paste0("Nota: ", notas)
  )

  # Quebrando as linhas longas e juntando o rodapé
  paste(str_wrap(linhas, width = largura), collapse = "\n")
}

# Criando uma função para gravar figuras em PNG a 300 dpi com tamanhos nomeados
gravar_figura <- function(grafico, arquivo, tamanho = "grafico",
                          largura = NULL, altura = NULL, dpi = 300) {

  # Exigindo extensão PNG
  if (!grepl("\\.png$", arquivo)) {
    stop("As figuras do projeto são PNG: ", arquivo)
  }

  # Obtendo as dimensões do tamanho nomeado, em centímetros
  dimensoes <- tamanhos_figura[[tamanho]]
  if (is.null(dimensoes)) {
    stop("Tamanho desconhecido: ", tamanho, ". Use: ", paste(names(tamanhos_figura), collapse = ", "))
  }

  # Interrompendo se o caminho passar do limite do Windows, que o ragg corta
  # sem avisar
  if (nchar(normalizePath(arquivo, winslash = "/", mustWork = FALSE)) > 259) {
    stop("Caminho com mais de 259 caracteres (limite do Windows): ", arquivo)
  }

  # Substituindo as dimensões informadas explicitamente
  largura <- largura %||% dimensoes[["largura"]]
  altura  <- altura %||% dimensoes[["altura"]]

  # Criando a pasta de saída
  dir.create(
    path         = dirname(arquivo),
    recursive    = TRUE,
    showWarnings = FALSE
  )

  # Informando o andamento
  message("Gravando ", arquivo)

  # Abrindo o dispositivo PNG do ragg
  agg_png(
    filename   = arquivo,
    width      = largura,
    height     = altura,
    units      = "cm",
    res        = dpi,
    background = cor_fundo
  )

  # Desenhando a figura e fechando o dispositivo mesmo em caso de erro
  tryCatch(
    print(grafico),
    finally = dev.off()
  )

  # Devolvendo o arquivo de forma invisível
  invisible(arquivo)
}

# Criando uma função para retirar acentos de textos
texto_sem_acento <- function(x) {

  # Transliterando para ASCII
  stri_trans_general(x, "Latin-ASCII")
}

# Criando uma função para o nome de arquivo de um distrito:
# <codigo>_<nome_sem_acento>[_<sufixo>].<extensao>
nome_arquivo_distrito <- function(codigo, nome, sufixo = NULL, extensao = "png") {

  # Exigindo códigos distritais em texto, com nove dígitos
  if (!is.character(codigo) || !all(grepl("^[0-9]{9}$", codigo))) {
    stop("O código do distrito deve ser texto com nove dígitos.")
  }

  # Criando uma função interna para padronizar trechos do nome
  padronizar <- function(x) {

    # Retirando acentos, passando para minúsculas e trocando separadores por _
    texto_sem_acento(x) |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9]+", "_") |>
      str_remove_all("^_+|_+$")
  }

  # Montando o nome do arquivo
  paste0(
    codigo,
    "_",
    padronizar(nome),
    if (!is.null(sufixo)) paste0("_", padronizar(sufixo)),
    ".",
    extensao
  )
}
