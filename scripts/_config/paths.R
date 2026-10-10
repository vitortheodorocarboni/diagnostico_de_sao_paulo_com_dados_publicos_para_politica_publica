################################################################################
##################################            ##################################
############################# CAMINHOS DO PROJETO ##############################
##################################            ##################################
################################################################################

################################################################################
##### I. Funções
################################################################################

# Criando uma função para encontrar a pasta com um marcador .here
encontrar_raiz <- function(inicio = getwd()) {

  # Normalizando o caminho da pasta inicial
  atual <- normalizePath(
    path     = inicio,
    winslash = "/",
    mustWork = TRUE
  )

  # Subindo na árvore de pastas até encontrar o marcador
  repeat {

    # Devolvendo a pasta atual se ela contiver o marcador
    if (file.exists(file.path(atual, ".here"))) {
      return(atual)
    }

    # Obtendo a pasta acima
    acima <- dirname(atual)

    # Interrompendo se o marcador não for encontrado até a raiz do disco
    if (identical(acima, atual)) {
      stop(
        "Marcador '.here' do projeto não encontrado a partir de: ",
        normalizePath(
          path     = inicio,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }

    # Atualizando a pasta atual
    atual <- acima
  }
}

# Criando uma função para localizar a raiz do acervo Dados
encontrar_dados <- function(raiz_projeto) {

  # Lendo a variável de ambiente DADOS_ROOT
  dados_root <- Sys.getenv("DADOS_ROOT", unset = "")

  # Escolhendo DADOS_ROOT ou, na falta dela, a pasta irmã Dados
  candidato <- if (nzchar(dados_root)) {
    dados_root
  } else {
    file.path(dirname(raiz_projeto), "Dados")
  }

  # Interrompendo se a pasta não existir
  if (!dir.exists(candidato)) {
    stop(
      "Acervo Dados não encontrado em: ", candidato,
      ". Defina DADOS_ROOT ou coloque o Dados na pasta irmã '../Dados'."
    )
  }

  # Normalizando o caminho do acervo
  candidato <- normalizePath(
    path     = candidato,
    winslash = "/",
    mustWork = TRUE
  )

  # Definindo o arquivo marcador do acervo
  marcador <- file.path(candidato, ".here")

  # Conferindo se o marcador existe e identifica o projeto Dados
  if (!file.exists(marcador) ||
      !identical(trimws(readLines(marcador, n = 1, warn = FALSE)), "Raiz do projeto Dados.")) {
    stop(
      "A pasta ", candidato, " não tem o marcador '.here' do projeto Dados ",
      "(esperado: 'Raiz do projeto Dados.')."
    )
  }

  # Devolvendo a raiz do acervo
  candidato
}

# Criando uma função para compor caminhos a partir da raiz do projeto
caminho_projeto <- function(...) {

  # Combinando a raiz do projeto com as partes do caminho
  file.path(raiz_projeto, ...)
}

# Criando uma função para compor caminhos dentro do acervo Dados
caminho_dados_acervo <- function(...) {

  # Combinando a raiz do acervo com as partes do caminho
  file.path(raiz_dados, ...)
}

# Criando uma função para compor caminhos na pasta de dados do projeto
caminho_data <- function(...) {

  # Combinando a raiz do projeto com a pasta data
  file.path(raiz_projeto, "data", ...)
}

# Criando uma função para compor caminhos na pasta de resultados
caminho_resultados <- function(...) {

  # Combinando a raiz do projeto com a pasta resultados
  file.path(raiz_projeto, "resultados", ...)
}


################################################################################
##### II. Raízes do projeto e do acervo Dados
################################################################################

# Encontrando a raiz do projeto
raiz_projeto <- encontrar_raiz()

# Encontrando a raiz do acervo Dados
raiz_dados <- encontrar_dados(raiz_projeto = raiz_projeto)


################################################################################
##### III. Diretórios necessários
################################################################################

# Criando a lista de diretórios necessários
diretorios_necessarios <- c(
  caminho_data(),
  caminho_resultados()
)

# Criando os diretórios necessários
for (diretorio in diretorios_necessarios) {

  # Criando o diretório sem avisos
  dir.create(
    path         = diretorio,
    recursive    = TRUE,
    showWarnings = FALSE
  )
}


################################################################################
##### IV. Regras do projeto
################################################################################

# Definindo o mínimo de registros de pessoas na amostra do Censo abaixo do qual
# o distrito recebe aviso de amostra menor (decisão do usuário, 2026-10-10);
# nenhum distrito é suprimido
minimo_registros_amostra <- 2000

# Definindo o mínimo de registros na OD abaixo do qual o distrito recebe aviso
# de amostra menor, e o piso abaixo do qual o distrito fica sem valor (decisão
# do usuário, 2026-10-10; contados no bloco usado, como o de trabalhadores)
minimo_registros_od <- 100
piso_registros_od   <- 30


################################################################################
##### V. Leitura dos tratados
################################################################################

# Carregando as funções de leitura dos tratados com conferência de SHA-256
source(caminho_projeto("scripts", "_config", "leitura.R"), encoding = "UTF-8")
