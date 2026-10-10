################################################################################
##################################            ##################################
################# 09.05) PERFIL DOS TRABALHADORES POR DISTRITO #################
##################################            ##################################
################################################################################

################################################################################
##### I. Pacotes
################################################################################

# # Instalando pacotes
# install.packages("arrow")
# install.packages("tidyverse")

# Carregando bibliotecas
library(arrow)
library(tidyverse)


################################################################################
##### II. Diretórios e arquivos
################################################################################

# Carregando os caminhos do projeto e as funções de leitura
source(file.path("scripts", "_config", "paths.R"), encoding = "UTF-8")

# Carregando a identidade visual
source(file.path("scripts", "_config", "identidade_visual.R"), encoding = "UTF-8")

# Definindo o diretório de dados do grupo
diretorio_grupo <- caminho_data("09_trabalhadores")

# Definindo o diretório de saída dos gráficos por distrito
diretorio_saida <- caminho_resultados("09_trabalhadores", "distritos")

# Definindo os indicadores do perfil, na ordem de apresentação
perfil_indicadores <- c(
  pct_mulheres          = "Mulheres",
  pct_negra             = "Pretos e pardos",
  pct_superior          = "Superior completo",
  pct_renda_ate_1000    = "Renda familiar per capita até R$ 1.000",
  pct_mora_distrito     = "Moram no próprio distrito",
  pct_mora_fora_capital = "Moram fora da capital"
)


################################################################################
##### III. Funções
################################################################################

# Criando uma função para desenhar o perfil de um distrito ao lado do da capital
desenhar_perfil <- function(dados, nome_titulo, subtitulo) {

  # Preparando as barras pareadas: distrito (vermelho) e capital (cinza)
  dados <- dados |>
    mutate(
      indicador_rotulo = factor(perfil_indicadores[indicador], levels = rev(perfil_indicadores)),
      territorio       = factor(
        if_else(nivel == "municipio", "Capital", nome_titulo),
        levels = c("Capital", nome_titulo)
      )
    )

  # Desenhando as barras lado a lado, com o valor ao fim de cada uma
  ggplot(dados, aes(x = valor, y = indicador_rotulo, fill = territorio)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width    = 0.75
    ) +
    geom_text(
      mapping  = aes(label = paste0(formatar_numero(valor, 1), "%")),
      position = position_dodge(width = 0.8),
      hjust    = -0.15,
      family   = familia_fonte,
      size     = 3,
      colour   = cor_texto
    ) +
    scale_fill_manual(
      values = setNames(c(cor_neutra, paleta_insper[["vermelho"]]), c("Capital", nome_titulo)),
      breaks = c(nome_titulo, "Capital"),
      name   = NULL
    ) +
    scale_x_continuous(
      labels = rotulo_numero(suffix = "%"),
      limits = c(0, 100),
      breaks = seq(0, 100, by = 25),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title    = nome_titulo,
      subtitle = subtitulo,
      x        = NULL,
      y        = NULL,
      caption  = legenda_fonte(
        fontes = "od",
        notas  = c(
          paste0(
            "Trabalhadores residentes na RMSP com primeiro trabalho de local fixo no distrito. Na capital, ",
            "os que moram no próprio distrito somam os trabalhadores que moram no distrito em que trabalham."
          ),
          "od_destino", "incerteza_amostra"
        )
      )
    ) +
    tema_insper() +
    theme(
      axis.text.y        = element_text(size = 9, colour = cor_texto),
      panel.grid.major.y = element_blank()
    )
}


################################################################################
##### IV. Dados
################################################################################

# Lendo os indicadores dos trabalhadores
trabalhadores <- read_parquet(file.path(diretorio_grupo, "trabalhadores.parquet"))

# Obtendo o perfil da capital
perfil_municipio <- trabalhadores |>
  filter(nivel == "municipio", indicador %in% names(perfil_indicadores))

# Organizando os totais e a idade média de cada território em formato largo
resumo <- trabalhadores |>
  filter(indicador %in% c("trabalhadores", "trabalhadores_por_morador", "idade_media")) |>
  select(codigo_distrito, nivel, indicador, valor) |>
  pivot_wider(names_from = indicador, values_from = valor)
resumo_municipio <- filter(resumo, nivel == "municipio")

# Listando os distritos com valor, com o aviso de amostra menor
distritos_lista <- trabalhadores |>
  filter(nivel == "distrito", !sem_valor) |>
  distinct(codigo_distrito, nome_rotulo, amostra_menor)


################################################################################
##### V. Exportação
################################################################################

# Criando o diretório de saída
dir.create(
  path         = diretorio_saida,
  recursive    = TRUE,
  showWarnings = FALSE
)

# Gravando o perfil de cada distrito com valor
message("Gerando o perfil dos ", nrow(distritos_lista), " distritos com valor")
for (i in seq_len(nrow(distritos_lista))) {

  # Selecionando o distrito
  codigo <- distritos_lista$codigo_distrito[i]
  nome   <- distritos_lista$nome_rotulo[i]
  linha  <- filter(resumo, codigo_distrito == codigo)

  # Descrevendo o volume e a idade média no subtítulo
  subtitulo <- paste0(
    "Perfil de quem trabalha no distrito, 2023: ", formatar_numero(round(linha$trabalhadores, -2)),
    " trabalhadores, ", formatar_numero(linha$trabalhadores_por_morador, 2),
    " por morador (capital: ", formatar_numero(resumo_municipio$trabalhadores_por_morador, 2),
    "); idade média de ", formatar_numero(linha$idade_media, 1), " anos (capital: ",
    formatar_numero(resumo_municipio$idade_media, 1), ")"
  ) |>
    str_wrap(width = 110)

  # Acrescentando o aviso de amostra menor
  if (distritos_lista$amostra_menor[i]) {
    subtitulo <- paste0(subtitulo, "\n", aviso_amostra_menor_od)
  }

  # Desenhando o perfil do distrito e da capital
  figura <- desenhar_perfil(
    dados       = bind_rows(
      filter(trabalhadores, codigo_distrito == codigo, indicador %in% names(perfil_indicadores)),
      perfil_municipio
    ),
    nome_titulo = nome,
    subtitulo   = subtitulo
  )

  # Gravando a figura com o código e o nome do distrito no nome do arquivo
  gravar_figura(
    grafico = figura,
    arquivo = file.path(diretorio_saida, nome_arquivo_distrito(codigo, nome, sufixo = "perfil")),
    tamanho = "grafico",
    altura  = 12
  )
}
