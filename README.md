# Diagnóstico de São Paulo com dados públicos para política pública

Retrato dos **96 distritos do Município de São Paulo** feito com dados públicos:
quem mora em cada distrito, como mora, como estuda, quanto ganha, em que
trabalha, como se desloca. O objetivo é oferecer a gestores públicos,
organizações da sociedade civil, pesquisadores e jornalistas uma base comum,
comparável e reprodutível para discutir as desigualdades entre os distritos e
orientar políticas públicas no território.

O repositório traz o código em R, as tabelas por distrito e as figuras (mapas da
cidade e gráficos para cada distrito), prontos para consulta. Todo indicador é
comparado com o do Município de São Paulo.

## O que há aqui

| Pasta | Conteúdo |
|---|---|
| [`resultados/`](resultados) | para quem quer **consultar**: por tema, as tabelas XLSX com os indicadores dos 96 distritos, os mapas da cidade e os gráficos que comparam os distritos; em `resultados/<tema>/distritos/`, os gráficos de cada distrito, com o código e o nome do distrito no nome do arquivo |
| [`dados/`](dados) | para quem quer **analisar**: os indicadores por distrito em Parquet e os extratos das fontes usados para calculá-los |
| [`scripts/`](scripts) | para quem quer **reproduzir ou estender**: o código em R que gera tudo, um script por passo |

São cerca de 8 mil figuras em PNG (300 dpi), 22 tabelas XLSX e 142 arquivos
Parquet.

## Temas

| Tema | Fonte | O que mostra |
|---|---|---|
| `00_territorio` | Censo e OD | os distritos e como cada fonte se liga a eles |
| `01_densidades` | Censo, universo | densidade demográfica e domiciliar, moradores por domicílio |
| `02_domicilios` | Censo, universo | tipos de domicílio, esgoto, arborização e outras condições do entorno |
| `03_sexo` | Censo, universo | razão de sexo, por idade |
| `04_etarias` | Censo, universo | envelhecimento, crianças e jovens, pirâmides etárias |
| `05_raca_cor` | Censo, universo | composição por cor ou raça, por idade |
| `06_escolaridade` | Censo, amostra e universo | alfabetização, nível de instrução, frequência escolar, anos de estudo, curso superior |
| `07_renda` | Censo, amostra | rendimento médio, faixas de salário mínimo, rendimento do trabalho e de outras fontes, rendimento familiar *per capita* |
| `08_religiosidade` | Censo, amostra | religião ou culto |
| `09_trabalhadores` | Pesquisa OD | perfil de quem trabalha no distrito e de onde vem |
| `10_deslocamentos` | Pesquisa OD | modos, duração e motivos das viagens; fluxos na Região Metropolitana |
| `11_moradia` | Censo, amostra e universo | domicílios alugados, próprios e em pagamento; condições materiais |
| `12_arranjos_familiares` | Censo, universo | unipessoais, famílias monoparentais, casais do mesmo sexo, idosos responsáveis |
| `13_obitos` | Censo, amostra e universo | mortalidade padronizada pela idade, óbitos antes dos 60 anos |
| `14_migracao` | Censo, amostra | origem dos moradores, migração recente, tempo de moradia |
| `15_trabalho_moradores` | Censo, amostra, e OD | ocupação, informalidade, setor de atividade, tempo e meio de transporte até o trabalho |
| `16_deficiencia` | Censo, amostra | pessoas com deficiência, por tipo, e com autismo |
| `17_unipessoais` | Censo, amostra e universo | quem mora sozinho, por sexo, idade, cor ou raça, escolaridade e renda |

## Fontes

- **Censo Demográfico 2022 (IBGE), universo:** resultados por setor censitário,
  somados no distrito a que cada setor pertence.
- **Censo Demográfico 2022 (IBGE), amostra:** estimativas por área de
  ponderação, atribuídas ao distrito onde está a maior parte da população de
  cada área. Não há microdados neste repositório.
- **Pesquisa Origem e Destino 2023 (Metrô de São Paulo):** estimativas por zona,
  somadas no distrito oficial da pesquisa, equivalente ao distrito do IBGE.
- **GeoSampa (Prefeitura de São Paulo):** corpos d'água e rede de metrô, trem e
  corredores de ônibus, só como referência nos mapas.

As fontes foram tratadas antes, num acervo de dados próprio. Este projeto lê só
essas bases tratadas, com o SHA-256 de cada arquivo conferido no código, e não
reparte estimativas por área.

## Como ler os resultados

- **Comparação com a cidade:** os indicadores vêm sempre ao lado do valor do
  Município de São Paulo. Proporções usam o denominador da própria tabela da
  fonte; somam-se contagens e estimativas, nunca médias de proporções.
- **Amostras pequenas:** na amostra do Censo, os distritos com menos de 2.000
  pessoas recenseadas na amostra recebem um aviso (asterisco ou contorno
  tracejado). Na OD, o aviso vale abaixo de 100 registros, e abaixo de 30 o
  distrito fica sem valor. Óbitos e domicílios unipessoais têm limites próprios,
  indicados nas figuras.
- **Sigilo do IBGE:** no universo, os totais por distrito somam só os valores
  publicados; as células omitidas pelo IBGE não são imputadas, e as categorias
  pequenas (por exemplo, amarela e indígena) ficam subestimadas.
- **Área de ponderação da Sé:** inclui 12 setores da República (3.222
  moradores). Nas figuras da amostra, isso vem em nota.

## Limitações

- A margem de erro das estimativas da amostra do Censo e da OD não é calculada.
  A OD tem cerca de dez vezes menos casos por zona que o Censo por área de
  ponderação; use os recortes distritais da OD com cautela.
- Recortes da OD por destino ou local de trabalho não são estratos do desenho
  amostral. A OD cobre viagens de um dia útil.
- O Censo 2022 não pergunta o valor do aluguel; a moradia usa só a condição de
  ocupação.
- Os óbitos informados pelos domicílios (2019 a julho de 2022) ficam abaixo dos
  registros civis e não medem o excesso de mortes da pandemia.
- A deficiência cresce com a idade, e as bases não permitem padronizá-la; o
  percentual de 60 anos ou mais vem ao lado. O mesmo vale para a frequência
  escolar e os anos de estudo, que dependem da estrutura etária.
- Nomes de países, municípios, ocupações e atividades vêm dos descritores do
  IBGE, sem correção.

## Como reproduzir

Requer R 4.6 com os pacotes `tidyverse`, `arrow`, `geoarrow`, `sf`, `lwgeom`,
`rmapshaper`, `openssl`, `jsonlite`, `systemfonts`, `ragg`, `showtext`,
`sysfonts`, `scales`, `colorspace`, `stringi`, `patchwork`, `ggrepel`,
`ggspatial`, `readxl` e `writexl`. Os scripts rodam a partir da raiz do
repositório (marcada pelo arquivo `.here`), por `Rscript` ou `source()`:

```bash
Rscript scripts/01_densidades/01_02.R
```

Cada tema segue o mesmo fluxo, em ordem numérica:

| Script | O que faz |
|---|---|
| `NN_01` (e o primeiro script de cada extensão) | lê as bases tratadas do acervo de dados e grava os extratos em `dados/<tema>/` |
| `NN_02` | calcula os indicadores, roda as conferências (e para se alguma falhar) e grava os Parquet e a tabela XLSX |
| `NN_03` em diante | geram os mapas, os gráficos que comparam os distritos e os gráficos por distrito |

Os scripts de leitura dependem do acervo de dados tratados, indicado pela
variável de ambiente `DADOS_ROOT`, que não está neste repositório. Os extratos
que eles produzem já estão em `dados/`: com eles, os demais scripts rodam sem o
acervo. Depois de `00_territorio`, os temas são independentes entre si.

`scripts/_config` reúne o que todos usam: caminhos, leitura das bases com
conferência do SHA-256, identidade visual (paleta, tema e gravação das
figuras), o molde dos gráficos por distrito e o universo de cada variável da
amostra. As figuras usam a fonte Inter, incluída em `fontes/` com a sua licença
(SIL Open Font License), e seguem a identidade visual do Insper.

## Próximos passos

Estender o diagnóstico aos distritos dos demais municípios da Região
Metropolitana de São Paulo e, depois, a todos os municípios do Estado de São
Paulo, em nível municipal.
