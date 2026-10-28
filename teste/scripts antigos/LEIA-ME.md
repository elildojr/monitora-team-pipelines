# Scripts revisados de auditoria e curadoria

Esta pasta contém uma proposta para substituir os três scripts de mesmo nome em `02_scripts/auditoria_curadoria`. Faça uma cópia de segurança dos scripts atuais antes de substituí-los. A análise sintática em R foi concluída; uma rodada completa ainda precisa ser testada no RStudio, pois os pacotes R instalados no computador não estavam acessíveis ao ambiente de testes.

## Preparação

Abra `monitora-team-pipelines.Rproj` no RStudio e confira se o diretório de trabalho é a raiz do projeto. Coloque um único conjunto de exports em `01_entrada/dados_brutos`: `projects.csv`, `cameras.csv`, `deployments.csv` e um ou mais `images_*.csv`. Os arquivos brutos não são modificados.

Instale os pacotes necessários se ainda não estiverem disponíveis:

```r
install.packages(c("tidyverse", "lubridate", "fs", "sf"))
```

Para o mapa opcional, instale também `leaflet` e `htmlwidgets`. As versões utilizadas são registradas no log da importação, mas não estão travadas: colaboradores devem usar versões compatíveis. Para reprodução exata dos resultados, compartilhem os mesmos exports e registrem e comparem as versões do R e dos pacotes.

## Executar uma rodada

```r
source("02_scripts/auditoria_curadoria/01_importar_exports_brutos.R", encoding = "UTF-8")
source("02_scripts/auditoria_curadoria/02_auditar_coordenadas.R", encoding = "UTF-8")
```

Cada importação cria um identificador de rodada. Os resultados essenciais ficam em `03_saida/auditoria_curadoria/tabelas/<rodada>/`:

- `01_resumo_importacao.csv`: contagens da importação.
- `02_coordenadas_para_revisao.csv`: instalações sinalizadas, com motivos e dados disponíveis para revisão.
- `02_coordenadas_compartilhadas.csv`: agrupamentos de coordenadas compartilhadas por nomes de estação diferentes.

Detalhes da importação e da auditoria espacial, inclusive hashes, verificações e tabelas completas, ficam nos arquivos `01_import_log.rds` e `02_coordinate_audit.rds`, dentro de `01_entrada/dados_processados/auditoria_curadoria/<rodada>/`. Esses arquivos são usados para rastreabilidade e para consultar detalhes no R. Para inspecionar, por exemplo:

```r
names(readRDS(file.path(audit_run_processed_dir, "02_coordinate_audit.rds")))
View(shared_coordinates_different_placenames)
```

A auditoria valida os arquivos espaciais com os hashes da importação e interrompe a execução se forem alterados. Nesse caso, execute a importação novamente. A validação por limites gerais não substitui a conferência com polígonos oficiais das unidades de conservação.

## Opções

Por padrão, o script não gera mapa e não grava uma cópia RDS dos milhões de registros de imagem. Os CSVs originais continuam sendo a fonte completa e os dados importados ficam disponíveis na sessão durante a execução. Para criar a cópia em uma rodada, defina a opção antes da importação:

```r
options(monitora.salvar_snapshot = TRUE)
source("02_scripts/auditoria_curadoria/01_importar_exports_brutos.R", encoding = "UTF-8")
options(monitora.salvar_snapshot = FALSE)
```

Para gerar um mapa interativo que reúne todos os projetos, defina a opção antes da auditoria espacial:

```r
options(monitora.gerar_mapa = TRUE)
source("02_scripts/auditoria_curadoria/02_auditar_coordenadas.R", encoding = "UTF-8")
options(monitora.gerar_mapa = FALSE)
```

O mapa e seus arquivos de apoio ficam em `03_saida/auditoria_curadoria/figuras/<rodada>/`. O mapa oferece imagem de satélite (Esri) e mapa de referência (OpenStreetMap); use o seletor no canto superior direito para alternar. Os fundos são carregados pela internet e não ficam embutidos no HTML. A ausência de conexão ou o bloqueio dos serviços pode deixar o fundo em branco. Ao compartilhar o mapa, envie também a pasta de arquivos de apoio criada ao lado dele.

As rodadas anteriores e seus resultados não são apagados. Para não acumular arquivos ao longo do tempo, revise e remova manualmente somente os resultados que sua equipe decidir que não precisa mais preservar. A opção de não gravar o RDS das imagens pode exigir ajustes se futuros scripts forem criados para depender desse arquivo; nesse caso, ative `monitora.salvar_snapshot` ou adapte esses scripts para ler os CSVs.
