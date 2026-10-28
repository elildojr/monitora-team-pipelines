# Auditoria e mapas no RStudio

Esta proposta contém três scripts revisados e uma camada com os limites de 15 UCs. O mapa é gerado pelo R, mas somente quando a opção `monitora.gerar_mapa` é ativada. O código passou pela verificação de sintaxe; ainda precisa ser executado e conferido no RStudio, pois os pacotes R instalados no computador não estavam acessíveis ao ambiente que preparou estes arquivos.

## 1. Instalar os arquivos no projeto

Abra `monitora-team-pipelines.Rproj` no RStudio. Faça uma cópia de segurança dos três scripts atuais e substitua-os pelos três arquivos `.R` deste pacote na pasta `02_scripts/auditoria_curadoria/`.

Copie a pasta `01_entrada/dados_referencia/` deste pacote para dentro da pasta do projeto, preservando o caminho. O arquivo que o script usa é:

```
01_entrada/dados_referencia/limites_ucs_icmbio_202608.geojson
```

Leia `FONTE.md` e revise a tabela `projetos_e_ucs.csv` nessa mesma pasta. Os nomes e códigos CNUC foram relacionados aos códigos dos projetos com base nos exports, mas a equipe deve conferir os vínculos. Um projeto que abranja mais de uma UC exigirá adaptação. Não altere os arquivos brutos.

Instale os pacotes, se necessário:

```r
install.packages(c("tidyverse", "lubridate", "fs", "sf", "leaflet", "htmlwidgets"))
```

## 2. Executar a auditoria e gerar o mapa

Com um conjunto ativo de exports em `01_entrada/dados_brutos/`, execute:

```r
source("02_scripts/auditoria_curadoria/01_importar_exports_brutos.R", encoding = "UTF-8")
options(monitora.gerar_mapa = TRUE)
source("02_scripts/auditoria_curadoria/02_auditar_coordenadas.R", encoding = "UTF-8")
options(monitora.gerar_mapa = FALSE)
```

A primeira linha cria uma nova rodada. Se a importação já foi concluída e os arquivos de entrada não mudaram, você pode executar somente as três últimas linhas para gerar o mapa da rodada mais recente. Para que uma execução posterior também gere mapa, ative a opção novamente.

O console informa a pasta da rodada. O mapa `02_coordinate_map_all_projects.html` fica em `03_saida/auditoria_curadoria/figuras/<rodada>/`. Ele contém os pontos e os limites com nomes das UCs, além de uma opção de imagem de satélite e outra de mapa de referência. Use o seletor para escolher o fundo ou mostrar e ocultar camadas. O fundo é buscado pela internet; o HTML não inclui as imagens de satélite. Ao compartilhar o HTML, mantenha também a pasta de recursos criada junto dele.

A camada mostra os limites selecionados, mas não determina automaticamente se um ponto está correto ou errado. Pontos podem estar fora do limite por diferentes razões e devem ser interpretados no contexto do projeto. A auditoria com polígonos e suas regras ainda precisa ser definida separadamente.

## 3. Resultados essenciais

As tabelas ficam em `03_saida/auditoria_curadoria/tabelas/<rodada>/`: resumo da importação, coordenadas para revisão e coordenadas compartilhadas entre nomes de estação. Os detalhes e hashes ficam em `01_entrada/dados_processados/auditoria_curadoria/<rodada>/` como arquivos RDS. A cópia RDS grande dos dados de imagem é opcional:

```r
options(monitora.salvar_snapshot = TRUE)
source("02_scripts/auditoria_curadoria/01_importar_exports_brutos.R", encoding = "UTF-8")
options(monitora.salvar_snapshot = FALSE)
```

Por padrão, os CSVs originais continuam como fonte completa e nenhuma cópia grande é gravada. Futuras etapas que dependam desse RDS precisarão ativar a opção ou ler os CSVs. Nenhum resultado de rodadas anteriores é apagado automaticamente.

As versões do R e dos pacotes são registradas no log, mas não estão travadas. Para reprodução em outro computador, compartilhe os mesmos exports, o GeoJSON, os scripts e as versões de software; o GitHub ignora alguns arquivos de dados neste repositório, então confirme quais arquivos precisam ser compartilhados fora dele.
