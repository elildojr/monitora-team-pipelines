# Auditoria dos dados TEAM/Monitora

A etapa de coordenadas identifica ocorrências espaciais para conferência no Wildlife Insights. Os registros de origem são preservados.

## Resultado único e caminho fixo

Os scripts atualizam somente este arquivo de resultados:

```text
01_entrada/dados_processados/auditoria_curadoria/auditoria_curadoria.rds
```

O arquivo contém três blocos, com os mesmos nomes utilizados pelos scripts e pelo QMD:

- `importacao`: resumo dos exports, problemas de leitura e vínculos entre os dados;
- `coordenadas`: coordenadas por deployment e possíveis inconsistências espaciais;
- `unidades_amostrais`: pares de proximidade e referências anuais auxiliares.

As etapas atualizam o mesmo RDS. Não são criadas subpastas por execução, CSVs de resumo, logs separados, cópias grandes das imagens ou mapas HTML independentes. O identificador da rodada fica nos metadados do RDS. Se os exports não mudaram, o identificador é reutilizado; se mudaram, a importação substitui os resultados atuais e a etapa 02 precisa ser executada novamente.

Os arquivos e as pastas das versões anteriores não são apagados. O novo RDS representa o conjunto atual, e não um histórico de todas as execuções.

## Execução no RStudio

Abra `monitora-team-pipelines.Rproj` e mantenha um conjunto ativo de exports em `01_entrada/dados_brutos/`.

Na importação dos dados, execute:

```r
source("02_scripts/auditoria_curadoria/01_importar_exports_brutos.R", encoding = "UTF-8")
```

Para auditar as coordenadas e comparar posições entre placenames, execute:

```r
source("02_scripts/auditoria_curadoria/02_auditar_coordenadas.R", encoding = "UTF-8")
```

A etapa 02 inclui as comparações entre placenames. No primeiro uso, pode aproveitar uma importação do formato anterior indicada por `AUDIT_CURRENT_RUN.txt`, sem reimportar as imagens; os resultados passam a ser salvos no caminho fixo.

Depois, abra `04_reports/Relatório de Auditoria de Coordenadas.qmd` e use **Render** no RStudio. O QMD somente lê o RDS fixo e apresenta a auditoria dos dados. A configuração `_quarto.yml` direciona os resultados para uma pasta fixa em `03_saida/relatorios/`, preservando o caminho da pasta de origem. O HTML deste relatório será `03_saida/relatorios/04_reports/relatorio_auditoria_coordenadas.html`, sem espaços ou acentos. As renderizações seguintes reutilizam esse mesmo destino.

O `.qmd` e a configuração `_quarto.yml` são fontes versionados no GitHub. O HTML, os demais resultados de `03_saida/` e os arquivos auxiliares de renderização são locais e ignorados pelo Git. O `README.md` de `03_saida/` é documentação e permanece versionado.

## Mapa das possíveis inconsistências

O QMD chama a função `funcoes/mapa_conflitos_coordenadas.R` para mostrar o mapa dentro do próprio relatório. Não é salvo um HTML de mapa separado. O mapa usa os resultados já calculados no RDS fixo, com o mesmo limiar de proximidade.

Os pontos mostram ocorrências de coordenadas e proximidade entre placenames. Os marcadores incluem nomes originais, deployments, coordenadas e anos de referência. As linhas conectam posições próximas com coincidência anual. Há imagem de satélite, mapa de referência e limites locais das UCs. Conflitos de câmera, lacunas anuais e avaliação de datas não são apresentados no mapa ou no relatório de coordenadas.

Para consultar no Viewer do RStudio, sem renderizar o relatório:

```r
source("02_scripts/auditoria_curadoria/funcoes/mapa_conflitos_coordenadas.R", encoding = "UTF-8")
auditoria <- readRDS("01_entrada/dados_processados/auditoria_curadoria/auditoria_curadoria.rds")
mapa_conflitos_coordenadas(auditoria)
```

Para visualizar somente Juruena, use `mapa_conflitos_coordenadas(auditoria, project_id = "2002576")`. São necessários os pacotes `leaflet`, `htmltools` e `sf`. O fundo cartográfico depende de conexão com a internet.

## Termos e critérios comuns

- **Unidade amostral:** cada `placename` de comparação dentro de um projeto. Um nome em outro projeto é outra unidade.
- **Deployment:** período de instalação e operação associado à unidade amostral.
- **Proximidade:** distância geográfica estritamente inferior a **500 m** entre unidades distintas no mesmo projeto. O valor é definido uma única vez em `audit_nearby_threshold_m`, no script 00.
- **Coincidência anual:** proximidade entre posições atribuídas a deployments que abrangem o mesmo ano de operação informado. Não comprova simultaneidade de operação.
- **Variação de posição do mesmo placename:** distância máxima acima de 30 m entre as coordenadas atribuídas ao nome no mesmo projeto. Não demonstra deslocamento físico da câmera.
- **Distância mínima geral:** menor distância entre as posições de dois placenames no conjunto disponível. Pode corresponder a anos diferentes.

O QMD apresenta somente a auditoria das coordenadas. As tabelas auxiliares de períodos e presença permanecem no RDS por compatibilidade; não constituem a auditoria das datas das campanhas, prevista para outra etapa. A verificação automática de pertencimento aos polígonos das UCs também permanece pendente.

## Consulta dos resultados

```r
auditoria <- readRDS("01_entrada/dados_processados/auditoria_curadoria/auditoria_curadoria.rds")
View(auditoria$coordenadas$deployment_coordinate_audit)
View(auditoria$unidades_amostrais$nearby_unit_pairs)
View(auditoria$coordenadas$station_coordinate_history)
View(auditoria$unidades_amostrais$nearby_deployment_year_pairs)
```
