# monitora-team-pipelines
teste
Workflow para construir produtos de dados do [Programa Monitora ICMBio](https://www.gov.br/icmbio/pt-br/assuntos/monitoramento), protocolo avançado TEAM. O principal objetivo desse workflow é transformar os dados do programa Monitora em Variáveis Essenciais de Biodiversidade - EBVs. EBVs são medidas padronizadas usadas para monitorar e relatar mudanças na biodiversidade global. Eles atuam como camada intermediária entre os dados brutos obtidos em campo, e indicadores agregados de biodiversidade (e.g., [Kissling et al., 2017](https://doi.org/10.1111/brv.12359)).

Primeiramente, os dados passam por um controle de qualidade, visando garantir sua consistência em termos de formatos de data e hora, coordenadas espaciais e taxonomia, de forma a se tornarem dados utilizáveis para EBV ("EBV-usable data sets").

Em seguida, os dados são inseridos em um modelo populacional Bernoulli-Poisson (BernP, também conhecido como Royle-Nichols) para se tornarem dados compatíveis com EBV ("EBV-ready data"). Mais especificamente, o  modelo estima a abundância relativa das populações, o que equivale à EBV "abundância de espécies" (classe de EBV "populações de espécies", [Jetz et al., 2019](https://www.nature.com/articles/s41559-019-0826-1)).

Estas estimativas populacionais podem ser usadas na produção de índices agregados de tendências de biodiversidade, como o Living Planet Index - LPI, e outros.

<img src="03_saida/abundances.png" title="Relative abundance trends" width="700">


## Estrutura do diretório

``` bash
├───01_entrada
|   ├───dados_brutos
|   ├───dados_processados
├───02_scripts
|   ├───funcoes
└───03_saida
```

## Nota aos colaboradores

Para todas as funções, nomes de objetos, etc, usar o [tidyverse style guide](https://style.tidyverse.org/)

Criar novos branches para modificações

## Workflow

Agregação dos dados brutos, baixados diretamente da plataforma [Wildlife Insights](https://app.wildlifeinsights.org)

Controle de qualidade dos dados (coordenadas, data e hora, taxonomia, duplicações)

Idealmente, os erros identificados ao longo do pipeline devem ser corrigidos diretamente na plataforma Wildlife Insights. No entanto, dado que o fluxo de entrada de novos dados é constante (de forma que a curadoria é um eterno trabalho em andamento), o pipeline também inclui a correção ou exclusão de dados problemáticos para garantir que dados utilizavéis estejam disponíveis a qualquer momento.


## Nota sobre os dados:
Arquivos binários (e.g., .csv e .rds) não foram incluídos neste repositório para evitar lentidão no Git e respeitar os limites de armazenamento do GitHub.
Assim, este repositório contém apenas o código necessário para processar os dados, assumindo que os arquivos de entrada estejam disponíveis localmente no diretório principal do repositório.


# Contato

[elildojr\@gmail.com](mailto:elildojr@gmail.com)
