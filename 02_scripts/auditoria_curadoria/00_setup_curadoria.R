# ============================================================
# 00_setup_auditoria.R
# Setup exclusivo do fluxo de auditoria e curadoria.
#
# Este script:
# - não lê nem modifica dados;
# - não chama scripts ou funções analíticas do supervisor;
# - não aplica filtros de independência, taxonomia ou blanks;
# - apenas configura o ambiente para a auditoria.
# ============================================================


#----- verificar se o projeto foi aberto corretamente

if (!file.exists("monitora-team-pipelines.Rproj")) {
  stop(
    paste0(
      "Abra o arquivo 'monitora-team-pipelines.Rproj' no RStudio ",
      "e execute os scripts a partir da raiz do projeto."
    ),
    call. = FALSE
  )
}

audit_project_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)


#----- verificar pacotes necessários para a auditoria

audit_required_packages <- c(
  "tidyverse",
  "lubridate",
  "fs",
  "sf",
  "leaflet"
)

audit_missing_packages <- audit_required_packages[
  !vapply(
    audit_required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(audit_missing_packages) > 0) {
  stop(
    paste0(
      "Os seguintes pacotes precisam ser instalados antes de iniciar ",
      "a auditoria:\n",
      paste(audit_missing_packages, collapse = ", "),
      "\n\nExecute no console:\ninstall.packages(c(",
      paste0('"', audit_missing_packages, '"', collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}


#----- carregar pacotes

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(sf)
  library(leaflet)
})


#----- definir caminhos exclusivos da auditoria

audit_raw_dir <- file.path(
  audit_project_root,
  "01_entrada",
  "dados_brutos"
)

audit_processed_dir <- file.path(
  audit_project_root,
  "01_entrada",
  "dados_processados",
  "auditoria_curadoria"
)

audit_output_dir <- file.path(
  audit_project_root,
  "03_saida",
  "auditoria_curadoria"
)

audit_tables_dir <- file.path(
  audit_output_dir,
  "tabelas"
)

audit_figures_dir <- file.path(
  audit_output_dir,
  "figuras"
)

audit_logs_dir <- file.path(
  audit_output_dir,
  "logs"
)

audit_functions_dir <- file.path(
  audit_project_root,
  "02_scripts",
  "auditoria_curadoria",
  "funcoes"
)


#----- verificar entrada e criar apenas diretórios da auditoria

if (!dir.exists(audit_raw_dir)) {
  stop(
    paste0(
      "O diretório de exports brutos não foi encontrado:\n",
      audit_raw_dir
    ),
    call. = FALSE
  )
}

audit_directories <- c(
  audit_processed_dir,
  audit_output_dir,
  audit_tables_dir,
  audit_figures_dir,
  audit_logs_dir,
  audit_functions_dir
)

invisible(
  lapply(
    audit_directories,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


#----- registrar versões dos pacotes usadas na auditoria

audit_package_versions <- tibble(
  package = audit_required_packages,
  version = vapply(
    audit_required_packages,
    function(package_name) {
      as.character(
        utils::packageVersion(package_name)
      )
    },
    character(1)
  )
)


#----- carregar apenas funções da auditoria

audit_function_files <- fs::dir_ls(
  audit_functions_dir,
  glob = "*.R",
  type = "file"
)

if (length(audit_function_files) > 0) {
  invisible(
    lapply(
      as.character(audit_function_files),
      source,
      encoding = "UTF-8"
    )
  )
}


#----- mensagem final

message(
  "Setup da auditoria concluído.\n",
  "Nenhum dado foi lido, filtrado ou modificado."
  
)