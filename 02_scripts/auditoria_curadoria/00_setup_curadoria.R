# 00_setup_curadoria.R
# Pacotes, caminhos, critérios e funções de leitura e gravação da auditoria.


#----- diretório do projeto

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


#----- dependências

audit_required_packages <- c(
  "tidyverse",
  "lubridate",
  "fs",
  "sf"
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
})


#----- caminhos da auditoria

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

audit_functions_dir <- file.path(
  audit_project_root,
  "02_scripts",
  "auditoria_curadoria",
  "funcoes"
)


#----- diretórios de entrada e resultados

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
  audit_processed_dir
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


#----- estrutura dos resultados e limiar de proximidade

audit_result_filename <- "auditoria_curadoria.rds"
audit_result_file <- file.path(audit_processed_dir, audit_result_filename)
audit_nearby_threshold_m <- 500

audit_new_result <- function(run_id) {
  list(
    metadata = list(
      audit_run_id = run_id,
      created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      nearby_threshold_m = audit_nearby_threshold_m,
      sampling_unit = "project_id + placename_key",
      local_correction = "not applied",
      local_exclusion = "not applied"
    ),
    importacao = NULL,
    coordenadas = NULL,
    unidades_amostrais = NULL
  )
}

audit_read_result <- function(required = TRUE) {
  if (file.exists(audit_result_file)) {
    return(readRDS(audit_result_file))
  }
  # Compatibilidade com a importação anterior ao RDS de caminho fixo.
  legacy_pointer <- file.path(audit_processed_dir, "AUDIT_CURRENT_RUN.txt")
  if (file.exists(legacy_pointer)) {
    legacy_run_id <- trimws(readLines(legacy_pointer, n = 1, warn = FALSE))
    if (length(legacy_run_id) == 1L && !is.na(legacy_run_id) &&
        grepl("^[0-9]{8}_[0-9]{6}$", legacy_run_id)) {
      legacy_import_file <- file.path(audit_processed_dir, legacy_run_id, "01_import_log.rds")
      if (file.exists(legacy_import_file)) {
        result <- audit_new_result(legacy_run_id)
        result$importacao <- readRDS(legacy_import_file)
        return(result)
      }
    }
  }
  if (required) {
    stop("Importação da rodada não encontrada. Execute 01_importar_exports_brutos.R.", call. = FALSE)
  }
  NULL
}

audit_save_result <- function(result) {
  result$metadata$updated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  saveRDS(result, audit_result_file, compress = "gzip")
  invisible(result)
}

audit_same_inputs <- function(previous_manifest, current_manifest) {
  required_columns <- c("source_file", "md5")
  if (!is.data.frame(previous_manifest) ||
      !all(required_columns %in% names(previous_manifest)) ||
      nrow(previous_manifest) != nrow(current_manifest)) {
    return(FALSE)
  }
  previous <- previous_manifest[order(previous_manifest$source_file), required_columns]
  current <- current_manifest[order(current_manifest$source_file), required_columns]
  !anyNA(previous$md5) && !anyNA(current$md5) &&
    identical(as.character(previous$source_file), as.character(current$source_file)) &&
    identical(as.character(previous$md5), as.character(current$md5))
}


#----- funções da auditoria

audit_function_files <- if (dir.exists(audit_functions_dir)) {
  fs::dir_ls(audit_functions_dir, glob = "*.R", type = "file")
} else {
  character()
}

if (length(audit_function_files) > 0) {
  invisible(
    lapply(
      as.character(audit_function_files),
      source,
      encoding = "UTF-8"
    )
  )
}


message("Ambiente da auditoria configurado.")
