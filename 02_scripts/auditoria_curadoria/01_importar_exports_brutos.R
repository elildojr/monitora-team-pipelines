# ============================================================
# 01_importar_exports_brutos.R
#
# Importação dos exports brutos do Wildlife Insights
# para auditoria e curadoria.
#
# Princípios:
# - nenhum registro é filtrado;
# - nenhuma coordenada ou timestamp é convertido nesta etapa;
# - nenhum image_id é removido ou deduplicado;
# - os arquivos brutos originais permanecem intocados;
# - cada execução cria uma rodada de auditoria independente;
# - as correções serão feitassempre no Wildlife Insights.
#
# Atenção:
# O diretório 01_entrada/dados_brutos deve conter apenas um
# conjunto ativo de exports do Wildlife Insights por vez.
# ============================================================


#----- setup exclusivo da auditoria

source(
  "02_scripts/auditoria_curadoria/00_setup_curadoria.R",
  encoding = "UTF-8"
)


#----- função local para validar colunas obrigatórias

assert_required_columns <- function(
    data,
    required_columns,
    data_name
) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "O arquivo '", data_name,
        "' não contém as seguintes colunas obrigatórias: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


#----- definir arquivos de entrada

projects_file <- file.path(
  audit_raw_dir,
  "projects.csv"
)

cameras_file <- file.path(
  audit_raw_dir,
  "cameras.csv"
)

deployments_file <- file.path(
  audit_raw_dir,
  "deployments.csv"
)

required_input_files <- c(
  projects_file,
  cameras_file,
  deployments_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0) {
  stop(
    paste0(
      "Os seguintes arquivos obrigatórios não foram encontrados:\n",
      paste(missing_input_files, collapse = "\n")
    ),
    call. = FALSE
  )
}

images_files <- list.files(
  path = audit_raw_dir,
  pattern = "^images_.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
) |>
  sort()

if (length(images_files) == 0) {
  stop(
    paste0(
      "Nenhum arquivo 'images_*.csv' foi encontrado em:\n",
      audit_raw_dir
    ),
    call. = FALSE
  )
}


#----- criar identificador desta rodada de auditoria

audit_run_id <- format(
  Sys.time(),
  format = "%Y%m%d_%H%M%S",
  tz = "UTC"
)

audit_run_processed_dir <- file.path(
  audit_processed_dir,
  audit_run_id
)

audit_run_tables_dir <- file.path(
  audit_tables_dir,
  audit_run_id
)

if (
  dir.exists(audit_run_processed_dir) ||
  dir.exists(audit_run_tables_dir)
) {
  stop(
    paste0(
      "Já existe uma rodada com o identificador ",
      audit_run_id,
      ". Execute novamente para gerar um novo identificador."
    ),
    call. = FALSE
  )
}

dir.create(
  audit_run_processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  audit_run_tables_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


#----- registrar proveniência dos arquivos importados

audit_input_files <- c(
  projects_file,
  cameras_file,
  deployments_file,
  as.character(images_files)
)

audit_input_manifest <- tibble(
  source_path = audit_input_files,
  source_file = basename(audit_input_files),
  size_bytes = as.numeric(
    file.info(audit_input_files)$size
  ),
  modified_at = format(
    file.info(audit_input_files)$mtime,
    tz = "UTC",
    usetz = TRUE
  ),
  md5 = unname(
    tools::md5sum(audit_input_files)
  )
)


#----- definir campos necessários à auditoria

projects_required_columns <- c(
  "project_id",
  "project_short_name"
)

cameras_required_columns <- c(
  "project_id",
  "camera_id",
  "camera_name"
)

deployments_required_columns <- c(
  "project_id",
  "deployment_id",
  "placename",
  "longitude",
  "latitude",
  "start_date",
  "end_date",
  "camera_id",
  "camera_name",
  "camera_functioning",
  "subproject_name",
  "event_name"
)

image_audit_columns <- c(
  "project_id",
  "deployment_id",
  "image_id",
  "filename",
  "location",
  "is_blank",
  "identified_by",
  "wi_taxon_id",
  "class",
  "order",
  "family",
  "genus",
  "species",
  "common_name",
  "uncertainty",
  "timestamp",
  "number_of_objects",
  "cv_confidence"
)


#----- importar projects.csv sem converter campos

projects_raw <- readr::read_csv(
  projects_file,
  col_types = readr::cols(
    .default = readr::col_character()
  ),
  na = character(),
  show_col_types = FALSE,
  progress = interactive(),
  name_repair = "minimal"
)

projects_parse_problems <- readr::problems(
  projects_raw
)

assert_required_columns(
  projects_raw,
  projects_required_columns,
  "projects.csv"
)


#----- importar cameras.csv sem converter campos

cameras_raw <- readr::read_csv(
  cameras_file,
  col_types = readr::cols(
    .default = readr::col_character()
  ),
  na = character(),
  show_col_types = FALSE,
  progress = interactive(),
  name_repair = "minimal"
)

cameras_parse_problems <- readr::problems(
  cameras_raw
)

assert_required_columns(
  cameras_raw,
  cameras_required_columns,
  "cameras.csv"
)


#----- importar deployments.csv sem converter campos

deployments_raw <- readr::read_csv(
  deployments_file,
  col_types = readr::cols(
    .default = readr::col_character()
  ),
  na = character(),
  show_col_types = FALSE,
  progress = interactive(),
  name_repair = "minimal"
)

deployments_parse_problems <- readr::problems(
  deployments_raw
)

assert_required_columns(
  deployments_raw,
  deployments_required_columns,
  "deployments.csv"
)


#----- verificar o esquema de todos os arquivos de imagem

image_headers <- purrr::map(
  as.character(images_files),
  function(image_file) {
    names(
      readr::read_csv(
        image_file,
        n_max = 0,
        col_types = readr::cols(
          .default = readr::col_character()
        ),
        na = character(),
        show_col_types = FALSE,
        name_repair = "minimal"
      )
    )
  }
)

reference_image_header <- image_headers[[1]]

image_schema_audit <- tibble(
  source_path = as.character(images_files),
  source_file = fs::path_file(images_files),
  columns = image_headers
) |>
  mutate(
    n_columns = lengths(columns),
    missing_required_columns = purrr::map(
      columns,
      function(header) {
        setdiff(
          image_audit_columns,
          header
        )
      }
    ),
    matches_first_schema = purrr::map_lgl(
      columns,
      function(header) {
        identical(
          header,
          reference_image_header
        )
      }
    )
  ) |>
  select(-columns)

schema_errors <- image_schema_audit |>
  filter(
    lengths(missing_required_columns) > 0
  )

if (nrow(schema_errors) > 0) {
  
  schema_details <- paste(
    paste0(
      schema_errors$source_file,
      ": ",
      purrr::map_chr(
        schema_errors$missing_required_columns,
        function(x) {
          paste(x, collapse = ", ")
        }
      )
    ),
    collapse = "\n"
  )
  
  stop(
    paste0(
      "Há arquivos de imagem sem colunas necessárias ",
      "para a auditoria:\n",
      schema_details
    ),
    call. = FALSE
  )
}


#----- importar todas as linhas dos arquivos images_*.csv
#
# timestamp permanece como texto.
# Não há conversão de fuso horário.
# Nenhum blank, registro de Computer Vision ou táxon incompleto
# é excluído nesta etapa.

images_import <- readr::read_csv(
  file = as.character(images_files),
  id = "source_path",
  col_select = tidyselect::all_of(
    image_audit_columns
  ),
  col_types = readr::cols(
    .default = readr::col_character()
  ),
  na = character(),
  show_col_types = FALSE,
  progress = interactive(),
  name_repair = "minimal"
)

images_parse_problems <- readr::problems(
  images_import
)

images_raw <- images_import |>
  mutate(
    source_file = fs::path_file(source_path)
  ) |>
  group_by(source_file) |>
  mutate(
    source_record = row_number()
  ) |>
  ungroup() |>
  mutate(
    audit_record_id = stringr::str_c(
      source_file,
      "::",
      source_record
    )
  ) |>
  select(-source_path) |>
  relocate(
    audit_record_id,
    source_file,
    source_record
  )

rm(images_import)

invisible(gc())


#----- verificações estruturais sem alterar registros

project_keys <- projects_raw |>
  distinct(project_id)

camera_keys <- cameras_raw |>
  filter(
    !is.na(camera_id),
    stringr::str_trim(camera_id) != ""
  ) |>
  distinct(
    project_id,
    camera_id
  )

deployment_keys <- deployments_raw |>
  distinct(
    project_id,
    deployment_id
  )

image_deployment_keys <- images_raw |>
  distinct(
    project_id,
    deployment_id
  )

deployment_camera_keys <- deployments_raw |>
  filter(
    !is.na(camera_id),
    stringr::str_trim(camera_id) != ""
  ) |>
  distinct(
    project_id,
    camera_id
  )

deployment_key_multiplicity <- deployments_raw |>
  count(
    project_id,
    deployment_id,
    name = "n_rows"
  ) |>
  filter(n_rows > 1) |>
  arrange(
    project_id,
    deployment_id
  )

deployment_duplicate_field_differences <- deployments_raw |>
  semi_join(
    deployment_key_multiplicity,
    by = c("project_id", "deployment_id")
  ) |>
  group_by(
    project_id,
    deployment_id
  ) |>
  summarise(
    across(
      everything(),
      function(x) n_distinct(x, na.rm = FALSE)
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(project_id, deployment_id),
    names_to = "field",
    values_to = "n_distinct_values"
  ) |>
  filter(n_distinct_values > 1) |>
  arrange(
    project_id,
    deployment_id,
    field
  )

deployments_without_project <- deployment_keys |>
  anti_join(
    project_keys,
    by = "project_id"
  )

cameras_without_project <- cameras_raw |>
  anti_join(
    project_keys,
    by = "project_id"
  )

image_deployments_not_found <- image_deployment_keys |>
  anti_join(
    deployment_keys,
    by = c(
      "project_id",
      "deployment_id"
    )
  )

deployments_without_images <- deployment_keys |>
  anti_join(
    image_deployment_keys,
    by = c(
      "project_id",
      "deployment_id"
    )
  )

deployment_camera_keys_not_found <- deployment_camera_keys |>
  anti_join(
    camera_keys,
    by = c(
      "project_id",
      "camera_id"
    )
  )

deployments_without_camera_id <- deployments_raw |>
  filter(
    is.na(camera_id) |
      stringr::str_trim(camera_id) == ""
  ) |>
  select(
    project_id,
    deployment_id,
    placename,
    camera_id,
    camera_name
  )


#----- resumo da importação, sem filtros

n_unique_image_ids <- n_distinct(
  na_if(
    stringr::str_trim(images_raw$image_id),
    ""
  ),
  na.rm = TRUE
)

import_summary <- tibble(
  metric = c(
    "n_project_rows",
    "n_unique_projects",
    "n_camera_rows",
    "n_unique_project_camera_keys_non_empty",
    "n_deployment_rows",
    "n_unique_project_deployment_keys",
    "n_image_records",
    "n_unique_image_ids",
    "n_blank_records",
    "n_computer_vision_records",
    "n_records_without_genus",
    "n_records_without_image_id",
    "n_records_without_timestamp",
    "n_deployment_keys_with_multiple_rows",
    "n_deployment_duplicate_metadata_fields",
    "n_deployment_keys_without_project",
    "n_camera_rows_without_project",
    "n_image_deployment_keys_not_found",
    "n_deployment_keys_without_images",
    "n_deployment_rows_without_camera_id",
    "n_deployment_camera_keys_not_found",
    "n_project_parse_problems",
    "n_camera_parse_problems",
    "n_deployment_parse_problems",
    "n_image_parse_problems"
  ),
  value = c(
    nrow(projects_raw),
    nrow(project_keys),
    nrow(cameras_raw),
    nrow(camera_keys),
    nrow(deployments_raw),
    nrow(deployment_keys),
    nrow(images_raw),
    n_unique_image_ids,
    sum(
      stringr::str_to_lower(
        stringr::str_trim(images_raw$is_blank)
      ) %in% c("1", "true"),
      na.rm = TRUE
    ),
    sum(
      stringr::str_to_lower(
        stringr::str_trim(images_raw$identified_by)
      ) == "computer vision",
      na.rm = TRUE
    ),
    sum(
      is.na(images_raw$genus) |
        stringr::str_trim(images_raw$genus) == "",
      na.rm = TRUE
    ),
    sum(
      is.na(images_raw$image_id) |
        stringr::str_trim(images_raw$image_id) == "",
      na.rm = TRUE
    ),
    sum(
      is.na(images_raw$timestamp) |
        stringr::str_trim(images_raw$timestamp) == "",
      na.rm = TRUE
    ),
    nrow(deployment_key_multiplicity),
    nrow(deployment_duplicate_field_differences),
    nrow(deployments_without_project),
    nrow(cameras_without_project),
    nrow(image_deployments_not_found),
    nrow(deployments_without_images),
    nrow(deployments_without_camera_id),
    nrow(deployment_camera_keys_not_found),
    nrow(projects_parse_problems),
    nrow(cameras_parse_problems),
    nrow(deployments_parse_problems),
    nrow(images_parse_problems)
  )
)


#----- criar objetos de saída da auditoria

audit_raw_exports <- list(
  metadata = list(
    audit_run_id = audit_run_id,
    imported_at_utc = format(
      Sys.time(),
      tz = "UTC",
      usetz = TRUE
    ),
    source = "Wildlife Insights raw exports",
    image_filtering = "none",
    temporal_independence_filter = "not applied",
    timestamp_handling = "imported as character; no timezone conversion",
    coordinate_handling = "imported as character; no coordinate conversion",
    image_columns_imported = image_audit_columns,
    note = paste(
      "The original CSV files remain the complete source.",
      "The R object preserves all records and the fields required",
      "for spatial, temporal and taxonomic auditing."
    )
  ),
  projects = projects_raw,
  cameras = cameras_raw,
  deployments = deployments_raw,
  images = images_raw
)

audit_import_log <- list(
  audit_run_id = audit_run_id,
  package_versions = audit_package_versions,
  input_manifest = audit_input_manifest,
  image_schema = image_schema_audit,
  import_summary = import_summary,
  parse_problems = list(
    projects = projects_parse_problems,
    cameras = cameras_parse_problems,
    deployments = deployments_parse_problems,
    images = images_parse_problems
  ),
  structural_checks = list(
    deployment_key_multiplicity =
      deployment_key_multiplicity,
    deployment_duplicate_field_differences =
      deployment_duplicate_field_differences,
    deployments_without_project =
      deployments_without_project,
    cameras_without_project =
      cameras_without_project,
    image_deployments_not_found =
      image_deployments_not_found,
    deployments_without_images =
      deployments_without_images,
    deployments_without_camera_id =
      deployments_without_camera_id,
    deployment_camera_keys_not_found =
      deployment_camera_keys_not_found
  )
)


#----- salvar resultados desta rodada

audit_raw_exports_file <- file.path(
  audit_run_processed_dir,
  "audit_raw_exports.rds"
)

audit_import_log_file <- file.path(
  audit_run_processed_dir,
  "01_import_log.rds"
)

saveRDS(
  audit_raw_exports,
  audit_raw_exports_file,
  compress = "gzip"
)

saveRDS(
  audit_import_log,
  audit_import_log_file,
  compress = "gzip"
)

readr::write_csv(
  audit_input_manifest,
  file.path(
    audit_run_tables_dir,
    "01_input_manifest.csv"
  )
)

readr::write_csv(
  import_summary,
  file.path(
    audit_run_tables_dir,
    "01_import_summary.csv"
  )
)

readr::write_csv(
  image_schema_audit,
  file.path(
    audit_run_tables_dir,
    "01_image_schema_audit.csv"
  )
)

readr::write_csv(
  deployment_key_multiplicity,
  file.path(
    audit_run_tables_dir,
    "01_deployments_with_multiple_rows.csv"
  )
)

readr::write_csv(
  deployment_duplicate_field_differences,
  file.path(
    audit_run_tables_dir,
    "01_deployment_duplicate_field_differences.csv"
  )
)

readr::write_csv(
  deployments_without_project,
  file.path(
    audit_run_tables_dir,
    "01_deployment_keys_without_project.csv"
  )
)

readr::write_csv(
  cameras_without_project,
  file.path(
    audit_run_tables_dir,
    "01_camera_rows_without_project.csv"
  )
)

readr::write_csv(
  image_deployments_not_found,
  file.path(
    audit_run_tables_dir,
    "01_image_deployment_keys_not_found.csv"
  )
)

readr::write_csv(
  deployments_without_images,
  file.path(
    audit_run_tables_dir,
    "01_deployment_keys_without_images.csv"
  )
)

readr::write_csv(
  deployments_without_camera_id,
  file.path(
    audit_run_tables_dir,
    "01_deployment_rows_without_camera_id.csv"
  )
)

readr::write_csv(
  deployment_camera_keys_not_found,
  file.path(
    audit_run_tables_dir,
    "01_deployment_camera_keys_not_found.csv"
  )
)

readr::write_csv(
  projects_parse_problems,
  file.path(
    audit_run_tables_dir,
    "01_projects_parse_problems.csv"
  )
)

readr::write_csv(
  cameras_parse_problems,
  file.path(
    audit_run_tables_dir,
    "01_cameras_parse_problems.csv"
  )
)

readr::write_csv(
  deployments_parse_problems,
  file.path(
    audit_run_tables_dir,
    "01_deployments_parse_problems.csv"
  )
)

readr::write_csv(
  images_parse_problems,
  file.path(
    audit_run_tables_dir,
    "01_images_parse_problems.csv"
  )
)


#----- registrar a rodada mais recente após salvar todos os arquivos

audit_current_run_file <- file.path(
  audit_processed_dir,
  "AUDIT_CURRENT_RUN.txt"
)

writeLines(
  audit_run_id,
  audit_current_run_file,
  useBytes = TRUE
)


#----- apresentar resumo no console

message(
  "\nImportação bruta concluída sem aplicação de filtros."
)

print(
  import_summary,
  n = Inf
)

message(
  "\nRodada de auditoria: ",
  audit_run_id
)

message(
  "\nObjeto bruto da auditoria salvo em:\n",
  audit_raw_exports_file
)

message(
  "\nTabelas da importação salvas em:\n",
  audit_run_tables_dir
)