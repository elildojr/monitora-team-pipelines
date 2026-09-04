# ============================================================
# 02_auditar_coordenadas.R
#
# Auditoria espacial dos deployments do Wildlife Insights.
#
# Princípios:
# - não altera coordenadas originais;
# - não exclui deployments;
# - preserva longitude e latitude como texto bruto;
# - cria uma tabela derivada, com uma linha por deployment;
# - gera flags e mapas para revisão no Wildlife Insights.
#
# Limite desta etapa:
# Este script não testa se cada ponto está dentro da respectiva
# Unidade de Conservação, pois isso exigirá polígonos oficiais
# das UCs. Ele verifica integridade, formato, domínio, duplicações
# espaciais e estabilidade das estações entre anos.
# ============================================================


#----- setup exclusivo da auditoria

source(
  "02_scripts/auditoria_curadoria/00_setup_curadoria.R",
  encoding = "UTF-8"
)


#----- parâmetros explícitos da auditoria espacial

coordinate_rounding_digits <- 6

station_shift_review_m <- 30

brazil_longitude_min <- -75
brazil_longitude_max <- -32
brazil_latitude_min <- -35
brazil_latitude_max <- 6


#----- localizar a rodada de auditoria mais recente

audit_current_run_file <- file.path(
  audit_processed_dir,
  "AUDIT_CURRENT_RUN.txt"
)

if (!file.exists(audit_current_run_file)) {
  stop(
    paste0(
      "O arquivo AUDIT_CURRENT_RUN.txt não foi encontrado em:\n",
      audit_processed_dir,
      "\n\nExecute primeiro o script ",
      "01_importar_exports_brutos.R."
    ),
    call. = FALSE
  )
}

audit_run_id <- readLines(
  audit_current_run_file,
  n = 1,
  warn = FALSE
)

if (
  length(audit_run_id) != 1 ||
  is.na(audit_run_id) ||
  stringr::str_trim(audit_run_id) == ""
) {
  stop(
    "AUDIT_CURRENT_RUN.txt não contém um identificador de rodada válido.",
    call. = FALSE
  )
}

audit_run_processed_dir <- file.path(
  audit_processed_dir,
  audit_run_id
)

audit_run_tables_dir <- file.path(
  audit_tables_dir,
  audit_run_id
)

audit_run_figures_dir <- file.path(
  audit_figures_dir,
  audit_run_id
)

audit_import_log_file <- file.path(
  audit_run_processed_dir,
  "01_import_log.rds"
)

if (!file.exists(audit_import_log_file)) {
  stop(
    paste0(
      "O log da importação não foi encontrado:\n",
      audit_import_log_file,
      "\n\nExecute novamente o script ",
      "01_importar_exports_brutos.R."
    ),
    call. = FALSE
  )
}

dir.create(
  audit_run_tables_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  audit_run_figures_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


#----- ler o log da importação

audit_import_log <- readRDS(
  audit_import_log_file
)

if (
  is.null(audit_import_log$input_manifest) ||
  is.null(audit_import_log$audit_run_id)
) {
  stop(
    paste0(
      "O arquivo 01_import_log.rds não possui a estrutura esperada. ",
      "Execute novamente 01_importar_exports_brutos.R."
    ),
    call. = FALSE
  )
}


#----- confirmar que os CSVs atuais são idênticos ao snapshot

spatial_input_files <- c(
  file.path(audit_raw_dir, "projects.csv"),
  file.path(audit_raw_dir, "deployments.csv")
)

missing_spatial_input_files <- spatial_input_files[
  !file.exists(spatial_input_files)
]

if (length(missing_spatial_input_files) > 0) {
  stop(
    paste0(
      "Os seguintes arquivos necessários à auditoria espacial ",
      "não foram encontrados:\n",
      paste(missing_spatial_input_files, collapse = "\n")
    ),
    call. = FALSE
  )
}

expected_file_hashes <- audit_import_log$input_manifest |>
  filter(
    source_file %in% basename(spatial_input_files)
  ) |>
  select(
    source_file,
    expected_md5 = md5
  )

current_file_hashes <- tibble(
  source_file = basename(spatial_input_files),
  current_md5 = unname(
    tools::md5sum(spatial_input_files)
  )
)

snapshot_file_check <- expected_file_hashes |>
  full_join(
    current_file_hashes,
    by = "source_file"
  ) |>
  mutate(
    hashes_match = !is.na(expected_md5) &
      !is.na(current_md5) &
      expected_md5 == current_md5
  )

hash_mismatches <- snapshot_file_check |>
  filter(!hashes_match)

if (nrow(hash_mismatches) > 0) {
  
  hash_details <- paste(
    paste0(
      hash_mismatches$source_file,
      " [hash do snapshot: ",
      hash_mismatches$expected_md5,
      "; hash atual: ",
      hash_mismatches$current_md5,
      "]"
    ),
    collapse = "\n"
  )
  
  stop(
    paste0(
      "Os arquivos CSV atuais não correspondem ao snapshot ",
      "importado na rodada ",
      audit_run_id,
      ".\n\n",
      hash_details,
      "\n\nNão prossiga com a auditoria espacial. ",
      "Execute novamente 01_importar_exports_brutos.R ",
      "para criar uma nova rodada documentada."
    ),
    call. = FALSE
  )
}


#----- funções locais da auditoria espacial

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


normalise_coordinate_text <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_squish(x)
  
  x <- dplyr::na_if(
    x,
    ""
  )
  
  stringr::str_replace_all(
    x,
    ",",
    "."
  )
}


is_decimal_coordinate <- function(x) {
  
  x <- normalise_coordinate_text(x)
  
  !is.na(x) &
    stringr::str_detect(
      x,
      "^[+-]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)$"
    )
}


parse_decimal_coordinate <- function(x) {
  
  x_normalised <- normalise_coordinate_text(x)
  
  valid_format <- is_decimal_coordinate(x_normalised)
  
  output <- rep(
    NA_real_,
    length(x_normalised)
  )
  
  output[valid_format] <- suppressWarnings(
    as.numeric(
      x_normalised[valid_format]
    )
  )
  
  output
}


normalise_placename <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_to_upper(
    stringr::str_squish(x)
  )
  
  x <- stringr::str_replace_all(
    x,
    "_",
    "-"
  )
  
  x <- stringr::str_replace_all(
    x,
    "\\s*-\\s*",
    "-"
  )
  
  dplyr::na_if(
    x,
    ""
  )
}


collapse_unique_values <- function(x) {
  
  x <- as.character(x)
  
  x[
    is.na(x) |
      stringr::str_trim(x) == ""
  ] <- "<MISSING>"
  
  paste(
    sort(unique(x)),
    collapse = " | "
  )
}


inside_brazil_envelope <- function(
    longitude,
    latitude
) {
  
  dplyr::between(
    longitude,
    brazil_longitude_min,
    brazil_longitude_max
  ) &
    dplyr::between(
      latitude,
      brazil_latitude_min,
      brazil_latitude_max
    )
}


max_pairwise_distance_m <- function(
    longitude,
    latitude
) {
  
  coordinates <- tibble(
    longitude = longitude,
    latitude = latitude
  ) |>
    filter(
      !is.na(longitude),
      !is.na(latitude)
    ) |>
    distinct()
  
  if (nrow(coordinates) <= 1) {
    return(0)
  }
  
  points <- sf::st_as_sf(
    coordinates,
    coords = c(
      "longitude",
      "latitude"
    ),
    crs = 4326,
    remove = FALSE
  )
  
  max(
    as.numeric(
      sf::st_distance(points)
    )
  )
}


save_coordinate_map <- function(
    data,
    output_file
) {
  
  if (nrow(data) == 0) {
    return(invisible(FALSE))
  }
  
  coordinate_map <- leaflet::leaflet(
    data = data
  ) |>
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron
    ) |>
    leaflet::addCircleMarkers(
      lng = ~longitude_num,
      lat = ~latitude_num,
      radius = 3,
      stroke = TRUE,
      weight = 1,
      color = ~map_colour,
      fillColor = ~map_colour,
      fillOpacity = 0.85,
      popup = ~popup_text
    )
  
  htmlwidgets::saveWidget(
    coordinate_map,
    file = output_file,
    selfcontained = FALSE
  )
  
  invisible(TRUE)
}


#----- importar apenas projects e deployments do snapshot validado
#
# Não carregamos o RDS completo com milhões de imagens, pois esta
# auditoria utiliza apenas os metadados espaciais. A identidade com
# o snapshot foi confirmada acima pelos hashes MD5.

projects_file <- file.path(
  audit_raw_dir,
  "projects.csv"
)

deployments_file <- file.path(
  audit_raw_dir,
  "deployments.csv"
)

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

projects_required_columns <- c(
  "project_id",
  "project_short_name"
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
  "camera_name"
)

assert_required_columns(
  projects_raw,
  projects_required_columns,
  "projects.csv"
)

assert_required_columns(
  deployments_raw,
  deployments_required_columns,
  "deployments.csv"
)


#----- preparar contexto dos projetos

project_context <- projects_raw |>
  select(
    project_id,
    project_short_name
  )

if ("country_code" %in% names(projects_raw)) {
  project_context <- projects_raw |>
    select(
      project_id,
      project_short_name,
      country_code
    )
} else {
  project_context <- project_context |>
    mutate(
      country_code = NA_character_
    )
}

project_context <- project_context |>
  distinct(
    project_id,
    .keep_all = TRUE
  )


#----- criar tabela espacial derivada por deployment
#
# Os dados brutos em deployments_raw não são alterados. Esta tabela
# apenas consolida as linhas repetidas do mesmo deployment e registra
# explicitamente eventuais conflitos entre os metadados.

deployment_coordinate_audit <- deployments_raw |>
  mutate(
    placename_key = normalise_placename(placename),
    deployment_year = stringr::str_extract(
      start_date,
      "\\b(?:19|20)\\d{2}\\b"
    )
  ) |>
  group_by(
    project_id,
    deployment_id
  ) |>
  summarise(
    n_deployment_rows = n(),
    
    placename_raw = first(placename),
    placename_raw_values = collapse_unique_values(placename),
    placename_key = first(placename_key),
    n_distinct_placename_raw = n_distinct(
      placename,
      na.rm = FALSE
    ),
    
    longitude_raw = first(longitude),
    longitude_raw_values = collapse_unique_values(longitude),
    n_distinct_longitude_raw = n_distinct(
      longitude,
      na.rm = FALSE
    ),
    
    latitude_raw = first(latitude),
    latitude_raw_values = collapse_unique_values(latitude),
    n_distinct_latitude_raw = n_distinct(
      latitude,
      na.rm = FALSE
    ),
    
    start_date_raw = first(start_date),
    start_date_raw_values = collapse_unique_values(start_date),
    end_date_raw = first(end_date),
    end_date_raw_values = collapse_unique_values(end_date),
    deployment_year = first(deployment_year),
    
    camera_id_raw_values = collapse_unique_values(camera_id),
    camera_name_raw_values = collapse_unique_values(camera_name),
    n_distinct_camera_id = n_distinct(
      camera_id,
      na.rm = FALSE
    ),
    n_distinct_camera_name = n_distinct(
      camera_name,
      na.rm = FALSE
    ),
    
    .groups = "drop"
  ) |>
  left_join(
    project_context,
    by = "project_id"
  ) |>
  mutate(
    duplicate_deployment_metadata =
      n_deployment_rows > 1,
    
    camera_metadata_conflict =
      n_distinct_camera_id > 1 |
      n_distinct_camera_name > 1,
    
    coordinate_values_conflict =
      n_distinct_longitude_raw > 1 |
      n_distinct_latitude_raw > 1
  )


#----- validar formato e domínio das coordenadas

deployment_coordinate_audit <- deployment_coordinate_audit |>
  mutate(
    longitude_clean_text = normalise_coordinate_text(
      longitude_raw
    ),
    latitude_clean_text = normalise_coordinate_text(
      latitude_raw
    ),
    
    longitude_format_valid = is_decimal_coordinate(
      longitude_raw
    ),
    latitude_format_valid = is_decimal_coordinate(
      latitude_raw
    ),
    
    longitude_num = parse_decimal_coordinate(
      longitude_raw
    ),
    latitude_num = parse_decimal_coordinate(
      latitude_raw
    ),
    
    is_brazil_project = dplyr::coalesce(
      stringr::str_to_upper(country_code) == "BRA",
      FALSE
    ),
    
    flag_missing_placename =
      is.na(placename_raw) |
      stringr::str_trim(placename_raw) == "",
    
    flag_missing_coordinate =
      is.na(longitude_clean_text) |
      is.na(latitude_clean_text),
    
    flag_invalid_decimal_format =
      !flag_missing_coordinate &
      (
        !longitude_format_valid |
          !latitude_format_valid
      ),
    
    flag_coordinate_values_conflict =
      coordinate_values_conflict,
    
    flag_outside_world_range =
      !flag_missing_coordinate &
      !flag_invalid_decimal_format &
      (
        !dplyr::between(
          longitude_num,
          -180,
          180
        ) |
          !dplyr::between(
            latitude_num,
            -90,
            90
          )
      ),
    
    coordinate_inside_brazil_envelope =
      inside_brazil_envelope(
        longitude_num,
        latitude_num
      ),
    
    flag_possible_latitude_longitude_swap =
      dplyr::coalesce(
        is_brazil_project &
          !flag_missing_coordinate &
          !flag_invalid_decimal_format &
          !coordinate_inside_brazil_envelope &
          inside_brazil_envelope(
            latitude_num,
            longitude_num
          ),
        FALSE
      ),
    
    flag_outside_brazil_envelope =
      dplyr::coalesce(
        is_brazil_project &
          !flag_missing_coordinate &
          !flag_invalid_decimal_format &
          !flag_outside_world_range &
          !coordinate_inside_brazil_envelope,
        FALSE
      ),
    
    valid_coordinate_for_geometry =
      !flag_missing_coordinate &
      !flag_invalid_decimal_format &
      !flag_outside_world_range &
      !flag_coordinate_values_conflict,
    
    coordinate_key = dplyr::if_else(
      valid_coordinate_for_geometry,
      stringr::str_c(
        sprintf(
          paste0(
            "%.",
            coordinate_rounding_digits,
            "f"
          ),
          longitude_num
        ),
        "_",
        sprintf(
          paste0(
            "%.",
            coordinate_rounding_digits,
            "f"
          ),
          latitude_num
        )
      ),
      NA_character_
    )
  )


#----- identificar coordenadas compartilhadas por placenames distintos

shared_coordinate_summary <- deployment_coordinate_audit |>
  filter(
    valid_coordinate_for_geometry,
    !is.na(coordinate_key)
  ) |>
  group_by(
    project_id,
    coordinate_key
  ) |>
  summarise(
    n_deployments = n(),
    n_distinct_placenames = n_distinct(
      dplyr::na_if(
        placename_key,
        ""
      ),
      na.rm = TRUE
    ),
    deployment_ids = collapse_unique_values(
      deployment_id
    ),
    placenames = collapse_unique_values(
      placename_raw
    ),
    longitude_num = first(longitude_num),
    latitude_num = first(latitude_num),
    .groups = "drop"
  ) |>
  arrange(
    desc(n_deployments),
    project_id,
    coordinate_key
  )

shared_coordinates_different_placenames <- shared_coordinate_summary |>
  filter(n_distinct_placenames > 1)

shared_coordinate_flags <- shared_coordinates_different_placenames |>
  transmute(
    project_id,
    coordinate_key,
    flag_shared_coordinates_different_placenames = TRUE
  )


#----- avaliar estabilidade espacial de cada estação entre anos

station_coordinate_history <- deployment_coordinate_audit |>
  filter(
    valid_coordinate_for_geometry,
    !is.na(placename_key)
  ) |>
  group_by(
    project_id,
    placename_key
  ) |>
  summarise(
    n_deployments = n(),
    n_years = n_distinct(
      deployment_year,
      na.rm = TRUE
    ),
    n_unique_coordinate_keys = n_distinct(
      coordinate_key
    ),
    max_station_coordinate_distance_m =
      max_pairwise_distance_m(
        longitude_num,
        latitude_num
      ),
    deployment_ids = collapse_unique_values(
      deployment_id
    ),
    coordinate_keys = collapse_unique_values(
      coordinate_key
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(max_station_coordinate_distance_m),
    project_id,
    placename_key
  )

station_coordinate_changes_over_threshold <- station_coordinate_history |>
  filter(
    n_unique_coordinate_keys > 1,
    max_station_coordinate_distance_m >
      station_shift_review_m
  )

station_history_for_join <- station_coordinate_history |>
  transmute(
    project_id,
    placename_key,
    station_n_deployments = n_deployments,
    station_n_years = n_years,
    station_n_unique_coordinate_keys =
      n_unique_coordinate_keys,
    station_max_coordinate_distance_m =
      max_station_coordinate_distance_m
  )


#----- reunir flags espaciais no deployment

deployment_coordinate_audit <- deployment_coordinate_audit |>
  left_join(
    shared_coordinate_flags,
    by = c(
      "project_id",
      "coordinate_key"
    )
  ) |>
  left_join(
    station_history_for_join,
    by = c(
      "project_id",
      "placename_key"
    )
  ) |>
  mutate(
    flag_shared_coordinates_different_placenames =
      dplyr::coalesce(
        flag_shared_coordinates_different_placenames,
        FALSE
      ),
    
    flag_station_shift_over_threshold =
      dplyr::coalesce(
        station_n_unique_coordinate_keys > 1 &
          station_max_coordinate_distance_m >
          station_shift_review_m,
        FALSE
      )
  )


#----- definir status e tabela longa de flags

coordinate_flag_columns <- c(
  "flag_missing_placename",
  "flag_missing_coordinate",
  "flag_invalid_decimal_format",
  "flag_coordinate_values_conflict",
  "flag_outside_world_range",
  "flag_possible_latitude_longitude_swap",
  "flag_outside_brazil_envelope",
  "flag_shared_coordinates_different_placenames",
  "flag_station_shift_over_threshold"
)

deployment_coordinate_audit <- deployment_coordinate_audit |>
  mutate(
    across(
      all_of(coordinate_flag_columns),
      function(x) dplyr::coalesce(
        x,
        FALSE
      )
    ),
    
    coordinate_flag_count = rowSums(
      across(
        all_of(coordinate_flag_columns)
      )
    ),
    
    coordinate_status = dplyr::if_else(
      coordinate_flag_count == 0,
      "OK",
      "REVIEW"
    )
  )

coordinate_flag_dictionary <- tibble(
  flag_variable = coordinate_flag_columns,
  flag_code = c(
    "MISSING_PLACENAME",
    "MISSING_COORDINATE",
    "INVALID_DECIMAL_FORMAT",
    "MULTIPLE_COORDINATE_VALUES",
    "OUTSIDE_WORLD_RANGE",
    "POSSIBLE_LATITUDE_LONGITUDE_SWAP",
    "OUTSIDE_BRAZIL_ENVELOPE",
    "SHARED_COORDINATES_DIFFERENT_PLACENAMES",
    "STATION_SHIFT_OVER_30_M"
  ),
  flag_description = c(
    "Deployment sem placename.",
    "Longitude e/ou latitude ausente.",
    "Longitude e/ou latitude não está em graus decimais.",
    "Mesmo deployment possui mais de um valor bruto de coordenada.",
    "Longitude e/ou latitude fora do domínio geográfico global.",
    "Valores podem corresponder a latitude e longitude invertidas.",
    "Projeto marcado como BRA, mas coordenada está fora do envelope territorial brasileiro.",
    "Coordenada compartilhada por dois ou mais placenames no mesmo projeto.",
    "Mesmo placename apresentou deslocamento máximo superior a 30 m entre deployments."
  )
)

coordinate_flags <- deployment_coordinate_audit |>
  select(
    project_id,
    project_short_name,
    country_code,
    deployment_id,
    placename_raw,
    placename_key,
    deployment_year,
    longitude_raw_values,
    latitude_raw_values,
    longitude_num,
    latitude_num,
    coordinate_status,
    all_of(coordinate_flag_columns)
  ) |>
  pivot_longer(
    cols = all_of(coordinate_flag_columns),
    names_to = "flag_variable",
    values_to = "flag_value"
  ) |>
  filter(flag_value) |>
  left_join(
    coordinate_flag_dictionary,
    by = "flag_variable"
  ) |>
  select(-flag_value) |>
  arrange(
    project_id,
    deployment_id,
    flag_code
  )

coordinate_flag_reasons <- coordinate_flags |>
  group_by(
    project_id,
    deployment_id
  ) |>
  summarise(
    coordinate_flag_codes = paste(
      flag_code,
      collapse = " | "
    ),
    coordinate_flag_descriptions = paste(
      flag_description,
      collapse = " | "
    ),
    .groups = "drop"
  )

deployment_coordinate_audit <- deployment_coordinate_audit |>
  left_join(
    coordinate_flag_reasons,
    by = c(
      "project_id",
      "deployment_id"
    )
  ) |>
  mutate(
    coordinate_flag_codes = dplyr::coalesce(
      coordinate_flag_codes,
      ""
    ),
    coordinate_flag_descriptions = dplyr::coalesce(
      coordinate_flag_descriptions,
      ""
    )
  )

coordinate_flag_summary <- coordinate_flags |>
  count(
    flag_code,
    flag_description,
    name = "n_deployments"
  ) |>
  arrange(
    desc(n_deployments),
    flag_code
  )


#----- síntese da auditoria espacial

coordinate_summary <- tibble(
  metric = c(
    "audit_run_id",
    "n_deployment_rows_raw",
    "n_unique_project_deployment_keys",
    "n_coordinate_audit_records",
    "n_duplicate_deployment_metadata",
    "n_camera_metadata_conflicts",
    "n_missing_placename",
    "n_missing_coordinate",
    "n_invalid_decimal_format",
    "n_multiple_coordinate_values",
    "n_outside_world_range",
    "n_possible_latitude_longitude_swap",
    "n_outside_brazil_envelope",
    "n_shared_coordinates_different_placenames",
    "n_station_shift_over_30_m",
    "n_coordinate_records_for_review",
    "n_total_coordinate_flags",
    "n_valid_coordinate_records_for_map",
    "n_station_coordinate_histories"
  ),
  value = c(
    audit_run_id,
    nrow(deployments_raw),
    n_distinct(
      deployments_raw$project_id,
      deployments_raw$deployment_id
    ),
    nrow(deployment_coordinate_audit),
    sum(
      deployment_coordinate_audit$duplicate_deployment_metadata
    ),
    sum(
      deployment_coordinate_audit$camera_metadata_conflict
    ),
    sum(
      deployment_coordinate_audit$flag_missing_placename
    ),
    sum(
      deployment_coordinate_audit$flag_missing_coordinate
    ),
    sum(
      deployment_coordinate_audit$flag_invalid_decimal_format
    ),
    sum(
      deployment_coordinate_audit$flag_coordinate_values_conflict
    ),
    sum(
      deployment_coordinate_audit$flag_outside_world_range
    ),
    sum(
      deployment_coordinate_audit$
        flag_possible_latitude_longitude_swap
    ),
    sum(
      deployment_coordinate_audit$
        flag_outside_brazil_envelope
    ),
    sum(
      deployment_coordinate_audit$
        flag_shared_coordinates_different_placenames
    ),
    sum(
      deployment_coordinate_audit$
        flag_station_shift_over_threshold
    ),
    sum(
      deployment_coordinate_audit$coordinate_status ==
        "REVIEW"
    ),
    nrow(coordinate_flags),
    sum(
      deployment_coordinate_audit$
        valid_coordinate_for_geometry
    ),
    nrow(station_coordinate_history)
  )
)


#----- produzir mapas de auditoria visual

map_data <- deployment_coordinate_audit |>
  filter(valid_coordinate_for_geometry) |>
  mutate(
    map_colour = dplyr::if_else(
      coordinate_status == "OK",
      "#1B9E77",
      "#D95F02"
    ),
    
    popup_text = paste0(
      "<strong>Projeto:</strong> ",
      project_short_name,
      " (", project_id, ")",
      "<br><strong>Deployment:</strong> ",
      deployment_id,
      "<br><strong>Estação:</strong> ",
      placename_raw,
      "<br><strong>Longitude:</strong> ",
      longitude_num,
      "<br><strong>Latitude:</strong> ",
      latitude_num,
      "<br><strong>Status:</strong> ",
      coordinate_status,
      "<br><strong>Flags:</strong> ",
      dplyr::if_else(
        coordinate_status == "OK",
        "Nenhuma",
        coordinate_flag_codes
      )
    )
  )

map_index <- tibble(
  map_scope = character(),
  project_id = character(),
  project_short_name = character(),
  map_file = character()
)

if (nrow(map_data) > 0) {
  
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    warning(
      paste0(
        "O pacote htmlwidgets não está disponível. ",
        "As tabelas da auditoria foram geradas, mas os mapas HTML não."
      ),
      call. = FALSE
    )
  } else {
    
    all_projects_map_file <- file.path(
      audit_run_figures_dir,
      "02_coordinate_map_all_projects.html"
    )
    
    save_coordinate_map(
      map_data,
      all_projects_map_file
    )
    
    map_index <- bind_rows(
      map_index,
      tibble(
        map_scope = "all_projects",
        project_id = NA_character_,
        project_short_name = NA_character_,
        map_file = all_projects_map_file
      )
    )
    
    project_map_index <- map_data |>
      distinct(
        project_id,
        project_short_name
      ) |>
      mutate(
        project_label = dplyr::if_else(
          is.na(project_short_name) |
            stringr::str_trim(project_short_name) == "",
          project_id,
          project_short_name
        ),
        safe_project_label = stringr::str_replace_all(
          stringr::str_to_lower(project_label),
          "[^a-z0-9]+",
          "_"
        ),
        safe_project_label = stringr::str_replace_all(
          safe_project_label,
          "^_|_$",
          ""
        ),
        map_file = file.path(
          audit_run_figures_dir,
          paste0(
            "02_coordinate_map_project_",
            project_id,
            "_",
            safe_project_label,
            ".html"
          )
        )
      )
    
    for (i in seq_len(nrow(project_map_index))) {
      
      project_id_i <- project_map_index$project_id[i]
      
      project_data <- map_data[
        map_data$project_id == project_id_i,
        ,
        drop = FALSE
      ]
      
      save_coordinate_map(
        project_data,
        project_map_index$map_file[i]
      )
    }
    
    map_index <- bind_rows(
      map_index,
      project_map_index |>
        transmute(
          map_scope = "project",
          project_id,
          project_short_name,
          map_file
        )
    )
  }
}


#----- reunir e salvar resultados da auditoria espacial

coordinate_audit_output <- list(
  metadata = list(
    audit_run_id = audit_run_id,
    created_at_utc = format(
      Sys.time(),
      tz = "UTC",
      usetz = TRUE
    ),
    source = "Wildlife Insights raw exports",
    local_coordinate_correction = "not applied",
    local_record_exclusion = "not applied",
    coordinate_input_handling =
      "longitude and latitude retained as raw character fields",
    brazil_envelope_note = paste(
      "Brazil envelope is a coarse screening rule only.",
      "It does not replace validation against official UC polygons."
    ),
    station_shift_review_threshold_m =
      station_shift_review_m
  ),
  snapshot_file_check = snapshot_file_check,
  coordinate_summary = coordinate_summary,
  coordinate_flag_dictionary =
    coordinate_flag_dictionary,
  deployment_coordinate_audit =
    deployment_coordinate_audit,
  coordinate_flags = coordinate_flags,
  coordinate_flag_summary =
    coordinate_flag_summary,
  shared_coordinate_summary =
    shared_coordinate_summary,
  shared_coordinates_different_placenames =
    shared_coordinates_different_placenames,
  station_coordinate_history =
    station_coordinate_history,
  station_coordinate_changes_over_threshold =
    station_coordinate_changes_over_threshold,
  map_index = map_index
)

coordinate_audit_output_file <- file.path(
  audit_run_processed_dir,
  "02_coordinate_audit.rds"
)

saveRDS(
  coordinate_audit_output,
  coordinate_audit_output_file,
  compress = "gzip"
)

readr::write_csv(
  snapshot_file_check,
  file.path(
    audit_run_tables_dir,
    "02_snapshot_file_check.csv"
  )
)

readr::write_csv(
  coordinate_summary,
  file.path(
    audit_run_tables_dir,
    "02_coordinate_summary.csv"
  )
)

readr::write_csv(
  deployment_coordinate_audit,
  file.path(
    audit_run_tables_dir,
    "02_deployment_coordinate_audit.csv"
  )
)

readr::write_csv(
  coordinate_flags,
  file.path(
    audit_run_tables_dir,
    "02_coordinate_flags.csv"
  )
)

readr::write_csv(
  coordinate_flag_summary,
  file.path(
    audit_run_tables_dir,
    "02_coordinate_flag_summary.csv"
  )
)

readr::write_csv(
  shared_coordinate_summary,
  file.path(
    audit_run_tables_dir,
    "02_shared_coordinate_summary.csv"
  )
)

readr::write_csv(
  shared_coordinates_different_placenames,
  file.path(
    audit_run_tables_dir,
    "02_shared_coordinates_different_placenames.csv"
  )
)

readr::write_csv(
  station_coordinate_history,
  file.path(
    audit_run_tables_dir,
    "02_station_coordinate_history.csv"
  )
)

readr::write_csv(
  station_coordinate_changes_over_threshold,
  file.path(
    audit_run_tables_dir,
    "02_station_coordinate_changes_over_30m.csv"
  )
)

readr::write_csv(
  coordinate_flag_dictionary,
  file.path(
    audit_run_tables_dir,
    "02_coordinate_flag_dictionary.csv"
  )
)

readr::write_csv(
  map_index,
  file.path(
    audit_run_tables_dir,
    "02_coordinate_map_index.csv"
  )
)


#----- apresentar resultados no console

message(
  "\nAuditoria de coordenadas concluída sem correções locais."
)

print(
  coordinate_summary,
  n = Inf
)

if (nrow(coordinate_flag_summary) > 0) {
  
  message("\nResumo das flags espaciais:")
  
  print(
    coordinate_flag_summary,
    n = Inf
  )
}

message(
  "\nObjeto da auditoria espacial salvo em:\n",
  coordinate_audit_output_file
)

message(
  "\nTabelas da auditoria espacial salvas em:\n",
  audit_run_tables_dir
)

if (nrow(map_index) > 0) {
  message(
    "\nMapas de auditoria visual salvos em:\n",
    audit_run_figures_dir
  )
}

message(
  "\nObservação: a verificação de pontos dentro das Unidades ",
  "de Conservação será feita em etapa posterior, com polígonos ",
  "oficiais das UCs."
)





#
list.files(
"03_saida/auditoria_curadoria/tabelas/20260903_195610",
pattern = "^02_",
full.names = FALSE
)


##Para visualizar a tabela q contém somente as 38 ocorrências que precisam ser #interpretadas.
shared_coords <- readr::read_csv(
  "03_saida/auditoria_curadoria/tabelas/20260903_195610/02_shared_coordinates_different_placenames.csv",
  show_col_types = FALSE
)

View(shared_coords)

names(shared_coords)


# para visualizar as combinações de nomes que compartilham a mesma coordenada
shared_coords |>
  dplyr::select(
    project_id,
    longitude_num,
    latitude_num,
    n_deployments,
    n_distinct_placenames,
    placenames,
    deployment_ids
  ) |>
  dplyr::arrange(
    project_id,
    dplyr::desc(n_deployments),
    longitude_num,
    latitude_num
  ) |>
  View()
