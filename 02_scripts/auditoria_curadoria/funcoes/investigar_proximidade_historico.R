# Distâncias entre placenames e referências anuais dos registros.
# Agrupamento: project_id + placename_key.
# As tabelas de períodos e presença são auxiliares e não integram o relatório espacial.

audit_collapse_values <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  paste(sort(unique(x)), collapse = " | ")
}

audit_geo_distance_m <- function(longitude, latitude) {
  lon <- longitude * pi / 180
  lat <- latitude * pi / 180
  haversine <- sin(outer(lat, lat, "-") / 2)^2 +
    outer(cos(lat), cos(lat), "*") * sin(outer(lon, lon, "-") / 2)^2
  haversine <- pmax(0, pmin(1, haversine))
  # Repor a dimensão da matriz após limitar os valores a [0, 1].
  dim(haversine) <- c(length(lon), length(lon))
  2 * 6371008.8 * atan2(sqrt(haversine), sqrt(1 - haversine))
}

audit_parse_iso_day <- function(x) {
  x <- trimws(as.character(x))
  valid_text <- !is.na(x) & grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}([ T][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?)?$",
    x
  )
  result <- as.Date(rep(NA_character_, length(x)))
  result[valid_text] <- suppressWarnings(as.Date(substr(x[valid_text], 1, 10), format = "%Y-%m-%d"))
  roundtrip <- !is.na(result) & format(result, "%Y-%m-%d") == substr(x, 1, 10)
  result[is.na(roundtrip) | !roundtrip] <- as.Date(NA)
  result
}

audit_station_pair_id <- function(project_id, unit_a, unit_b) {
  paste(project_id, unit_a, unit_b, sep = " :: ")
}

audit_station_spatiotemporal <- function(deployments, nearby_threshold_m) {
  required <- c(
    "project_id", "project_short_name", "deployment_id", "placename_raw", "placename_key",
    "longitude_num", "latitude_num", "valid_coordinate_for_geometry",
    "start_date_raw", "end_date_raw", "start_date_raw_values", "end_date_raw_values",
    "camera_id_raw_values", "camera_name_raw_values"
  )
  if (!is.data.frame(deployments) || !all(required %in% names(deployments))) {
    stop("Campos obrigatórios ausentes na tabela de coordenadas.", call. = FALSE)
  }
  if (length(nearby_threshold_m) != 1L || !is.finite(nearby_threshold_m) || nearby_threshold_m <= 0) {
    stop("O limiar de proximidade deve ser um número positivo em metros.", call. = FALSE)
  }
  d <- as.data.frame(deployments, stringsAsFactors = FALSE)
  if (anyDuplicated(paste(d$project_id, d$deployment_id, sep = "\u001f"))) {
    stop("A tabela de coordenadas deve conter uma linha por project_id/deployment_id.", call. = FALSE)
  }
  d$project_id <- as.character(d$project_id)
  d$placename_key <- as.character(d$placename_key)
  unit_valid <- !is.na(d$placename_key) & nzchar(trimws(d$placename_key))
  start_day <- audit_parse_iso_day(d$start_date_raw)
  end_day <- audit_parse_iso_day(d$end_date_raw)
  start_conflict <- grepl(" | ", d$start_date_raw_values, fixed = TRUE)
  end_conflict <- grepl(" | ", d$end_date_raw_values, fixed = TRUE)
  start_day[start_conflict] <- as.Date(NA)
  end_day[end_conflict] <- as.Date(NA)
  inverted <- !is.na(start_day) & !is.na(end_day) & end_day < start_day
  end_day[inverted] <- as.Date(NA)
  start_year <- as.integer(format(start_day, "%Y"))
  period_valid <- !is.na(start_day) & !is.na(end_day)
  issue_codes <- vapply(seq_len(nrow(d)), function(i) {
    codes <- character()
    if (!unit_valid[i]) codes <- c(codes, "UNIDADE_SEM_PLACENAME")
    if (start_conflict[i]) codes <- c(codes, "INICIOS_CONFLITANTES")
    else if (is.na(start_day[i])) codes <- c(codes, "INICIO_AUSENTE_OU_NAO_INTERPRETAVEL")
    if (end_conflict[i]) codes <- c(codes, "FINS_CONFLITANTES")
    else if (inverted[i]) codes <- c(codes, "FIM_ANTERIOR_AO_INICIO")
    else if (is.na(end_day[i])) codes <- c(codes, "FIM_AUSENTE_OU_NAO_INTERPRETAVEL")
    paste(codes, collapse = " | ")
  }, character(1))
  period_audit <- data.frame(
    project_id = d$project_id, project_short_name = d$project_short_name,
    deployment_id = d$deployment_id, placename = d$placename_raw,
    placename_key = d$placename_key, start_date_raw = d$start_date_raw,
    end_date_raw = d$end_date_raw, start_day = as.character(start_day),
    end_day = as.character(end_day), start_year = start_year,
    complete_period_valid = period_valid, issue_codes = issue_codes,
    stringsAsFactors = FALSE
  )

  # Referência anual da comparação espacial: anos abrangidos pelo período informado.
  # Sem término interpretável, utiliza-se apenas o ano de início.
  operation_parts <- lapply(which(unit_valid & !is.na(start_year)), function(i) {
    years <- if (period_valid[i]) {
      seq.int(start_year[i], as.integer(format(end_day[i], "%Y")))
    } else {
      start_year[i]
    }
    data.frame(row_index = i, project_id = d$project_id[i],
               placename_key = d$placename_key[i], operation_year = years,
               stringsAsFactors = FALSE)
  })
  operation_rows <- if (length(operation_parts)) do.call(rbind, operation_parts) else {
    data.frame(row_index = integer(), project_id = character(), placename_key = character(), operation_year = integer())
  }

  # Tabelas auxiliares de presença por ano de início; comparação nos anos com dados do projeto.
  history_rows <- list()
  presence_rows <- list()
  coverage_rows <- list()
  for (project in sort(unique(d$project_id))) {
    project_indices <- which(d$project_id == project)
    project_name <- d$project_short_name[project_indices[1]]
    project_years <- sort(unique(start_year[project_indices][!is.na(start_year[project_indices])]))
    if (length(project_years)) {
      for (year in seq.int(min(project_years), max(project_years))) {
        year_indices <- project_indices[which(start_year[project_indices] == year)]
        coverage_rows[[length(coverage_rows) + 1L]] <- data.frame(
          project_id = project, project_short_name = project_name, year = year,
          n_deployments_started = length(year_indices),
          n_units_observed = length(unique(d$placename_key[year_indices][unit_valid[year_indices]])),
          project_has_records = length(year_indices) > 0,
          stringsAsFactors = FALSE
        )
      }
    }
    for (unit in sort(unique(d$placename_key[project_indices][unit_valid[project_indices]]))) {
      unit_indices <- project_indices[d$placename_key[project_indices] == unit & unit_valid[project_indices]]
      # Descartar índices indeterminados por placename ausente.
      unit_indices <- unit_indices[!is.na(unit_indices)]
      years <- sort(unique(start_year[unit_indices][!is.na(start_year[unit_indices])]))
      first_year <- if (length(years)) min(years) else NA_integer_
      last_year <- if (length(years)) max(years) else NA_integer_
      first_project_year <- if (length(project_years)) min(project_years) else NA_integer_
      last_project_year <- if (length(project_years)) max(project_years) else NA_integer_
      gap_years <- if (length(years)) setdiff(project_years[project_years > first_year & project_years < last_year], years) else integer()
      later_years <- if (length(years)) project_years[project_years > last_year] else integer()
      positions <- match(years, project_years)
      return_years <- if (length(years) > 1L) years[-1L][diff(positions) > 1L] else integer()
      codes <- character()
      if (!length(years)) codes <- c(codes, "SEM_ANO_DE_INICIO_VALIDO")
      if (length(gap_years)) codes <- c(codes, "LACUNA_COM_REAPARECIMENTO")
      if (length(later_years)) codes <- c(codes, "NAO_OBSERVADA_EM_ANOS_POSTERIORES")
      raw_names <- audit_collapse_values(d$placename_raw[unit_indices])
      history_rows[[length(history_rows) + 1L]] <- data.frame(
        project_id = project, project_short_name = project_name, placename_key = unit,
        placenames_raw = raw_names, n_deployments = length(unit_indices), n_start_years = length(years),
        first_start_year = first_year, last_start_year = last_year,
        first_project_year = first_project_year, last_project_year = last_project_year,
        observed_start_years = audit_collapse_values(years),
        gap_years_with_later_return = audit_collapse_values(gap_years),
        return_years_after_gap = audit_collapse_values(return_years),
        project_years_after_last_record = audit_collapse_values(later_years),
        n_gap_years = length(gap_years), n_later_project_years_without_record = length(later_years),
        observed_in_latest_project_year = length(years) > 0 && last_year == last_project_year,
        first_observed_after_project_start = length(years) > 0 && first_year > first_project_year,
        investigation_codes = paste(codes, collapse = " | "),
        interpretation = if (length(later_years)) {
          "Sem registro nos anos posteriores disponíveis do projeto."
        } else if (length(gap_years)) {
          "Ausência em ano intermediário com dados do projeto e registro posterior."
        } else if (!length(years)) {
          "Referência anual indisponível: ano de início não interpretável."
        } else {
          "Registrada no último ano disponível do projeto."
        },
        stringsAsFactors = FALSE
      )
      for (year in project_years) {
        year_indices <- unit_indices[which(start_year[unit_indices] == year)]
        status <- if (!length(years)) "SEM_ANO_VALIDO" else if (length(year_indices)) {
          if (year %in% return_years) "PRESENTE_APOS_LACUNA" else "PRESENTE"
        } else if (year < first_year) "ANTES_DA_PRIMEIRA_OBSERVACAO" else if (year > last_year) {
          "NAO_OBSERVADA_APOS_ULTIMO_REGISTRO"
        } else "LACUNA_ENTRE_REGISTROS"
        presence_rows[[length(presence_rows) + 1L]] <- data.frame(
          project_id = project, project_short_name = project_name, placename_key = unit,
          placenames_raw = raw_names, start_year = year, observed = length(year_indices) > 0,
          n_deployments_started = length(year_indices), presence_status = status,
          deployment_ids = audit_collapse_values(d$deployment_id[year_indices]),
          camera_ids = audit_collapse_values(d$camera_id_raw_values[year_indices]),
          camera_names = audit_collapse_values(d$camera_name_raw_values[year_indices]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  station_history <- if (length(history_rows)) do.call(rbind, history_rows) else data.frame()
  presence <- if (length(presence_rows)) do.call(rbind, presence_rows) else data.frame()
  coverage <- if (length(coverage_rows)) do.call(rbind, coverage_rows) else data.frame()

  # Pares de placenames distintos abaixo do limiar, em qualquer ano do mesmo projeto.
  valid_geometry <- unit_valid & !is.na(d$valid_coordinate_for_geometry) & d$valid_coordinate_for_geometry &
    is.finite(d$longitude_num) & is.finite(d$latitude_num)
  locations <- unique(d[valid_geometry, c("project_id", "placename_key", "longitude_num", "latitude_num")])
  candidate_parts <- list()
  for (project in sort(unique(locations$project_id))) {
    points <- locations[locations$project_id == project, , drop = FALSE]
    if (nrow(points) < 2L) next
    distances <- audit_geo_distance_m(points$longitude_num, points$latitude_num)
    indices <- which(upper.tri(distances) & distances < nearby_threshold_m, arr.ind = TRUE)
    for (k in seq_len(nrow(indices))) {
      a <- indices[k, 1]; b <- indices[k, 2]
      if (points$placename_key[a] == points$placename_key[b]) next
      if (points$placename_key[a] > points$placename_key[b]) { swap <- a; a <- b; b <- swap }
      candidate_parts[[length(candidate_parts) + 1L]] <- data.frame(
        pair_id = audit_station_pair_id(project, points$placename_key[a], points$placename_key[b]),
        project_id = project, unit_a = points$placename_key[a], unit_b = points$placename_key[b],
        minimum_distance_m = distances[a, b],
        longitude_a = points$longitude_num[a], latitude_a = points$latitude_num[a],
        longitude_b = points$longitude_num[b], latitude_b = points$latitude_num[b],
        stringsAsFactors = FALSE
      )
    }
  }
  candidates <- if (length(candidate_parts)) do.call(rbind, candidate_parts) else {
    data.frame(pair_id = character(), project_id = character(), unit_a = character(), unit_b = character(),
               minimum_distance_m = numeric(), longitude_a = numeric(), latitude_a = numeric(), longitude_b = numeric(), latitude_b = numeric())
  }
  candidates <- candidates[order(candidates$minimum_distance_m), , drop = FALSE]
  candidates <- candidates[!duplicated(candidates$pair_id), , drop = FALSE]

  # Pares de deployments próximos por ano informado e relação entre os períodos.
  detail_parts <- list()
  operation_groups <- split(operation_rows, paste(operation_rows$project_id, operation_rows$operation_year))
  for (group in operation_groups) {
    group <- group[valid_geometry[group$row_index], , drop = FALSE]
    if (nrow(group) < 2L) next
    rows <- group$row_index
    distances <- audit_geo_distance_m(d$longitude_num[rows], d$latitude_num[rows])
    indices <- which(upper.tri(distances) & distances < nearby_threshold_m, arr.ind = TRUE)
    for (k in seq_len(nrow(indices))) {
      ia <- indices[k, 1]; ib <- indices[k, 2]
      a <- rows[ia]; b <- rows[ib]
      if (d$placename_key[a] == d$placename_key[b]) next
      if (d$placename_key[a] > d$placename_key[b]) {
        swap <- a; a <- b; b <- swap
      }
      year <- group$operation_year[1]
      overlap_days <- NA_integer_
      relation <- "PERIODO_INDETERMINADO"
      if (period_valid[a] && period_valid[b]) {
        lower <- max(start_day[a], start_day[b], as.Date(paste0(year, "-01-01")))
        upper <- min(end_day[a], end_day[b], as.Date(paste0(year, "-12-31")))
        overlap_days <- max(0L, as.integer(upper - lower) + 1L)
        relation <- if (upper > lower) "DATAS_SOBREPOSTAS" else if (upper == lower) {
          "APENAS_DATA_LIMITE_COMUM"
        } else "PERIODOS_SEM_SOBREPOSICAO"
      }
      ids_a <- strsplit(d$camera_id_raw_values[a], " | ", fixed = TRUE)[[1]]
      ids_b <- strsplit(d$camera_id_raw_values[b], " | ", fixed = TRUE)[[1]]
      shared_ids <- setdiff(intersect(ids_a, ids_b), c("", "<MISSING>"))
      detail_parts[[length(detail_parts) + 1L]] <- data.frame(
        pair_id = audit_station_pair_id(d$project_id[a], d$placename_key[a], d$placename_key[b]),
        project_id = d$project_id[a], project_short_name = d$project_short_name[a],
        operation_year = year, unit_a = d$placename_key[a], unit_b = d$placename_key[b],
        deployment_a = d$deployment_id[a], deployment_b = d$deployment_id[b],
        camera_ids_a = d$camera_id_raw_values[a], camera_ids_b = d$camera_id_raw_values[b],
        start_a = as.character(start_day[a]), end_a = as.character(end_day[a]),
        start_b = as.character(start_day[b]), end_b = as.character(end_day[b]),
        distance_m = distances[ia, ib], period_relation = relation,
        overlapping_calendar_days = overlap_days,
        shared_camera_ids = audit_collapse_values(shared_ids),
        stringsAsFactors = FALSE
      )
    }
  }
  details <- if (length(detail_parts)) do.call(rbind, detail_parts) else {
    data.frame(pair_id = character(), project_id = character(), project_short_name = character(),
      operation_year = integer(), unit_a = character(), unit_b = character(), deployment_a = character(), deployment_b = character(),
      camera_ids_a = character(), camera_ids_b = character(), start_a = character(), end_a = character(), start_b = character(), end_b = character(),
      distance_m = numeric(), period_relation = character(), overlapping_calendar_days = integer(), shared_camera_ids = character())
  }
  pair_rows <- lapply(seq_len(nrow(candidates)), function(i) {
    pair <- candidates[i, , drop = FALSE]
    related <- details[details$pair_id == pair$pair_id, , drop = FALSE]
    unit_years <- function(unit) sort(unique(operation_rows$operation_year[
      operation_rows$project_id == pair$project_id & operation_rows$placename_key == unit]))
    common_years <- intersect(unit_years(pair$unit_a), unit_years(pair$unit_b))
    overlap <- related$period_relation == "DATAS_SOBREPOSTAS"
    boundary <- related$period_relation == "APENAS_DATA_LIMITE_COMUM"
    unknown <- related$period_relation == "PERIODO_INDETERMINADO"
    status <- if (any(overlap)) "PROXIMAS_COM_SOBREPOSICAO_DE_DATAS" else if (any(boundary)) {
      "PROXIMAS_COM_DATA_LIMITE_COMUM"
    } else if (any(unknown)) "PROXIMAS_NO_MESMO_ANO_PERIODO_INDETERMINADO" else if (nrow(related)) {
      "PROXIMAS_NO_MESMO_ANO_SEM_SOBREPOSICAO"
    } else if (!length(unit_years(pair$unit_a)) || !length(unit_years(pair$unit_b))) {
      "PROXIMIDADE_SEM_ANO_PARA_COMPARAR"
    } else "PROXIMIDADE_OBSERVADA_ENTRE_ANOS_DISTINTOS"
    pair$project_short_name <- d$project_short_name[match(pair$project_id, d$project_id)]
    pair$common_operation_years <- audit_collapse_values(common_years)
    pair$nearby_same_years <- audit_collapse_values(related$operation_year)
    pair$overlapping_years <- audit_collapse_values(related$operation_year[overlap])
    pair$n_common_operation_years <- length(common_years)
    pair$n_nearby_same_years <- length(unique(related$operation_year))
    pair$n_deployment_year_pairs <- nrow(related)
    pair$n_pairs_overlapping_dates <- sum(overlap)
    pair$n_pairs_boundary_date_only <- sum(boundary)
    pair$n_pairs_without_overlap <- sum(related$period_relation == "PERIODOS_SEM_SOBREPOSICAO")
    pair$n_pairs_unknown_period <- sum(unknown)
    pair$investigation_status <- status
    pair
  })
  pairs <- if (length(pair_rows)) do.call(rbind, pair_rows) else {
    empty <- candidates
    empty$project_short_name <- character()
    empty$common_operation_years <- character(); empty$nearby_same_years <- character(); empty$overlapping_years <- character()
    for (name in c("n_common_operation_years", "n_nearby_same_years", "n_deployment_year_pairs", "n_pairs_overlapping_dates",
                   "n_pairs_boundary_date_only", "n_pairs_without_overlap", "n_pairs_unknown_period")) empty[[name]] <- integer()
    empty$investigation_status <- character()
    empty
  }
  if (nrow(pairs)) pairs <- pairs[order(pairs$project_id, pairs$minimum_distance_m), , drop = FALSE]
  if (nrow(details)) details <- details[order(details$project_id, details$operation_year, details$distance_m), , drop = FALSE]
  summary_values <- c(
    n_sampling_units = nrow(station_history),
    n_nearby_unit_pairs_any_year = nrow(pairs),
    n_nearby_unit_pairs_same_year = sum(pairs$n_nearby_same_years > 0),
    n_nearby_unit_pairs_overlapping_dates = sum(pairs$n_pairs_overlapping_dates > 0),
    n_nearby_deployment_year_pairs = nrow(details),
    n_units_with_gap_and_return = if (nrow(station_history)) sum(station_history$n_gap_years > 0) else 0,
    n_units_not_observed_later = if (nrow(station_history)) sum(station_history$n_later_project_years_without_record > 0) else 0,
    n_units_observed_latest_project_year = if (nrow(station_history)) sum(station_history$observed_in_latest_project_year) else 0,
    n_deployments_with_period_issue = sum(nzchar(period_audit$issue_codes)),
    n_project_calendar_years_without_records = if (nrow(coverage)) sum(!coverage$project_has_records) else 0
  )
  list(
    metadata = list(
      created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      sampling_unit = "project_id + placename_key",
      nearby_threshold_m = nearby_threshold_m,
      nearby_rule = "distancia estritamente menor que o limiar; nomes distintos no mesmo projeto; inclui distancia zero",
      distance_method = "haversine; raio da esfera: 6371008.8 m",
      proximity_year_basis = "todos os anos civis entre inicio e fim validos; com fim desconhecido, somente ano de inicio e periodo indeterminado",
      overlap_basis = "intervalos de datas inclusivos, sem inferir simultaneidade intradiaria; contato apenas na data-limite e separado",
      continuity_year_basis = "ano de inicio do deployment como referencia de campanha; somente anos com dados do proprio projeto",
      absence_interpretation = "ausência de registro nos exports; motivo não determinado",
      local_correction = "not applied", local_exclusion = "not applied", r_version = R.version.string
    ),
    summary = data.frame(metric = names(summary_values), value = as.integer(summary_values), row.names = NULL),
    nearby_unit_pairs = pairs, nearby_deployment_year_pairs = details,
    station_temporal_history = station_history, station_year_presence = presence,
    project_year_coverage = coverage, deployment_period_audit = period_audit
  )
}

