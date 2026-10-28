# Conferência dos períodos cadastrados e das datas e horas das imagens.
# Critérios de extremidades e duração provenientes de 03_check_dates.R.

audit_date_min <- function(x) {
  if (any(!is.na(x))) min(x, na.rm = TRUE) else x[NA_integer_][1]
}

audit_date_max <- function(x) {
  if (any(!is.na(x))) max(x, na.rm = TRUE) else x[NA_integer_][1]
}

audit_date_values <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- "<MISSING>"
  paste(sort(unique(x)), collapse = " | ")
}

audit_date_false <- function(x) {
  x[is.na(x)] <- FALSE
  x
}

audit_date_codes <- function(flags) {
  codes <- rep("", nrow(flags))
  for (name in names(flags)) {
    selected <- which(audit_date_false(flags[[name]]))
    codes[selected] <- ifelse(nzchar(codes[selected]),
                               paste(codes[selected], name, sep = " | "), name)
  }
  codes
}

audit_date_parse <- function(x) {
  text <- trimws(as.character(x))
  missing <- is.na(text) | text == ""
  date_only <- !missing & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)
  datetime <- !missing & grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?$", text
  )
  date_text <- substr(text, 1, 10)
  unique_dates <- unique(date_text[date_only | datetime])
  dates <- suppressWarnings(as.Date(unique_dates, format = "%Y-%m-%d"))
  roundtrip <- !is.na(dates) & format(dates, "%Y-%m-%d") == unique_dates
  dates[is.na(roundtrip) | !roundtrip] <- as.Date(NA)
  day <- dates[match(date_text, unique_dates)]
  day[!(date_only | datetime)] <- as.Date(NA)
  hour <- suppressWarnings(as.numeric(substr(text, 12, 13)))
  minute <- suppressWarnings(as.numeric(substr(text, 15, 16)))
  second <- suppressWarnings(as.numeric(substring(text, 18)))
  valid_clock <- audit_date_false(datetime & !is.na(day) &
    hour >= 0 & hour < 24 & minute >= 0 & minute < 60 & second >= 0 & second < 60)
  seconds <- hour * 3600 + minute * 60 + second
  seconds[!valid_clock] <- NA_real_
  status <- rep("INVALID", length(text))
  status[missing] <- "MISSING"
  status[date_only & !is.na(day)] <- "DATE_ONLY"
  status[valid_clock] <- "DATETIME"
  day[!status %in% c("DATE_ONLY", "DATETIME")] <- as.Date(NA)
  data.frame(
    day = day,
    clock_seconds = seconds,
    clock_number = as.numeric(day) * 86400 + seconds,
    specific_clock = valid_clock & seconds != 0,
    status = status,
    stringsAsFactors = FALSE
  )
}

audit_date_read <- function(file, required_columns, selected_columns = NULL) {
  data <- readr::read_csv(
    file, col_types = readr::cols(.default = readr::col_character()),
    col_select = if (is.null(selected_columns)) tidyselect::everything() else tidyselect::all_of(selected_columns),
    na = character(), trim_ws = FALSE, show_col_types = FALSE,
    progress = interactive(), name_repair = "minimal"
  )
  missing <- setdiff(required_columns, names(data))
  if (length(missing)) {
    stop(basename(file), ": campos ausentes: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(readr::problems(data))) {
    stop("Problemas na leitura de ", basename(file), ". Confira a importação antes de continuar.", call. = FALSE)
  }
  data
}

audit_dates_hours <- function(raw_dir, image_files, boundary_gap_days, minimum_deployment_days) {
  stopifnot(length(boundary_gap_days) == 1L, is.finite(boundary_gap_days), boundary_gap_days >= 0,
            length(minimum_deployment_days) == 1L, is.finite(minimum_deployment_days), minimum_deployment_days >= 0)
  projects <- audit_date_read(file.path(raw_dir, "projects.csv"), c("project_id", "project_short_name"))
  raw_deployments <- audit_date_read(file.path(raw_dir, "deployments.csv"),
                                    c("project_id", "deployment_id", "placename", "start_date", "end_date"))
  for (column in c("camera_functioning", "camera_id", "remarks", "event_name")) {
    if (!column %in% names(raw_deployments)) raw_deployments[[column]] <- ""
  }
  if (!"project_blank_images" %in% names(projects)) projects$project_blank_images <- ""
  projects <- projects |>
    dplyr::distinct(project_id, .keep_all = TRUE) |>
    dplyr::select(project_id, project_short_name, project_blank_images)

  # Uma linha por chave; divergências de datas permanecem identificadas.
  d <- raw_deployments |>
    dplyr::mutate(source_record = dplyr::row_number()) |>
    dplyr::group_by(project_id, deployment_id) |>
    dplyr::summarise(
      placename = audit_date_values(placename),
      camera_id = audit_date_values(camera_id),
      start_date_raw = dplyr::first(start_date),
      end_date_raw = dplyr::first(end_date),
      start_date_raw_values = audit_date_values(start_date),
      end_date_raw_values = audit_date_values(end_date),
      n_start_values = dplyr::n_distinct(start_date),
      n_end_values = dplyr::n_distinct(end_date),
      camera_functioning = audit_date_values(camera_functioning),
      remarks = audit_date_values(remarks),
      event_name = audit_date_values(event_name),
      deployment_source_records = paste(source_record, collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::left_join(projects, by = "project_id")
  start <- audit_date_parse(d$start_date_raw)
  end <- audit_date_parse(d$end_date_raw)
  d$start_date <- start$day
  d$end_date <- end$day
  d$start_clock_number <- start$clock_number
  d$end_clock_number <- end$clock_number
  d$date_values_conflict <- d$n_start_values > 1 | d$n_end_values > 1
  d$start_specific_clock <- start$specific_clock & !d$date_values_conflict
  d$end_specific_clock <- end$specific_clock & !d$date_values_conflict
  d$period_inverted <- !d$date_values_conflict & audit_date_false(
    d$end_date < d$start_date |
      (d$start_specific_clock & d$end_specific_clock & d$end_clock_number < d$start_clock_number)
  )
  d$start_usable <- !is.na(d$start_date) & !d$date_values_conflict & !d$period_inverted
  d$end_usable <- !is.na(d$end_date) & !d$date_values_conflict & !d$period_inverted
  d$duration_days <- as.numeric(d$end_date - d$start_date)
  d$duration_days[d$date_values_conflict] <- NA_real_
  d$start_year <- as.integer(format(d$start_date, "%Y"))
  d$start_year[d$date_values_conflict] <- NA_integer_
  key <- function(project, deployment) paste(project, deployment, sep = "\u001f")
  deployment_keys <- key(d$project_id, d$deployment_id)
  file_summaries <- daily_parts <- evidence_parts <- extreme_parts <- list()
  n_image_records <- 0L
  n_orphan_records <- 0L

  # Leitura por arquivo para limitar o uso de memória. Todas as linhas participam.
  image_columns <- c("project_id", "deployment_id", "image_id", "filename", "location", "timestamp")
  for (file in image_files) {
    message("Conferindo datas: ", basename(file))
    images <- audit_date_read(file, image_columns, image_columns)
    images$source_file <- rep(basename(file), nrow(images))
    images$source_record <- seq_len(nrow(images))
    parsed <- audit_date_parse(images$timestamp)
    images$photo_date <- parsed$day
    index <- match(key(images$project_id, images$deployment_id), deployment_keys)
    images$deployment_index <- index
    images$clock_number <- parsed$clock_number
    valid_date <- !is.na(parsed$day)
    valid_clock <- parsed$status == "DATETIME"
    before_date <- audit_date_false(valid_date & d$start_usable[index] & parsed$day < d$start_date[index])
    after_date <- audit_date_false(valid_date & d$end_usable[index] & parsed$day > d$end_date[index])
    before_time <- audit_date_false(valid_clock & d$start_usable[index] & d$start_specific_clock[index] &
      parsed$day == d$start_date[index] & parsed$clock_number < d$start_clock_number[index])
    after_time <- audit_date_false(valid_clock & d$end_usable[index] & d$end_specific_clock[index] &
      parsed$day == d$end_date[index] & parsed$clock_number > d$end_clock_number[index])
    images$before_date <- before_date
    images$after_date <- after_date
    images$before_time <- before_time
    images$after_time <- after_time
    images$timestamp_missing <- parsed$status == "MISSING"
    images$timestamp_invalid <- parsed$status == "INVALID"
    images$time_missing <- parsed$status == "DATE_ONLY"
    n_image_records <- n_image_records + nrow(images)
    n_orphan_records <- n_orphan_records + sum(is.na(index))

    file_summaries[[length(file_summaries) + 1L]] <- images |>
      dplyr::group_by(project_id, deployment_id) |>
      dplyr::summarise(
        n_image_records = dplyr::n(), n_valid_image_dates = sum(!is.na(photo_date)),
        n_missing_timestamps = sum(timestamp_missing), n_invalid_timestamps = sum(timestamp_invalid),
        n_images_without_time = sum(time_missing), n_before_start_date = sum(before_date),
        n_after_end_date = sum(after_date), n_before_start_time = sum(before_time),
        n_after_end_time = sum(after_time),
        first_image_date = audit_date_min(photo_date), last_image_date = audit_date_max(photo_date),
        first_clock_number = audit_date_min(clock_number), last_clock_number = audit_date_max(clock_number),
        .groups = "drop"
      )
    daily_parts[[length(daily_parts) + 1L]] <- images |>
      dplyr::filter(!is.na(photo_date), !is.na(deployment_index)) |>
      dplyr::group_by(project_id, deployment_id, photo_date) |>
      dplyr::summarise(n_records = dplyr::n(),
        n_outside_period = sum(before_date | after_date | before_time | after_time), .groups = "drop")

    image_flags <- data.frame(
      IMAGE_WITHOUT_DEPLOYMENT = is.na(index),
      IMAGE_TIMESTAMP_MISSING = images$timestamp_missing,
      IMAGE_TIMESTAMP_INVALID = images$timestamp_invalid,
      IMAGE_TIME_MISSING = images$time_missing,
      IMAGES_BEFORE_START_DATE = before_date, IMAGES_AFTER_END_DATE = after_date,
      IMAGES_BEFORE_START_TIME = before_time, IMAGES_AFTER_END_TIME = after_time
    )
    flagged <- which(rowSums(image_flags) > 0)
    evidence <- images[flagged, c(image_columns, "source_file", "source_record", "photo_date")]
    evidence$flag_codes <- audit_date_codes(image_flags[flagged, , drop = FALSE])
    evidence_parts[[length(evidence_parts) + 1L]] <- evidence

    # Registros que permitem localizar as extremidades em cada arquivo.
    eligible <- which(valid_date & !is.na(index))
    sorting_number <- as.numeric(parsed$day) * 86400 + ifelse(valid_clock, parsed$clock_seconds, 0)
    ordered <- eligible[order(index[eligible], sorting_number[eligible], images$source_record[eligible])]
    first <- ordered[!duplicated(index[ordered])]
    reversed <- rev(ordered)
    last <- reversed[!duplicated(index[reversed])]
    positions <- unique(c(first, last))
    extremes <- images[positions, c(image_columns, "source_file", "source_record", "photo_date")]
    extremes$sorting_number <- sorting_number[positions]
    extreme_parts[[length(extreme_parts) + 1L]] <- extremes
  }

  count_columns <- c("n_image_records", "n_valid_image_dates", "n_missing_timestamps", "n_invalid_timestamps",
                     "n_images_without_time", "n_before_start_date", "n_after_end_date", "n_before_start_time", "n_after_end_time")
  image_summary <- dplyr::bind_rows(file_summaries) |>
    dplyr::group_by(project_id, deployment_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(count_columns), sum),
      first_image_date = audit_date_min(first_image_date), last_image_date = audit_date_max(last_image_date),
      first_clock_number = audit_date_min(first_clock_number), last_clock_number = audit_date_max(last_clock_number),
      .groups = "drop"
    )
  d <- d |>
    dplyr::left_join(image_summary, by = c("project_id", "deployment_id")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(count_columns), function(x) tidyr::replace_na(x, 0L)))
  if (nrow(d) != length(deployment_keys) || sum(d$n_image_records) + n_orphan_records != n_image_records) {
    stop("A associação das imagens aos deployments alterou a contagem dos registros.", call. = FALSE)
  }
  d$diff_start_days <- as.numeric(d$first_image_date - d$start_date)
  d$diff_end_days <- as.numeric(d$end_date - d$last_image_date)
  d$diff_start_days[!d$start_usable] <- NA_real_
  d$diff_end_days[!d$end_usable] <- NA_real_
  d$diff_start_hours <- (d$first_clock_number - d$start_clock_number) / 3600
  d$diff_end_hours <- (d$end_clock_number - d$last_clock_number) / 3600
  complete_image_clocks <- d$n_image_records > 0 & d$n_missing_timestamps == 0 &
    d$n_invalid_timestamps == 0 & d$n_images_without_time == 0
  d$diff_start_hours[!d$start_usable | !d$start_specific_clock | !complete_image_clocks] <- NA_real_
  d$diff_end_hours[!d$end_usable | !d$end_specific_clock | !complete_image_clocks] <- NA_real_

  extremes <- dplyr::bind_rows(extreme_parts)
  first_rows <- extremes |>
    dplyr::arrange(sorting_number, source_file, source_record) |>
    dplyr::distinct(project_id, deployment_id, .keep_all = TRUE)
  last_rows <- extremes |>
    dplyr::arrange(dplyr::desc(sorting_number), source_file, source_record) |>
    dplyr::distinct(project_id, deployment_id, .keep_all = TRUE)
  d <- d |>
    dplyr::left_join(first_rows |>
      dplyr::select(project_id, deployment_id, first_timestamp_raw = timestamp,
                    first_image_id = image_id, first_source_file = source_file,
                    first_source_record = source_record), by = c("project_id", "deployment_id")) |>
    dplyr::left_join(last_rows |>
      dplyr::select(project_id, deployment_id, last_timestamp_raw = timestamp,
                    last_image_id = image_id, last_source_file = source_file,
                    last_source_record = source_record), by = c("project_id", "deployment_id"))

  flags <- data.frame(
    START_DATE_MISSING = start$status == "MISSING", END_DATE_MISSING = end$status == "MISSING",
    START_DATE_INVALID = start$status == "INVALID", END_DATE_INVALID = end$status == "INVALID",
    CONFLICTING_DEPLOYMENT_DATES = d$date_values_conflict, END_BEFORE_START = d$period_inverted,
    SHORT_DEPLOYMENT_PERIOD = audit_date_false(!d$period_inverted & d$duration_days >= 0 &
      d$duration_days < minimum_deployment_days),
    NO_IMAGE_RECORDS = d$n_image_records == 0,
    NO_INTERPRETABLE_IMAGE_DATES = d$n_image_records > 0 & d$n_valid_image_dates == 0,
    IMAGE_TIMESTAMP_MISSING = d$n_missing_timestamps > 0,
    IMAGE_TIMESTAMP_INVALID = d$n_invalid_timestamps > 0,
    IMAGE_TIME_MISSING = d$n_images_without_time > 0,
    IMAGES_BEFORE_START_DATE = d$n_before_start_date > 0, IMAGES_AFTER_END_DATE = d$n_after_end_date > 0,
    START_GAP_OVER_THRESHOLD = audit_date_false(d$diff_start_days > boundary_gap_days),
    END_GAP_OVER_THRESHOLD = audit_date_false(d$diff_end_days > boundary_gap_days),
    IMAGES_BEFORE_START_TIME = d$n_before_start_time > 0, IMAGES_AFTER_END_TIME = d$n_after_end_time > 0
  )
  dictionary <- c(
    START_DATE_MISSING = "Início do deployment ausente.", END_DATE_MISSING = "Término do deployment ausente.",
    START_DATE_INVALID = "Data ou hora de início não interpretável.",
    END_DATE_INVALID = "Data ou hora de término não interpretável.",
    CONFLICTING_DEPLOYMENT_DATES = "Valores distintos de início ou término para o mesmo deployment.",
    END_BEFORE_START = "Término anterior ao início cadastrado.",
    SHORT_DEPLOYMENT_PERIOD = paste0("Período cadastrado inferior a ", minimum_deployment_days, " dias."),
    NO_IMAGE_RECORDS = "Deployment sem registros de imagem no export.",
    NO_INTERPRETABLE_IMAGE_DATES = "Nenhum registro de imagem com data interpretável.",
    IMAGE_TIMESTAMP_MISSING = "Registro de imagem sem timestamp.",
    IMAGE_TIMESTAMP_INVALID = "Timestamp de imagem não interpretável.",
    IMAGE_TIME_MISSING = "Timestamp de imagem informa apenas a data.",
    IMAGES_BEFORE_START_DATE = "Imagem com data anterior ao início cadastrado.",
    IMAGES_AFTER_END_DATE = "Imagem com data posterior ao término cadastrado.",
    START_GAP_OVER_THRESHOLD = paste0("Mais de ", boundary_gap_days, " dias entre o início cadastrado e a primeira data com imagens."),
    END_GAP_OVER_THRESHOLD = paste0("Mais de ", boundary_gap_days, " dias entre a última data com imagens e o término cadastrado."),
    IMAGES_BEFORE_START_TIME = "Imagem anterior ao horário cadastrado, na mesma data de início.",
    IMAGES_AFTER_END_TIME = "Imagem posterior ao horário cadastrado, na mesma data de término.",
    IMAGE_WITHOUT_DEPLOYMENT = "Chave de projeto/deployment da imagem não encontrada no cadastro."
  )
  d$flag_count <- rowSums(flags)
  d$flag_codes <- audit_date_codes(flags)
  d$flag_descriptions <- vapply(strsplit(d$flag_codes, " | ", fixed = TRUE), function(codes) {
    paste(unname(dictionary[codes[nzchar(codes)]]), collapse = " | ")
  }, character(1))
  d$audit_status <- ifelse(d$flag_count > 0, "REVIEW", "OK")
  date_flags <- d |>
    dplyr::select(project_id, project_short_name, deployment_id, placename, start_year) |>
    dplyr::bind_cols(flags) |>
    tidyr::pivot_longer(cols = dplyr::all_of(names(flags)), names_to = "flag_code", values_to = "flag_value") |>
    dplyr::filter(flag_value) |>
    dplyr::mutate(flag_description = unname(dictionary[flag_code])) |>
    dplyr::select(-flag_value)

  first_index <- match(key(first_rows$project_id, first_rows$deployment_id), deployment_keys)
  last_index <- match(key(last_rows$project_id, last_rows$deployment_id), deployment_keys)
  start_evidence <- first_rows[which(d$diff_start_days[first_index] > boundary_gap_days), , drop = FALSE]
  end_evidence <- last_rows[which(d$diff_end_days[last_index] > boundary_gap_days), , drop = FALSE]
  start_evidence$sorting_number <- NULL
  end_evidence$sorting_number <- NULL
  start_evidence$flag_codes <- rep("START_GAP_OVER_THRESHOLD", nrow(start_evidence))
  end_evidence$flag_codes <- rep("END_GAP_OVER_THRESHOLD", nrow(end_evidence))
  evidence <- dplyr::bind_rows(c(evidence_parts, list(start_evidence, end_evidence))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(image_columns, "source_file", "source_record", "photo_date")))) |>
    dplyr::summarise(flag_codes = paste(sort(unique(unlist(strsplit(flag_codes, " | ", fixed = TRUE)))), collapse = " | "),
                     .groups = "drop") |>
    dplyr::left_join(d |>
      dplyr::select(project_id, deployment_id, project_short_name, placename, start_year,
                    start_date_raw_values, end_date_raw_values, camera_functioning, remarks),
      by = c("project_id", "deployment_id"))
  daily <- dplyr::bind_rows(daily_parts) |>
    dplyr::group_by(project_id, deployment_id, photo_date) |>
    dplyr::summarise(n_records = sum(n_records), n_outside_period = sum(n_outside_period), .groups = "drop")
  project_year_summary <- d |>
    dplyr::group_by(project_id, project_short_name, start_year) |>
    dplyr::summarise(
      n_deployments = dplyr::n(), n_deployments_for_review = sum(audit_status == "REVIEW"),
      n_before_start = sum(n_before_start_date > 0 | n_before_start_time > 0),
      n_after_end = sum(n_after_end_date > 0 | n_after_end_time > 0),
      n_start_gap = sum(diff_start_days > boundary_gap_days, na.rm = TRUE),
      n_end_gap = sum(diff_end_days > boundary_gap_days, na.rm = TRUE), .groups = "drop"
    )
  summary <- tibble::tibble(
    metric = c("n_deployments_audited", "n_image_records_read", "n_deployments_for_review",
               "n_image_records_without_deployment", "n_deployments_without_images",
               "n_deployments_with_specific_start_time", "n_deployments_with_specific_end_time"),
    value = c(nrow(d), n_image_records, sum(d$audit_status == "REVIEW"), n_orphan_records,
              sum(d$n_image_records == 0), sum(d$start_specific_clock), sum(d$end_specific_clock))
  )
  list(
    metadata = list(
      created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      reference_scripts = c("03_check_dates.R", "calculate_time_lag.R", "plot_deployment_operation.R"),
      boundary_gap_days = boundary_gap_days, minimum_deployment_days = minimum_deployment_days,
      date_rule = "diff_start = primeira data de imagem - início; diff_end = término - última data de imagem",
      hour_rule = "horário cadastrado diferente de meia-noite; diferenças horárias dos extremos exigem timestamps completos; imagens individuais são verificadas quando têm hora interpretável; nenhuma conversão de fuso",
      grouping = "project_id + ano de início do deployment; o agrupamento não identifica formalmente a campanha",
      image_count_unit = "linhas dos exports, incluindo identificações múltiplas da mesma imagem",
      interpretation = "intervalo sem imagens e duração curta são indicações para conferência, não comprovação de falha",
      local_correction = "not applied", local_exclusion = "not applied"
    ),
    summary = summary, deployment_date_audit = d, date_flags = date_flags,
    flag_dictionary = tibble::tibble(flag_code = names(dictionary), flag_description = unname(dictionary)),
    flag_summary = date_flags |>
      dplyr::count(flag_code, flag_description, name = "n_deployments") |>
      dplyr::arrange(dplyr::desc(n_deployments), flag_code),
    project_year_summary = project_year_summary, image_records_for_review = evidence,
    daily_records = daily,
    field_notes = d |>
      dplyr::filter(remarks != "<MISSING>") |>
      dplyr::select(project_id, project_short_name, deployment_id, placename, start_year, camera_functioning, remarks)
  )
}

consultar_auditoria_datas <- function(audit_result, project_id = NULL, year = NULL, only_review = TRUE) {
  if (is.null(audit_result$datas_horas)) stop("Execute 03_auditar_datas_horas.R.", call. = FALSE)
  rows <- audit_result$datas_horas$deployment_date_audit
  if (!is.null(project_id)) rows <- rows[rows$project_id %in% as.character(project_id), , drop = FALSE]
  if (!is.null(year)) rows <- rows[rows$start_year %in% year, , drop = FALSE]
  if (only_review) rows <- rows[rows$audit_status == "REVIEW", , drop = FALSE]
  rows |>
    dplyr::select(project_id, project_short_name, start_year, placename, deployment_id,
                  start_date_raw_values, end_date_raw_values, first_timestamp_raw, last_timestamp_raw,
                  diff_start_days, diff_end_days, diff_start_hours, diff_end_hours, duration_days,
                  camera_functioning, remarks, flag_descriptions) |>
    dplyr::arrange(project_id, start_year, deployment_id)
}

plotar_operacao_datas <- function(audit_result, project_id, year = NULL, only_review = FALSE) {
  if (is.null(audit_result$datas_horas)) stop("Execute 03_auditar_datas_horas.R.", call. = FALSE)
  rows <- audit_result$datas_horas$deployment_date_audit
  rows <- rows[rows$project_id %in% as.character(project_id), , drop = FALSE]
  if (!is.null(year)) rows <- rows[rows$start_year %in% year, , drop = FALSE]
  if (only_review) rows <- rows[rows$audit_status == "REVIEW", , drop = FALSE]
  if (!nrow(rows)) stop("Não há deployments para o recorte informado.", call. = FALSE)
  rows <- rows[order(rows$project_id, rows$deployment_id), , drop = FALSE]
  rows$plot_key <- paste(rows$project_id, rows$deployment_id, sep = " :: ")
  daily <- audit_result$datas_horas$daily_records |>
    dplyr::inner_join(rows |>
      dplyr::select(project_id, deployment_id, plot_key), by = c("project_id", "deployment_id")) |>
    dplyr::mutate(period_status = ifelse(n_outside_period > 0, "Fora do período cadastrado", "Demais registros"))
  intervals <- rows[rows$start_usable & rows$end_usable, , drop = FALSE]
  plot_dates <- c(intervals$start_date, intervals$end_date, daily$photo_date)
  if (!any(!is.na(plot_dates))) {
    stop("O recorte não contém datas interpretáveis. Consulte a tabela de ocorrências.", call. = FALSE)
  }
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = intervals,
      ggplot2::aes(x = start_date, xend = end_date, y = plot_key, yend = plot_key),
      linewidth = 0.35, colour = "#333333") +
    ggplot2::geom_point(data = daily,
      ggplot2::aes(x = photo_date, y = plot_key, size = n_records, colour = period_status), alpha = 0.7) +
    ggplot2::scale_y_discrete(limits = rev(rows$plot_key), labels = rev(rows$deployment_id), drop = FALSE) +
    ggplot2::scale_colour_manual(values = c("Demais registros" = "#2768A8", "Fora do período cadastrado" = "#B2182B"),
                                 name = NULL) +
    ggplot2::scale_size_continuous(name = "Registros de imagem") +
    ggplot2::labs(x = "Data registrada", y = NULL,
      title = paste(unique(rows$project_short_name), collapse = " / "),
      subtitle = if (is.null(year)) "Deployments no conjunto disponível" else paste("Ano de início:", paste(year, collapse = ", ")),
      caption = "Linha: período cadastrado. Pontos: registros por dia. Espaços sem pontos não comprovam falha de funcionamento.") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7), legend.position = "bottom")
}
