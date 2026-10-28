# Mapa interativo dos resultados já calculados da auditoria.
# Retorna um objeto leaflet para o Viewer ou o QMD; não grava arquivos.
mapa_conflitos_coordenadas <- function(
    audit_result,
    project_id = NULL,
    uc_boundaries_file = file.path("01_entrada", "dados_referencia", "limites_ucs_icmbio_202608.geojson")
) {
  packages <- c("leaflet", "htmltools", "sf")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Pacotes necessários para mostrar o mapa: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (is.null(audit_result$coordenadas)) {
    stop("Os resultados de coordenadas ainda não estão disponíveis.", call. = FALSE)
  }
  d <- as.data.frame(audit_result$coordenadas$deployment_coordinate_audit)
  valid <- !is.na(d$valid_coordinate_for_geometry) & d$valid_coordinate_for_geometry &
    is.finite(d$longitude_num) & is.finite(d$latitude_num)
  if (!is.null(project_id)) valid <- valid & as.character(d$project_id) %in% as.character(project_id)
  d <- d[valid, , drop = FALSE]
  d$project_id <- as.character(d$project_id)
  e <- function(x) as.character(htmltools::htmlEscape(as.character(x)))
  collapse <- function(x) paste(sort(unique(as.character(x[!is.na(x) & nzchar(as.character(x))]))), collapse = " | ")
  deployment_key <- function(project, deployment) paste(project, deployment, sep = " :: ")
  position_key <- function(project, longitude, latitude) {
    paste(project, sprintf("%.6f", longitude), sprintf("%.6f", latitude), sep = " :: ")
  }
  threshold <- as.numeric(audit_result$metadata$nearby_threshold_m)
  threshold_label <- format(threshold, scientific = FALSE, trim = TRUE)
  group_review <- "Coordenadas/metadados para revisão"
  group_nearby <- paste0("Unidades próximas no mesmo ano (<", threshold_label, " m)")
  group_history <- "Proximidade histórica entre unidades"
  group_clear <- "Sem sinalização espacial"
  group_lines <- "Pares próximos no mesmo ano"
  colours <- setNames(c("#D73027", "#E69F00", "#377EB8", "#238B45"),
                      c(group_review, group_nearby, group_history, group_clear))

  map <- leaflet::leaflet(height = 650) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Imagem de satélite") |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "Mapa de referência")
  if (!nrow(d)) {
    return(leaflet::addControl(map, "Não há coordenadas válidas para representar neste recorte.", position = "topright"))
  }

  unit_results <- audit_result$unidades_amostrais
  pairs <- if (!is.null(unit_results)) as.data.frame(unit_results$nearby_unit_pairs) else data.frame()
  details <- if (!is.null(unit_results)) as.data.frame(unit_results$nearby_deployment_year_pairs) else data.frame()
  history <- if (!is.null(unit_results)) as.data.frame(unit_results$station_temporal_history) else data.frame()
  row_keys <- deployment_key(d$project_id, d$deployment_id)
  same_year_keys <- if (nrow(details)) {
    unique(c(deployment_key(details$project_id, details$deployment_a),
             deployment_key(details$project_id, details$deployment_b)))
  } else character()
  historical_pairs <- if (nrow(pairs)) pairs[pairs$n_nearby_same_years == 0, , drop = FALSE] else pairs
  historical_positions <- if (nrow(historical_pairs)) {
    c(position_key(historical_pairs$project_id, historical_pairs$longitude_a, historical_pairs$latitude_a),
      position_key(historical_pairs$project_id, historical_pairs$longitude_b, historical_pairs$latitude_b))
  } else character()
  d$map_position_key <- position_key(d$project_id, d$longitude_num, d$latitude_num)
  d$map_nearby_same_year <- row_keys %in% same_year_keys
  d$map_historical_nearby <- d$map_position_key %in% historical_positions
  d$map_coordinate_review <- d$coordinate_status == "REVIEW" |
    (!is.na(d$camera_metadata_conflict) & d$camera_metadata_conflict)

  positions <- lapply(split(seq_len(nrow(d)), d$map_position_key), function(indices) {
    rows <- d[indices, , drop = FALSE]
    project <- rows$project_id[1]
    units <- unique(rows$placename_key)
    group <- if (any(rows$map_coordinate_review, na.rm = TRUE)) group_review else if (any(rows$map_nearby_same_year, na.rm = TRUE)) {
      group_nearby
    } else if (any(rows$map_historical_nearby, na.rm = TRUE)) group_history else group_clear
    reasons <- if ("coordinate_flag_descriptions" %in% names(rows)) {
      collapse(rows$coordinate_flag_descriptions)
    } else collapse(rows$coordinate_flag_codes)
    if (any(rows$camera_metadata_conflict, na.rm = TRUE)) reasons <- paste(reasons, "Conflito de metadados de câmera.")
    if (!nzchar(trimws(reasons))) reasons <- "Sem sinalização de integridade espacial nos critérios avaliados."
    popup <- paste0(
      "<strong>Projeto:</strong> ", e(rows$project_short_name[1]),
      "<br><strong>Unidades amostrais:</strong> ", e(collapse(rows$placename_raw)),
      "<br><strong>Coordenadas:</strong> ", rows$longitude_num[1], ", ", rows$latitude_num[1],
      "<br><strong>Anos de início dos deployments:</strong> ", e(collapse(rows$deployment_year)),
      "<br><strong>Deployments:</strong> ", e(collapse(rows$deployment_id)),
      "<br><strong>Sinalizações:</strong> ", e(trimws(reasons))
    )
    if (nrow(pairs)) {
      nearby <- pairs[as.character(pairs$project_id) == project &
                        (pairs$unit_a %in% units | pairs$unit_b %in% units), , drop = FALSE]
      if (nrow(nearby)) {
        items <- vapply(seq_len(nrow(nearby)), function(i) {
          years <- nearby$nearby_same_years[i]
          overlaps <- nearby$overlapping_years[i]
          paste0(e(nearby$unit_a[i]), " / ", e(nearby$unit_b[i]), ": ",
            formatC(nearby$minimum_distance_m[i], digits = 1, format = "f", decimal.mark = ","), " m",
            "; proximidade no mesmo ano: ", if (nzchar(years)) e(years) else "não registrada",
            "; datas sobrepostas: ", if (nzchar(overlaps)) e(overlaps) else "não registradas")
        }, character(1))
        popup <- paste0(popup, "<br><br><strong>Proximidade registrada no histórico:</strong><br>",
                        paste(items, collapse = "<br>"))
      }
    }
    if (nrow(history)) {
      temporal <- history[as.character(history$project_id) == project & history$placename_key %in% units, , drop = FALSE]
      if (nrow(temporal)) {
        items <- vapply(seq_len(nrow(temporal)), function(i) {
          paste0(e(temporal$placename_key[i]), ": anos ", e(temporal$observed_start_years[i]),
            "; lacunas com retorno: ", if (nzchar(temporal$gap_years_with_later_return[i])) e(temporal$gap_years_with_later_return[i]) else "nenhuma registrada",
            "; anos posteriores sem registro: ", if (nzchar(temporal$project_years_after_last_record[i])) e(temporal$project_years_after_last_record[i]) else "nenhum registrado")
        }, character(1))
        popup <- paste0(popup, "<br><br><strong>Histórico anual:</strong><br>", paste(items, collapse = "<br>"),
                        "<br>Ausência nos exports não confirma descontinuação.")
      }
    }
    data.frame(longitude = rows$longitude_num[1], latitude = rows$latitude_num[1],
               group = group, colour = colours[[group]],
               label = paste(rows$project_short_name[1], collapse(rows$placename_raw), sep = " — "),
               popup = popup, stringsAsFactors = FALSE)
  })
  positions <- do.call(rbind, positions)

  if (file.exists(uc_boundaries_file)) {
    ucs <- sf::st_read(uc_boundaries_file, quiet = TRUE)
    ucs <- ucs[as.character(ucs$project_id) %in% unique(d$project_id), , drop = FALSE]
    if (nrow(ucs)) {
      ucs <- sf::st_transform(ucs, 4326)
      labels <- ucs |> sf::st_transform(3857) |> sf::st_point_on_surface() |> sf::st_transform(4326)
      map <- map |>
        leaflet::addPolygons(data = ucs, group = "Limites das UCs", color = "#20BFFF",
          weight = 2, fillOpacity = 0.05, label = ~nome_uc,
          popup = ~paste0("<strong>UC:</strong> ", htmltools::htmlEscape(nome_uc),
                          "<br><strong>CNUC:</strong> ", htmltools::htmlEscape(as.character(cnuc)))) |>
        leaflet::addLabelOnlyMarkers(data = labels, group = "Limites das UCs", label = ~nome_uc,
          labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "auto", textOnly = FALSE))
    }
  }
  for (group in names(colours)) {
    points <- positions[positions$group == group, , drop = FALSE]
    if (nrow(points)) {
      map <- leaflet::addCircleMarkers(map, data = points, group = group,
        lng = ~longitude, lat = ~latitude, radius = if (group == group_clear) 3 else 6,
        color = ~colour, fillColor = ~colour, fillOpacity = if (group == group_clear) 0.5 else 0.9,
        weight = 1, popup = ~popup, label = ~label)
    }
  }

  # As linhas usam os deployments efetivamente comparados no mesmo ano.
  if (nrow(details)) {
    a <- match(deployment_key(details$project_id, details$deployment_a), row_keys)
    b <- match(deployment_key(details$project_id, details$deployment_b), row_keys)
    eligible <- which(!is.na(a) & !is.na(b))
    line_keys <- paste(details$pair_id[eligible], d$map_position_key[a[eligible]], d$map_position_key[b[eligible]])
    for (indices in split(eligible, line_keys)) {
      i <- indices[1]
      popup <- paste0(
        "<strong>Unidades:</strong> ", e(details$unit_a[i]), " / ", e(details$unit_b[i]),
        "<br><strong>Distância:</strong> ", formatC(min(details$distance_m[indices]), digits = 1, format = "f", decimal.mark = ","), " m",
        "<br><strong>Anos de operação próximos:</strong> ", e(collapse(details$operation_year[indices])),
        "<br><strong>Anos com datas sobrepostas:</strong> ", e(collapse(details$operation_year[indices][details$period_relation[indices] == "DATAS_SOBREPOSTAS"]))
      )
      map <- leaflet::addPolylines(map,
        lng = c(d$longitude_num[a[i]], d$longitude_num[b[i]]),
        lat = c(d$latitude_num[a[i]], d$latitude_num[b[i]]),
        group = group_lines, color = "#E69F00", weight = 2, opacity = 0.8, dashArray = "4,4", popup = popup)
    }
  }
  map |>
    leaflet::addLayersControl(
      baseGroups = c("Imagem de satélite", "Mapa de referência"),
      overlayGroups = c("Limites das UCs", names(colours), group_lines),
      options = leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::addLegend(position = "bottomright", colors = unname(colours), labels = names(colours),
                       title = "Sinalizações para investigação", opacity = 1) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    leaflet::fitBounds(min(d$longitude_num), min(d$latitude_num), max(d$longitude_num), max(d$latitude_num))
}
