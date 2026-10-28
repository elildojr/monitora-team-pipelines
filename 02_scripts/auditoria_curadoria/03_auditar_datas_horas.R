# 03_auditar_datas_horas.R
# Conferência baseada em 03_check_dates.R, calculate_time_lag.R e
# plot_deployment_operation.R. Registra ocorrências sem alterar os dados.

source("02_scripts/auditoria_curadoria/00_setup_curadoria.R", encoding = "UTF-8")

#----- conferir os arquivos da importação

audit_result <- audit_read_result()
expected_manifest <- audit_result$importacao$input_manifest
if (is.null(expected_manifest)) {
  stop("Importação não encontrada. Execute 01_importar_exports_brutos.R.", call. = FALSE)
}

image_files <- sort(list.files(audit_raw_dir, pattern = "^images_.*\\.csv$",
                              full.names = TRUE, ignore.case = TRUE))
input_files <- c(file.path(audit_raw_dir, c("projects.csv", "cameras.csv", "deployments.csv")),
                 image_files)
if (!length(image_files) || any(!file.exists(input_files))) {
  stop("Conjunto de exports incompleto em 01_entrada/dados_brutos.", call. = FALSE)
}
current_manifest <- data.frame(
  source_file = basename(input_files),
  md5 = unname(tools::md5sum(input_files)),
  stringsAsFactors = FALSE
)
if (!audit_same_inputs(expected_manifest, current_manifest)) {
  stop("Os exports diferem da importação registrada. Execute 01_importar_exports_brutos.R antes da etapa 03.",
       call. = FALSE)
}

#----- calcular a auditoria e atualizar o mesmo RDS

date_audit_output <- audit_dates_hours(
  raw_dir = audit_raw_dir,
  image_files = image_files,
  boundary_gap_days = audit_date_gap_days,
  minimum_deployment_days = audit_min_deployment_days
)
date_audit_output$metadata$audit_run_id <- audit_result$metadata$audit_run_id
audit_result$datas_horas <- date_audit_output
audit_result <- audit_save_result(audit_result)

message("Auditoria de datas e horas concluída.")
print(date_audit_output$summary, n = Inf)
if (nrow(date_audit_output$flag_summary)) {
  message("Ocorrências para conferência:")
  print(date_audit_output$flag_summary, n = Inf)
}
message("Resultados: ", audit_result_file)
message("Consulta: consultar_auditoria_datas(audit_result, project_id = '2002576', year = 2017)")
message("Gráfico: plotar_operacao_datas(audit_result, project_id = '2002576', year = 2017)")
