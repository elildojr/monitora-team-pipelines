#' Verificar a presença de colunas obrigatórias
#'
#' Interrompe a execução quando uma ou mais colunas esperadas
#' não estão presentes no objeto importado.
#'
#' @param data Data frame ou tibble.
#' @param required_columns Vetor com os nomes das colunas obrigatórias.
#' @param data_name Nome usado na mensagem de erro.
#'
#' @return Invisivelmente TRUE quando todas as colunas estão presentes.

check_required_columns <- function(
    data,
    required_columns,
    data_name = deparse(substitute(data))
) {
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "O objeto '", data_name,
        "' não contém as seguintes colunas obrigatórias: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}