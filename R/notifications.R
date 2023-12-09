#' Format a status row for printing
#'
#' @param status_row a single row of a `statuses` object
#' @param sep character to separate each element in formatted output
#'
#' @return character (formatted message)
#' @export
format_status_msg <- function(status_row, sep = "\n")
{
    stopifnot(NROW(status_row) == 1)
    paste0("unit: ", status_row$Unit, sep,
           "mini-unit: ", status_row$`Mini-Unit`, sep,
           "phase: ", status_row$Phase, sep,
           "task: ", status_row$Task, sep,
           "signoff by: ", status_row$`Signoff by`, sep,
           "status: ", status_row$Status, sep)
}

#' Send notification
#'
#' @param to either "CENTER" or "NIH" or a specific METER
#' @param status_row a single row of a `statuses` object
#'
#' @return NULL
#' @export
notify <- function(to = "CENTER", status_row)
{


    invisible()
}
