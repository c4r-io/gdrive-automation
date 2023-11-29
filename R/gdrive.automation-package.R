#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stringr str_glue
#' @importFrom officer docx_summary
## usethis namespace: end
NULL

#' Expected Structure of Statuses
#'
#' @format ## `who`
#' A data frame with 22 rows and 5 columns:
#' \describe{
#'   \item{phase}{Unit Roadmap Phase}
#'   \item{parse_group}{Identify which statuses occur in a shared signoff box}
#'   \item{task}{name of the task}
#'   \item{signoff}{name of the party responsible for signoff}
#'   \item{label}{string that identifies the status}
#' }
"parsing_dat"
