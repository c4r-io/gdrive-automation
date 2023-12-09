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

utils::globalVariables(".data")

.onLoad <- function(libname, pkgname)
{
    op <- options()
    op.gdrv_auto_env <- list(
        # URL for the googlesheet with the central DB of all units
        gdrv_auto_env.URL_db_units = "https://docs.google.com/spreadsheets/d/1cdEwmaUIXPvEIdVOow8tQCPjsz7hseO4h7aaS8ylImA/edit#gid=472058338",

        # URL for the googlesheet with the log
        gdrv_auto_env.URL_log = "https://docs.google.com/spreadsheets/d/1n3rmcM94r_RL-QPTEuxP2D2g2BlrB_Hhdw1Ag_FkUsA/edit?usp=sharing",

        # text names for phases
        gdrv_auto_env.phase_names = c("1. Unit Ideation",
                         "2. Unit Outline",
                         "3. Unit Presentations",
                         "4. Activity Design and Prototyping",
                         "5. Unit Assembly",
                         "6. Internal Testing",
                         "7. Polish, Review, Testing"),

        # regex Pattern for Allowed Statuses
        gdrv_auto_env.status_pattern = "Submitted|Under review|Approved|Not started"
    )
    toset <- !(names(op.gdrv_auto_env) %in% names(op))
    if (any(toset)) options(op.gdrv_auto_env[toset])

    invisible()
}
