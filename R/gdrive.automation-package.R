#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stringr str_glue
#' @importFrom officer docx_summary
## usethis namespace: end
NULL

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

        # board id for notifications
        gdrv_auto_env.monday_board_id = "5704206865",

        # regex Pattern for Allowed Statuses
        gdrv_auto_env.statuses.regex_pattern = "Submitted|Under review|Approved|Not started",

        # variables for parsing checking statuses
        gdrv_auto_env.statuses.parsing_dat = parsing_dat,
        gdrv_auto_env.statuses.num_checks = 50,
        gdrv_auto_env.statuses.num_mini_units = 8,
        gdrv_auto_env.statuses.activity_phase = 4,
        gdrv_auto_env.statuses.num_activity_checks = sum(parsing_dat$phase == 4),
        gdrv_auto_env.statuses.mini_unit_idx = seq(from = 1 + sum(parsing_dat$phase < 4), length.out = 8 * sum(parsing_dat$phase == 4))
    )
    toset <- !(names(op.gdrv_auto_env) %in% names(op))
    if (any(toset)) options(op.gdrv_auto_env[toset])

    invisible()
}
