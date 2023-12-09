#' Read the database of links for all units
#'
#' @param id The URL for the googlesheet with the central DB of all units
#'
#' @return A tibble
#' @export
read_db_units <- function(id = getOption("gdrv_auto_env.URL_db_units"))
{
    googlesheets4::read_sheet(id, sheet = "Unit Info", skip = 1)
}


#' Read the tasks for a single unit
#'
#' @param url The URL for the googlesheet containing tasks for a single unit
#' @param sheet The name of the sheet to specify which unit, defaults to `"unit 1"`
#'
#' @return A tibble
#' @export
read_tracker_statuses <- function(url, sheet = "unit 1")
{
    googlesheets4::read_sheet(url, sheet, skip = 1) %>%
        as_statuses()
}

