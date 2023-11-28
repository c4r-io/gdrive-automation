#' Default URL for the googlesheet with the central DB of all units
#'
#' @return string with the URL for the
#' @export
URL_db_units <- function()
{
    "https://docs.google.com/spreadsheets/d/1cdEwmaUIXPvEIdVOow8tQCPjsz7hseO4h7aaS8ylImA/edit#gid=472058338"
}

#' Read the database of links for all units
#'
#' @param id The URL for the googlesheet with the central DB of all units
#'
#' @return A tibble
#' @export
read_db_units <- function(id = URL_db_units())
{
    googlesheets4::read_sheet(id, sheet = "Unit Info", skip = 1)
}


#' Read the tasks for a single unit
#'
#' @param id The URL for the googlesheet containing tasks for a single unit
#' @param sheet The name of the sheet to specify which unit, defaults to `"unit 1"`
#'
#' @return A tibble
#' @export
read_unit_tasks <- function(id, sheet = "unit 1")
{
    googlesheets4::read_sheet(id, sheet, skip = 1)
}

