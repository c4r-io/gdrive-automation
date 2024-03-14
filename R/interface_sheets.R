#' Read the database of links for all units
#'
#' @param id The URL for the googlesheet with the central DB of all units
#'
#' @return A tibble
#' @export
read_db_units <- function(id = getOption("gdrv_auto_env.URL_db_units"))
{
    googlesheets4::read_sheet(id, sheet = "Unit Overview", skip = 2)
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
    statuses_colnames <- getOption("gdrv_auto_env.statuses.columns")
        c("Unit", "Mini-Unit", "Phase", "Task", "Signoff by", "Status")
    result <- googlesheets4::read_sheet(url, sheet, skip = 1, trim_ws = FALSE) %>%
        as_statuses()
    stopifnot(identical(names(result), statuses_colnames))
    dplyr::select(result, statuses_colnames)
}

#' Update the tracker data
#'
#' @param tracker_dat new data to put into the tracker
#' @param tracker_url url for the tracker
#' @param tracker_sheet sheet for the tracker
#'
#' @return NULL
#' @export
update_tracker_data <- function(tracker_dat, tracker_url, tracker_sheet)
{
    statuses_colnames <- getOption("gdrv_auto_env.statuses.columns")
    to_write <- dplyr::select(tracker_dat, statuses_colnames)
    googlesheets4::range_write(tracker_url,
                               to_write,
                               tracker_sheet,
                               range = "A3",
                               col_names = FALSE,
                               reformat = FALSE)
    log_action(paste0("Syncing Tracker Data: ", tracker_dat$Unit[1]),
               url = tracker_url)
}
