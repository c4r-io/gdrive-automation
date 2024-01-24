
#' Verify roadmap files against central list of
#'
#' @param db_units the tibble with the central DB of all units
#' @param roadmap_files the dribble with all files matching "Unit Roadmap" in the filename
#'
#' @return NULL
#' @export
check_files_roadmap <- function(db_units = read_db_units(),
                                roadmap_files = find_files_roadmap())
{
    db_ids <- googledrive::as_id(db_units$`Roadmap URL`)
    roadmap_ids <- roadmap_files$id

    new_roadmaps <- setdiff(roadmap_ids, db_ids)
    unlisted_roadmaps <- roadmap_files[roadmap_files$id %in% new_roadmaps,][["name"]]
    if (length(stats::na.omit(unlisted_roadmaps)) > 0)
    {
        warning(paste(c("\n== Unlisted Roadmaps ==", unlisted_roadmaps), collapse = "\n"))
    }

    missing_roadmaps <- setdiff(db_ids, roadmap_ids)
    if (length(stats::na.omit(missing_roadmaps)) > 0)
    {
        warning(paste(c("\n== Missing Roadmaps ==", missing_roadmaps), collapse = "\n"))
    }
    invisible()
}

#' Find all the Unit Roadmap files in the Shared Drive
#'
#' @return A dribble
#' @export
find_files_roadmaps <- function()
{
    c4r_drive <- "(C4R) Community for Rigor"
    googledrive::drive_find("Unit Roadmap", shared_drive = c4r_drive)
}

#' Download Data on Units and Statuses
#'
#' @param data_file path to the desired download location
#'
#' @return a list with components for the statuses, units, and timestamp
#' @export
download_unit_data <- function(data_file = "unit_data.RDS")
{
    gdrv_auth()

    unit_data <- read_db_units()
    tracker_urls <- unit_data$`unit tasks URL`
    tracker_sheets <- unit_data$`Sheet Name`
    unit_statuses <- list()

    for (idx in seq(NROW(unit_data)))
    {
        tracker_url <- tracker_urls[idx]
        tracker_sheet <- tracker_sheets[idx]

        if (anyNA(c(tracker_url, tracker_sheet)))
        {
            next
        }

        statuses <- read_tracker_statuses(tracker_url, tracker_sheet)
        statuses$unit_id <- unit_data$`unit-id`[idx]
        unit_statuses[[idx]] <- statuses
    }
    unit_statuses <- do.call(rbind, unit_statuses)

    result <- list(unit_statuses = unit_statuses,
                   unit_data = unit_data,
                   timestamp = Sys.time())
    saveRDS(result, data_file)
    result
}
