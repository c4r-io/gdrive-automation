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

#' Verify roadmap files against central list of
#'
#' @param db_units the tibble with the central DB of all units
#' @param roadmap_files the dribble with all files matching "Unit Roadmap" in the filename
#'
#' @return NULL
#' @export
check_roadmap_files <- function(db_units = read_db_units(),
                                roadmap_files = find_roadmap_files())
{
    db_ids <- googledrive::as_id(db_units$`Roadmap URL`)
    roadmap_ids <- roadmap_files$id

    new_roadmaps <- setdiff(roadmap_ids, db_ids)
    unlisted_roadmaps <- roadmap_files[roadmap_files$id %in% new_roadmaps,]
    if (length(na.omit(unlisted_roadmaps)) > 0)
    {
        warning(paste(unlisted_roadmaps, collapse = "\n"))
    }

    missing_roadmaps <- setdiff(db_ids, roadmap_ids)
    if (length(na.omit(missing_roadmaps)) > 0)
    {
        warning(paste(missing_roadmaps, collapse = "\n"))
    }
    invisible()
}

#' Find all the Unit Roadmap files in the Shared Drive
#'
#' @return A dribble
#' @export
find_roadmap_files <- function()
{
    c4r_drive <- "(C4R) Community for Rigor"
    googledrive::drive_find("Unit Roadmap", shared_drive = c4r_drive)
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
