
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
    if (length(stats::na.omit(unlisted_roadmaps)) > 0)
    {
        warning(paste(unlisted_roadmaps, collapse = "\n"))
    }

    missing_roadmaps <- setdiff(db_ids, roadmap_ids)
    if (length(stats::na.omit(missing_roadmaps)) > 0)
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

#' Find the labels for each Phase
#'
#' @param content a data.frame (output from \link[officer]{docx_summary})
#'
#' @return a data.frame
#' @export
find_roadmap_phases <- function(content)
{
    headings_1 <- subset(content,
                         content_type %in% "paragraph" &
                             style_name %in% "heading 1")
    idx <- which(grepl("Phase \\d", headings_1$text))
    headings_1[idx, ]
}

#' Find the signoff sections
#'
#' @param content a data.frame (output from \link[officer]{docx_summary})
#'
#' @return a data.frame
#' @export
find_roadmap_signoffs <- function(content)
{
    headings_23 <- subset(content,
                         content_type %in% "paragraph" &
                             style_name %in% c("heading 2", "heading 3"))
    idx <- which(grepl("Sign[ -][Oo]ffs?", headings_23$text))
    # headings_23[idx, ]
}

#' Create a status row
#'
#' @param df data.frame of status to be appended to
#' @param string passed to \link[stringr]{str_extract}
#' @param pattern passed to \link[stringr]{str_extract}
#' @param task name of the task in the new status row
#' @param signoff name of the signoff in the new status row
#'
#' @return a data.frame
#' @export
add_status <- function(df, string, pattern, task, signoff)
{
    status <- stringr::str_extract(string, pattern, group = 1)
    rbind(df,
          c(task = task, signoff = signoff, status = status))
}
