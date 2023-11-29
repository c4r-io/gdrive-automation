
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
#' @param NUM_PHASES number of phases to expect
#'
#' @return a data.frame
#' @export
find_roadmap_phases <- function(content, NUM_PHASES = 7)
{
    phase_pattern <- "Phase\\s*(\\d+)"
    headings_1 <- subset(content,
                         content_type %in% "paragraph" &
                             style_name %in% "heading 1")
    idx <- which(grepl(phase_pattern, headings_1$text))
    phase_labels <- headings_1[idx, ]

    # check if numbers of the labels are c(1, 2, ..., NUM_PHASES)
    phase_label_ids <- stringr::str_extract(phase_labels$text, phase_pattern, group = 1)
    stopifnot(identical(as.integer(phase_label_ids), seq(NUM_PHASES)))

    phase_labels
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
    headings_23[idx, ]
}

#' Find Statuses
#'
#' @param content a data.frame (output from \link[officer]{docx_summary})
#'
#' @return a data.frame
#' @export
find_statuses <- function(content)
{
    utils::data("parsing_dat")
    parsing_dat$pattern <- stringr::str_glue("({STATUSES()})\\s*{parsing_dat$label}")

    ## find phase label and signoff labels
    phase_labels <- find_roadmap_phases(content)
    signoff_labels <- find_roadmap_signoffs(content)

    ## setup loop
    statuses_df <- data.frame(phase = numeric(), mini_unit = numeric(),
                              task = character(), signoff = character(),
                              status = character())
    prev_phase <- 0
    curr_mini_unit <- NA

    ## for each signoff
    #    find next table after "sign off" label
    #    identify phase
    #    identify parse group
    #    perform parsing of statuses
    #   identify appopriate parsing
    for (curr_signoff_doc_index in signoff_labels$doc_index)
    {
        # find next table after "sign off" label
        curr_statuses <- subset(content,
                                content_type %in% "table cell" &
                                    doc_index > curr_signoff_doc_index &
                                    doc_index < curr_signoff_doc_index + 3)[1, ]

        # identify phase
        curr_phase <- max(which(curr_statuses$doc_index > phase_labels$doc_index))
        stopifnot(curr_statuses$doc_index < phase_labels$doc_index[curr_phase + 1])

        # identify parse group
        if (curr_phase == 4)
        {
            parse_grp_idx <- 1
            if (is.na(curr_mini_unit))
            {
                curr_mini_unit <- 1
            } else {
                curr_mini_unit <- curr_mini_unit + 1
            }
        } else if (curr_phase != prev_phase) {
            parse_grp_idx <- 1
            curr_mini_unit <- NA
        } else if (curr_phase == prev_phase) {
            parse_grp_idx <- parse_grp_idx + 1
            curr_mini_unit <- NA
        }
        prev_phase <- curr_phase

        # perform parsing of statuses
        status_format <- subset(parsing_dat, phase == curr_phase & parse_group == parse_grp_idx)
        result <- parse_statuses(curr_statuses$text, status_format)
        result$phase <- curr_phase
        result$mini_unit <- curr_mini_unit
        statuses_df <- rbind(statuses_df, result)
    }

    statuses_df
}

#' Extract Statuses from a Given String
#'
#' @param string the text string containing the statuses to be extracted
#' @param status_format the data.frame containing the information about statuses: columns `task` and `signoff` to pass to the output, and a column `pattern` containing the regex pattern to extract the status
#'
#' @return a data.frame with the `task` and `signoff` columns from the `status_format` param, and `status` containing the extracted status
#' @export
parse_statuses <- function(string, status_format)
{
    data.frame(task = status_format$task,
               signoff = status_format$signoff,
               status = stringr::str_extract(string, status_format$pattern, group = 1))
}

#' Regex Pattern for Allowed Statuses
#'
#' @return A character string
#' @export
STATUSES <- function()
{
    return("Submitted|Under review|Approved|Not started")
}
