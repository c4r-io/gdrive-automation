
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

#' Find Phase 1 Statuses
#'
#' @param table_cells a data.frame, subset of the output from \link[officer]{docx_summary} that are the table cells
#' @inheritParams check_signoff_doc_index
#'
#' @return a data.frame
#' @export
find_status_phase_1 <- function(table_cells, phase_labels, signoff_labels)
{
    PHASE <- 1

    # find location and check
    signpost_table_id <- min(which(grepl("Instructions will be provided",
                                           table_cells$text)))
    signpost_id_shift <- 5
    approvals <- table_cells[signpost_table_id + signpost_id_shift, ]
    check_signoff_doc_index(approvals$doc_index,
                            phase_labels, signoff_labels,
                            phase = PHASE)

    # extract statuses
    rbind(
        find_status(approvals$text, str_glue("({STATUSES()})\\s*Review by CENTER"),
                    phase = PHASE, task = "Phase 1", signoff = "CENTER"),
        find_status(approvals$text, str_glue("({STATUSES()})\\s*Topic Approval by NIH/NINDS"),
                    phase = PHASE, task = "Phase 1", signoff = "NIH/NINDS PO"),
        find_status(approvals$text, str_glue("({STATUSES()})\\s*Review by SC"),
                    phase = PHASE, task = "Phase 1", signoff = "SC")
    )
}

#' @describeIn find_status_phase_1 Find Phase 2 Statuses
find_status_phase_2 <- function(table_cells, phase_labels, signoff_labels)
{
    PHASE <- 2

    # find location and check
    signpost_table_id <- min(which(grepl("The presentation should convey",
                                         table_cells$text)))
    signpost_id_shift <- 2
    approvals <- table_cells[signpost_table_id + signpost_id_shift, ]
    check_signoff_doc_index(approvals$doc_index,
                            phase_labels, signoff_labels,
                            phase = PHASE)

    # extract statuses
    find_status(approvals$text, str_glue("({STATUSES()})\\s*Review by CENTER"),
                phase = PHASE, task = "Phase 2", signoff = "CENTER")
}

#' @describeIn find_status_phase_1 Find Phase 3 Statuses
find_status_phase_3 <- function(table_cells, phase_labels, signoff_labels)
{
    PHASE <- 3

    # find location and check
    signpost_table_id <- min(which(grepl("comprise the short-form",
                                         table_cells$text)))
    signpost_id_shift <- 2
    approvals <- table_cells[signpost_table_id + signpost_id_shift, ]
    check_signoff_doc_index(approvals$doc_index,
                            phase_labels, signoff_labels,
                            phase = PHASE)

    # extract statuses
    status_short <- find_status(approvals$text, str_glue("({STATUSES()})[\\s\\w-]*Review by CENTER"),
                                phase = PHASE, task = "Short-form Unit Presentation", signoff = "CENTER")

    signpost_table_id <- min(which(grepl("contents of the full unit",
                                         table_cells$text)))
    signpost_id_shift <- 2
    approvals <- table_cells[signpost_table_id + signpost_id_shift, ]
    check_signoff_doc_index(approvals$doc_index,
                            phase_labels, signoff_labels,
                            phase = PHASE, signoff = PHASE + 1)

    # extract statuses
    status_long <- find_status(approvals$text, str_glue("({STATUSES()})[\\s\\w-]*Review by CENTER"),
                               phase = PHASE, task = "Long-form Unit Presentation", signoff = "CENTER")

    rbind(status_short, status_long)
}

#' Check That Doc Index for Sign-Off is in Range
#'
#' @param doc_index index for the paragraph with the signoff
#' @param phase_labels a data.frame, subset of the output from \link[officer]{docx_summary} that are the paragraphs corresponding to the labels of the start of each phase
#' @param signoff_labels a data.frame, subset of the output from \link[officer]{docx_summary} that are the paragraphs corresponding to the "Sign offs" headings
#' @param phase which phase to check
#' @param signoff which signoff to check
#'
#' @return NULL
#' @export
check_signoff_doc_index <- function(doc_index,
                                    phase_labels, signoff_labels,
                                    phase, signoff = phase)
{
    stopifnot(
        doc_index > phase_labels$doc_index[phase],
        doc_index  < phase_labels$doc_index[phase + 1],
        doc_index > signoff_labels$doc_index[signoff],
        doc_index <= signoff_labels$doc_index[signoff] + 3)
}


#' Create a status row
#'
#' @param string passed to \link[stringr]{str_extract}
#' @param pattern passed to \link[stringr]{str_extract}
#' @param phase number of the phase in the new status row
#' @param task name of the task in the new status row
#' @param signoff name of the signoff in the new status row
#'
#' @return a data.frame
#' @export
find_status <- function(string, pattern, phase, task, signoff)
{
    status <- stringr::str_extract(string, pattern, group = 1)
    c(phase = phase, task = task, signoff = signoff, status = status)
}

#' Regex Pattern for Allowed Statuses
#'
#' @return A character string
#' @export
STATUSES <- function()
{
    return("Submitted|Under review|Approved|Not started")
}
