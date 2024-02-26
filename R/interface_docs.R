#' Download a roadmap and extract statuses
#'
#' @param roadmap_url the url of a roadmap file
#' @param dl_path path to where to download the roadmap file
#'
#' @return a data.frame of statuses, mirroring the structure of \link{read_tracker_statuses}
#' @export
read_roadmap_statuses <- function(roadmap_url, dl_path = tempfile(fileext = ".docx"))
{
    ## download roadmap as docx
    googledrive::drive_download(roadmap_url, dl_path)

    ## read in docx
    roadmap <- suppressWarnings(officer::read_docx(dl_path))
    content <- officer::docx_summary(roadmap, preserve = TRUE)

    # find title
    title <- extract_roadmap_title(content)
    mini_unit_names <- extract_roadmap_mini_units(content)

    ## find statuses
    statuses <- extract_roadmap_statuses(content) %>%
        format_statuses(title = title, mini_unit_names = mini_unit_names) %>%
        as_statuses()
}

#' Extract the title of a roadmap
#'
#' @param content a data.frame (output from \link[officer]{docx_summary})
#'
#' @return character
#' @export
extract_roadmap_title <- function(content)
{
    title_idx <- which(content$style_name %in% "heading 3" &
        grepl("title", content$text))
    title_cell_idx <- content$content_type %in% "table cell" &
        content$doc_index > min(content$doc_index[title_idx])
    content[title_cell_idx, "text"][1]
}

#' Extract the mini-unit titles of a roadmap
#'
#' @param content a data.frame (output from \link[officer]{docx_summary})
#'
#' @return character vector (of length = # of mini-units)
#' @export
extract_roadmap_mini_units <- function(content)
{
    num_mini_units <- getOption("gdrv_auto_env.statuses.num_mini_units")

    table_cells <- subset(content, content_type %in% "table cell")
    tt <- table(table_cells$doc_index)
    mini_unit_doc_index <- max(as.numeric(names(tt)[tt == max(tt)]))
    mini_unit_table <- subset(table_cells, doc_index == mini_unit_doc_index)
    stopifnot(NROW(mini_unit_table) >= 5 * (num_mini_units + 1))
    if (NROW(mini_unit_table) > 5 * (num_mini_units + 1))
    {
        warning("Table of Mini-Units has too many rows")
    }
    mini_unit_table$text[seq(from = 2, length.out = num_mini_units)]
}

#' Extract the labels for each Phase
#'
#' @param content a data.frame (output from \link[officer]{docx_summary})
#' @param NUM_PHASES number of phases to expect
#'
#' @return a data.frame
#' @export
extract_roadmap_phases <- function(content, NUM_PHASES = 7)
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

#' Extract the signoff sections
#'
#' @param content a data.frame (output from \link[officer]{docx_summary})
#'
#' @return a data.frame
#' @export
extract_roadmap_signoffs <- function(content)
{
    headings_23 <- subset(content,
                          content_type %in% "paragraph" &
                              style_name %in% c("heading 2", "heading 3"))
    idx <- which(grepl("Sign[ -][Oo]ffs?", headings_23$text))
    headings_23[idx, ]
}

#' Extract roadmap statuses
#'
#' @param content a data.frame (output from \link[officer]{docx_summary})
#'
#' @return a data.frame
#' @export
extract_roadmap_statuses <- function(content)
{
    parsing_dat <- getOption("gdrv_auto_env.statuses.parsing_dat")
    status_pattern <- getOption("gdrv_auto_env.statuses.regex_pattern")
    parsing_dat$pattern <- stringr::str_glue("({status_pattern})\\s*{parsing_dat$label}")

    ## find phase label and signoff labels
    phase_labels <- extract_roadmap_phases(content)
    signoff_labels <- extract_roadmap_signoffs(content)

    ## setup loop
    statuses_df <- data.frame(phase = numeric(), mini_unit = numeric(),
                              task = character(), signoff = character(),
                              status = character(), notes = character())
    prev_phase <- 0
    curr_mini_unit <- NA

    ## for each signoff
    #    find next table after "sign off" label
    #    identify phase
    #    identify parse group
    #    perform parsing of statuses
    #    identify appropriate parsing
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
        if (curr_phase == getOption("gdrv_auto_env.statuses.activity_phase"))
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

        # if activity phase, find activity description box
        if (curr_phase == getOption("gdrv_auto_env.statuses.activity_phase"))
        {
            prev_tables <- subset(content,
                         content_type %in% "table cell" &
                             doc_index < curr_statuses$doc_index)
            result$notes <- utils::tail(prev_tables, 1)$text
        }
        result$phase <- curr_phase
        result$mini_unit <- curr_mini_unit
        statuses_df <- rbind(statuses_df, result)
    }

    statuses_df
}

#' Cleanup and Format Statuses
#'
#' @param statuses a data.frame, usually the output of \link{extract_roadmap_statuses}
#' @param mini_unit_names a character vector, usually the output of \link{extract_roadmap_mini_units}
#' @param title name of the unit
#'
#' @return a data.frame
#' @export
format_statuses <- function(statuses, title, mini_unit_names)
{
    stopifnot(!anyNA(statuses$status))

    activity_phase <- getOption("gdrv_auto_env.statuses.activity_phase")
    num_expected_mini_units <- getOption("gdrv_auto_env.statuses.num_mini_units")

    # make sure there are 8 mini-units in phase 4
    num_mini_units <- subset(statuses, phase == activity_phase)[, "mini_unit"] %>%
        unique() %>%
        length()
    num_missing_mini_units <- num_expected_mini_units - num_mini_units

    if (num_missing_mini_units > 0)
    {
        d <- subset(statuses, phase == activity_phase & mini_unit == 1) %>%
            dplyr::mutate(status = "Not started", notes = NA)
        to_add <- do.call("rbind", replicate(num_missing_mini_units, d, simplify = FALSE))
        to_add$mini_unit <- rep(seq(to = num_expected_mini_units,
                                    length.out = num_missing_mini_units),
                                each = NROW(d))

        statuses <- rbind(statuses, to_add)
    }

    phase_names <- getOption("gdrv_auto_env.phase_names")
    # reorder and format statuses
    statuses %>%
        dplyr::arrange(phase, mini_unit) %>%
        dplyr::mutate(unit = title,
                      phase = phase_names[phase],
                      mini_unit = mini_unit_names[mini_unit]) %>%
        dplyr::select(Unit = unit,
                      `Mini-Unit` = mini_unit,
                      Phase = phase,
                      Task = task,
                      `Signoff by` = signoff,
                      Status = status,
                      notes)
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
               status = stringr::str_extract(string, status_format$pattern, group = 1),
               notes = NA)
}
