#' Get Statuses Data and Process
#'
#' @param roadmap_url URL of a google doc (Unit Roadmap)
#' @param tracker_url URL of a google sheet (Unit Tracker)
#' @param tracker_sheet name of the google sheet for the specific unit's tasks
#'
#' @return NULL
#' @export
sync_statuses <- function(roadmap_url, tracker_url, tracker_sheet)
{
    tryCatch({
        # get statuses from unit roadmap
        roadmap_dat <- read_roadmap_statuses(roadmap_url)

        # get statuses from task spreadsheet
        tracker_dat <- read_tracker_statuses(tracker_url, tracker_sheet)

        # check for same number of rows
        stopifnot(NROW(roadmap_dat) == NROW(tracker_dat))
        tracker_has_updates <- FALSE

        # check for updated unit title
        if (title(roadmap_dat) != title(tracker_dat))
        {
            title(tracker_dat) <- title(roadmap_dat)
            log_action(paste0("Updating title to '", title(tracker_dat), "'"),
                       url = tracker_url,
                       "ACTION TAKEN")
            tracker_has_updates <- TRUE
        }

        # check for updated mini-unit names
        if (!identical(mini_units(roadmap_dat),
                       mini_units(tracker_dat)))
        {
            mini_units(tracker_dat) <- mini_units(roadmap_dat)
            log_action(paste0("Updating mini-unit titles to {'",
                              paste0(mini_units(tracker_dat), collapse = "', '"),
                              "'}"),
                       url = tracker_url,
                       "ACTION TAKEN")
            tracker_has_updates <- TRUE
        }

        # check for differences in statuses
        diff_statuses <- which(roadmap_dat$Status != tracker_dat$Status)
        if (length(diff_statuses) != 0)
        {
            tracker_dat <- handle_diff_statuses(roadmap_dat, roadmap_url,
                                                tracker_dat, tracker_url)
            tracker_has_updates <- TRUE
        }

        # CLEANUP: merge staged todos and updated tracker data
        merge_todo()
        if (tracker_has_updates)
        {
            update_tracker_data(tracker_dat, tracker_url, tracker_sheet)
        }

    }, error = function(e) {
        log_action(e$message, type = "ERROR")
    })

    invisible()
}


#' Handle Differences in Roadmap and Tracker Statuses
#'
#' @param roadmap_dat object of type `statuses` (from Roadmap)
#' @param roadmap_url character (URL of roadmap)
#' @param tracker_dat object of type `statuses` (from tracker)
#' @param tracker_url character (URL of tracker)
#'
#' @return object of type `statuses`
#' @export
handle_diff_statuses <- function(roadmap_dat, roadmap_url,
                                 tracker_dat, tracker_url)
{
    diff_statuses <- which(roadmap_dat$Status != tracker_dat$Status)

    for (i in diff_statuses)
    {
        roadmap_status <- roadmap_dat$Status[i]
        tracker_status <- tracker_dat$Status[i]


        if (roadmap_status == "Submitted" &&
            tracker_status == "Not started") {
            # if roadmap is submitted and tracker is not started
            # take action for submission

            tracker_dat$Status[i] <- roadmap_status
            log_action(paste0("Updating status: {\n",
                              format_status_msg(tracker_dat[i,]),
                              "}"),
                       url = tracker_url,
                       "ACTION TAKEN")
            notify(msg = format_status_msg(roadmap_status),
                   notify_text = "New Submission",
                   to = "Hao Ye")

        } else if (roadmap_status == "Approved" &&
                   roadmap_dat$`Signoff by`[i] == "METER") {
            # if roadmap is approved and signoff is by METER
            # take action for approval

            tracker_dat$Status[i] <- roadmap_status
            log_action(paste0("Updating status: {\n",
                              format_status_msg(tracker_dat[i,]),
                              "}"),
                       url = tracker_url,
                       "ACTION TAKEN")
            notify(msg = format_status_msg(roadmap_status),
                   notify_text = "METER Approved",
                   to = "Hao Ye")

        } else if (tracker_status == "Approved" &&
                   roadmap_status != "Approved") {
            # if tracker is approved and roadmap is not approved
            # log action needed (update roadmap)

            stage_todo(paste0("Approve: {\n",
                              format_status_msg(tracker_dat[i,]),
                              "}"),
                       url = roadmap_url)
        } else {
            # if any other discrepancy
            # log action needed

            stage_todo(paste0("Unknown Status Difference: {\n",
                              format_status_msg(tracker_dat[i,]),
                              "}"),
                       url = roadmap_url)
        }
    }

    tracker_dat
}
