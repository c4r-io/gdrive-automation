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
    # stop early if missing values
    if (anyNA(c(roadmap_url, tracker_url, tracker_sheet)))
    {
        log_action(paste0("Skipping Unit with NA values: \n",
                          "  roadmap_url: ", roadmap_url, "\n",
                          "  tracker_url: ", tracker_url, "\n",
                          "  tracker_sheet: ", tracker_sheet, "\n"),
                   url = tracker_url,
                   type = "INFO")
        return(invisible())
    }

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
        if (tracker_has_updates)
        {
            update_tracker_data(tracker_dat, tracker_url, tracker_sheet)
        }

    }, error = function(e) {
        log_action(e$message, url = roadmap_url, type = "ERROR")
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
        roadmap_status <- roadmap_dat[i, "Status"]
        tracker_status <- tracker_dat[i, "Status"]
        phase <- match(roadmap_dat[i, "Phase"], getOption("gdrv_auto_env.phase_names"))

        if (roadmap_status == "Submitted" &&
            tracker_status == "Not started" &&
            phase == 4) {
            # if roadmap is submitted and tracker is not started AND phase == 4
            tracker_dat$Status[i] <- roadmap_status
            roadmap_task <- roadmap_dat[i, "Task"]

            if (roadmap_task == "Activity Description" ||
                roadmap_task == "Activity Demonstration") {
            ## notify Hao for METER to review Activity Description OR Activity Demonstration
                status_msg <- format_status_msg(roadmap_dat[i,])
                log_action(paste0("Updating status: {\n", status_msg, "}"),
                           url = tracker_url,
                           "ACTION TAKEN")
                notify(msg = status_msg,
                       notify_text = format_notification_msg("METER to review: ", tracker_dat[i, ]),
                       to = "Hao Ye")
            } else if (roadmap_task == "Activity Tech Specs") {
            ## notify Thomas to review Activity Tech Spec
                status_msg <- format_status_msg(roadmap_dat[i,])
                log_action(paste0("Updating status: {\n", status_msg, "}"),
                           url = tracker_url,
                           "ACTION TAKEN")
                notify(msg = status_msg,
                       notify_text = format_notification_msg("Production to review: ", tracker_dat[i, ]),
                       to = "Thomas McDonald")
            } else if (roadmap_task == "Prototype") {
                ## notify Hao and Thomas to review Prototype
                status_msg <- format_status_msg(roadmap_dat[i,])
                log_action(paste0("Updating status: {\n", status_msg, "}"),
                           url = tracker_url,
                           "ACTION TAKEN")
                notify(msg = status_msg,
                       notify_text = format_notification_msg("CENTER to review: ", tracker_dat[i, ]),
                       to = c("Thomas McDonald", "Hao Ye"))
            } else {
                stage_todo(paste0("Unknown Status Difference: {\n",
                                  format_status_msg(roadmap_dat[i,]),
                                  "}"),
                           url = roadmap_url)
            }
        } else if (roadmap_status == "Submitted" &&
                   tracker_status == "Not started" && phase < 4) {
            # if roadmap is submitted and tracker is not started
            # take action for submission

            tracker_dat$Status[i] <- roadmap_status
            status_msg <- format_status_msg(roadmap_dat[i,])
            log_action(paste0("Updating status: {\n", status_msg, "}"),
                       url = tracker_url,
                       "ACTION TAKEN")
            notify(msg = status_msg,
                   notify_text = format_notification_msg("Submitted: ", roadmap_dat[i, ]),
                   to = "Hao Ye")

        } else if (roadmap_status == "Approved" &&
                   roadmap_dat$`Signoff by`[i] == "METER") {
            # if roadmap is approved and signoff is by METER
            # take action for approval

            tracker_dat$Status[i] <- roadmap_status
            status_msg <- format_status_msg(roadmap_dat[i,])
            log_action(paste0("Updating status: {\n", status_msg, "}"),
                       url = tracker_url,
                       "ACTION TAKEN")
            notify(msg = status_msg,
                   notify_text = format_notification_msg("Approved: ", roadmap_dat[i, ]),
                   to = "Hao Ye")

        } else if (tracker_status == "Approved" &&
                   roadmap_status != "Approved") {
            # if tracker is approved and roadmap is not approved
            # log action needed (update roadmap)

            stage_todo(paste0("Approve: {\n",
                              format_status_msg(roadmap_dat[i,]),
                              "}"),
                       url = roadmap_url)
        } else {
            # if any other discrepancy
            # log action needed

            stage_todo(paste0("Unknown Status Difference: {\n",
                              format_status_msg(roadmap_dat[i,]),
                              "}"),
                       url = roadmap_url)
        }
    }

    tracker_dat
}
