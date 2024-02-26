#' Get Statuses Data and Process
#'
#' @param roadmap_url URL of a google doc (Unit Roadmap)
#' @param tracker_url URL of a google sheet (Unit Tracker)
#' @param tracker_sheet name of the google sheet for the specific unit's tasks
#' @param unit_id unit id
#'
#' @return NULL
#' @export
sync_statuses <- function(roadmap_url, tracker_url, tracker_sheet, unit_id = "")
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
        roadmap_dat$unit_id <- unit_id

        # get statuses from task spreadsheet
        tracker_dat <- read_tracker_statuses(tracker_url, tracker_sheet)
        tracker_dat$unit_id <- unit_id

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
    }, warning = function(w) {
        log_action(w$message, url = roadmap_url, type = "WARNING")
    }
    )

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
        status_msg <- format_status_msg(roadmap_dat[i,])
        task <- roadmap_dat[i, "Task"]
        signoff_by <- roadmap_dat$`Signoff by`[i]
        unit_id <- roadmap_dat[i, "unit_id"]

        if (roadmap_status == "Submitted" &&
            tracker_status == "Not started" &&
            phase == 4) {
            # if roadmap is submitted and tracker is not started AND phase == 4
            tracker_dat$Status[i] <- roadmap_status

            if (task == "Activity Description" ||
                task == "Activity Demonstration") {
            ## notify Hao for METER to review Activity Description OR Activity Demonstration
                log_status_update(status_msg, tracker_url)

                notify(item_name = paste("New Submission:", unit_id, "-", task),
                       item_body = status_msg,
                       notify_text = format_notification_msg("METER to review: ", tracker_dat[i, ]),
                       to = "Hao Ye")
            } else if (task == "Activity Tech Specs") {
            ## notify Thomas to review Activity Tech Spec
                log_status_update(status_msg, tracker_url)

                notify(item_name = paste("New Submission:", unit_id, "-", task),
                       item_body = status_msg,
                       notify_text = format_notification_msg("Production to review: ", tracker_dat[i, ]),
                       to = "Thomas McDonald")
            } else if (task == "Prototype") {
                ## notify Hao and Thomas to review Prototype
                log_status_update(status_msg, tracker_url)

                notify(item_name = paste("New Submission:", unit_id, "-", task),
                       item_body = status_msg,
                       notify_text = format_notification_msg("CENTER to review: ", tracker_dat[i, ]),
                       to = c("Thomas McDonald", "Hao Ye"))
            } else {
                stage_todo(paste0("Unknown Status Difference: {\n", status_msg, "}"),
                           url = roadmap_url)
            }
        } else if (roadmap_status == "Submitted" &&
                   tracker_status == "Not started" && phase < 4) {
            # if roadmap is submitted and tracker is not started
            # take action for submission
            log_status_update(status_msg, tracker_url)
            tracker_dat$Status[i] <- roadmap_status

            notify(item_name = paste("New Submission:", unit_id, "-", task),
                   item_body = status_msg,
                   notify_text = format_notification_msg("CENTER to review: ", roadmap_dat[i, ]),
                   to = "Hao Ye")
        } else if (roadmap_status == "Under review") {
            # if roadmap is under review
            log_status_update(status_msg, tracker_url)
            tracker_dat$Status[i] <- roadmap_status

            notify(item_name = paste("Now in Review:", unit_id, "-", task),
                   item_body = status_msg,
                   notify_text = format_notification_msg(paste(signoff_by, "reviewing: "), roadmap_dat[i, ]),
                   to = "Hao Ye")
        } else if (roadmap_status == "Approved") {
            # if roadmap is approved
            # take action for approval
            log_status_update(status_msg, tracker_url)
            tracker_dat$Status[i] <- roadmap_status

            notify(item_name = paste("New Approval:", unit_id, "-", task),
                   item_body = status_msg,
                   notify_text = format_notification_msg(paste(signoff_by, "Approved: "), roadmap_dat[i, ]),
                   to = "Hao Ye")

        } else {
            # if any other discrepancy
            # log action needed

            stage_todo(paste0("Unhandled Status Difference: {\n", status_msg, "}"),
                       url = roadmap_url)
        }
    }

    tracker_dat
}
