#' Verify that x is an object of type `statuses` or is coerceable
#'
#' @param x an object of type `statuses` or a data.frame
#'
#' @return logical
#' @export
check_statuses <- function(x)
{
    warning("`check_statuses() is not yet implemented... returning TRUE")
    TRUE
}

#' Coerce into type `statuses`
#'
#' @param x object to be coerced, probably a data.frame
#'
#' @return object of type `statuses`
#' @export
as_statuses <- function(x)
{
    if (!check_statuses())
    {
        stop("Unable to convert object into type `statuses`")
    }
    if (!is.statuses(x))
    {
        class(x) <- c("statuses", class(x))
    }
    x
}

#' Check if object is of type `statuses`
#'
#' @param x object to check
#'
#' @return logical
#' @export
is.statuses <- function(x)
{
    "statuses" %in% class(x)
}

#' Get unit title of `statuses` object
#'
#' @param x object of type `statuses`
#'
#' @return character (length 1)
#' @export
title <- function(x)
{
    stopifnot(is.statuses(x))
    x$Unit[1]
}

#' Set unit title of `statuses` object
#'
#' @param x object of type `statuses`
#' @param value new title (character)
#'
#' @return object of type `statuses`
#' @export
`title<-` <- function(x, value)
{
    stopifnot(is.statuses(x))
    x$Unit <- value
    x
}

#' Get mini-unit titles of `statuses` object
#'
#' @param x object of type `statuses`
#'
#' @return character (length 8)
#' @export
mini_units <- function(x)
{
    stopifnot(is.statuses(x))
    mini_unit_idx <- seq(from = 7, to = 38, by = 4)
    x$`Mini-Unit`[mini_unit_idx]
}

#' Set mini-unit titles of `statuses` object
#'
#' @param x object of type `statuses`
#' @param value new mini-unit titles (character)
#'
#' @return object of type `statuses`
#' @export
`mini_units<-` <- function(x, value)
{
    stopifnot(is.statuses(x))
    if (length(value) < 8)
    {
        value <- c(value, rep.int(NA, 8 - length(value)))
    } else if (length(value) > 8)
    {
        value <- value(1:8)
    }
    x$`Mini-Unit`[7:38] <- rep(value, each = 4)
    x
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
    # check for updated statuses
    diff_statuses <- which(roadmap_dat$Status != tracker_dat$Status)

    if (length(diff_statuses) == 0)
    {
        return(tracker_dat)
    }

    for (i in diff_statuses)
    {
        roadmap_status <- roadmap_dat$Status[i]
        tracker_status <- tracker_dat$Status[i]


        if (roadmap_status == "Submitted" &&
            tracker_status == "Not started") {
            # if roadmap is submitted and tracker is not started
            # take action for submission

            tracker_dat$Status[i] <- roadmap_status
            log_action(paste0("Updating status: {",
                              format_status_msg(tracker_dat[i,]),
                              "}"),
                       url = tracker_url,
                       "ACTION TAKEN")
            notify(to = "CENTER",
                   msg = format_status_msg(roadmap_status))

        } else if (roadmap_status == "Approved" &&
                   roadmap_dat$`Signoff by`[i] == "METER") {
            # if roadmap is approved and signoff is by METER
            # take action for approval

            tracker_dat$Status[i] <- roadmap_status
            log_action(paste0("Updating status: {",
                              format_status_msg(tracker_dat[i,]),
                              "}"),
                       url = tracker_url,
                       "ACTION TAKEN")
            notify(to = "CENTER",
                   msg = format_status_msg(roadmap_status))

        } else if (tracker_status == "Approved" &&
                   roadmap_status != "Approved") {
            # if tracker is approved and roadmap is not approved
            # log action needed (update roadmap)

            stage_todo(paste0("Approve: {",
                              format_status_msg(tracker_dat[i,]),
                              "}"),
                       url = roadmap_url)
        } else {
            # if any other discrepancy
            # log action needed

            stage_todo(paste0("Unknown Status Difference: {",
                              format_status_msg(tracker_dat[i,]),
                              "}"),
                       url = roadmap_url)
        }
    }

    tracker_dat
}
