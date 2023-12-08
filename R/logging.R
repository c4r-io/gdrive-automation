#' Record an action in the log
#'
#' All changes to files in the C4R googledrive that are performed by this
#' package should be accompanied with a log entry, added via this function.
#'
#' @param action Description of the change
#' @param note Any relevant notes (e.g. if the change is part of a larger set
#'   of changes)
#' @param type Type of message ("INFO", "ERROR", "ACTION TAKEN", "ACTION NEEDED")
#' @param loop_num Number indicating which processing loop the log message is associated with
#'
#' @return NULL
#' @export
log_action <- function(action = "", note = "", type = "INFO", loop_num = NA)
{
    my_log <- access_log()
    log_row <- data.frame(datetime = format(Sys.time(), "%Y-%M-%d %X %Z"),
                          action = action,
                          note = note,
                          type = type,
                          loop_num = loop_num)
    googlesheets4::sheet_append(my_log, log_row)
    invisible()
}

#' Get the largest loop_num value in the log
#'
#' @return an integer
#' @export
get_last_loop_num <- function()
{
    my_log <- access_log()
    log <- googlesheets4::read_sheet(my_log)
    loop_num <- log$loop_num

    if (all(is.na(loop_num)))
    {
        return(0)
    }
    return(max(loop_num, na.rm = TRUE))
}


#' Access the log of actions of this package
#'
#' @return A dribble, containing the info for the log googlesheet
#' @export
access_log <- function()
{
    do_auth()
    googledrive::drive_get("https://docs.google.com/spreadsheets/d/1n3rmcM94r_RL-QPTEuxP2D2g2BlrB_Hhdw1Ag_FkUsA/edit?usp=sharing")
}


