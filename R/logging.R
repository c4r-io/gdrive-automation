#' Record an action in the log
#'
#' All changes to files in the C4R googledrive that are performed by this
#' package should be accompanied with a log entry, added via this function.
#'
#' @param action Description of the change
#' @param url URL of the source of the action
#' @param type Type of message ("INFO", "ERROR", "ACTION TAKEN", "ACTION NEEDED")
#' @param loop_num Number indicating which processing loop the log message is associated with
#'
#' @return NULL
#' @export
log_action <- function(action = "", url = "", type = "INFO", loop_num = NULL)
{
    loop_num <- fix_loop_num(loop_num)

    my_log <- access_log()
    log_row <- data.frame(datetime = format(Sys.time(), "%Y-%m-%d %X %Z"),
                          action = action,
                          url = url,
                          type = type,
                          loop_num = loop_num)
    googlesheets4::sheet_append(my_log, log_row, sheet = "log")
    invisible()
}

#' Record a todo in the log
#'
#' All changes to files in the C4R googledrive that are performed by this
#' package should be accompanied with a log entry, added via this function.
#'
#' @param action Description of the todo item
#' @param url the URL for where to do the TODO
#' @param loop_num Number indicating which processing loop the todo message is associated with
#'
#' @return NULL
#' @export
stage_todo <- function(action = "", url = "", loop_num = NULL)
{
    loop_num <- fix_loop_num(loop_num)

    my_log <- access_log()
    log_row <- data.frame(datetime = format(Sys.time(), "%Y-%m-%d %X %Z"),
                          action = action,
                          url = url,
                          loop_num = loop_num)
    googlesheets4::sheet_append(my_log, log_row, sheet = "todo-staging")
    invisible()
}

#' Merge missing todos from staging sheet
#'
#' @return NULL
#' @export
merge_todo <- function()
{
    # look for todo items in staging but not yet in the todo list
    my_log <- access_log()
    todo <- googlesheets4::read_sheet(my_log, sheet = "todo")
    todo_staging <- googlesheets4::read_sheet(my_log, sheet = "todo-staging")

    todo$id <- paste(todo$action, todo$url)
    todo_staging$id <- paste(todo_staging$action, todo_staging$url)
    missing_todo <- setdiff(todo_staging$id, todo$id)

    # copy missing todo items in staging to todo list
    if (length(missing_todo) > 0)
    {
        missing_todo_idx <- match(missing_todo, todo_staging$id)
        todo_to_add <- todo_staging[missing_todo_idx, c("datetime", "action", "url", "loop_num")]
        googlesheets4::sheet_append(my_log, todo_to_add, sheet = "todo")

        # sent notification
        notify_msg <- paste0(stringr::str_glue("TODO #{seq_along(missing_todo_idx)}\n",
                                               "- action = {todo_staging$action[missing_todo_idx]}\n",
                                               "- url = {todo_staging$url[missing_todo_idx]}\n"),
                             collapse = "\n")
        notify(item_name = "Action Required on C4R Tracking Board",
               item_body = notify_msg,
               notify_text = notify_msg,
               to = "Hao Ye")

        # clear todo-staging
        blank_todo <- todo_staging[c(), c("datetime", "action", "url", "loop_num")]
        googlesheets4::write_sheet(blank_todo, my_log, sheet = "todo-staging")
    }

    invisible()
}


#' Fix the loop_num using the stored value or set to NA
#'
#' @param loop_num number indicating which processing loop is active
#'
#' @return a number, the current loop number
#' @export
fix_loop_num <- function(loop_num)
{
    if (is.null(loop_num))
    {
        loop_num <- getOption("gdrv_auto_env.loop_num")
        if (is.null(loop_num))
        {
            loop_num <- NA
        }
    }
    loop_num
}

#' Set the loop number for the current logging session
#'
#' @return a number, the current loop number
#' @export
set_loop_num_var <- function()
{
    loop_num <- get_last_loop_num() + 1
    options("gdrv_auto_env.loop_num" = loop_num)
    invisible(loop_num)
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
    googledrive::drive_get(getOption("gdrv_auto_env.URL_log"))
}


