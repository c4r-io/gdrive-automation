#' Format a status row for printing
#'
#' @descrption `format_status_msg()` contains all the details of a status update, whereas `format_notification_msg()` has just the phase number and the task
#'
#' @param status_row a single row of a `statuses` object
#' @param sep character to separate each element in formatted output
#' @name msg
#'
#' @return character (formatted message)
#' @export
format_status_msg <- function(status_row, sep = "\n")
{
    stopifnot(NROW(status_row) == 1)
    paste0("unit: ", status_row$Unit, sep,
           "mini-unit: ", status_row$`Mini-Unit`, sep,
           "phase: ", status_row$Phase, sep,
           "task: ", status_row$Task, sep,
           "signoff by: ", status_row$`Signoff by`, sep,
           "status: ", status_row$Status, sep)
}

#' @rdname msg
#'
#' @param prefix optional string to put in front of message
#'
#' @export
format_notification_msg <- function(prefix, status_row, sep = "\n")
{
    stopifnot(NROW(status_row) == 1)
    paste0(prefix,
           "phase ", stringr::str_extract(status_row$Phase, "^(\\d+)", 1),
           "; ", status_row$Task, sep)
}

#' Send notification
#'
#' @param msg contents of the notification
#' @param notify_text message on the notification itself
#' @param to a vector of names or emails
#'
#' @return NULL
#' @export
notify <- function(msg, notify_text = "New Notification", to = "Hao Ye")
{
    notify_ids <- match_users(to)
    item_id <- create_item(msg)
    for (user_id in notify_ids)
    {
        send_notification(notify_text, item_id, user_id)
    }

    invisible()
}

#' Send a notification on the item in Monday
#'
#' @param msg contents of the notification
#' @param item_id id of the item for the notification to refer to
#' @param user_id id of the user to notify
#'
#' @return NULL
#' @export
send_notification <- function(msg, item_id, user_id)
{
    q_notify <- jsonlite::toJSON(
        list(query = stringr::str_glue("mutation {
            create_notification (user_id: [user_id], target_type: Project, target_id: [item_id], text: \"[msg]\") {
                id
            }
        }", .open = "[", .close = "]")),
        auto_unbox = TRUE)

    resp <- post_query(q_notify)
    invisible()
}

#' Create a new item on the Notifications board in Monday
#'
#' @param msg contents of the item to be created
#' @param board_id id of the board where the item will be added
#'
#' @return character (item_id of new item)
#' @export
create_item <- function(msg, board_id = getOption("gdrv_auto_env.monday_board_id"))
{
    q_create_item <- jsonlite::toJSON(
        list(query = stringr::str_glue("mutation {
            create_item (board_id: [board_id], item_name: \"[msg]\",
                         column_values: \"{\\\"date4\\\":\\\"[Sys.Date()]\\\"}\") {
                id
            }
        }", .open = "[", .close = "]")),
        auto_unbox = TRUE)

    resp <- post_query(q_create_item)
    out <- httr::content(resp)
    out$data$create_item$id
}

#' Match user name or email to user_id
#'
#' @param to vector of names or emails
#'
#' @return numeric ids
#' @export
match_users <- function(to = "Hao Ye")
{
    monday_users <- get_users()
    email_match <- match(to, monday_users$email)
    name_match <- match(to, monday_users$name)
    match_idx <- ifelse(is.na(email_match), name_match, email_match)

    if (any(is.na(match_idx)))
    {
        warning("Unable to match user for notifications: ", paste0(to[is.na(match_idx)], collapse = ", \n"))
    }

    monday_users$id[stats::na.omit(match_idx)]
}


#' Query Monday for user emails and ids
#'
#' @inheritParams post_query
#'
#' @return data.frame (columns: `email` and `id`)
#' @export
get_users <- function(token = get_monday_token())
{
    monday_users <- getOption("grdv_auto_env.monday_users")
    if (!is.null(monday_users))
    {
        return(monday_users)
    }

    q_users <- jsonlite::toJSON(
        list(query = "query {
                users { email id name }
             }"),
        auto_unbox = TRUE)

    resp <- post_query(q_users)
    out <- httr::content(resp)
    monday_users <- data.frame(do.call(rbind, out$data$users)) %>%
        dplyr::mutate_all(unlist)
    options("grdv_auto_env.monday_users" = monday_users)
    monday_users
}

#' Send query to Monday API
#'
#' @param query json (formatted according to \url{https://developer.monday.com/api-reference/docs/introduction-to-graphql})
#' @param token api access token
#'
#' @return result from \link[httr]{POST}
#' @export
post_query <- function(query, token = get_monday_token())
{
    base_url <- "https://api.monday.com/v2/"
    httr::POST(url = base_url,
               config = httr::add_headers(Authorization = token,
                                          `Content-Type` = "application/json",
                                          `API-Version` = "2023-07"),
               body = query)
}

