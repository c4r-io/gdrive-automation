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
