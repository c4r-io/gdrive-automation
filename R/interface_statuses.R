#' Verify that x is an object of type `statuses` or is coerceable
#'
#' @param x an object of type `statuses` or a data.frame
#'
#' @return logical
#' @export
check_statuses <- function(x)
{
    # total number of status rows
    if (NROW(x) != getOption("gdrv_auto_env.statuses.num_checks"))
    {
        return(FALSE)
    }

    # phases
    phase_idx <- match(x$Phase, getOption("gdrv_auto_env.phase_names"))
    if (any(is.na(phase_idx)))
    {
        return(FALSE)
    }

    # activity checks
    activity_idx <- phase_idx == getOption("gdrv_auto_env.statuses.activity_phase")
    expected_activity_checks <- getOption("gdrv_auto_env.statuses.num_activity_checks") * getOption("gdrv_auto_env.statuses.num_mini_units")
    if (sum(activity_idx) != expected_activity_checks)
    {
        return(FALSE)
    }

    # mini-unit names
    if (!all(is.na(x$`Mini-Unit`[!activity_idx])))
    {
        return(FALSE)
    }
    mini_unit_names <- x$`Mini-Unit`[activity_idx]
    if (length(unique(mini_unit_names)) > getOption("gdrv_auto_env.statuses.num_mini_units"))
    {
        return(FALSE)
    }

    # statuses
    if (!all(grepl(getOption("gdrv_auto_env.statuses.regex_pattern"), x$Status)))
    {
        return(FALSE)
    }

    # all checks passed
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
    if (!check_statuses(x))
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
#' @return character (length = of mini-units)
#' @export
mini_units <- function(x)
{
    stopifnot(is.statuses(x))
    mini_unit_idx <- getOption("gdrv_auto_env.statuses.mini_unit_idx")
    num_checks <- getOption("gdrv_auto_env.statuses.num_activity_checks")
    name_idx <- seq(from = min(mini_unit_idx), to = max(mini_unit_idx), by = num_checks)
    mini_unit_names <- x$`Mini-Unit`[mini_unit_idx]
    mini_unit_names[mini_unit_names == ""] <- NA

    mini_unit_names
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
    num_expected_mini_units <- getOption("gdrv_auto_env.statuses.num_mini_units")
    mini_unit_idx <- getOption("gdrv_auto_env.statuses.mini_unit_idx")
    num_checks <- getOption("gdrv_auto_env.statuses.num_activity_checks")

    if (length(value) < num_expected_mini_units)
    {
        value <- c(value, rep.int(NA, num_expected_mini_units - length(value)))
    } else if (length(value) > num_expected_mini_units)
    {
        value <- value(seq(num_expected_mini_units))
    }
    x$`Mini-Unit`[mini_unit_idx] <- rep(value, each = num_checks)
    x
}
