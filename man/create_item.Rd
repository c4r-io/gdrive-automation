% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notifications.R
\name{create_item}
\alias{create_item}
\title{Create a new item on the Notifications board in Monday}
\usage{
create_item(item_name, board_id = getOption("gdrv_auto_env.monday_board_id"))
}
\arguments{
\item{item_name}{contents of the item to be created}

\item{board_id}{id of the board where the item will be added}
}
\value{
character (item_id of the new item)
}
\description{
Create a new item on the Notifications board in Monday
}
