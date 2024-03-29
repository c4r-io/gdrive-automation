% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notifications.R
\name{notify}
\alias{notify}
\title{Send notification}
\usage{
notify(
  item_name,
  item_body = "",
  notify_text = "New Notification",
  to = "Hao Ye"
)
}
\arguments{
\item{item_name}{name of the Monday item to be created}

\item{item_body}{contents of the Monday item (added as an update)}

\item{notify_text}{message on the notification itself}

\item{to}{a vector of names or emails}
}
\description{
Send notification
}
