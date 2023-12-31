% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface_docs.R
\name{parse_statuses}
\alias{parse_statuses}
\title{Extract Statuses from a Given String}
\usage{
parse_statuses(string, status_format)
}
\arguments{
\item{string}{the text string containing the statuses to be extracted}

\item{status_format}{the data.frame containing the information about statuses: columns \code{task} and \code{signoff} to pass to the output, and a column \code{pattern} containing the regex pattern to extract the status}
}
\value{
a data.frame with the \code{task} and \code{signoff} columns from the \code{status_format} param, and \code{status} containing the extracted status
}
\description{
Extract Statuses from a Given String
}
