% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractdata.R
\name{fp_children}
\alias{fp_children}
\alias{fp_coverage}
\alias{fp_paging}
\alias{fp_created}
\title{Get the collected data per Object ID}
\usage{
fp_children(
  .data,
  col_target = NA,
  col_paging = NA,
  col_created = NA,
  timestamp = F,
  level = 0
)

fp_coverage(.data, col_target, .parents = NA, level = 0)

fp_paging(.data, col_paging, .parents = NA, level = 0)

fp_created(.data, col_created, timestamp = FALSE, .parents = NA, level = 0)
}
\arguments{
\item{.data}{the loaded data from Facepager}

\item{timestamp}{on Twitter, the timestamp must be parsed (timestamp == "twitter"),
on Instagram, the timestamp must first be converted to a date (timestamp == TRUE)}

\item{col_target=NA}{coverage per object ID}

\item{col_paging=NA}{offcuts and last cursor per object ID}

\item{col_created=NA}{first and last date per object ID}

\item{level=0}{the node level}
}
\value{
a tibble with ?
}
\description{
Get the collected data per Object ID

Helper function for fp_children()

Helper function for fp_children()

helper function for fp_children()
}
\examples{
fp_children (data)
}
