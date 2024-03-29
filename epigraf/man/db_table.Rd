% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{db_table}
\alias{db_table}
\title{Get data from a database table}
\usage{
db_table(table, db, deleted = FALSE, cond = c())
}
\arguments{
\item{table}{Table name}

\item{db}{A connection object (object) or the database name (character)}

\item{deleted}{Deleted records are skipped by default. Set to TRUE, to get all records.}

\item{cond}{A character or a character vector of conditions, e.g.
"id = 10"}
}
\description{
Get data from a database table
}
