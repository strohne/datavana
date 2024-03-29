% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/craft.R
\name{.craft_add_id}
\alias{.craft_add_id}
\title{Add ID column}
\usage{
.craft_add_id(ds, newcol, table, type, col_id, parent = NULL)
}
\arguments{
\item{ds}{A tibble}

\item{newcol}{New column}

\item{table}{The table}

\item{type}{The type as character value or an unquoted column name}

\item{col_id}{The column where IDs are stored or a single character value}

\item{prefix}{Will be inserted between type and col_id (e.g. for sections, the article id)}
}
\description{
Add ID column
}
