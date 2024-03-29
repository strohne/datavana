% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractdata.R
\name{fp_children_bynode}
\alias{fp_children_bynode}
\title{Get the number of childnodes for each node shown in Facepager}
\usage{
fp_children_bynode(data, .global = T, .maxlevel = 1)
}
\arguments{
\item{data}{the loaded data from Facepager}

\item{.maxlevel}{the node level}

\item{.global=T}{...}
}
\value{
a tibble with 4 columns: parent_id, parent_no, parent_objectid, children

a boxplot
}
\description{
Get the number of childnodes for each node shown in Facepager
}
\examples{
fp_children_bynode(data)
}
