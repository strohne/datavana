% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.R
\name{epi_create_property_items}
\alias{epi_create_property_items}
\title{Create filled items and properties from values}
\usage{
epi_create_property_items(
  data,
  col_articletype,
  col_value,
  col_prop,
  sectiontype,
  itemtype
)
}
\arguments{
\item{data}{A data frame containing the columns articletype and norm_iri}

\item{col_articletype}{The column in data specifying the articletype}

\item{col_value}{The column in data specifying the value}

\item{col_prop}{The column in data containing the propertytype}

\item{sectiontype}{A string with the name of the section}

\item{itemtype}{A string with the name of the itemtype}
}
\description{
Create filled items and properties from values
}
