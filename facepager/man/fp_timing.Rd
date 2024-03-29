% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{fp_timing}
\alias{fp_timing}
\title{Get requests per minute}
\usage{
fp_timing(data, f.objecttype = "data", f.querystatus = "fetched (200)")
}
\arguments{
\item{data}{the loaded data from Facepager}

\item{f.objecttype}{object types to be included in the calculation of requests/minute.
Usually "data", because "seed" and "offcut" do not contain any data.}

\item{f.querystatus}{query status to be included in the calculation of requests/minute.
Only those nodes are included here that have been fetched in full like "fetched(200)".}
}
\value{
a list with Start, End, Duration, Active hours, Active minutes and Requests per day
a line plot with requests per minute as yvalue and the time es xvalue
}
\description{
Get requests per minute
}
\examples{
fp_timing(data)
}
