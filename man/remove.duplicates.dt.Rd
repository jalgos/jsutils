% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.data.table.R
\name{remove.duplicates.dt}
\alias{remove.duplicates.dt}
\title{Removing duplicate records}
\usage{
remove.duplicates.dt(D, key)
}
\arguments{
\item{D}{data.table}

\item{key}{set of columns that defines a unique record}
}
\description{
Returns a data.table where there is only one record by group. By default the first record is chosen
}
\seealso{
data.table::unique
}
