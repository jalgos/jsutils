% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_matrix.R
\name{mtdt.add.row.col.names}
\alias{mtdt.add.row.col.names}
\title{Matrix to data.table}
\usage{
mtdt.add.row.col.names(D, M, one.based = TRUE)
}
\arguments{
\item{D}{triplet representation}

\item{M}{Matrix}

\item{one.based}{indices start at 1 or 0?}
}
\description{
Adds two columns i.names and j.names to the triplet represantation of a Matrix
}
