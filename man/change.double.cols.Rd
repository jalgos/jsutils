% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.data.table.R
\name{change.double.cols}
\alias{change.double.cols}
\title{Change column suffixes}
\usage{
change.double.cols(TAB, ter1, ter2, termination2 = "\\\\.1", termination1 = "")
}
\arguments{
\item{TAB}{data.table}

\item{ter1}{new pattern for suffix 1}

\item{ter2}{new pattern for suffix 2}

\item{termination2}{old pattern for suffix 2}

\item{termination1}{old pattern for suffix 1}
}
\description{
Changes the default column suffixes added by data.table when merging
}
