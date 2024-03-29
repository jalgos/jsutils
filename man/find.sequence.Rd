% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{find.sequence}
\alias{find.sequence}
\title{Finding Sequences}
\usage{
find.sequence(R, increment = 1L, sort = FALSE, unique = FALSE)
}
\arguments{
\item{R}{vector}

\item{increment}{value}

\item{sort}{should the vector be sorted beforehand}

\item{unique}{are we only interested in unique occurences}
}
\description{
Finds sequences in a vector. Sequences are subset of consecutive items. Two items are consecutive if x[N + 1] - x[N] == increment
}
