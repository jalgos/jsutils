% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_matrix.R
\name{gen.inverse}
\alias{gen.inverse}
\title{Generalized Inverse}
\usage{
gen.inverse(a, b, fsolve = solve)
}
\arguments{
\item{a}{Matrix}

\item{b}{Right hand side of the equation a.x = b}

\item{fsolve}{The function to use to solve the generalized linear system}
}
\description{
Computes the generalized solution to a linear system for rectangular matrices.
}
