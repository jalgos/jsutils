% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.control.flow.R
\name{conditions}
\alias{conditions}
\alias{condition}
\alias{is.condition}
\alias{is.error}
\alias{is.warning}
\alias{is.message}
\alias{custom.error}
\alias{custom.warning}
\alias{custom.message}
\alias{dimension.mismatch.error}
\alias{non.finite.error}
\alias{maxiter.error}
\alias{no.convergence.error}
\title{Custom Conditions}
\usage{
condition(
  subclass,
  mess,
  parent.class = character(0),
  call = sys.call(-1),
  ...
)

is.condition(x)

is.error(x)

is.warning(x)

is.message(x)

custom.error(...)

custom.warning(...)

custom.message(...)

dimension.mismatch.error()

non.finite.error()

maxiter.error()

no.convergence.error()
}
\arguments{
\item{subclass}{Class of the condition}

\item{parent.class}{Must be a subclass of "condition". "error", "warning" and "message" are valid}

\item{call}{Needed by condition}

\item{...}{to be added to the structure}

\item{x}{condition object}
}
\description{
Custom Conditions
}
\section{Functions}{
\itemize{
\item \code{condition}: Taken as is form http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling. Creates a custom condition that can later on easily caught.

\item \code{is.condition}: checks whether object is a condition

\item \code{is.error}: checks whether object is a condition

\item \code{is.warning}: checks whether object is a condition

\item \code{is.message}: checks whether object is a condition

\item \code{custom.error}: Creates a custom error

\item \code{custom.warning}: Creates a custom warning

\item \code{custom.message}: Creates a custom message

\item \code{dimension.mismatch.error}: Useful error: dimension mismatch

\item \code{non.finite.error}: Useful error: NA or infinite values

\item \code{maxiter.error}: Useful error: Maximum number of iterations

\item \code{no.convergence.error}: Useful error: Model does not converge
}}

\examples{
## Custom conditions are caught with \code{tryCatch} or \code{withCallingHandlers} as follows:
tryCatch(stop(condition("mycondition", "Hello Worms!")),
         mycondition = function(cond) print("Yiiiihaaaa!"))
}
