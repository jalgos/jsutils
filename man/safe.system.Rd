% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{safe.system}
\alias{safe.system}
\title{Call to System}
\usage{
safe.system(
  command,
  error.message = paste("Error running command:", command),
  safe = TRUE,
  ...
)
}
\arguments{
\item{command}{Command to be executed}

\item{error.message}{Message in case the system call fails}

\item{safe}{Should errors be caught?}
}
\description{
Calls 'system' and returns an error if the call failed
}
