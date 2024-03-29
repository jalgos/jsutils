% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{divide.args}
\alias{divide.args}
\title{Scripts Argument}
\usage{
divide.args(args = commandArgs())
}
\arguments{
\item{args}{Command line arguments that need to be parsed}
}
\description{
Transform the command line arguments into a named character vector
}
\details{
Will split occurence such as (--)?option=value into a named character vector of value where the names are the option names.
}
\examples{
\code{divide.args('RScript --my.number=10 my.fit.param1=up --all.extract=1.56')}
}
