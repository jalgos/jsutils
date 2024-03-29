% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type.conversion.R
\docType{data}
\name{type.conversion}
\alias{type.conversion}
\alias{empty.date}
\alias{empty.time}
\alias{empty.posix}
\alias{util.type.fun.map}
\alias{csv.convert.date}
\alias{util.type.csv.map}
\alias{gen.conv.date}
\alias{gen.conv.posix}
\alias{sec.conv.posix}
\alias{mill.conv.posix}
\alias{convIDate}
\alias{util.type.conv.map}
\title{Type conversion}
\usage{
empty.date()

empty.time()

empty.posix()

util.type.fun.map

csv.convert.date(format)

util.type.csv.map

gen.conv.date(format)

gen.conv.posix(format)

sec.conv.posix(origin = "1970-01-01")

mill.conv.posix(origin = "1970-01-01")

convIDate(x)

util.type.conv.map
}
\arguments{
\item{format}{format of the date}

\item{origin}{Origin date.}
}
\description{
Fonctions to convert character to other types.
}
\details{
Type conversion is often needed when scraping or aggregating data from different sources. This utility functions are mostly used by interface objects like MongoRTables.
}
\section{Functions}{
\itemize{
\item \code{empty.date}: creates an empty vector of type IDate

\item \code{empty.time}: creates an empty vector of type ITime

\item \code{empty.posix}: creates an empty vector of type POSIXct

\item \code{csv.convert.date}: wrapper around format to convert dates

\item \code{gen.conv.date}: Generic function to convert from chararcter to IDate

\item \code{gen.conv.posix}: Generic function to convert from chararcter to POSIX

\item \code{sec.conv.posix}: Function to convert numeric to POSIXct objects

\item \code{mill.conv.posix}: Function to convert milliseconds to POSIXct objects

\item \code{convIDate}: Function to convert integers to IDate objects
}}

\section{Type maps}{

\itemize{
\item{\code{util.type.fun.map}:} {List that maps type and empty vector creation}
}


\itemize{
\item{\code{util.type.csv.map}:} {List that maps type and the conversion function to serialize the object. Used to send it to mongo}
}


\itemize{
\item{\code{util.type.conv.map}:} {List that maps the type and the function that converts character into the desired type}
}
}

\seealso{
format
}
\keyword{datasets}
