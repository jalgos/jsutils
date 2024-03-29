% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.save.objects.R
\name{save_load}
\alias{save_load}
\alias{save.object}
\alias{read.object}
\title{Saving Loading Objects}
\usage{
save.object(object, file, ..., logger = NULL)

read.object(file, ..., logger = NULL)
}
\arguments{
\item{object}{object to save}

\item{file}{full path of the saving destination}

\item{...}{To be passed to the saving / reading method}

\item{logger}{JLogger}
}
\description{
Saving Loading Objects
}
\section{Functions}{
\itemize{
\item \code{save.object}: Uses `saveRDS`, `qs::qsave`, `arrow::write_parquet`, ... based on the extension of file

\item \code{read.object}: Uses `readRDS`, `qs::qread`, `arrow::read_parquet`, ... based on the extension of file
}}

