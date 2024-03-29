% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{transform.filename}
\alias{transform.filename}
\alias{replace.variables}
\title{Templating filenames}
\usage{
\method{transform}{filename}(fn, subs = base.sub.list)

replace.variables(fn, subs = base.sub.list)
}
\arguments{
\item{fn}{Template filename}

\item{subs}{named list that associates template variable and their values}
}
\description{
Tool to templatize filenames. Useful when needing a ressource that depends on the local configuration
}
\details{
The names in subs are regex patterns. Any matching regex patterns will be replaced by the value. To avoid replacing parts of words it is advised to start the template variable by a character that is not found in the string to replace. '$%' can be used for example. \cr
replace.variables is an alias.
}
\examples{
transform.filename("$wd/$dperl/my.script.$scnum.pl", c("$wd" = ".", "$dperl" = "perl", "$scnum" = "10"))
}
