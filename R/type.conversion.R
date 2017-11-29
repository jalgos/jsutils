###Type handling and stuff#######
#' @name type.conversion
#' @title Type conversion
#' @description Fonctions to convert character to other types.
#' @details Type conversion is often needed when scraping or aggregating data from different sources. This utility functions are mostly used by interface objects like MongoRTables.
NULL

#' @describeIn type.conversion creates an empty vector of type IDate
#' @export
empty.date <- function() data.table::as.IDate("1970-01-01")[0]

#' @describeIn type.conversion creates an empty vector of type ITime
#' @export
empty.time <- function() data.table::as.ITime("00:00:00")[0]

#' @describeIn type.conversion creates an empty vector of type POSIXct
#' @export
empty.posix <- function() as.POSIXct(numeric(0), origin = "1970-01-01")


#' @rdname type.conversion
#' @section Type maps:
#' \itemize{
#' \item{\code{util.type.fun.map}:} {List that maps type and empty vector creation}
#' }
#' @format NULL
#' @export
util.type.fun.map <- list(character = character,
                          factor = factor,
                          integer = integer,
                          numeric = numeric,
                          logical = logical,
                          IDate = empty.date,
                          IDate.y_m_d = empty.date,
                          ITime = empty.time,
                          IDate.y.m.d = empty.date,
                          IDate.d.m.y = empty.date,
                          IDate.ymd = empty.date,
                          POSIX = empty.posix)

#' @describeIn type.conversion wrapper around format to convert dates
#' @export
csv.convert.date <- function(format) function(x) format(x, format = format)

#' @rdname type.conversion
#' @section Type maps:
#' \itemize{
#' \item{\code{util.type.csv.map}:} {List that maps type and the conversion function to serialize the object. Used to send it to mongo}
#' }
#' @format NULL
#' @export
util.type.csv.map <- c(IDate = "integer",
                       IDate.y_m_d = "character",
                       IDate.y.m.d = csv.convert.date("%Y.%m.%d"),
                       IDate.d.m.y = csv.convert.date("%d.%m.%Y"),
                       IDate.ymd = csv.convert.date("%Y%m%d"),
                       POSIX = csv.convert.date("%Y%m%d %H:%M:%S") ## Won't take into account the timezone
                       )


#' @describeIn type.conversion Generic function to convert from chararcter to IDate
#' @param format format of the date
#' @seealso format
#' @export
gen.conv.date <- function(format) function(x) data.table::as.IDate(x, format = format)

#' @describeIn type.conversion Generic function to convert from chararcter to POSIX
#' @export
gen.conv.posix <- function(format) function(x) as.POSIXct(x, format = format)

#' @describeIn type.conversion Function to convert numeric to POSIXct objects
#' @param origin Origin date.
#' @export
sec.conv.posix <- function(origin = "1970-01-01") function(x) as.POSIXct(as.numeric(x), origin = origin)

#' @describeIn type.conversion Function to convert milliseconds to POSIXct objects
#' @export
mill.conv.posix <- function(origin = "1970-01-01") function(x) as.POSIXct(as.numeric(x) / 1000, origin = origin)

#' @describeIn type.conversion Function to convert integers to IDate objects
convIDate <- function(x)
{
    if(class(x) != "integer") x <- as.integer(x)
    data.table::as.IDate(x, origin = "1970-01-01")
}

#' Type Conversion
#'
#' Converts object that don't match a set of values
#' @param conv.fun Conversion function
#' @param default.val Default value
#' @param null.val A vector of values that should not be converted
#' @export
partial.conversion <- function(conv.fun, default.val, null.val = "")
{
    function(x)
    {
        F <- !x %in% null.val
        y <- rep(default.val, length(x))
        y[F] <- conv.fun(x[F])
        y
    }
}

#' @rdname type.conversion
#' @section Type maps:
#' \itemize{
#' \item{\code{util.type.conv.map}:} {List that maps the type and the function that converts character into the desired type}
#' }
#' @format NULL
#' @export
util.type.conv.map <- list(IDate = convIDate,
                           ITime = function(x) data.table::as.ITime(x, format = "%H:%M:%S"),
                           factor = as.factor,
                           IDate.y_m_d = data.table::as.IDate,
                           IDate.y.m.d = gen.conv.date("%Y.%m.%d"),
                           IDate.d.m.y = gen.conv.date("%d.%m.%Y"),
                           IDate.ymd = gen.conv.date("%Y%m%d"),
                           POSIXct = partial.conversion(as.POSIXct, default.val = as.POSIXct(NaN, origin = "1970-01-01")),
                           POSIX = gen.conv.posix("%Y-%m-%dT%H:%M:%S"),
                           POSIXsec = sec.conv.posix(),
                           POSIXmill = mill.conv.posix(),
                           integer = as.integer,
                           numeric = as.numeric)

#' Converts Date to Hexadecimal
#'
#' Conversion tool that is sometimes needed to interface with other services
#' @export
date.to.hexmode <- function(d)
{
    as.hexmode(as.integer(as.POSIXct(d)))
}

#' Converts Date
#'
#' Converts various types of Date to a tractable one
#' @param date date vector
#' @param date.class class to be converted to
#' @param origin origin for type conversion fonction
#' @export
convert.generic.date <- function(date,
                                 date.class,
                                 origin = "1970-01-01")
{
    do.call(paste0("as.", date.class), list(date, origin = origin))
}
