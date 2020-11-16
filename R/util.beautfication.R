
#' Wider Printing
#' 
#' Increases width of object printing
#' @details Courtesy of Steve Lianoglou
#' http://stackoverflow.com/questions/1172485/how-to-increase-the-number-of-columns-using-r-in-linux
#' @param howWide The width in number of columns to print tables and matrices
#' @export
wide.screen <- function(howWide = Sys.getenv("COLUMNS"))
{
    options(width = as.integer(howWide))
}


#' @export format.pct
format.pct <- function(value,
                       multiplier = 100,
                       digits = 2,
                       color = "BRIGHT.CYAN")
{
    color.string(format(value * multiplier,
                        digits = digits),
                 color)
}

#' Default Value if NULL provided
#'
#' Returns `default` if `val` is null
#' @param val Value to test for nullity
#' @param default Value to return if null
#' @export din
din <- function(val,
                default)
{
    if(is.null(val))
        default
    else
        val
}
    
