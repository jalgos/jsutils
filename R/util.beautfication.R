
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
