#' @title Building expressions
#' @name build.expr
#' @description One way to parameterize complex expression in the 'j' argument of "[" for data.tables is to use expressions.
#' @param funstring Function to apply to args as a string
#' @param args Character vector for the arguments of the function. The value is the variable name anf the names are the parameter names of the function
#' @param ... not used
#' @details 'fun.expr' produces expression of the form 'f(a, b ,c)'. 'assign.expr' produces expression such as 'c("a", "b") := list(f(c), g(d))'
NULL

#' @describeIn build.expr Builds the expression that is the application of a function to some parameters
#' @examples fun.expr("atan2", c(x = 3, y = 4))
#' @export
fun.expr <- function(funstring,
                     args,
                     ...,
                     protect.names = FALSE)
{
    nms <- names(args)
    sepa <- rep("", length(args))
    nmv <- nms != "" 
    sepa[nms != ""] <- "="
    nms[nms != ""] <- '"' %p% nms[nms != ""] %p% '"'
    if(protect.names)
    {
        args <- '`' %p% args %p% '`'
    }
    args <- paste(paste(nms, sepa, args, sep = ""), collapse = ",")
    paste(c(funstring, "(", args, ")"), collapse = "")
}

#' @describeIn build.expr Builds an assignation expression (:=)
#' @param var new variable names to be assigned. character vector
#' @param exprst string for computing the new columns
#' @examples assign.expr(c("a", "b"), "list(x + y, cos(z))")
#' @export
assign.expr <- function(var, exprst)
{
    var <- paste0("'", var, "'")
    var <- fun.expr("c", var)
    paste(var, exprst, sep = ":=")
}

#' @describeIn build.expr Builds the expression of a list. To be put in j in "[.data.table"
#' @examples make.list.expression(c(a = "sin(x)", b = "x + y", c = "u + 4"))
#' @param exprvec named vector of character expresions.
#' @param parse Should the result be parsed into an object of type 'expression'
#' @param expr.names Names of the variables
#' @export
make.list.expression <- function(exprvec,
                                 parse = TRUE,                                 
                                 expr.names = names(exprvec))
{
    if(!length(expr.names)) expr.names <- paste("V", 1:length(exprvec), sep = "")
    varnames <- paste(paste("`", expr.names, sep=""), "`", sep = "")
    exprs <- paste(varnames, exprvec, sep = "=")
    exprs <- paste0(paste0("list(", paste(exprs, collapse = ",")), ")")
    if(!parse) return (exprs)
    return(parse(text = exprs))
}

#' @describeIn build.expr Add quotes around each entry of a character vector
#' @export
add.quotes <-function(var)
{
    paste0('"', paste0(var, '"'))
}
