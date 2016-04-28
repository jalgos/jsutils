## Common functions on data.table. Some are actually natively available in the package. We encourage the user to check

#' Plus Join
#'
#' Plus join is a type of join where the columns of the same name are added together. Missing values in one of the two tables are considered to be 0 by default
#' @param D1 first data.table
#' @param D2 second data.table
#' @param col.plus Columns to add
#' @param all Forwarded to data.table::merge. Control which records should be kept
#' @param ... Parameters forwarded to data.table::merge. 'by' needs to be provided
#'@export
plus.join <- function(D1,
                      D2,
                      col.plus = setdiff(intersect(names(D1), names(D2)), by),
                      all = TRUE,
                      ...)
{
    DM <- data.table::merge(D1, D2, all = all, ...)
    lsa <- sapply(col.plus, function(x) fun.expr("jsutils::safe.sum", c(paste0(x, ".x"), paste0(x, ".y"))))
    ple <- assign.expr(col.plus, fun.expr("list", lsa))    
    DM[, eval(parse(text = ple))]
    delr <- assign.expr(mpaste0(col.plus,  c(".x", ".y")), "NULL")
    DM[, eval(parse(text = delr))]
    DM
}

plus.join.test <- function()
{
    dt1 <- data.table(u = sample(letters, 15), x = 1:15, b = rexp(15))
    dt2 <- data.table(u = sample(letters, 15), x = 1:15, b = rexp(15))
    data.table::setkey(dt1, u)
    data.table::setkey(dt2, u)
    list(dt1 = dt1, dt2 = dt2, res = plus.join(dt1, dt2))
}

## Rendered obsolete by melt and dcast
dtextract1 <- function(DT,
                       var,
                       var.name,
                       var.value)
{
    filt <- sprintf("%s == '%s'", var.name, var)
    DF <- DT[eval(parse(text = filt))]
    DF[, eval(var.name) := NULL]
    setnames(DF, var.value, var)
}

## DITTO
cols.from.var <- function(DT,
                          var.name,
                          var.value)
{
    vars <- unique(DT[, var.name, with = FALSE][[1]])
    L <- lapply(vars, dtextract1, DT = DT, var.name = var.name, var.value = var.value)
    over(L, cbind)
}

#' Converting column types
#'
#' Converts a set of columns.
#' @param D Table to convert
#' @param lconv Mapping between column names and new types
#' @export
convert.columns <- function(D, lconv)
{
    N <- length(lconv)
    nmconv <- intersect(names(lconv), names(D))
    if(N < 1) return(D)
    for(nm in nmconv)
    {
        t <- lconv[[nm]]
        convert.cols(D, nm, t)
    }
    D
}

convert.cols <- function(D, col, type___)
{
    cl <- class(D[[col]])
    if(type___ %in% cl) return()
    FUN <- util.type.conv.map[[type___]]
    if(!is.null(FUN)) suppressWarnings(D[, eval(col) := FUN(D[[col]])])
    else D[, eval(col) := suppressWarnings(as(D[[col]], type___))]
}

#' Column classes
#'
#' Shortcut to get the class of each columns of the table
#' @param D table
#' @export
col.classes <- function(D)
{
    D[, sapply(.SD, function(x) class(x)[1])]
}

add.token.key <- function(D)
{
    if(dim(D)[1] > 0) D[, key := 1L]
    else D[, key := integer(0)]
}

#' Cartesian product of two data.tables
#'
#' Will create a table of size nrow(D1) * nrow(D2) consisting of all the possible combinations of rows in D1 and D2
#' @param D1 data.table
#' @param D2 data.table
#' @param size.limit A limit on the size of the result. If the result exceeds that size data is randomly sampled from D1 and D2
#' @param ... Not used
#' @export
cartesian.data.table <- function(D1,
                                 D2 = D1,
                                 size.limit = Inf,
                                 ...)
{
    if(is.null(D1) || is.null(D2)) return(NULL)
    N1 <- dim(D1)[1]
    N2 <- dim(D2)[1]
    PS <- as.numeric(N1) * as.numeric(N2)
    if(PS > size.limit)
    {
        rsample <- sqrt(size.limit / PS)
        N1 <- sample(1:N1, as.integer(rsample * N1))
        N2 <- sample(1:N2, as.integer(rsample * N2))
        D1 <- copy(D1[N1])
        D2 <- copy(D2[N2])
    }
    else
    {
        D1 <- copy(D1)
        D2 <- copy(D2)
    }

    common.names <- intersect(names(D1), names(D2))
    if(length(common.names) > 0)
    {
        setnames(D1, common.names, paste0(common.names, "1"))
        setnames(D2, common.names, paste0(common.names, "2"))
    }
    add.token.key(D1)
    add.token.key(D2)
    
    data.table::setkey(D1, key)
    data.table::setkey(D2, key)
    D <- D1[D2, allow.cartesian = TRUE]
    D[, key := NULL]
    D
}

#' Cartesian product of a several data.tables
#'
#' @param L list of data.tables
#' @param ... Parameters to be forwarded to cartesian.data.table
#' @export
cartesian.list <- function(L, ...)
{
    ## yet another split and run
    N <- length(L)
    if(N == 1) return(as.data.table(L))
    n <- N %/% 2
    cartesian.data.table(cartesian.list(L[1:n]), cartesian.list(L[(n + 1):N]), ...)
}
#' Cartesian product of several vectors
#'
#' @param ... vectors that will be used to compute the cartesian product
#' @export
create.cartesian.dt <- function(...)
{
    data.table::as.data.table(expand.grid(...))
}

#' Cartesian Apply
#'
#' Applies a function for all possible combinations of a set of parameters using a data.table
#' @param funapp Function to apply
#' @param Largs list of parameters
#' @export
dt.cart.apply <- function(funapp, Largs, ...)
{
    DT <- data.table::as.data.table(expand.grid(Largs))
    cc <- col.classes(DT)
    ncvt <- names(DT)[cc == "factor"]
    if(length(ncvt) > 0)
    {
        cvt <- rep("character", length(ncvt))
        names(cvt) <- ncvt
        convert.columns(DT, cvt)
    }
    if(is.character(funapp)) funapp <- get(funapp)
    DT[, do.call(funapp, c(.BY, list(...))), by = c(names(DT))]
}
#' Get column names using regex
#'
#' Will get the column names of a table that match a given regex
#' @param reg.names regex to use
#' @param DT data.table
#' @param ignore.case sets case tolerance, default is case doesn't matter
#' @param value returning indices or names, defaults to names
#' @param ... forwarded to grep
#' @export
get.dt.names <- function(reg.names,
                         DT,
                         ignore.case = TRUE,
                         value = TRUE,
                         ...)
{
    grep(reg.names, names(DT), value = value, ignore.case = ignore.case, ...)
}
#' @title Building expressions
#' @name build.expr
#' One way to parameterize complex expression in the 'j' argument of "[" for data.tables is to yse expressions.
#' @param funstring Function to apply to args as a string
#' @param args Character vector for the arguments of the function. The value is the variable name anf the names are the parameter names of the function
#' @param ... not used
#' @details 'fun.expr' produces expression of the form 'f(a, b ,c)'. 'assign.expr' produces expression such as 'c("a", "b") := list(f(c), g(d))'
#' @examples fun.expr("atan2", c(x = 3, y = 4))
fun.expr <- function(funstring, args, ...)
{
    nms <- names(args)
    sepa <- rep("", length(args))
    sepa[nms != ""] <- "="
    args <- paste(paste(nms, sepa, args, sep = ""), collapse = ",")
    paste(c(funstring, "(", args, ")"), collapse = "")
}

#' @rdname buld.expr
#' @param var new variable names to be assigned. character vector
#' @param exprst string for computing the new columns
#' @examples assign.expr(c("a", "b"), "list(x + y, cos(z))")
assign.expr <- function(var, exprst)
{
    var <- paste0("'", var, "'")
    var <- fun.expr("c", var)
    paste(var, exprst, sep = ":=")
}

#' @title Filtering non-finite values
#' @description Computed the set of rows that have a non-finite value in the numeric columns.
#' @name filter.na
#' @return 'dt.filter.na' returns the set of bad rows while 'dt.filter.out.na' returns a data.table where the bad rows have been filtered out
#' @param DT Table to be filtered
#' @param numeric.cols Numeric columns in the table, defaults to the entire table
#' @param good.fun Function that filters out bad values. It should return FALSE for the unwanted values. It will be applied on each column one by one
#' @param ... Extra parameters to be passed on to 'dt.filter.na' and 'good.fun'
#' @export
dt.filter.na <- function(DT,
                         numeric.cols = names(DT),
                         good.fun = is.finite,
                         ...)
{
    DT[, over(lapply(.SD, good.fun, ...), "&"), .SDcols = numeric.cols]
}
#' @export
dt.filter.out.na <- function(DT,
                             ...)
{
    F <- dt.filter.na(DT, ...)
    DT[F]
}

#' Change column suffixes
#'
#' Changes the default column suffixes added by data.table when merging
#' @param TAB data.table
#' @param ter1 new pattern for suffix 1
#' @param ter2 new pattern for suffix 2
#' @param termination1 old pattern for suffix 1
#' @param termination2 old pattern for suffix 2
#' @export
change.double.cols <- function(TAB,
                               ter1,
                               ter2,
                               termination2 = "\\.1",
                               termination1 = "")
{
    nm <- grep(termination2, names(TAB), value = TRUE)
    nmw <- gsub(termination2, termination1, nm)
    nm1 <- gsub(termination2, ter1, nm)
    nm2 <- gsub(termination2, ter2, nm)
    data.table::setnames(TAB, c(nmw, nm), c(nm1, nm2))
}
