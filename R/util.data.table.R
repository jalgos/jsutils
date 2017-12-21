## Common functions on data.table. Some are actually natively available in the package. We encourage the user to check

#' @importClassesFrom data.table data.table
#' @importFrom data.table data.table as.data.table setkey copy
NULL

## Creating generics


#' @export
setGeneric('setkeyv', data.table::setkeyv)

#' @export
setGeneric('key', data.table::key)

#' @export
setGeneric('setkey', data.table::setkey)



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
                      by,
                      all = TRUE,
                      ...)
{
    DM <- merge(D1, D2, all = all, by = by, ...)
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
    setkey(dt1, u)
    setkey(dt2, u)
    list(dt1 = dt1, dt2 = dt2, res = plus.join(dt1, dt2))
}

to.convention.names <- function(names)
{
    conv.names <- gsub("([A-Z]+)([A-Z][a-z])", "\\1.\\2", names)
    conv.names <- gsub("([a-z\\d])([A-Z])", "\\1.\\2", conv.names)
    conv.names <- gsub("-|_", ".", conv.names)

    return(tolower(conv.names))
}

#' Set convention names
#'
#' Turn every column name of a data table to a convention format with lower case and point separated column names
#'
#' @param DT data.table
#'
#' @export
set.convention.names <- function(DT,
                                 ...,
                                 logger = logger.fun.name.logger())
{
    conv.names <- to.convention.names(names(DT))
    jlog.debug(logger, "Turning names", paste(names(DT), collapse = " ") %c% BY, "to", conv.names %c% BC)
    setnames(DT, names(DT), conv.names)
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

#' @title Converting column types
#' @name convert.columns
NULL

#' @describeIn convert.columns Converts a set of columns.
#' @param D Table to convert
#' @param lconv Mapping between column names and new types
#' @export
convert.columns <- function(D, lconv, ...)
{
    old.class <- class(D)
    as.data.table(D)
    N <- length(lconv)
    nmconv <- intersect(names(lconv), names(D))
    if(N < 1) return(D)
    for(nm in nmconv)
    {
        t <- lconv[[nm]]
        convert.cols(D, nm, t, ...)
    }
    setattr(D, "class", old.class)
    D
}

#' @describeIn convert.columns Converts one column to desired type. Using convert.columns is prefered that directly using this function
#' @param col Column to convert
#' @param type___ Type to convert to
#' @param conv.map Map that gives the converting function for each type
#' @export
convert.cols <- function(D,
                         col,
                         type___,
                         conv.map = util.type.conv.map,
                         ...)
{
    cl <- class(D[[col]])
    if(type___ %in% cl) return()
    FUN <- conv.map[[type___]]
    if(is.null(FUN))
        FUN <- function(x) as(x, type___)
    jconv <- parse(text = sprintf("%s := FUN(%s)", col, col))
    suppressWarnings(D[, eval(jconv)])  
}

#' Column classes
#'
#' Shortcut to get the class of each columns of the table
#' @param D table
#' @param cols For which columns the classes should be returned
#' @param class.fun Which function should be used to determine the type. Can be `typeof` or `class` or any function of your choice
#' @param apply.fun Should we use `lapply` or `sapply`?
#' @export
col.classes <- function(D,
                        cols = names(D),
                        class.fun = function(X) class(X)[1],
                        apply.fun = sapply)
{
    ccs <- apply.fun(cols, function(col) class.fun(D[[col]]))
    names(ccs) <- cols
    ccs
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
#' @import data.table
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
    
    setkey(D1, key)
    setkey(D2, key)
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
    as.data.table(expand.grid(...))
}

#' Cartesian Apply
#'
#' Applies a function for all possible combinations of a set of parameters using a data.table
#' @param funapp Function to apply
#' @param Largs list of parameters
#' @export
dt.cart.apply <- function(funapp, Largs, ...)
{
    DT <- as.data.table(expand.grid(Largs))
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

#' @title Filtering non-finite values
#' @description Computed the set of rows that have a non-finite value in the numeric columns.
#' @name filter.na
#' @return 'dt.filter.na' returns the set of bad rows while 'dt.filter.out.na' returns a data.table where the bad rows have been filtered out
#' @param DT Table to be filtered
#' @param numeric.cols Numeric columns in the table, defaults to the entire table
#' @param good.fun Function that filters out bad values. It should return FALSE for the unwanted values. It will be applied on each column one by one
#' @param ... Extra parameters to be passed on to 'dt.filter.na' and 'good.fun'
NULL

#' @rdname filter.na
#' @export
dt.filter.na <- function(DT,
                         numeric.cols = names(DT),
                         good.fun = is.finite,
                         ...)
{
    DT[, over(lapply(.SD, good.fun, ...), "&"), .SDcols = numeric.cols]
}
#' @rdname filter.na
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
    setnames(TAB, c(nmw, nm), c(nm1, nm2))
}

type.funs <- list(numeric = numeric,
                  integer = integer,
                  character = character,
                  logical = logical,
                  factor = make.empty.factors,
                  IDate = function(N) as.IDate(rep(NA, N)))

#' Building a data.table
#' 
#' Creates empty columns of a given type
#' @param type string that defines the type
#' @param N length of the column
#' @export
empty.column <- function(type, N)
{
    ft <- type.funs[[type]]
    ft(N)
}

load.data.table <- function(io, io.header, col.types, ...)
{
    H <- scan(io.header, what = "character", sep = ",")
    ctypes  <- unlist(col.types[H])
    as.data.table(read.table(io, colClasses = ctypes, ...))
}

#' Evaluating valid subsets
#'
#' Evaluate several filters related to a 'data.table'
#' @param Tab a data.table
#' @param Filts list of expressions or logical vectors.
#' @export
evalLand <- function(Tab, Filts)
{
    RF <- TRUE
    if(class(Filts) == "logical") return(Filts)
    
    if(class(Filts) == "expression")
    {
        for(F in Filts) RF  <- RF & Tab[, eval(F)]
        RF[!is.finite(RF)] <- FALSE
        return(RF)
    }
    for(i in 1:length(Filts)) RF <- RF & Tab[, eval(Filts[[i]])]
    RF[!is.finite(RF)] <- FALSE
    RF
}

#' Centering columns
#'
#' Recenters a set of columns
#' @export
recenter <- function(T, name.cols, fun = mean, ...)
{
    for(name.col in name.cols)
    {
        setnames(T, name.col, "x..c")
        T[, x..c := x..c - fun(x..c, ...)]
        setnames(T, "x..c", name.col)
    }
}

#' Adding ranking columns
#'
#' Adds a rank column for the column specified
#' @param T a data.table
#' @param name.cols the columns to be ranked
#' @param prefix the prefix that will be appended to the ranking columns
#' @param pct Should the ranking be expressed in percentage
#' @param by A grouping to do the ranking in
#' @export
add.rank.column <- function(T, name.cols, prefix = "rnk", pct = FALSE, by = NULL)
{
    for(name.col in name.cols)
    {
        N <- dim(T)[1]
        ex.str <- "%rncol := rank(-%col)"
        if(pct) ex.str <- paste0(ex.str, "/N")
        ex.str <- gsub("%col", name.col, ex.str)
        ex.str <- gsub("%rncol", paste(prefix, name.col, sep="."), ex.str)
        T[, eval(parse(text = ex.str)), by = by]
    }
    T
}

#' Flipping a data.table
#'
#' Transposes a data.table so the columns become the rows and vice versa
#' @param DT a data.table
#' @param by Columns to be ommited
#' @export
flip <- function(DT, by)
{
    #nicely prints low dimension data tables
    if(missing(by)) return(cbind(data.table(col = names(DT)), t(as.matrix(DT))))
    keys <- as.character(DT[[by]])
    DT <- DT[, setdiff(names(DT), by), with = FALSE]
    FD <- as.data.table(t(as.matrix(DT)))
    setnames(FD, keys)
    return(cbind(data.table(col = names(DT)), FD))
}

#' @name dt.grep
#' @title Merging using grep
#' @description These are tools to merge data.tables using grep instead of an exact match
#' @param D A data.table
#' @param D1 A data.table
#' @param D2 A data.table
#' @param keys Keys to exactly match on
#' @param vals Values to match. A list with one entry per column
#' @param akeys Approximate keys. List of columns to match on.
#' @param fgrep Grep function to use
#' @param keep.match.col Should the columns used to match be included in the result
NULL

#' @describeIn dt.grep Returns a subset of D which matches vals for the columns akeys
#' @export
dtgrep <- function(D, vals, akeys, fgrep = grep, keep.match.col = FALSE, ...)
{
    if(is.character(fgrep)) fgrep <- getFunction(fgrep)
    if(length(akeys) == 0) return(D)
    
    if(!keep.match.col) rcols <- setdiff(names(DT), akeys)
    
    if(dim(D)[1] > 0) F <- D[, over(mapply(fgrep, vals, .SD, MoreArgs = list(...), SIMPLIFY = FALSE), intersect), .SDcols = akeys]
    else F <- integer(0)
    
    if(sum(F) > 0) MD <- copy(D[F, .SD, .SDcols = rcols])
    else MD <- copy(D[0, .SD, .SDcols = rcols][1])
    
    if(keep.match.col) setnames(MD, akeys, paste(akeys, "match", sep = "."))
    return(MD)
}


#' @describeIn dt.grep Merges with grep
#' @export
grep.merge <- function(D1, D2, keys = character(0), akeys = character(0), fgrep = grep, ...)
{
    ## performs a merge using an approximative grep
    ## D = merge(D1,D2,keys = keys)
    if(nrow(D1) == 0)
    {
        R <- merge(D1, D2, by = c(keys, akeys))
        R[,eval(paste(akeys, "match", sep=".")) := R[, akeys, with = FALSE]]
        return(R)
    }
    nk <- length(keys)
    if(nk > 0) FK <- 1:nk
    else FK <- integer(0)
    nak <- length(akeys)
    FAK <- setdiff(1:(nk + nak), FK)
    
    R <- D1[, dtgrep(dtgrep(D2, .BY[FK], keys, which.equals), .BY[FAK], akeys, fgrep, keep.match.col = TRUE, ...), by = c(keys, akeys)]
    return(R)
}

#' @describeIn dt.grep Approximate merge
#' @export
amerge <- function(D1, D2, keys = character(0), akeys = character(0), fgrep = grep, similarity.fun, ...)
{
    if(length(keys) == 0) D <- cartesian.data.table(D1, D2)
    else D <- merge(D1, D2, by = keys, suffixes = c("1", "2"))
    akeys1 <- paste0(akeys, "1")
    akeys2 <- paste0(akeys, "2")
    D[similarity.fun(as.data.frame(D)[akeys1], as.data.frame(D)[akeys2], ...)]   
}

#' Random data set
#'
#' Generate a random data sets using random.cat.var
#' @seealso random.cat.var
#' @param set.size Size of the data set
#' @param varsdescription A list of categorical variable description to be forwarded to random.cat.var
#' @export
random.data.set <- function(set.size, varsdescription)
{
    D <- NULL
    as.data.table(lapply(varsdescription, random.cat.var, set.size))
}

#' Removing duplicate records
#'
#' Returns a data.table where there is only one record by group. By default the first record is chosen
#' @param D data.table
#' @param key set of columns that defines a unique record
#' @seealso data.table::unique
#' @export
remove.duplicates.dt <- function(D, key)
{
    D[,`.nrecord` := 1:.N, by = c(key)]
    D <- D[`.nrecord` == 1]
    D[,`.nrecord`:= NULL]
    D
}

#' Identify record
#'
#' Creates a column that uniquely identifies a set of records
#' @param cols Column names
#' @param vals Values
#' @export
make.id <- function(cols,
                    vals)
{
    over(mapply(cols, vals, FUN = paste0, SIMPLIFY = FALSE), paste0, "")
}

#' @export
setGeneric("merge", merge)

## Util for ddata.table

#' In place assignation
#'
#' Modifies a list in place
#' @param target List to modify
#' @param src Elements to assign to the list
#' @export
inplace <- function(target, src)
{
    invisible(.Call('jsutils_inplace', PACKAGE = 'jsutils', target, src))
}
