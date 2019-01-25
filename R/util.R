
#' Useful settings
#'
#' Sets a few options and creates global variables to ease up development
#' @export
jalgos.settings <- function()
{
    options(datatable.allow.cartesian = TRUE)
    options(digits.secs = 6)
    Sys.setenv(R_HISTSIZE = "300000")

    DAYSECONDS <<- 24 * 60 * 60

    ##creates a dummy datatable to run tests on
    if(!"dummyDT" %in% ls(envir = .GlobalEnv)) dummyDT <<- data.table::data.table(a = c("HAHA", "HIHI", "HOHO", "HUHU"), b = rnorm(4), y = 1:4, dt = rep(Sys.Date(), 4))
}

#' Add Ellipsis
#'
#' Adds the ... argument to a function. Useful when ... is forwarded down to functions that don't take the ellipsis as argument
#' @export
add.ellipsis <- function(f) function(x, ...) f(x)

#' @name first.last
#' @title Easy access to first and last element
#' @param X a vector or a list
NULL

#' @describeIn first.last first element
#' @export
first <- function(X)
{
    if(is.list(X)) X[[1]]
    else X[1]
}

#' @describeIn first.last last element
#' @export
last <- function(X)
{
    if(is.list(X)) X[[length(X)]]
    else X[[length(X)]]
}

#' @name positive.negative.value
#' @title Positive and negative value
#' @param X Numeric vector
NULL

#' @describeIn positive.negative.value Positive value. Negative values are replaced with 0
#' @export
positive.val <- function(X)
{
    X[X < 0] <- 0
    X
}

#' @describeIn positive.negative.value Negative value. Positive values are replaced with 0
#' @export
negative.val <-function(X)
{
    X[X > 0] <-0
    X
}

#' Binding factor vectors
#'
#' Binds two factor vectors. Will take care of setting the new levels. Binding with a call to \code{c} returns a integer vector
#' @param F1 factor vector
#' @param F2 factor vector
#' @param lev New levels
#' @export
fusion.factors <- function(F1,
                           F2,
                           lev = union(levels(F1), levels(F2)))
{
    return(factor(c(as.character(F1), as.character(F2)), levels = lev))
}


#' Aligning factor levels
#'
#' Transform a list of factors into a list of factors with compatible levels
#' @param ... Any number of factor vectors
#' @param facts a list of factor vectors
#' @export
align.factors <- function(...,
                          facts = list(...))
{
    glevels <- unique(unlist(lapply(list(...), levels)))
    lapply(facts,
           function(fact) factor(as.character(fact), levels = glevels))
}
                          

#' Test equality between factors
#'
#' Tests equality for factor vectors with possibly different levels. This is simply done by converting the factors to character.
#' @param F1 factor or character vector
#' @param F2 factor or character vector
#' @export
eqfactors <- function(F1, F2)
{
    return(as.character(F1) == as.character(F2))
}


#' Dividing intervals
#' @export
divide.intervals <- function(D, n)
{
    il <- sapply(D, function(dh){dh[2] - dh[1]})
    if(sum(il) == 0)
    {
        dv <- unlist(lapply(D, function(d, nd){rep(d[1], nd)}, n / length(D)))
        if(length(dv) != n) dv <- c(dv, dv[1:(n - length(dv))])
        return(dv)
    }
    il <- il / sum(il)
    nil <- n * il
    pil <- nil %% 1;
    demi <- which(pil == .5)
    add <- .1
    for(i in demi)
    {
        nil[i] <- nil[i] + add;
        add <- -add;
    }
    nil <- round(nil);
    unlist(mapply(FUN = function(d, np){d[1] + (d[2] - d[1]) * seq(0.5, np - .5, 1) / np}, D, nil))
}

#' Scripts Argument
#'
#' Transform the command line arguments into a named character vector
#' @details Will split occurence such as (--)?option=value into a named character vector of value where the names are the option names.
#' @param args Command line arguments that need to be parsed
#' @examples \code{divide.args('RScript --my.number=10 my.fit.param1=up --all.extract=1.56')}
#' @export
divide.args <- function(args = commandArgs())
{
    add.args <- grep("=", args, value = TRUE)
    L <- strsplit(add.args, "=")
    R <- NULL
    for(l in L)
    {
        l[[1]] <- gsub("-", "", l[[1]])
        R[[l[[1]]]] <- l[[2]]
    }
    R
}

#' Templating filenames
#'
#' Tool to templatize filenames. Useful when needing a ressource that depends on the local configuration
#' @param fn Template filename
#' @aliases replace.variables
#' @param subs named list that associates template variable and their values
#' @examples transform.filename("$wd/$dperl/my.script.$scnum.pl", c("$wd" = ".", "$dperl" = "perl", "$scnum" = "10"))
#' @details The names in subs are regex patterns. Any matching regex patterns will be replaced by the value. To avoid replacing parts of words it is advised to start the template variable by a character that is not found in the string to replace. '$%' can be used for example. \cr
#' replace.variables is an alias.
#' @export transform.filename
transform.filename <- function(fn, subs = base.sub.list)
{
    if(is.null(subs)) return(fn)
    subs[sapply(subs, is.null)] <- ""

    NM <- sort(names(subs), decreasing = TRUE)
    for(nm in NM)
    {
        pat <- gsub(x = nm, pattern = "\\$", replacement = "\\\\$")
        fn <- as.vector(sapply(fn, {function(fn, repls) sapply(repls, function(repl) gsub(x = fn, pattern = pat, replacement = repl))}, repls = subs[[nm]]))
    }
    return(fn)
}

#' @rdname transform.filename
#' @export
replace.variables <- transform.filename

#' Empty factor vector
#'
#' Creates an empty factor vector of arbitrary size
#' @export
make.empty.factors <- function(N)
{
    factor(character(N))
}

## Not sure it still works
permut <- function(V)
{
    n <- length(V)
    if(n == 1)
    {
        return(list(V))
    }
    L <- NULL
    for(v in V)
    {
        Lv <- arrange(V[V != v])
        L <- c(L,lapply(Lv, c, v))
    }
    return(L)
}

#' @title Simple functions
#' @name simple.function
#' @description Aliases for very basic function that are used here and there
NULL

#' @describeIn simple.function returns 0
#' @export
null.fun <- function(...)
{
    0
}

#' @describeIn simple.function returns 1
#' @export
one.const.fun <- function(...)
{
    1
}

#' @describeIn simple.function returns \code{val}
#' @export
const.fun <- function(..., val = 0)
{
    val
}

#' @title Recursive paste
#' @name recursive.paste
#' @description Computes all the possible string concatenation between members of c1 and c2
#' @param c1 Character vector
#' @param c2 Character vector
#' @param ... parameters to be forwarded to paste or paste0
#' @seealso paste, paste0
NULL

#' @describeIn recursive.paste Uses paste
#' @export
mpaste <- function(c1, c2, ...)
{
    unlist(lapply(c1, paste, c2, ...))
}
#' @describeIn recursive.paste Uses paste0
#' @export
mpaste0 <- function(c1, c2, ...)
{
    unlist(lapply(c1, paste0, c2, ...))
}

#' @name scale.center
#' @title Scaling and centering numeric vectors
#' @description tools to center numeric vectors around their mean and scale them by their standard deviation
#' @param X numeric vector
#' @param cent Centering
#' @param scaling Scaling
NULL

#' @describeIn scale.center Centers a numeric vector
#' @export
center <- function(X, cent = mean(X, na.rm = TRUE))
{
    return(X - cent)
}

#' @describeIn scale.center Scales a numeric vector
#' @export
scale <- function(X, scaling = sd(X, na.rm = TRUE))
{
    return(X / scaling)
}

#' @describeIn scale.center Center then scales
#' @export
scale.and.center <- function(X,
                             cent = mean(X, na.rm = TRUE),
                             scaling = sd(X, na.rm = TRUE))
{
    return(scale(center(X, cent), scaling))
}

#' @name nearly.equal
#' @title Equality for doubles
#' @description Checks for equality allowing for a margin of error
#' @param x numeric vector
#' @param y numeric vector
#' @param set numeric vector
#' @param tol Tolerance for equality i.e abs(x - y) < tol
NULL
#' @describeIn nearly.equal Check if two doubles are equal up to a given margin of error
#' @export
nearly.equal <- function(x, y, tol = .Machine$double.eps ^ .5)
{
    abs(x - y) < tol
}

#' @describeIn nearly.equal Check if a double are equal up to a given margin of error
#' @export
nearly.in.set <- function(x, set, tol = .Machine$double.eps ^ .5)
{
    if(is.null(set)) return(FALSE)
    any(sapply(set, nearly.equal, x = x, tol = tol))
}

random.distrib <- function(N, skew)
{
    P <- rgamma(N, skew)
    return(P / sum(P))
}

#' Random categorial
#'
#' Draws an arbitrary number of items from a categorical distribution
#' @seealso random.data.set
#' @param var List specifying the distribution. Fields 'name' is mandatory, then either a list of items in the field 'items' or a number of items in the fiels 'nvars' need to be specified. Finally the field 'replace' controls whether items can be drawn several time. If missing defaults to TRUE
#' @param set.size Size of the sample to be drawn
#' @export
random.cat.var <- function(var, set.size)
{
    var.name <- var$name
    items <- var$items

    if(is.null(items))
    {
        nvals <- var$nvars
        items <- 1:nvals
    }
    replace <- TRUE

    if(!is.null(var$replace)) replace <- var$replace

    V <- list(sample(items, set.size, replace = replace))
    names(V) <- var.name
    V
}
#' Indexes where match
#'
#' Returns the list of indices where X equals Y
#' @export
which.equals <- function(X, Y)
{
    if(is.factor(X)) X <- as.character(X)
    if(is.factor(Y)) Y <- as.character(Y)
    which(X == Y)
}

## Don't remember what it does
min.adist <- function(X,
                      Y,
                      max.dist = list(min.distance = 0, max.distance = 0.1, diff.distance = 1),
                      nkeep = 1,
                      value = FALSE,
                      ...)
{
    #assuming X as a length of 1
    X <- as.character(X)
    Y <- as.character(Y)
    ncx <- nchar(X)
    ncy <- nchar(Y)
    diff.dist <- abs(ncx - ncy)
    max.dist <- pmax(ncx, ncy)
    min.dist <- pmin(ncx, ncy)
    minf <- ifelse(is.null(max.dist$min), 0, max.dist$min)
    maxf <- ifelse(is.null(max.dist$max), 0, max.dist$max)
    difff <- ifelse(is.null(max.dist$diff), 0, max.dist$diff)

    maxd <- minf * min.dist + difff * diff.dist + maxf * max.dist
    D <- as.vector(adist(X, Y, ...))
    nkeep <- min(length(D), nkeep)
    if(nkeep == 0) return(list(indices = integer(0), edist = integer(0)))
    IK <- order(D)[1:nkeep]
    IK <- IK[ is.finite(D[IK]) & D[IK] <= maxd[IK] ]
    if(value) MK <- Y[IK]
    else MK <- IK
    return(list(matches = MK , edist = D[IK]))
}

#' Missing parameters
#'
#' Opposite of missing.
#' @export
defined <- function(x) !missing(x)

#' @name ranking.rating
#' @title Rankings ratings
#' @param X Numeric vector
#' @param fun.proj Function to project ratings to a different scale
#' @param filter Subset to the ratings to take the max from
#' @param rank.norm Rank to use to recenter ratings
#' @details Tools to rescale and recenter ratings. It is usually useful to rescale ratings according to the maximum value that occurs or according to the nth biggest value
NULL

#' @describeIn ranking.rating Applies a transformation and rescale the rankings so they now appear as a proportion of the biggest value
#' @export
rating.normalize <- function(X, fun.proj = exp, filter = TRUE)
{
    fun.proj(X) / fun.proj(max(X[as.logical(filter)], na.rm = TRUE))
}

#' @describeIn ranking.rating Recenter values so the a given rank has now a value of 0
#' @export
rank.normalize <- function(X, rank.norm)
{
    if(length(X) == 0) return(X)
    centering <- median(sort(X[[1]], decreasing = TRUE)[rank.norm], na.rm = TRUE)
    return(lapply(X, "-", centering))
}

#' @title Reading input from user
#' @name user.io
#' @param ... Forwarded to grepl
#' @param yes.value If the user input matches this pattern the function returns TRUE
#' @details User input terminates when the user presses 'Enter'
NULL

#' @describeIn user.io Suspends execution and return the user input
#' @export
get.keyboard.input <- function(...)
{
    io <- stdin()
    x <- readLines(io, 1)
    return(x)
}

#' @describeIn user.io Suspends execution and prompts the user to press 'Enter' to continue
#' @export
press.enter.to.continue <- function(...)
{
    cat("Press Enter to continue\n")
    get.keyboard.input()
}

#' @describeIn user.io Suspends execution. User must enter a pattern matching yes.value to continue
#' @export
y.to.continue <- function(yes.value = '^yes$', ...)
{
    grepl(yes.value, get.keyboard.input(), ...)
}

#uuid
#' Universally unique identifier
#'
#' Generates a UUID
#' @export
get.uuid <- function()
{
  uuid::UUIDgenerate()
}

#' Date Formatting
#'
#' Formats a date into the %Y%m%d format
#' @export
yyyymmdd <- function(date)
{
    if(is(date, "Date") || is(date, "POSIXt"))
    {
        return(format(date, "%Y%m%d"))
    }
    return(date)
}

#' Finding Sequences
#'
#' Finds sequences in a vector. Sequences are subset of consecutive items. Two items are consecutive if x[N + 1] - x[N] == increment
#' @param R vector
#' @param increment value
#' @param sort should the vector be sorted beforehand
#' @param unique are we only interested in unique occurences
#' @export
find.sequence <- function(R,
                          increment = 1L,
                          sort = FALSE,
                          unique = FALSE)
{
    if(unique) R <- unique(R)
    if(sort) R <- sort(R)
    N <- length(R)
    V <- R[-1] - R[-N] != increment
    return(list(from = R[c(TRUE, V)], to = R[c(V, TRUE)]))
}

#' Proportion
#'
#' Shortcut to computing q1 / (q1 + q1)
#' @export
proportion <- function(q1, q2)
{
    q1 / ( q1 + q2 )
}

#' Date Algebra
#'
#' Adds months years and days to a date object
#' @export
add.to.date <- function(date, y = 0, m = 0, d = 0, ...)
{
    date <- as.POSIXct(date)
    year(date) <- year(date) + y
    month(date) <- month(date) + m
    day(date) <- day(date) + d
    return(date)
}

#' Set algebra
#'
#' Performs the the difference between the union of two sets and the itersection of the same two sets.
#' @export
un.diff.int <- function(S1, S2)
{
    setdiff(union(S1, S2),intersect(S1, S2))
}

#' @title Memory usage
#' @name memory.usage
#' @description Tools to monitor memory usage
NULL

#' @describeIn memory.usage Outputs the memory usage of all R object in a given environment
#' @param LOBJS List of objects to evaluate
#' @param envir Environment to evaluate
#' @param all Should all objects be considered
#' @param nshow Only show the nshow biggest object
#' @export
memory.usage <- function(LOBJS = NULL,
                         envir = .GlobalEnv,
                         all = TRUE,
                         nshow = 10)
{
    if(is.null(LOBJS)) sort(sapply(ls(envir = envir, all.names = all), function(x) mb.object.size(get(x, envir = envir))), decreasing = TRUE)[1:nshow]
    else sort(sapply(LOBJS, mb.object.size), decreasing = TRUE)[1:N]
}

#' @describeIn memory.usage Gets the size of an object in MBs
#' @param x the object
#' @export
mb.object.size <- function(x)
{
    return(object.size(x) / 2 ^ 20)
}

#' Matrix::identity
#'
#' Identity for object of class Matrix
#' @export
Matrix.identity <- function(n)
{
    Diagonal(n)
}

#' Shift
#'
#' Function to shift an object up down left or right
#' @param X object to be shifted
#' @param ... shifting parameters
#' @export
setGeneric("shift", function(X, ...) standardGeneric("shift"))

#' Call to System
#'
#' Calls 'system' and returns an error if the call failed
#' @param command Command to be executed
#' @param error.message Message in case the system call fails
#' @param safe Should errors be caught?
#' @export
safe.system <- function(command,
                        error.message = paste("Error running command:", command),
                        safe = TRUE,
                        ...)
{
    err <- system(command, ...)
    if(err != 0 & safe) stop(error.message)
}

## Deep assignation of retrieve in list

#' List assignment
#'
#' Request or assigns values deep in the list at any arbitrary depth.
#' @param L the list
#' @param keys Vector of the keys. A path in the tree formed in the
#' @param value Optional value to be assigned
#' @examples L = deep.list(list(), c("A", "B", "C"), 1)
#' @examples deep.list(L, c("A", "B", "C"))
#' @export
deep.list <- function(L = list(),
                      keys,
                      value)
{
    if(length(keys) == 1)
    {
        if(missing(value)) return(L[[keys]])
        L[[keys]] <- value
        return(L)
    }
    if(missing(value)) return(deep.list(L[[keys[1]]], keys[-1]))
    L[[keys[1]]] <- as.list(deep.list(L[[keys[1]]], keys[-1], value))
    L
}

#' Rep for list
#'
#' Repeats any object N times into a list
#' @usage rep.list(x, N)
#' @export rep.list
rep.list <- function(x,
                     N)
{
    R <- list()
    R[1:N] <- list(x)
    R
}

#' Indices of a Vector
#'
#' Returns the indices of a vector. Either 1:N if N > 0 or integer(0).
#' @param X vector
#' @param N length of the vector
#' @export
indexation <- function(X,
                       N = length(X))
{
    if(length(N) == 0)
        integer(0)
    else if(N > 0)
        1:N
    else
        integer(0)
}


#' Names a list
#'
#' @param L Either a list or a vector that will be put in a singleton
#' @param name Name to be used
#' @usage put.names(L, name = "x", ...)
#' @export put.names
put.names <- function(L,
                      name = "x",
                      ...)
{
    if(!is.list(L)) L <- list(L)
    names(L) <- name
    L
}

#' Removing Last Element
#'
#' Removes the last element of a list or vector
#' @export
remove.last <- function(X)
{
    X[-length(X)]
}

#' Try Catch With Switch To Enable / Disable
#'
#' One may want to disable Try / Catch to see what causes an error. This function is a simple wrapper around \code{tryCatch} to do so.
#' @param expr Expression to execute
#' @param ... Arguments to be forwarded to tryCatch
#' @param dev.mode If set to true won't try to catch any error
#' @export
try.catch <- function(expr,
                      ...,
                      dev.mode = FALSE)
{
    if(dev.mode)
        expr
    else
        tryCatch(expr,
                 ...)
}

#' Global To Local Buffer
#'
#' Transform a global Buffer into a local buffer to process 0. Mostly for testing purposes.
#' @param obj object to gather
#' @export
setGeneric("to.local", function(obj) obj)
