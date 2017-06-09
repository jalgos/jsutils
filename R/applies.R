#' Over
#'
#' Applies a bivariate function recursively to a list and the result of the previous call.
#' @param L List of objects
#' @param FUN Function to be recursively applied
#' @param default.value The result of the function in case the list provided is empty
#' @return FUN(X[1], FUN(X[2], FUN(. . ., FUN(X[N-1], X[N]), `...`), . . .), `...`), `...`). If the list has only one element it will return X[1]
#' @details The first to argument of the function should be of type of the object in the list provided.
#' @examples \code{D = data.table(x = 1:10, u = letters[1:10])}
#' \code{over(list(D, D, D, D), rbind)}
#' @export
over <- function(L, FUN, default.value = NULL, ...)
{
    if(length(L) == 0) return(default.value)
    if(is.character(FUN)) FUN = getMethod(FUN)
    
    N <- length(L)
    R <- L[[N]]
    if(N == 1) return (R)

    for(i in (N - 1):1) R <- FUN(L[[i]], R, ...)
    
    return(R)
}

#' Opposite
#'
#' Returns the opposite function
#' @export
neg.fun <- function(FUN) function(...) -FUN(...)

#' Simple Apply
#'
#' Applies f to X and ...
#' @seealso do.call
#' @export
apply.fun <- function(f, X, ...) f(X, ...)

gridl <- function(log = NULL, ...)
{
    grid(list(...)$nx, list(...)$ny)
}

#' @name cartesian.apply
#' @title Cartesian Apply
#' @details To be used when a function needs to be applied to all the combinations of a set of lists of parameters
#' @param L1 A list of arguments
#' @param L2 A list of arguments
#' @param funapp Function to apply to the combinations of L1 and L2 members
#' @param ... Additional parameters to be passed on to funapp
#' @param fapply First apply function to use
#' @param fapply2 Second apply function to use
#' @param unlist Should the result be unlisted
#' @param Largs List of arguments to produce the grid on which the function will be applied
NULL

#' @describeIn cartesian.apply Applies a function to all the combination in L1 and L2
#' @export
cart.apply <- function(L1,
                       L2 = L1,
                       funapp,
                       ...,
                       fapply = lapply,
                       fapply2 = fapply,
                       unlist = FALSE)
{
    CAR <- fapply(L1, function(i1, L2, ...)
    {
        fapply2(L2, function(x, y, funapp, ...)
        {
            funapp <- match.fun(funapp)
            funapp(x, y, ...)
        }, x = i1, ...)
    },
    L2 = L2, funapp = funapp, ...)
    if(unlist) base::unlist(CAR, recursive = FALSE)
    else CAR
}

#' @describeIn cartesian.apply Applies when we want to compute f(xi, xj) only when i <= j
#' @export
half.cart.apply <- function(L1,
                            L2 = L1,
                            funapp,
                            ...,
                            fapply = lapply,
                            fapply2 = fapply,
                            unlist = FALSE)
{
    if(length(L1) != length(L2)) stop("In half.cart.apply both arguments must have the same size")
    N <- length(L1)
    if(N > 0) LI <- 1:N
    else LI <- numeric(0)
    CAR <- fapply(LI, function(i, ...) fapply2(i:N, function(j, ...) funapp(L1[[i]], L2[[j]], ...), ...), ...)
    if(unlist) base::unlist(CAR)
    else CAR
}

#' @describeIn cartesian.apply Applies a function to the cartesian product of an arbitrary number of lists. Uses a data.table to do so
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
    DT[, do.call(funapp, c(.BY, ...)), by = c(names(DT))]
}
