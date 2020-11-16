

#' @name non.finite
#' @title Handling non finite values
#' @param X Numeric vector
#' @param Y Numeric vector
NULL

#' @describeIn non.finite Sum where non finite values are replaced with 0. Returns X + Y where non finite values in both vectors are replaced with 0
#' @export
safe.sum <- function(X,
                     Y,
                     finite.fun = is.finite,
                     replacement.value = 0)
{
    X[!finite.fun(X)] <- replacement.value
    Y[!finite.fun(Y)] <- replacement.value
    X + Y
}

#' @describeIn non.finite Averages several vectors ommiting non finite values
#' @param ... Numeric vectors of equal length
#' @param finite.fun Function that return TRUE if a value is finite
#' @export
safe.mean <- function(...,
                      finite.fun = is.finite)
{
    N <- -1
    for(X in list(...))
    {
        if(N == -1)
        {
            N <- length(X)
            nobs <- rep(0,N)
            sums <- rep(0,N)
        }
        if(length(X) != N) stop("one item as a different size")
        goodX <-  finite.fun(X)
        nobs <- nobs + goodX
        sums[goodX] <- sums[goodX] + X[goodX]
    }
    return(sums / nobs)
}

#' @describeIn non.finite Gets the max of a numeric vector without warnings
#' @export
safe.max <- function(X, ...)
{
    #Max without warning
    if(na.rm) X <- X[!is.na(X)]
    if(length(X) == 0) return(NaN)
    max(X)
}

#' @describeIn non.finite Gets the min of a numeric vector without warnings
#' @export
safe.min <- function(X, ...)
{
    #Min without warning
    if(na.rm) X <- X[!is.na(X)]
    if(length(X) == 0) return(NaN)
    min(X)
}

#' @describeIn non.finite Computes the variance on the filtered data
#' @param var.fun Variance function to be used
#' @export 
safe.var <- function(X,
                     Y = X,
                     var.fun = var,
                     ...)
{
    F <- outlier.filter(X, ...)
    var.fun(X[F], Y[F], na.rm = TRUE)
}

#' @describeIn non.finite Computes the correlation on the filtered data
#' @export 
safe.cor <- function(X,
                     Y,
                     ...)
{
    F <- is.finite(X + Y) & outlier.filter(X, ...)
    cor(X[F], Y[F])
}


#' @describeIn non.finite Replace non finite entries
#' @export
replaceNA <- function(X, Y = 0, finite.fun = is.finite)
{
    Y <- rep(Y, length.out = length(X))
    goodX <- finite.fun(X)
    X[!goodX] <- Y[!goodX]
    return(X)
}

#' @describeIn non.finite Returns a logical vector that filters out data outside of the range defined by the given quantile
#' @param qt Proportion of the distribution to filter out. Removes the same amount in both ends.
#' @param down.qt Proportion to ommit in the lower tail
#' @param up.qt Proportion to ommit in the upper tail
#' @export
outlier.filter <- function(X,
                           qt = 0.01,
                           down.qt = qt / 2,
                           up.qt = 1 - qt / 2,
                           ...)
{
    qts <- quantile(X, c(down.qt, up.qt), na.rm = TRUE)
    X %between% qts
}

#' Sum on a subset
#'
#' Sums two vectors on a subset of the indices
#' @export
filter.sum <- function(X, Y, filter)
{
    Z <- X
    Z[filter] <- Z[filter] + Y
    return(Z)
}

##is.finite fails in case prod(dims) is passed the integer bound

#' Non Finite Coefficients
#'
#' Returns the number of non finite entries in a matrix
#' @param M the matrix
#' @usage sum.non.finite(M)
#' @export sum.non.finite
sum.non.finite <- function(M) nrow(mat.to.triplet(M)[!is.finite(x)])

#' Non finite elements
#' @export
setGeneric("non.finite.elements", function(x) sum(!is.finite(x)))

#' @rdname non.finite.elements
#' @export
setMethod("non.finite.elements", c(x = "sparseMatrix"), function(x) non.finite.elements(x@x))


#' Check Finiteness For All Types
#'
#' Returns true for each entry if x is finite. Can take factors and characters as well
#' @param x vector to check
#' @export gen.is.finite
gen.is.finite <- function(x)
{
    if(is.factor(x) || is.character(x))
        return(!is.na(x))

    return(is.finite(x))
}
