#' Matrix Equality Expectation
#'
#' Tests whether two matrix represent the same data by converting them to a triplet representation.
#' @details Calls to \code{expect_equal} yield to unexpected results. If the goal of the comparison is only to check whether the conceptual matrices represented by M1 and M2 are identical this does the trick.
#' @param M1 Matrix of any form that can be converted to a triplet with function \code{mat.to.triplet}
#' @param M2 DITTO
#' @param compare.dimnames Should dimnames be compared as well
#' @param tol Tolerance for equality
#' @param agg Should triplet representation that have several entries for the same coordinates be aggregated
#' @param ... Not used
#' @export
expect_equal_matrices <- function(M1,
                                  M2,
                                  compare.dimnames = TRUE,
                                  tol = .Machine$double.eps,
                                  agg = TRUE,
                                  ...)
{
    if(!is(M1, "genMatrix"))
        M1 <- as.matrix(M1)
    
    if(!is(M2, "genMatrix"))
        M2 <- as.matrix(M2)
    
    testthat::expect_equal(dim(M1), dim(M2))
    if(compare.dimnames)
        testthat::expect_equal(dimnames(M1), dimnames(M2))
    
    if(agg)
    {
        MT1 <- mat.to.triplet(M1)[, .(x = sum(x)), by = list(i, j)][order(i, j, x)][is.na(x) | abs(x) >= tol]
        MT2 <- mat.to.triplet(M2)[, .(x = sum(x)), by = list(i, j)][order(i, j, x)][is.na(x) | abs(x) >= tol]
    }
    else
    {
        MT1 <- mat.to.triplet(M1)[order(i, j, x)][is.na(x) | abs(x) >= tol][, .(i, j, x)]
        MT2 <- mat.to.triplet(M2)[order(i, j, x)][is.na(x) | abs(x) >= tol][, .(i, j, x)]
    }

    testthat::expect_equal(as.data.frame(MT1), as.data.frame(MT2))
}
