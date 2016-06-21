#' Matrix Equality Expectation
#'
#' Tests whether two matrix represent the same data by converting them to a triplet representation.
#' @details Calls to \code{expect_equal} yield to unexpected results. If the goal of the comparison is only to check whether the conceptual matrices represented by M1 and M2 are identical this does the trick.
#' @param M1
#' @param M2
#' @param compare.dimnames Should dimnames be compared as well
#' @export
expect_equal_matrices <- function(M1,
                                  M2,
                                  compare.dimnames = TRUE,
                                  tol = .Machine$double.eps)
{
    testthat::expect_equal(dim(M1), dim(M2))
    if(compare.dimnames) testthat::expect_equal(dimnames(M1), dimnames(M2))
    testthat::expect_equal(mat.to.data.table(M1)[order(i, j, x)][abs(x) >= tol],
                           mat.to.data.table(M2)[order(i, j, x)][abs(x) >= tol])
}
