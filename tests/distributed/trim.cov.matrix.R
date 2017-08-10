test.trim.cov.matrix <- function()
{
    jlog.info(ltester, "Testing trim cov matrix")
    N <- 1000L
    tol <- .2
    S <- random.cov.matrix(N)
    S <- bcast(S)
    DS <- distributedhugesparse::as.DHM(S)
    TS <- trim.cov.matrix(S,
                          tol = tol)
    DTS <- trim.cov.matrix(DS,
                           tol = tol)
    expect_true(is(DS, "DHugeMatrix"))
    expect_equal_matrices(TS, DTS[])
    jlog.info(ltester, "Testing trim cov matrix. Passed!")
}

test.trim.cov.matrix()
