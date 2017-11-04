library(Matrix)
library(hugesparse)
context("Matrix operations")

test_that("trim.cov.matrix works",
{
    N <- 100L
    S <- random.cov.matrix(N)
    tol <- .2
    SE <- trim.cov.matrix(S,
                          tol = tol)
    D <- Matrix::Diagonal(x = sqrt(Matrix::diag(S)))
    D1 <- Matrix::Diagonal(x = 1 / sqrt(Matrix::diag(S)))
    C <- D1 %*% S %*% D1
    C[abs(C) < tol] <- 0
    ST <- D %*% C %*% D
    expect_equal_matrices(ST, SE)
})
